(* Notes:
   - Local deque does not have to synchronize with the writer, only other readers.
   - Tail index marks the "logical tail", physical one is the cell with oldest Value.
   - Local dequeue and enqueue are quite close to wait freedom. Enqueuer can simply
   check whether the slot it will get is free before incrementing tail, dequeuer can
   just roll back after a few spins. I don't think it makes sense to implement that
   in the general case though, as we don't actually want to return in those cases.
   - Some accesses do not have to be atomic but Atomic.t does not have unsafe
   methods. Probably doesn't matter on x86.
*)

type 'a t = {
  head : int Atomic.t;
  mask : int;
  size_exponent : int;
  array : 'a node option Atomic.t Array.t;
  tail : int Atomic.t;
  help_ind : int Atomic.t;
  help_arr : 'a help Atomic.t Array.t;
  help_mask : int;
}

and 'a node = { ind : int option; value : 'a }
and 'a help = { free : bool; v : 'a option }

let create ~size_exponent () =
  let size = Int.shift_left 1 size_exponent in
  let help_size = 128 in
  {
    head = Atomic.make 0;
    mask = size - 1;
    tail = Atomic.make 0;
    size_exponent;
    array = Array.init size (fun _ -> Atomic.make None);
    help_ind = Atomic.make 0;
    help_arr =
      Array.init help_size (fun _ -> Atomic.make { free = true; v = None });
    (* size should be >= max number of domains (or more precisely, stealers) *)
    help_mask = help_size - 1;
  }

let indicative_size { head; tail; _ } =
  max (Atomic.get tail - Atomic.get head) 0

module Local = struct
  let rec book_helping_cell ({ help_ind; help_arr; help_mask; _ } as t) : int =
    let help_ind_val = Atomic.fetch_and_add help_ind 1 in
    let help_cell = help_arr.(help_ind_val land help_mask) in
    let help_cell_val = Atomic.get help_cell in
    match help_cell_val with
    | { free = true; v = None } ->
        if
          Atomic.compare_and_set help_cell help_cell_val
            { free = false; v = None }
        then help_ind_val
        else book_helping_cell t
    | { free = false; _ } ->
        (* not free ! As each domain can only have one cell, it should not be a blocking issue *)
        book_helping_cell t
    | { free = true; _ } -> assert false

  let rec push ({ tail; head; mask; array; _ } as t) element =
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    let size = mask + 1 in
    let current_size = tail_val - head_val in
    assert (current_size <= size);
    if current_size = size then false
    else
      let index = tail_val land mask in
      let cell = Array.get array index in
      (match Atomic.get cell with
      | None -> ()
      | Some _ -> help_steal t tail_val);
      Atomic.set cell (Some { value = element; ind = None });
      Atomic.set tail (tail_val + 1);
      true

  and help_steal { mask; array; help_arr; _ } tail_val : unit =
    let cell_index = tail_val land mask in
    let cell = Array.get array cell_index in
    match Atomic.get cell with
    | None -> () (* stealer has finished stealing *)
    | Some { value; ind } as node -> (
        match ind with
        | None -> () (* stealer has finished stealing *)
        | Some hind ->
            if
              Atomic.compare_and_set help_arr.(hind) { free = false; v = None }
                { free = false; v = Some value }
            then
              (* value to be stolen is stocked in the help array,
                   we can try to free the cell *)
              ignore (Atomic.compare_and_set cell node None)
              (* either it works and the stealer can retrieve the value
                 in the help array or this CAS did not work, which means
                 the stealer has done its jobs -> both case are Ok. *)
            else () (* stealer has finished stealing *))

  and steal_one ({ head; tail; mask; array; help_arr; help_mask; _ } as t) =
    (* assumes there's space in the queue *)
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    let size = tail_val - head_val in
    if size < 1 then raise Exit
    else
      let help_ind = book_helping_cell t land help_mask in
      let new_head_val = head_val + 1 in
      let acquired_item = Atomic.compare_and_set head head_val new_head_val in
      if not acquired_item then (
        assert (
          Atomic.compare_and_set help_arr.(help_ind) { free = false; v = None }
            { free = true; v = None });
        raise Exit)
      else
        let index = head_val land mask in
        let cell = Array.get array index in
        let node = Atomic.get cell in
        if Atomic.compare_and_set cell node None then (
          (* free the help cell *)
          Atomic.set help_arr.(help_ind) { free = true; v = None };
          let { value; _ } = Option.get node in
          value)
        else
          match Atomic.get help_arr.(help_ind) with
          | { free = false; v } ->
              assert (Option.is_some v);
              (* free the help cell *)
              Atomic.set help_arr.(help_ind) { free = true; v = None };
              Option.get v
          | _ -> assert false

  and steal ~from ({ mask = local_mask; _ } as local) =
    (* need to be careful with from queue, which is not local *)
    let local_size = local_mask + 1 in
    let ({
           head = from_head;
           tail = from_tail;
           mask = from_mask;
           array = from_array;
           help_mask = from_help_mask;
           help_arr = from_help_arr;
           _;
         }
          : 'a t) =
      from
    in
    (* assumes there's space in the queue *)
    let from_tail_val = Atomic.get from_tail in
    let from_head_val = ref (Atomic.get from_head) in
    let from_size = from_tail_val - !from_head_val in
    if from_size < 1 then 0
    else
      let from_help_ind = book_helping_cell from land from_help_mask in
      let max_to_steal =
        (* steal even if there's a single element, thus *+1* *)
        min ((from_size + 1) / 2) local_size
      in
      let stolen = ref 0 in
      let break = ref false in

      while (not !break) && !stolen < max_to_steal do
        let new_head_val = !from_head_val + 1 in
        let acquired_item =
          Atomic.compare_and_set from_head !from_head_val new_head_val
        in

        if not acquired_item then (

          if not (Atomic.compare_and_set
                    from_help_arr.(from_help_ind)
                    { free = false; v = None } { free = true; v = None }) then
          failwith "steal issue #1";
          break := true)
        else (
          if not (from_tail_val - !from_head_val >= 0) then failwith "steal issue #2";
          let from_index = !from_head_val land from_mask in
          let cell = Array.get from_array from_index in
          let node = Atomic.get cell in
          let value = (Option.get node).value in
          let to_push =
            if Atomic.compare_and_set cell node None then (
              (* Steal succeeds. We can now free the help cell *)
              Atomic.set from_help_arr.(from_help_ind) { free = true; v = None };
              value)
            else
              (* Producer has finished the steal *)
              match Atomic.get from_help_arr.(from_help_ind) with
              | { free = false; v } ->
                  (* free the help cell *)
                  Atomic.set
                    from_help_arr.(from_help_ind)
                    { free = true; v = None };
                  Option.get v
              | _ -> failwith "steal issue #3"
          in
          incr stolen;
          let pushed = push local to_push in
          (* What should we do if `push` has failed ? *)
          if not pushed then failwith "steal issue #4";
          incr from_head_val)
      done;
      !stolen

  let pop { head; tail; mask; array; _ } : 'a option =
    (* dequeue is optimistic because it can use the fact that
       there is no enqueuer *)
    let old_head_val = Atomic.fetch_and_add head 1 in
    let tail_val = Atomic.get tail in
    assert (old_head_val - tail_val <= 0);
    if old_head_val = tail_val then (
      (* nobody else would speculate *)
      (* CAS here is not necessary but leaving it it for assertion.
         Can be removed once we're confident. *)
      assert (Atomic.compare_and_set head (old_head_val + 1) old_head_val);
      (* Atomic.set head index; *)
      None)
    else
      let index = old_head_val land mask in
      let cell = Array.get array index in
      let item = Atomic.get cell in
      assert (Option.is_some item);
      assert (Atomic.compare_and_set cell item None);
      Some (Option.get item).value

  (* successfuly rolled back *)
  let is_empty_thorough { head = _; tail; mask; array; _ } : bool =
    let size = Array.length array in
    let tail_value = Atomic.get tail in
    let seen_not_free = ref false in
    let i = ref (size - 1) in
    while (not !seen_not_free) && !i >= 0 do
      let cell = Array.get array ((tail_value + !i) land mask) in
      seen_not_free := Atomic.get cell != None;
      i := !i - 1
    done;
    not !seen_not_free

  let is_empty queue =
    let { head; tail; _ } = queue in
    let tail_val = Atomic.get tail in
    let head_val = Atomic.get head in
    if tail_val - head_val != 0 then false else is_empty_thorough queue
end

module Resizable = struct
  type nonrec 'a t = 'a t Atomic.t

  module Local = struct
    let resize (t : 'a t) : unit =
      let t_val = Atomic.get t in
      let { size_exponent; _ } = Atomic.get t in
      let new_t_val = create ~size_exponent:(size_exponent + 1) () in
      (* transfer all elements *)
      let finished = ref false in
      while not !finished do
        match Local.pop t_val with
        | None -> finished := true
        | Some item -> assert (Local.push new_t_val item)
      done;
      (* save the new queue *)
      Atomic.set t new_t_val

    let rec push_with_autoresize (t : 'a t) (element : 'a) =
      let t_val = Atomic.get t in
      if Local.push t_val element then ()
      else (
        resize t;
        push_with_autoresize t element)

    let push t item = Local.push (Atomic.get t) item
    let pop t = Local.pop (Atomic.get t)
    let steal ~from t = Local.steal ~from:(Atomic.get from) (Atomic.get t)
    let steal_one t = Local.steal_one (Atomic.get t)
    let is_empty t = Local.is_empty (Atomic.get t)
  end

  let create ~size_exponent () = Atomic.make (create ~size_exponent ())
end

module M = struct
  (*
     Fitted into the module type expected by Domainslib.
     Note stealing now only takes 1 element.
  *)

  let create () = Resizable.create ~size_exponent:5 ()
  let push : 'a Resizable.t -> 'a -> unit = Resizable.Local.push_with_autoresize

  let pop t =
    match Resizable.Local.pop t with Some v -> v | None -> raise Exit

  let steal = Resizable.Local.steal_one
end
