module Llist = struct
  type 'a marked_ptr = bool * 'a node option
  and 'a t = { compare : 'a -> 'a -> int; t : 'a marked_ptr Atomic.t }
  and 'a node = { key : 'a; next : 'a marked_ptr Atomic.t }

  type 'a local = {
    prev : (bool * 'a node option) Atomic.t;
    curr : bool * 'a node option;
    next : bool * 'a node option;
  }

  let init ?(compare = Stdlib.compare) () : 'a t =
    { compare; t = Atomic.make (false, None) }

  (* Warning on find *)
  let find compare key t : bool * 'a local =
    let rec try_again () =
      let local = { prev = t; curr = Atomic.get t; next = (false, None) } in
      let rec loop local =
        match local.curr with
        | _pmark, None -> (false, local)
        | _pmark, Some { key = ckey; next = ato_next } ->
            let next = Atomic.get ato_next in
            let local = { local with next } in
            if Atomic.get local.prev != local.curr then
              (* Another domain has changed the current node *)
              try_again ()
            else if not (fst next) then
              let comp = compare ckey key in
              if comp >= 0 then (comp = 0, local)
              else
                let local =
                  { prev = ato_next; curr = next; next = (false, None) }
                in
                loop local
            else if
              (* the current node has been marked for deletion
                 -> we try to remove it now *)
              Atomic.compare_and_set local.prev local.curr
                (false, snd local.next)
            then
              (* it worked ! great, let's continue searching our key ! *)
              let local =
                {
                  local with
                  curr = (false, snd local.next);
                  next = (false, None);
                }
              in
              loop local
            else
              (* Another domain removed the marked node before us, we
                 need to begin again *)
              try_again ()
      in
      loop local
    in
    let res = try_again () in
    res

  let unsafe_insert compare (key : 'a) t : bool * 'a local =
    let rec loop () =
      let is_found, local = find compare key t in
      if is_found then (* key already in the list *)
        (false, local)
      else
        (* not in ! We can add it ! [t.curr] has been set up by [find] to
           be the next node : we have everything to create our new node. *)
        let node = { key; next = Atomic.make local.curr } in
        (* let's try to add it! *)
        if Atomic.compare_and_set local.prev local.curr (false, Some node) then
          (true, local)
        else loop ()
    in
    loop ()

  let insert (key : 'a) (t : 'a t) = fst (unsafe_insert t.compare key t.t)

  let unsafe_delete compare (key : 'a) t =
    let rec loop () =
      let is_found, local = find compare key t in
      if not is_found then (false, local)
      else
        let curr = local.curr |> snd |> Option.get in
        if
          not
            (Atomic.compare_and_set curr.next local.next (true, snd local.next))
        then loop ()
        else if Atomic.compare_and_set local.prev local.curr local.next then
          (true, local)
        else (
          ignore (find compare key t);
          (true, local))
    in
    loop ()

  let delete (key : 'a) (t : 'a t) = fst (unsafe_delete t.compare key t.t)

  let mem key t =
    let is_found, _ = find t.compare key t.t in
    is_found
end

let reverse x =
  let x = x land 0xff_ff_ff_ff in
  let x = ((x land 0xaa_aa_aa_aa) lsr 1) lor ((x land 0x55_55_55_55) lsl 1) in
  let x = ((x land 0xcc_cc_cc_cc) lsr 2) lor ((x land 0x33_33_33_33) lsl 2) in
  let x = ((x land 0xf0_f0_f0_f0) lsr 4) lor ((x land 0x0f_0f_0f_0f) lsl 4) in
  let x = ((x land 0xff_00_ff_00) lsr 8) lor ((x land 0x00_ff_00_ff) lsl 8) in
  (x lsr 16) lor (x lsl 16) land 0xffffffff

let compute_hkey k = reverse k lor 0x00_00_00_01

module Htbl = struct
  type 'b kind = Dummy | Regular of 'b

  type 'a t = {
    (* total item count *)
    count : int Atomic.t;
    (* current table size *)
    size : int Atomic.t;
    (* maximum number of items in a bucket (on average)  *)
    max : int;
    (* pointers to the linked list *)
    buckets : (int * 'a kind) Llist.marked_ptr Atomic.t array;
  }

  let compare (a, _) (b, _) = Stdlib.compare a b

  let new_dummy_node id llist =
    let new_key = reverse id in
    let dummy_node = (new_key, Dummy) in
    let is_inserted, local = Llist.unsafe_insert compare dummy_node llist in

    if not is_inserted then
      let (_, local) : bool * 'a Llist.local =
        Llist.find compare (new_key, Dummy) llist
      in
      local.prev
    else local.prev

  let init size =
    let llist = Llist.init ~compare () in
    {
      count = Atomic.make 0;
      size = Atomic.make size;
      buckets = Array.init size (fun id -> new_dummy_node id llist.t);
      max = 3;
    }

  (** unset most significant turn on bit *)
  let get_parent id =
    let a = id lor (id lsr 1) in
    let a = a lor (a lsr 2) in
    let a = a lor (a lsr 4) in
    let a = a lor (a lsr 8) in
    let a = a lor (a lsr 16) in
    (a lsr 1) land id

  let rec init_bucket t id =
    let parent_id = get_parent id in
    match Atomic.get t.buckets.(parent_id) with
    | _, None -> init_bucket t parent_id
    | _ ->
        let new_node = new_dummy_node id t.buckets.(parent_id) in
        t.buckets.(id) <- new_node

  let get_bucket id t =
    let bucket = id land (Atomic.get t.size - 1) in
    (match Atomic.get t.buckets.(bucket) with
    | _, None -> init_bucket t bucket
    | _, _ -> ());
    bucket

  let insert id value t =
    let bucket = get_bucket id t in

    let new_node = (compute_hkey id, Regular value) in
    let is_inserted, _local =
      Llist.unsafe_insert compare new_node t.buckets.(bucket)
    in
    if not is_inserted then false
    else
      (* to change when resizable hash table*)
      let prev_count = Atomic.get t.count in
      Atomic.incr t.count;
      if prev_count + 1 > Atomic.get t.size * t.max then failwith "too big.";
      true

  let find id t =
    let bucket = get_bucket id t in
    let key = compute_hkey id in
    let is_found, local = Llist.find compare (key, Dummy) t.buckets.(bucket) in
    if not is_found then None
    else
      match local.curr with
      | _, Some { key = _, Regular k; _ } -> Some k
      | _, _ -> failwith "Should not happen"

  let mem id t =
    let bucket = get_bucket id t in
    let key = compute_hkey id in
    let is_found, _ = Llist.find compare (key, Dummy) t.buckets.(bucket) in
    is_found

  let delete id t =
    let bucket = get_bucket id t in
    let key = compute_hkey id in
    let is_deleted, _ =
      Llist.unsafe_delete compare (key, Dummy) t.buckets.(bucket)
    in
    if not is_deleted then false
    else (
      Atomic.decr t.count;
      true)
end
