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
        | _pmark, Some { key = ckey; next = atomic_next } ->
            let next = Atomic.get atomic_next in
            let local = { local with next } in
            if Atomic.get local.prev != local.curr then
              (* Another domain has changed the current node *)
              try_again ()
            else if not (fst next) then
              (* not a node that need to be deleted : we check if we got the right key *)
              let comp = compare ckey key in
              if comp >= 0 then (comp = 0, local)
                (* if comp = 0, this is the right key, else that means
                   the key is not here as the list is sorted *)
              else
                let local =
                  { prev = atomic_next; curr = next; next = (false, None) }
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

  let unsafe_add compare (key : 'a) t : bool * 'a local =
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

  let add (key : 'a) (t : 'a t) = fst (unsafe_add t.compare key t.t)

  let unsafe_remove compare (key : 'a) t =
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

  let remove (key : 'a) (t : 'a t) = fst (unsafe_remove t.compare key t.t)

  let mem key t =
    let is_found, _ = find t.compare key t.t in
    is_found
end

module Common = struct
  (* Key value used in the linked list are computed with [reverse] for the
        dummy node and with [compute_hkey] for the regular node.

     Example of a linked list, written in base 2, simplified to 8 bits
     integer. Only key are written, as values of regular nodes do not
     change the linked list order.

      [ 0000 0000 (0, dummy) ]
     -> [ 0001 0001 (8, regular)]
     -> [ 0100 0000 (2, dummy)]
     -> [ 0101 0001 (10, regular)]
     -> [ 0110 1001 (22, regular)]
     -> [ 1000 0000 (1, dummy)]
     -> [ 1000 1001 (17, regular)]
     -> [ 1100 0000 (3, dummy)]
  *)
  let reverse x =
    (* works for int32 *)
    let x = x land 0xff_ff_ff_ff in
    let x = ((x land 0xaa_aa_aa_aa) lsr 1) lor ((x land 0x55_55_55_55) lsl 1) in
    let x = ((x land 0xcc_cc_cc_cc) lsr 2) lor ((x land 0x33_33_33_33) lsl 2) in
    let x = ((x land 0xf0_f0_f0_f0) lsr 4) lor ((x land 0x0f_0f_0f_0f) lsl 4) in
    let x = ((x land 0xff_00_ff_00) lsr 8) lor ((x land 0x00_ff_00_ff) lsl 8) in
    (x lsr 16) lor (x lsl 16) land 0xffffffff

  let compute_hkey k = reverse k lor 0x00_00_00_01

  (** unset most significant turn on bit (for int32) *)
  let unset_msb key =
    let a = key lor (key lsr 1) in
    let a = a lor (a lsr 2) in
    let a = a lor (a lsr 4) in
    let a = a lor (a lsr 8) in
    let a = a lor (a lsr 16) in
    (a lsr 1) land key

  let compare (a, _) (b, _) = Stdlib.compare a b

  type 'b kind = Dummy | Regular of 'b
  type key = int

  let new_dummy_node id llist =
    let new_key = reverse id in
    let dummy_node = (new_key, Dummy) in
    let _, local = Llist.unsafe_add compare dummy_node llist in
    (* If the insertion succeeded ([is_added] = true) then [local.prev] contains the
       atomic value of the new node.
       If it failed that means another domain has inserted it. In this case,
       [local.curr] also contains the value we seek as the [Llist.find] function
       calls by [Llist.unsafe_add] will have stopped at the right place. *)
    Atomic.get local.prev
end

module Htbl = struct
  open Common

  type 'a t = {
    (* current number of buckets (minus 1) *)
    mask : int;
    (* pointers to the linked list *)
    buckets : (key * 'a kind) Llist.marked_ptr Atomic.t array;
  }

  let init ~size_exponent =
    let size = Int.shift_left 1 size_exponent in
    let mask = size - 1 in
    let llist = Llist.init ~compare () in
    {
      mask;
      buckets =
        Array.init size (fun key -> Atomic.make (new_dummy_node key llist.t));
    }

  (** [init_bucket buckets ind] inits a bucket and all its parents if needed *)
  let rec init_bucket buckets ind =
    let parent_ind = unset_msb ind in
    let parent_bucket = buckets.(parent_ind) in
    (* initialize parents recursively if needeed *)
    (if parent_ind <> ind then
     match Atomic.get parent_bucket with
     | _, None -> init_bucket buckets parent_ind
     | _ -> ());

    (* initialize current node *)
    let new_node = new_dummy_node ind parent_bucket in
    (* would Atomic.CAS be better ? *)
    Atomic.set buckets.(ind) new_node

  (** [get_bucket_id key buckets mask] searches the bucket's index corresponding
      to [key] (= [key mod t.size]), initializes if needed and returns
      it. *)
  let get_bucket_ind key buckets mask =
    let ind = key land mask in
    (match Atomic.get buckets.(ind) with
    | _, None -> init_bucket buckets ind
    | _, _ -> ());
    ind

  (** [add key value t] *)
  let add key value { buckets; mask; _ } =
    let bucket_ind = get_bucket_ind key buckets mask in
    let new_node = (compute_hkey key, Regular value) in

    let is_added, _local =
      Llist.unsafe_add compare new_node buckets.(bucket_ind)
    in
    is_added

  let find key { buckets; mask; _ } =
    let bucket = get_bucket_ind key buckets mask in
    let key = compute_hkey key in
    let is_found, local = Llist.find compare (key, Dummy) buckets.(bucket) in
    if not is_found then None
    else
      match local.curr with
      | _, Some { key = _, Regular k; _ } -> Some k
      | _, _ -> failwith "Should not happen"

  let mem key { buckets; mask; _ } =
    let bucket = get_bucket_ind key buckets mask in
    let key = compute_hkey key in
    let is_found, _ = Llist.find compare (key, Dummy) buckets.(bucket) in
    is_found

  let remove key { buckets; mask; _ } =
    let bucket = get_bucket_ind key buckets mask in
    let key = compute_hkey key in
    let is_removed, _ =
      Llist.unsafe_remove compare (key, Dummy) buckets.(bucket)
    in
    is_removed
end

module Htbl_resizable = struct
  open Common

  (* pointers to a node in linked list *)
  type 'a bucket = (key * 'a kind) Llist.marked_ptr Atomic.t
  type 'a segment = 'a bucket array

  type 'a t = {
    (* total item count *)
    count : int Atomic.t;
    (* current number of buckets (minus 1) *)
    size : int Atomic.t;
    (* maximum number of items in a bucket (on average).  Maximun
       number of elements in an hash table [t] is [t.sizes] * [t.max] *)
    max : int;
    segments : 'a segment option Atomic.t array;
    size_max : int;
    segment_size : int;
  }

  let init ~size_exponent =
    let segment_size = Int.shift_left 1 size_exponent in
    let llist = Llist.init ~compare () in
    let grow_exp = 10 in
    let exp_max = grow_exp + size_exponent in
    let size_max = Int.shift_left 1 exp_max in
    let first_seg =
      Array.init segment_size (fun j -> Atomic.make (new_dummy_node j llist.t))
    in
    let segments =
      Array.init (Int.shift_left 1 grow_exp) (fun i ->
          match i with
          | 0 -> Atomic.make (Some first_seg)
          | _ -> Atomic.make None)
    in
    {
      count = Atomic.make 0;
      size = Atomic.make segment_size;
      segment_size;
      size_max;
      segments;
      max = 2;
    }

  let is_empty { count; _ } = Atomic.get count = 0

  let get_segment t segment_ind =
    let seg = t.segments.(segment_ind) in
    match Atomic.get seg with
    | Some seg -> seg
    | None ->
        (* create a new segment *)
        let new_segment =
          Array.init t.segment_size (fun _ -> Atomic.make (false, None))
        in
        (* initialize (can failed if another domain has already done it) *)
        Atomic.compare_and_set seg None (Some new_segment) |> ignore;
        (* should never fail *)
        Option.get (Atomic.get seg)

  let get_bucket_ref t ind =
    let segment_ind = ind / t.segment_size in
    let segment = get_segment t segment_ind in
    segment.(ind land (t.segment_size - 1))

  (** [init_bucket buckets ind] inits a bucket and all its parents if needed *)
  let rec init_bucket t bucket bucket_ind =
    (* get parent index *)
    let parent_ind = unset_msb bucket_ind in
    let parent_bucket = get_bucket_ref t parent_ind in
    (* initialize parents recursively if necessary *)
    (if parent_ind <> bucket_ind then
     match Atomic.get parent_bucket with
     | _, None -> init_bucket t parent_bucket parent_ind
     | _, Some _ -> ());

    (* initialize current node *)
    let new_node = new_dummy_node bucket_ind parent_bucket in
    Atomic.set bucket new_node

  let get_bucket key t =
    let mask_val = Atomic.get t.size - 1 in
    let bucket_ind = key land mask_val in
    let bucket = get_bucket_ref t bucket_ind in
    match Atomic.get bucket with
    | _, Some _ -> bucket
    | _, None ->
        init_bucket t bucket bucket_ind;
        bucket

  let rec grow t count =
    let size_val = Atomic.get t.size in
    if count > size_val * t.max && size_val <= t.size_max then
      let new_size = size_val * 2 in
      if Atomic.compare_and_set t.size size_val new_size then ()
      else grow t count

  (** [add key value t] *)
  let add key value t =
    let bucket = get_bucket key t in
    let new_node = (compute_hkey key, Regular value) in

    let is_added, _local = Llist.unsafe_add compare new_node bucket in
    if not is_added then false
    else
      let prev_count = Atomic.fetch_and_add t.count 1 in
      grow t prev_count;
      true

  let add_no_resize key value t =
    let bucket = get_bucket key t in
    let new_node = (compute_hkey key, Regular value) in

    let is_added, _local = Llist.unsafe_add compare new_node bucket in
    if not is_added then false
    else (
      Atomic.incr t.count;
      true)

  let find key t =
    let bucket = get_bucket key t in
    let key = compute_hkey key in
    let is_found, local = Llist.find compare (key, Dummy) bucket in
    if not is_found then None
    else
      match local.curr with
      | _, Some { key = _, Regular k; _ } -> Some k
      | _, _ -> failwith "Should not happen"

  let mem key t =
    let bucket = get_bucket key t in
    let key = compute_hkey key in
    let is_found, _ = Llist.find compare (key, Dummy) bucket in
    is_found

  let remove key t =
    let bucket = get_bucket key t in
    let key = compute_hkey key in
    let is_removed, _ = Llist.unsafe_remove compare (key, Dummy) bucket in
    if not is_removed then false
    else (
      Atomic.decr t.count;
      true)
end
