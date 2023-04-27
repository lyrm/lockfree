type markable_reference = { node : node; marked : bool }
(** markable reference: stores a reference to a node and has a field to specify if it is marked *)

and node = { key : int; height : int; next : markable_reference Atomic.t array }

exception Failed_snip

type t = { head : node; max_height : int }

let null_node = { key = Int.max_int; height = 0; next = [||] }

(** create_new_node: creates a new node with some value and height *)
let create_new_node value height =
  let next =
    Array.init (height + 1) (fun _ ->
        Atomic.make { node = null_node; marked = false })
  in
  { key = value; height; next }

(** create_dummy_node_array: Creates a new array with the different node for each index *)
let create_dummy_node_array max_height = Array.make (max_height + 1) null_node

(** Get a random level from 1 till max_height (both included) *)
let get_random_level max_height =
  let rec count_level cur_level =
    if cur_level == max_height || Random.bool () then cur_level
    else count_level (cur_level + 1)
  in
  count_level 1

(** Create a new skiplist *)
let create ?(max_height = 10) () =
  let tail = create_new_node Int.max_int max_height in
  let next =
    Array.init (max_height + 1) (fun _ ->
        Atomic.make { node = tail; marked = false })
  in
  let head = { key = Int.min_int; height = max_height; next } in
  { head; max_height }

(** Compares old_node and old_mark with the atomic reference and if they are the same then
    Replaces the value in the atomic with node and mark *)
let compare_and_set_mark_ref (atomic, old_node, old_mark, node, mark) =
  let current = Atomic.get atomic in
  let set_mark_ref () =
    Atomic.compare_and_set atomic current { node; marked = mark }
  in
  let current_node = current.node in
  current_node == old_node && current.marked = old_mark
  && ((current_node == node && current.marked = mark) || set_mark_ref ())

(** Returns true if key is found within the skiplist else false;
    Irrespective of return value, fills the preds and succs array with
    the predecessors nodes with smaller key and successors nodes with greater than
    or equal to key
  *)
let find_in (key, preds, succs, { head; max_height }) =
  let rec iterate (prev, curr, succ, mark, level) =
    if mark then
      let snip =
        compare_and_set_mark_ref (prev.next.(level), curr, false, succ, false)
      in
      if not snip then raise Failed_snip
      else
        let { node = curr; _ } = Atomic.get prev.next.(level) in
        let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
        iterate (prev, curr, succ, mark, level)
    else if curr.key < key then
      let { node = new_succ; marked = new_mark } =
        Atomic.get succ.next.(level)
      in
      iterate (curr, succ, new_succ, new_mark, level)
    else (prev, curr)
  in
  let rec update_arrays prev level =
    let { node = curr; _ } = Atomic.get prev.next.(level) in
    let { node = succ; marked = mark } = Atomic.get curr.next.(level) in
    try
      let prev, curr = iterate (prev, curr, succ, mark, level) in
      preds.(level) <- prev;
      succs.(level) <- curr;
      if level > 0 then update_arrays prev (level - 1) else curr.key == key
    with Failed_snip -> update_arrays head max_height
  in
  update_arrays head max_height

(** Adds a new key to the skiplist sl. *)
let add ({ max_height; _ } as sl) key =
  let top_level = get_random_level max_height in
  let preds = create_dummy_node_array max_height in
  let succs = create_dummy_node_array max_height in
  let rec repeat () =
    let found = find_in (key, preds, succs, sl) in
    if found then false
    else
      let new_node =
        {
          key;
          height = top_level;
          next =
            Array.init (top_level + 1) (fun level ->
                Atomic.make { node = succs.(level); marked = false });
        }
      in
      let pred = preds.(0) in
      let succ = succs.(0) in
      if
        not
          (compare_and_set_mark_ref
             (pred.next.(0), succ, false, new_node, false))
      then repeat ()
      else
        let rec update_levels level =
          let rec set_next () =
            let pred = preds.(level) in
            let succ = succs.(level) in
            if
              compare_and_set_mark_ref
                (pred.next.(level), succ, false, new_node, false)
            then ()
            else (
              find_in (key, preds, succs, sl) |> ignore;
              set_next ())
          in
          set_next ();
          if level < top_level then update_levels (level + 1)
        in
        update_levels 1;
        true
  in
  repeat ()

(** Returns true if the key is within the skiplist, else returns false *)
let mem { head; max_height; _ } key =
  let rec search (pred, curr, succ, mark, level) =
    if mark then
      let { node = new_succ; marked = new_mark } =
        Atomic.get succ.next.(level)
      in
      search (pred, succ, new_succ, new_mark, level)
    else if curr.key < key then
      let { node = new_succ; marked = new_mark } =
        Atomic.get succ.next.(level)
      in
      search (curr, succ, new_succ, new_mark, level)
    else if level > 0 then
      let level = level - 1 in
      let new_curr = (Atomic.get pred.next.(level)).node in
      let { node = new_succ; marked = new_mark } =
        Atomic.get new_curr.next.(level)
      in
      search (pred, new_curr, new_succ, new_mark, level)
    else curr.key == key
  in
  let curr = (Atomic.get head.next.(max_height)).node in
  let { node = succ; marked = mark } = Atomic.get curr.next.(max_height) in
  search (head, curr, succ, mark, max_height)

(** Returns true if the removal was successful and returns false if the key is not present within the skiplist *)
let remove ({ max_height; _ } as sl) key =
  let preds = create_dummy_node_array max_height in
  let succs = create_dummy_node_array max_height in
  let found = find_in (key, preds, succs, sl) in
  if not found then false
  else
    let nodeToRemove = succs.(0) in
    let nodeHeight = nodeToRemove.height in
    let rec mark_levels succ level =
      let _ =
        compare_and_set_mark_ref
          (nodeToRemove.next.(level), succ, false, succ, true)
      in
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then mark_levels succ level
    in
    let rec update_upper_levels level =
      let { node = succ; marked = mark } =
        Atomic.get nodeToRemove.next.(level)
      in
      if not mark then mark_levels succ level;
      if level > 1 then update_upper_levels (level - 1)
    in
    let rec update_bottom_level succ =
      let iMarkedIt =
        compare_and_set_mark_ref (nodeToRemove.next.(0), succ, false, succ, true)
      in
      let { node = succ; marked = mark } = Atomic.get succs.(0).next.(0) in
      if iMarkedIt then (
        find_in (key, preds, succs, sl) |> ignore;
        true)
      else if mark then false
      else update_bottom_level succ
    in
    update_upper_levels nodeHeight;
    update_bottom_level @@ (Atomic.get nodeToRemove.next.(0)).node
