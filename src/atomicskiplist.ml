type 'a markablereference = { node : 'a; marked : bool }
(** markable reference: stores a reference to a node and has a field to specify if it is marked *)

type 'a node = {
  key : int;
  height : int;
  next : 'a node option markablereference Atomic.t array;
}

type 'a t = { head : 'a node }

let max_height = 10

(** create_new_node: creates a new node with some value and height *)
let create_new_node value height =
  let next =
    let arr =
      Array.make (max_height + 1) (Atomic.make { node = None; marked = false })
    in
    for level = 1 to max_height do
      arr.(level) <- Atomic.make { node = None; marked = false }
    done;
    arr
  in
  { key = value; height; next }

(** create_dummy_node_array: Creates a new array with the different node for each index *)
let create_dummy_node_array () =
  let arr =
    Array.make (max_height + 1) (create_new_node Int.max_int max_height)
  in
  for i = 1 to max_height do
    arr.(i) <- create_new_node Int.max_int max_height
  done;
  arr

(** Get a random level from 1 till max_height (both included) *)
let get_random_level () =
  let rec count_level cur_level =
    if cur_level == max_height || Random.float 1.0 <= 0.5 then cur_level
    else count_level (cur_level + 1)
  in
  count_level 1

(** Create a new skiplist *)
let create () =
  let tail =
    {
      key = Int.max_int;
      height = max_height;
      next =
        Array.make (max_height + 1)
          (Atomic.make { node = None; marked = false });
    }
  in
  let next =
    let arr =
      Array.make (max_height + 1)
        (Atomic.make { node = Some tail; marked = false })
    in
    for level = 1 to max_height do
      arr.(level) <- Atomic.make { node = Some tail; marked = false }
    done;
    arr
  in
  let head = { key = Int.min_int; height = max_height; next } in
  { head }

(** get_mark_ref: Returns the node and the mark from an Atomic markablereference *)
let get_mark_ref atomic_ref =
  let ref = Atomic.get atomic_ref in
  (ref.node, ref.marked)

(** get_ref: Returns only the node from an Atomic markablereference *)
let get_ref atomic_ref =
  let ref = Atomic.get atomic_ref in
  ref.node

(** Compares old_node and old_mark with the atomic reference and if they are the same then 
    Replaces the value in the atomic with node and mark *)
let compare_set_mark_ref atomic old_node old_mark node mark =
  let current = Atomic.get atomic in
  let set_mark_ref () =
    Atomic.compare_and_set atomic current { node; marked = mark }
  in
  match current.node, old_node with 
  | Some current_node, Some old_node -> (
    match node with 
    | Some node -> current_node == old_node && current.marked = old_mark && ((current_node == node && current.marked = mark) || set_mark_ref ())
    | None -> false
  )
  | None,_|_,None -> false

(** Returns true if key is found within the skiplist else false;
    Irrespective of return value, fills the preds and succs array with 
    the predecessors nodes with smaller key and successors nodes with greater than 
    or equal to key 
  *)
let find_in key preds succs sl =
  let head = sl.head in
  let init level =
    let prev = Some head in
    let curr = get_ref (Option.get prev).next.(level) in
    let succ, mark = get_mark_ref (Option.get curr).next.(level) in
    (prev, curr, succ, mark)
  in
  let rec iterate prev curr succ mark level =
    let prev_node = Option.get prev in
    if mark then
      let snip =
        compare_set_mark_ref prev_node.next.(level) curr false succ false
      in
      if not snip then (None, None)
      else
        let curr = get_ref prev_node.next.(level) in
        let succ, mark = get_mark_ref (Option.get curr).next.(level) in
        iterate prev curr succ mark level
    else if (Option.get curr).key < key then
      let new_succ, mark = get_mark_ref (Option.get succ).next.(level) in
      iterate curr succ new_succ mark level
    else (prev, curr)
  in
  let rec update_arrays level =
    let prev, curr, succ, mark = init level in
    let prev, curr = iterate prev curr succ mark level in
    if prev = None && curr = None then update_arrays max_height
    else (
      preds.(level) <- Option.get prev;
      succs.(level) <- Option.get curr;
      if level > 0 then update_arrays (level - 1)
      else (Option.get curr).key == key)
  in
  update_arrays max_height

(** Adds a new key to the skiplist sl. *)
let add sl key =
  let top_level = get_random_level () in
  let preds = create_dummy_node_array () in
  let succs = create_dummy_node_array () in
  let rec repeat () =
    let found = find_in key preds succs sl in
    if found then false
    else
      let new_node = create_new_node key top_level in
      for level = 0 to top_level do
        let succ = succs.(level) in
        let mark_ref = { node = Some succ; marked = false } in
        new_node.next.(level) <- Atomic.make mark_ref
      done;
      let pred = preds.(0) in
      let succ = succs.(0) in
      if
        not
          (compare_set_mark_ref pred.next.(0) (Some succ) false (Some new_node)
             false)
      then repeat ()
      else
        let rec update_levels level =
          let rec set_next () =
            let pred = preds.(level) in
            let succ = succs.(level) in
            if
              compare_set_mark_ref pred.next.(level) (Some succ) false
                (Some new_node) false
            then ()
            else (
              ignore (find_in key preds succs sl);
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
let find sl key = 
  let rec search pred curr succ mark level = 
    assert ((Option.get pred).key < key);
    Printf.printf "%d " (Option.get curr).key;
    if mark then
      let curr = succ in 
      let succ, mark = get_mark_ref (Option.get curr).next.(level) in 
      search pred curr succ mark level
    else 
      if (Option.get curr).key < key then 
        let pred = curr in 
        let curr = get_ref (Option.get pred).next.(level) in 
        let succ, mark = get_mark_ref (Option.get curr).next.(level) in 
        search pred curr succ mark level 
      else 
        if level > 0 then
          let level = (level - 1) in 
          let curr = get_ref (Option.get pred).next.(level) in 
          let succ, mark = get_mark_ref (Option.get curr).next.(level) in 
          search pred curr succ mark level 
        else 
          (Option.get curr).key == key
  in
  let pred = sl.head in 
  let curr = get_ref pred.next.(max_height) in 
  let succ, mark = get_mark_ref (Option.get curr).next.(max_height) in 
  search (Some pred) curr succ mark max_height 
 

(** Returns true if the removal was successful and returns false if the key is not present within the skiplist *)
let remove sl key =
  let preds = create_dummy_node_array () in
  let succs = create_dummy_node_array () in
  let found = find_in key preds succs sl in
  if not found then false
  else
    let nodeToRemove = succs.(0) in
    let nodeHeight = nodeToRemove.height in
    let rec mark_levels succ level =
      let _ =
        compare_set_mark_ref nodeToRemove.next.(level) succ false succ true
      in
      let succ, mark = get_mark_ref nodeToRemove.next.(level) in
      if not mark then mark_levels succ level
    in
    let rec update_upper_levels level =
      let succ, mark = get_mark_ref nodeToRemove.next.(level) in
      if not mark then mark_levels succ level;
      if level > 1 then update_upper_levels (level - 1)
    in
    let rec update_bottom_level succ =
      let iMarkedIt =
        compare_set_mark_ref nodeToRemove.next.(0) succ false succ true
      in
      let succ, mark = get_mark_ref succs.(0).next.(0) in
      if iMarkedIt then (
        ignore (find_in key preds succs sl);
        true)
      else if mark then false
      else update_bottom_level succ
    in
    update_upper_levels nodeHeight;
    let succ, _ = get_mark_ref nodeToRemove.next.(0) in
    update_bottom_level succ