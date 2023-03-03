type 'a markablereference = { node : 'a; marked : bool }

type 'a node = {
  key : int;
  height : int;
  next : 'a node option markablereference Atomic.t array;
}

type 'a t = { head : 'a node; }

let max_height = 10

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

let create_dummy_node_array () =
  Array.make (max_height + 1) (create_new_node Int.max_int max_height)

let get_random_level () =
  let rec count_level cur_level =
    if ((cur_level == max_height) || Random.float 1.0 <= 0.5) then cur_level else count_level (cur_level + 1)
  in
  count_level 1

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
  let head = { key = Int.min_int;  height = max_height; next } in
  { head }

(** let marked ref = ref.marked == false  
let get_ref ref = ref.node **)
let get_mark_ref atomic_ref =
  let ref = Atomic.get atomic_ref in
  (ref.node, ref.marked)

let get_ref atomic_ref =
  let ref = Atomic.get atomic_ref in
  ref.node

let compare_set_mark_ref atomic old_node old_mark node mark =
  let current = Atomic.get atomic in
  let set_mark_ref () =
    Atomic.compare_and_set atomic current { node; marked = mark }
  in
  match (current.node, old_node) with
  | Some current_node, Some old_node ->
      current_node == old_node && current.marked == old_mark && set_mark_ref ()
  | _, _ -> false

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
      if not snip then
        let prev, curr, succ, mark = init level in
        iterate prev curr succ mark level
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
    preds.(level) <- Option.get prev;
    succs.(level) <- Option.get curr;
    if level > 0 then update_arrays (level - 1)
    else (Option.get curr).key == key
  in
  update_arrays max_height

let add sl key =
  let top_level = get_random_level () in
  let preds = create_dummy_node_array () in
  let succs = create_dummy_node_array () in
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
    let rec set_bottom_level () =
      if
        not
          (compare_set_mark_ref pred.next.(0) (Some succ) false (Some new_node)
             false)
      then set_bottom_level ()
    in
    set_bottom_level ();
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

let find sl key =
  let preds = create_dummy_node_array () in
  let succs = create_dummy_node_array () in
  find_in key preds succs sl

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
      let succ, _ = get_mark_ref nodeToRemove.next.(level) in
      mark_levels succ level;
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
