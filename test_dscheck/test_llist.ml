module Llist = Lockfree.Linked_list
module Atomic = Dscheck.TracedAtomic

let total_checked = ref 0

let llist_of_list list =
  let t = Llist.init () in
  List.iter (fun elt -> Llist.insert elt t |> ignore) list;
  t

(* insert/delete *)
let test1 n l =
  (* initialisation *)
  let t = llist_of_list l in
  let is_inserted = ref false in
  let is_deleted = ref false in
  let was_in = List.mem n l in
  Atomic.spawn (fun () -> is_inserted := Llist.insert n t);
  Atomic.spawn (fun () -> is_deleted := Llist.delete n t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          match (!is_inserted, !is_deleted) with
          | true, true -> if was_in then Llist.mem n t else not @@ Llist.mem n t
          | true, false -> (not was_in) && Llist.mem n t
          | false, true -> was_in && (not @@ Llist.mem n t)
          | false, false -> false))

(* delete/insert *)
let _test2 n l =
  (* initialisation *)
  let t = llist_of_list l in
  let is_inserted = ref false in
  let is_deleted = ref false in
  let was_in = List.mem n l in
  Atomic.spawn (fun () -> is_deleted := Llist.delete n t);
  Atomic.spawn (fun () -> is_inserted := Llist.insert n t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          match (!is_inserted, !is_deleted) with
          | true, true -> if was_in then Llist.mem n t else not @@ Llist.mem n t
          | true, false -> (not was_in) && Llist.mem n t
          | false, true -> was_in && (not @@ Llist.mem n t)
          | false, false -> false))

(* insert/insert *)
let _test3 m n l =
  let t = llist_of_list l in
  let n_is_inserted = ref false in
  let m_is_inserted = ref false in
  let m_was_in = List.mem m l in
  let n_was_in = List.mem n l in
  Atomic.spawn (fun () -> n_is_inserted := Llist.insert n t);
  Atomic.spawn (fun () -> m_is_inserted := Llist.insert m t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          if m <> n then
            !m_is_inserted = not m_was_in && !n_is_inserted = not n_was_in
          else
            (match (!m_is_inserted, !n_is_inserted) with
            | false, false -> m_was_in
            | false, true | true, false -> not m_was_in
            | true, true -> false)
            && Llist.mem n t))

(* delete/delete *)
let _test4 m n l =
  let t = llist_of_list l in
  let n_is_deleted = ref false in
  let m_is_deleted = ref false in
  let m_was_in = List.mem m l in
  let n_was_in = List.mem n l in
  Atomic.spawn (fun () -> n_is_deleted := Llist.delete n t);
  Atomic.spawn (fun () -> m_is_deleted := Llist.delete m t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          if m <> n then !m_is_deleted = m_was_in && !n_is_deleted = n_was_in
          else
            (match (!m_is_deleted, !n_is_deleted) with
            | false, false -> not m_was_in
            | false, true | true, false -> m_was_in
            | true, true -> false)
            && (not @@ Llist.mem n t)))

let () =
  let l1234 = [ 1; 2; 3; 4 ] in
  (* Test 1 : Insert and delete the same element in parallel *)
  (* with the empty list *)
  Atomic.trace (fun () -> test1 0 []);
  (* an element in the input llist *)
  Atomic.trace (fun () -> test1 1 l1234); (*
  (* an element that is not in the input llist *)
    Atomic.trace (fun () -> test1 5 l1234);*)

  (* Test 2 : Delete and insert the same element in parallel *)
  (* with the empty list *)
  (*Atomic.trace (fun () -> test2 0 []);
  (* an element in the input llist *)
  Atomic.trace (fun () -> test2 1 l1234);
  (* an element that is not in the input llist *)
  Atomic.trace (fun () -> test2 5 l1234);

  (* Test 2 : Inserting 2 elements in parallel *)
  (* twice the same element in an empty list *)
  Atomic.trace (fun () -> test3 0 0 []);
  (* two different elements in an empty list *)
  Atomic.trace (fun () -> test3 0 1 []);
  (* twice the same element in the input llist *)
  Atomic.trace (fun () -> test3 1 1 l1234);
  (* twice the same element not in input llist *)
  Atomic.trace (fun () -> test3 5 5 l1234);
  (* 2 differents elements both in input llist *)
  Atomic.trace (fun () -> test3 1 2 l1234);
  Atomic.trace (fun () -> test3 2 1 l1234);
  (* 2 differents elements not in input llist *)
  Atomic.trace (fun () -> test3 5 6 l1234);
  (* 2 differents elements, one in the input llist*)
  Atomic.trace (fun () -> test3 1 5 l1234);

  (* Test 3 : Deleting 2 elements in parallel *)
  (* twice the same element in an empty list *)
  Atomic.trace (fun () -> test4 0 0 []);
  (* two different elements in an empty list *)
  Atomic.trace (fun () -> test4 0 1 []);
  (* twice the same element in the input llist *)
  Atomic.trace (fun () -> test4 1 1 l1234);
  (* twice the same element not in input llist *)
  Atomic.trace (fun () -> test4 5 5 l1234);
  (* 2 differents elements both in input llist *)
  Atomic.trace (fun () -> test4 1 2 l1234);
  (* 2 differents elements not in input llist *)
  Atomic.trace (fun () -> test4 5 6 l1234);
  (* 2 differents elements, one in the input llist*)
    Atomic.trace (fun () -> test4 1 5 l1234);*)
  Printf.printf "Total checked: %d\n" !total_checked
