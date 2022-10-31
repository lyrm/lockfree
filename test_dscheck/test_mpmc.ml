module Mpmc_queue = Lockfree.Mpmc_queue
module Atomic = Dscheck.TracedAtomic

let total_checked = ref 0

let mpmc_of_list size_exponent list =
  let t = Mpmc_queue.create ~size_exponent () in
  List.iter (fun elt -> Mpmc_queue.push t elt |> ignore) list;
  t

(* push pop *)
let _test1 n l size_exp =
  (* initialisation *)
  let t = mpmc_of_list size_exp l in
  let is_pushed = ref false in
  let is_popped = ref None in
  let head = match l with [] -> None | x :: _ -> Some x in
  let is_full = List.length l = 1 lsl size_exp in

  Atomic.spawn (fun () -> is_pushed := Mpmc_queue.push t n);
  Atomic.spawn (fun () -> is_popped := Mpmc_queue.pop t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          match (!is_pushed, !is_popped, head) with
          | true, Some p, None -> p = n
          | true, Some p, Some h -> if is_full then p = h else p = n || p = h
          | true, None, None -> not is_full
          | true, None, Some _ -> false
          | false, Some _, None -> false
          | false, Some p, Some h -> is_full && p = h
          | false, None, _ -> false))

(* push push *)
let _test2 n l size_exp =
  let t = mpmc_of_list size_exp l in
  let is_pushed1 = ref false in
  let is_pushed2 = ref false in
  let size = 1 lsl size_exp in

  Atomic.spawn (fun () -> is_pushed1 := Mpmc_queue.push t n);
  Atomic.spawn (fun () -> is_pushed2 := Mpmc_queue.push t n);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          match (!is_pushed1, !is_pushed2) with
          | true, true -> List.length l <= size - 2
          | true, false | false, true -> List.length l = size - 1
          | false, false -> List.length l = size))

(* pop pop *)
let _test3 l size_exp =
  let t = mpmc_of_list size_exp l in
  let is_popped1 = ref None in
  let is_popped2 = ref None in

  Atomic.spawn (fun () -> is_popped1 := Mpmc_queue.pop t);
  Atomic.spawn (fun () -> is_popped2 := Mpmc_queue.pop t);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          match (!is_popped1, !is_popped2, l) with
          | Some a, Some b, x :: y :: _ -> (a = x && b = y) || (a = y && b = x)
          | Some a, None, x :: [] | None, Some a, x :: [] -> x = a
          | None, None, [] -> true
          | _ -> false))

let _test () =
  let size_exponent = 3 in
  let t = Mpmc_queue.create ~size_exponent () in
  for i = 0 to (1 lsl size_exponent) - 1 do
    Mpmc_queue.push t i |> ignore
  done;

  let is_pushed1 = ref false in
  let is_pushed2 = ref false in

  Atomic.spawn (fun () -> is_pushed1 := Mpmc_queue.push t 0);
  Atomic.spawn (fun () -> is_pushed2 := Mpmc_queue.push t 0);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () -> (not !is_pushed1) && not !is_pushed2))

let () =
  (* Test 1 : Push and pop in parallel *)
  (* with the empty list *)
  (*Atomic.trace (fun () -> _test1 0 [] 2);
    (* with a not empty, not full list *)
    Atomic.trace (fun () -> _test1 0 [ 1; 2 ] 2);
    (* an element in the input llist *)
      Atomic.trace (fun () -> _test1 0 [ 1; 2; 3; 4 ] 2);*)

  (* Test 2 : push and push in parallel *)
  (* with the empty list *)
  (* Atomic.trace (fun () -> _test2 0 [] 2);
     (* with a not empty, not full list *)
     Atomic.trace (fun () -> _test2 0 [ 1; 2 ] 2);
     (* an element in the input llist *)
     Atomic.trace (fun () -> _test2 0 [ 1; 2; 3; 4 ] 2);*)

  (* Test 3 : pop and pop in parallel *)
  (* with the empty list *)
  (*Atomic.trace (fun () -> _test3 [] 2);
    (* with a not empty, not full list *)
    Atomic.trace (fun () -> _test3 [ 1; 2 ] 2);
    (* an element in the input llist *)
      Atomic.trace (fun () -> _test3 [ 1; 2; 3; 4 ] 2);*)
  Atomic.trace _test;
  Printf.printf "Total checked: %d\n" !total_checked
(*
let test () =
  let t = create ~size_exponent:1 () in
  let t1 = push t 1 in
  let t2 = push t 2 in
  assert (t1 && t2);
  let sema = Semaphore.Binary.make false in

  let d1 =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        push t 0)
  in
  let d2 =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        push t 0)
  in
  assert (not (Domain.join d1 || Domain.join d2))
*)
