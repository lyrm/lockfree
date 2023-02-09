(* Some conclusion deduced from the simple benchmarks below :

   1- the costy function is (obviously) the [find] function

   2- compare to a basic implementation, the longer the list the less
   efficient is the lockfree implementation. *)

module Llist = Lockfree.Linked_list

(** [measure_and_launch_n_tests n t] launchs [n] times the test [t]
   and  returns the number of seconds required for the [n] tests. *)
let measure_and_launch_n_tests ntest (test : unit -> unit) =
  let n = 10 in
  let a =
    Array.init n (fun _ ->
        let tstart = Unix.gettimeofday () in
        for _i = 0 to (ntest / n) - 1 do
          test ()
        done;
        let tend = Unix.gettimeofday () in
        tend -. tstart)
  in
  Array.sort compare a;
  a.((n / 2) - 1)

(** Naive sequential implementation of a non-thread-safe sorted linked
   list.  It is used here to get an idea of how slower the lockfree
   linked list is.  *)
module Sllist : sig
  type 'a t

  val init : unit -> 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t

  (*  val delete : 'a -> 'a t -> 'a t*)
end = struct
  type 'a t = Node of 'a * 'a t | Empty

  let init () = Empty

  let mem key l =
    let rec loop = function
      | Empty -> false
      | Node (x, xs) -> if x >= key then key = x else loop xs
    in
    loop l

  let add key l =
    let rec loop = function
      | Empty -> Node (key, Empty)
      | Node (x, xs) ->
          if x = key then Node (x, xs)
          else if x > key then Node (key, Node (x, xs))
          else Node (x, loop xs)
    in
    loop l

  let _delete key l =
    let rec loop = function
      | Empty -> l
      | Node (x, xs) ->
          if x = key then xs else if x > key then l else Node (x, loop xs)
    in
    loop l
end

(** [random_list n ~max] generates random list of integers of size n *)
let random_list ?(max = 100) n =
  let arr = Array.make n 0 in
  for i = 0 to n - 1 do
    arr.(i) <- Random.int max
  done;
  arr |> Array.to_list

(** [test_add_par n ls] makes n domains [add] the n lists
   contained in [ls] in parallel. It checks with [find] that all
   unique elements of all lists in [ls] are in the resulting list. *)
let test_add_par (ls, unique_elts) =
  let t = Llist.init () in
  let sema = Semaphore.Counting.make (Array.length ls) in

  let work l =
    Semaphore.Counting.acquire sema;
    while Semaphore.Counting.get_value sema <> 0 do
      Domain.cpu_relax ()
    done;
    List.iter (fun elt -> Llist.add elt t |> ignore) l
  in
  let domains = Array.map (fun l -> Domain.spawn (fun () -> work l)) ls in
  Array.iter Domain.join domains;

  let res = List.for_all (fun elt -> Llist.mem elt t) unique_elts in
  if not res then failwith "error add par"

(** [test_add_seq] does the same as [test_add_par] but with only
   one domains, adding the lists sequentially. *)
let test_add_seq (ls, unique_elts) =
  let t = Llist.init () in
  Array.iter
    (fun l ->
      Domain.spawn (fun () ->
          List.iter (fun elt -> Llist.add elt t |> ignore) l)
      |> Domain.join)
    ls;
  let res = List.for_all (fun elt -> Llist.mem elt t) unique_elts in
  if not res then failwith "error add seq"

(** [test_add_sslist] does the same as [test_add_seq] but with
   the [Sllist] implementation of a sorted linked list. *)
let test_add_sllist (ls, unique_elts) =
  let t = Sllist.init () in
  let t =
    Array.fold_left
      (fun t l ->
        Domain.spawn (fun () ->
            List.fold_left (fun t elt -> Sllist.add elt t) t l)
        |> Domain.join)
      t ls
  in

  let res = List.for_all (fun elt -> Sllist.mem elt t) unique_elts in
  if not res then failwith "error add sllist"

let test_add () =
  (*
 Parameters : every test is run 100 (ntest) times. For each tests
 (1000 (nsize) x ndomains) elements are inserted. The elements are
 between 0 and 999. There are actually two variables here : the number
 of domains and the number of elements added that grows while the maximum
 number of elements in the sorted linked list does not (one unique element
 can only be once in the list, so there can not be more than 1000 elements in it)

     _______________________________
    |        |         |            |
    | Nb of  | Ratio   | Ratio      |
    | domain | seq/par | Sllist/par |
    |________|_________|____________|
    |   1    |  0.98   |    0.25    |
    |   2    |  1.4    |    0.4     |
    |   4    |  1.95   |    0.62    |
    |   8    |  3.1    |    1.2     |
    |   16   |  1.4    |    0.5     |
    |________|_________|____________|

 *)
  Random.self_init ();
  let ntest = 10000 in
  let nsize = 100 in
  let ndomains = 3 in
  let list_size = nsize / ndomains in
  let max = nsize in

  let ls = Array.init ndomains (fun _i -> random_list list_size ~max) in
  let unique_elts =
    Array.fold_left (fun ls l -> l @ ls) [] ls |> List.sort_uniq compare
  in
  Format.printf
    "**Parameters:**\n\
     #domains: %d\n\
     #elements: %d\n\
     #elt/list: %d\n\
     #unique elts: %d (between 0 and %d)\n\n"
    ndomains nsize list_size (List.length unique_elts) (max - 1);
  let ls_uni = (ls, unique_elts) in

  let test_add_sllist () = test_add_sllist ls_uni in
  let test_add_seq () = test_add_seq ls_uni in
  let test_add_par () = test_add_par ls_uni in

  let tpar = measure_and_launch_n_tests ntest test_add_seq in
  let tseq = measure_and_launch_n_tests ntest test_add_par in
  let tseq2 = measure_and_launch_n_tests ntest test_add_sllist in

  Format.printf
    "**Results:**\nSllist: %.3f seconds\nSeq: %.3f seconds\nPar: %.3f seconds\n"
    tseq2 tseq tpar;
  Format.printf "Ratio seq/par: %0.2f\nRation Sllist/par: %.2f\n" (tseq /. tpar)
    (tseq2 /. tpar)

(*
let simple_test () =
  let ntest = 100_000 in
  let nsize = 10 in
  let l = List.init nsize (fun i -> i) in
  let t = Llist.init () in

  let test l =
    let d =
      Domain.spawn (fun () ->
          List.iter (fun elt -> Llist.add elt t |> ignore) l;
          List.iter (fun elt -> Llist.delete elt t |> ignore) l;
          Llist.clean_local t)
    in
    Domain.join d;
    Llist.close t
  in

  Format.printf "**BEGIN.**@.";
  measure_and_launch_n_tests ntest (fun () -> test l) |> ignore;
  Format.printf "**END.**@."
*)
let _ = test_add ()
