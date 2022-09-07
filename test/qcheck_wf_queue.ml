module wf_queue = Lockfree.Wf_queue

(* Sequential building of a queue *)
let push_list wq l = List.iter (Wf_queue.push wq) l

(* [extract_n_from_d q f n] extract [n] elements of [q] by calling [n]
   times the function [f] on [q]. *)
let pop_n wq n =
  let rec loop acc = function
    | 0 -> acc
    | n ->
        let a = Wf_queue.pop wq in
        Domain.cpu_relax ();
        loop (a :: acc) (n - 1)
  in
  loop [] n |> List.rev

let keep_some l = List.filter Option.is_some l |> List.map Option.get
let keep_n_first n = List.filteri (fun i _ -> i < n)

let none_at_the_end l =
  let rec loop seen_a_none = function
    | [] -> true
    | Some _ :: xs -> if seen_a_none then false else loop seen_a_none xs
    | None :: xs -> loop true xs
  in
  loop false l

let rec compare l l1 l2 =
  match (l, l1, l2) with
  | [], [], [] -> true
  | [], _, _ -> false
  | _, [], _ -> l = l2
  | _, _, [] -> l = l1
  | x :: l', y :: l1', z :: l2' ->
      if x = y && x = z then compare l' l1 l2' || compare l' l1' l2
      else if x = y then compare l' l1' l2
      else if x = z then compare l' l1 l2'
      else false

let tests_sequential =
  QCheck.
    [
      (* TEST 1 - sequential:
         forall q and n, pop (push queue i; queue) = Some i*)
      Test.make ~name:"push_and_pop"
        (pair (list int) int)
        (fun i ->
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in

          (* Testing property *)
          Wf_queue.push wq i;
          Wf_queue.pop wq = Some i);
      (* TEST 2 - sequential:
          [pop] on an empty queue returns None *)
      Test.make ~name:"pop_empty" (list int) (fun lpush ->
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in
          push_list wq lpush;
          let _ = pop_n wq (List.length lpush) in

          (* Testing property *)
          match Wf_queue.pop wq with Some _ -> false | None -> true);
      (* TEST 3 - sequential :
         forall l, l' and with q built by pushing in order (l@l')
                  pop q :: pop q :: pop q :: ... :: [] = List.rev l' *)
      Test.make ~name:"pops_are_in_order"
        (pair (list int) (list int))
        (fun (l, l') ->
          assume (l' <> []);
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in
          push_list wq (l @ l');

          let pop_list = pop_n wq (List.length l) in
          List.map Option.get pop_list = l);
      (* TEST 4 - sequential :
         forall q of size n, forall m > n, poping m times returns (m-n) None. *)
      Test.make ~name:"popping_an_empty_queue_returns_None" ~count:1
        (pair (list int) small_nat)
        (fun (l, m) ->
          assume (m > 0);
          let n = List.length l in
          let m = m + n in
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in
          push_list wq l;
          let popped = pop_n wq m in
          let none_count =
            List.filter (function None -> true | _ -> false) popped
            |> List.length
          in

          none_count = m - n);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1 - two domains in parallel.
         Checks that the proper error is raised if too many domains
         try to registered. *)
      Test.make ~name:"Too_many_registered_domains" (list int) (fun lpush ->
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in
          push_list wq lpush;

          let d1 =
            Domain.spawn (fun () ->
                try
                  let _ = Wf_queue.register q in
                  false
                with Wf_queue.Too_many_registered_domains -> true)
          in
          (* Testing property *)
          Domain.join d1);
      (* TEST 2 - two domains in parallel.
         Checks that the proper error is raised when a domain tries to
         use a handler it does not own. *)
      Test.make ~name:"Not_handler_owner"
        (pair (list int) int)
        (fun (lpush, i) ->
          let q = Wf_queue.init ~num_domain:1 in
          let wq = Wf_queue.register q in
          push_list wq lpush;

          let d1 =
            Domain.spawn (fun () ->
                try
                  Wf_queue.push wq i;
                  false
                with Wf_queue.Not_handler_owner -> true)
          in
          (* Testing property *)
          Domain.join d1);
      (* TEST 3 - two domains in parallel
         Sequential pushes and concurrent pops.

         Checks that no [pop] are missing and that the result is
         linearisable, meaning it is possible to merge popped values
         of both domains to obtain the pushed values without reordering.

         For example, if [1;2;3] is pushed, acceptable outputs would be :
         - [Some 1; Some 2; None; None] and [Some 3; None]
         - [None; None] and [Some 1; Some 2; Some 3; None; None]
         - [Some 1; Some 3] and [Some 2; None; None]

         Unacceptable outputs are :
         - [None; Some 1] and [Some 2; Some 3]
         - [Some 2; Some 1] and [Some 3]
         - [Some 1; None] and [Some 2; None]
      *)
      Test.make ~name:"parallel_pops"
        (pair (list int) (pair small_nat small_nat))
        (fun (lpush, (npop1, npop2)) ->
          assume (List.length lpush <= npop1 + npop2);

          (* Sequential pushes *)
          let q = Wf_queue.init ~num_domain:3 in
          let wq = Wf_queue.register q in
          push_list wq lpush;
          let sema = Semaphore.Counting.make 2 in

          let work npop =
            Semaphore.Counting.acquire sema;
            while Semaphore.Counting.get_value sema <> 0 do
              Domain.cpu_relax ()
            done;
            let wq = Wf_queue.register q in
            pop_n wq npop
          in

          (* Popping domain 1 *)
          let domain1 = Domain.spawn (fun () -> work npop1) in
          (* Popping domain 2 *)
          let domain2 = Domain.spawn (fun () -> work npop2) in
          let pop1 = Domain.join domain1 in
          let pop2 = Domain.join domain2 in

          (* Testing property *)
          none_at_the_end pop1
          && none_at_the_end pop2
          && List.length pop1 = npop1
          && List.length pop2 = npop2
          && compare lpush (keep_some pop1) (keep_some pop2));
      (* TEST 4 - two domains in parallel
         Concurrent pushes then sequential pops.
      *)
      Test.make ~name:"parallel_pushes"
        (pair (list int) (pair (list int) (list int)))
        (fun (lpush, (lpush1, lpush2)) ->
          (* Initialization *)
          let q = Wf_queue.init ~num_domain:3 in
          let wq = Wf_queue.register q in
          push_list wq lpush;

          let sema = Semaphore.Counting.make 2 in

          let work lpush =
            Semaphore.Counting.acquire sema;
            while Semaphore.Counting.get_value sema <> 0 do
              Domain.cpu_relax ()
            done;
            let wq = Wf_queue.register q in
            List.iter
              (fun elt ->
                Wf_queue.push wq elt;
                Domain.cpu_relax ())
              lpush
          in

          (* Domain 1 : pushes all values of [lpush1] *)
          let domain1 = Domain.spawn (fun () -> work lpush1) in

          (* Domain 1 : pushes all values of [lpush1] *)
          let domain2 = Domain.spawn (fun () -> work lpush2) in

          let () = Domain.join domain1 in
          let () = Domain.join domain2 in

          (* Sequential pops *)
          let all_values =
            pop_n wq
              (List.length lpush + List.length lpush1 + List.length lpush2)
          in

          let n_last =
            List.rev all_values
            |> keep_n_first (List.length lpush1 + List.length lpush2)
            |> List.rev
            |> keep_some
          in

          (* Testing property *)
          Wf_queue.pop wq = None
          && keep_n_first (List.length lpush) all_values |> keep_some = lpush
          && compare n_last lpush1 lpush2);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Wf_queue"
    [
      ("sequential_tests", to_alcotest tests_sequential);
      ("two_domains_tests", to_alcotest tests_two_domains);
    ]
;;

main ()
