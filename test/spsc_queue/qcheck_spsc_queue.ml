module Spsc_queue = Lockfree.Single_prod_single_cons_queue

let keep_some l = List.filter Option.is_some l |> List.map Option.get
let keep_n_first n = List.filteri (fun i _ -> i < n)

let pop_n_times q n =
  let rec loop count acc =
    if count = 0 then acc
    else
      let v = Spsc_queue.pop_opt q in
      Domain.cpu_relax ();
      loop (count - 1) (v :: acc)
  in
  loop n [] |> List.rev

let tests =
  [
    (* TEST 1 - sequential :
       Sequential pushes then pops. Checks that the behaviour is similar to
       one of a FIFO queue. *)
    QCheck.(
      Test.make ~name:"seq_pop_push"
        (pair (list int) small_nat)
        (fun (l, npop) ->
          (* Making sure we do not create a too big queue. Other
             tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l < size_max);

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in

          (* Sequential pushed : not Full exception should be
             raised. *)
          let not_full_queue =
            try
              List.iter (Spsc_queue.push q) l;
              true
            with Spsc_queue.Full -> false
          in

          (* Consumer domain pops *)
          let consumer = Domain.spawn (fun () -> pop_n_times q npop) in
          let pops = Domain.join consumer in

          (* Property *)
          not_full_queue
          && List.length pops = npop
          && keep_some pops = keep_n_first (min (List.length l) npop) l));
    (* TEST 2 - one producer, one consumer:
       Parallel pushes and pops. Checks that the behaviour is similar to
       one of a FIFO queue. *)
    QCheck.(
      Test.make ~name:"par_pop_push"
        (pair (pair (list int) (list int)) small_nat)
        (fun ((l, l'), npop) ->
          (* Making sure we do not create a too big queue. Other
               tests are checking the behaviour of a full queue.*)
          let size_exponent = 8 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length l + List.length l' < size_max);

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in
          let barrier = Barrier.create 2 in
          List.iter (Spsc_queue.push q) l;

          (* Consumer pops *)
          let consumer =
            Domain.spawn (fun () ->
                Barrier.wait_until_full barrier;
                pop_n_times q npop)
          in

          let producer =
            Domain.spawn (fun () ->
                (* Making sure the consumer can start *)
                Barrier.wait_until_full barrier;
                (* Main domain pushes.*)
                List.iter
                  (fun elt ->
                    Spsc_queue.push q elt;
                    Domain.cpu_relax ())
                  l')
          in

          let popped = Domain.join consumer in
          let _ = Domain.join producer in
          let popped_val = popped |> keep_some in

          (* Property *)
          List.length popped = npop
          && popped_val = keep_n_first (List.length popped_val) (l @ l')));
    (* TEST 3 - one producer:
       Checks that pushing too much raise exception Full. *)
    QCheck.(
      Test.make ~name:"push_full" (list int) (fun l ->
          let size_exponent = 4 in
          let size_max = Int.shift_left 1 size_exponent in

          (* Initialization *)
          let q = Spsc_queue.create ~size_exponent in

          let is_full =
            try
              List.iter (Spsc_queue.push q) l;
              false
            with Spsc_queue.Full -> true
          in

          (* Property *)
          (List.length l > size_max && is_full)
          || (List.length l <= size_max && not is_full)));
    (* TEST 4 - one producer, one consumer:
       Check semantic of [peek]. *)
    QCheck.(
      Test.make ~name:"par_push_peek" (list int) (fun lpush ->
          let lpush = List.sort_uniq compare lpush in

          (* Initialization *)
          let barrier = Barrier.create 2 in
          let size_exponent = 10 in
          let size_max = Int.shift_left 1 size_exponent in
          assume (List.length lpush < size_max);

          let q = Spsc_queue.create ~size_exponent in

          let producer =
            Domain.spawn (fun () ->
                Barrier.wait_until_full barrier;
                List.iter
                  (fun elt ->
                    Spsc_queue.push q elt;
                    Domain.cpu_relax ())
                  lpush)
          in
          let watcher =
            Domain.spawn (fun () ->
                Barrier.wait_until_full barrier;
                List.map
                  (fun _ ->
                    let peeked = Spsc_queue.peek_opt q in
                    Option.iter (fun _ -> Spsc_queue.pop q |> ignore) peeked;
                    Domain.cpu_relax ();
                    peeked)
                  lpush)
          in

          (* Property *)
          Domain.join producer;
          let peeked =
            Domain.join watcher |> List.filter Option.is_some
            |> List.map Option.get
          in
          let rec check lpush peeked =
            match (lpush, peeked) with
            | [], [] -> true
            | [], _ -> false
            | _, [] -> true
            | push :: lpush', peek :: peeked' when push = peek ->
                if
                  List.filteri (fun i _ -> i < List.length peeked') lpush'
                  = peeked'
                then true
                else check lpush' peeked
            | _ :: lpush', _ -> check lpush' peeked
          in
          check lpush peeked));
  ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Spsc_queue" [ ("spsc_queue", to_alcotest tests) ]
;;

main ()
