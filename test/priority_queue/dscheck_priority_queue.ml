open Priority_queue

module Atomic = Dscheck.TracedAtomic
(** This is needed in this order as the skiplist.ml file contains
    {[
      module Atomic = Multicore_magic.Transparent_atomic
    ]}
    which is in multicore-magic-dscheck library only a subset of
    [Dscheck.TracedAtomic] function. *)

let _test_max_height_of () =
  let s = create ~max_height:1 ~compare () in
  assert (max_height_of s = 1);
  let s = create ~max_height:10 ~compare () in
  assert (max_height_of s = 10);
  let s = create ~max_height:30 ~compare () in
  assert (max_height_of s = 30)

let _two_mem () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let found1 = ref false in
      let found2 = ref false in

      Atomic.spawn (fun () ->
          add pq 1 1;
          found1 := mem pq 1);

      Atomic.spawn (fun () -> found2 := mem pq 2);

      Atomic.final (fun () -> Atomic.check (fun () -> !found1 && not !found2)))

let _two_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:3 ~compare:Int.compare () in

      Atomic.spawn (fun () -> add pq 1 1);
      Atomic.spawn (fun () -> add pq 2 2);

      Atomic.final (fun () -> Atomic.check (fun () -> mem pq 1 && mem pq 2)))

let _two_add_same () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:3 ~compare:Int.compare () in

      Atomic.spawn (fun () -> add pq 1 1);
      Atomic.spawn (fun () -> add pq 1 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              find_opt pq 1 = Some 1 || find_opt pq 1 = Some 2)))

let _two_remove () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in

      Atomic.spawn (fun () ->
          add pq 1 1;
          removed1 := remove_min_opt pq);

      Atomic.spawn (fun () -> removed2 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              match (!removed1, !removed2) with
              | None, Some (1, 1) | Some (1, 1), None -> true
              | _ -> false);
          Atomic.check (fun () -> not @@ mem pq 1)))

let _two_remove2 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:2 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in
      let removed3 = ref None in
      add pq 1 1;

      Atomic.spawn (fun () -> removed1 := remove_min_opt pq);

      Atomic.spawn (fun () ->
          removed2 := remove_min_opt pq;
          removed3 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !removed3 = None);
          Atomic.check (fun () ->
              match (!removed1, !removed2) with
              | None, Some (1, 1) | Some (1, 1), None -> true
              | _ -> false);
          Atomic.check (fun () -> not @@ mem pq 1)))

let _two_remove_fifo () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in

      Atomic.spawn (fun () ->
          removed1 := remove_min_opt pq;
          removed2 := remove_min_opt pq);
      Atomic.spawn (fun () ->
          add pq 1 1;
          add pq 2 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              match (!removed1, !removed2) with
              | None, None -> mem pq 1 && mem pq 2
              | Some (1, 1), None | None, Some (1, 1) ->
                  (not @@ mem pq 1) && mem pq 2
              | Some (1, 1), Some (2, 2) -> true
              | _ -> false)))

let _two_remove_add () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in

      Atomic.spawn (fun () ->
          add pq 1 1;
          removed1 := remove_min_opt pq);
      Atomic.spawn (fun () -> add pq 1 2);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!removed1 = Some (1, 1) && remove_min_opt pq = Some (1, 2))
              || (!removed1 = Some (1, 2) && remove_min_opt pq = Some (1, 1)))))

let _two_remove_add2 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in

      Atomic.spawn (fun () ->
          add pq 2 2;
          removed1 := remove_min_opt pq);
      Atomic.spawn (fun () -> add pq 1 1);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              (!removed1 = Some (1, 1) && remove_min_opt pq = Some (2, 2))
              || (!removed1 = Some (2, 2) && remove_min_opt pq = Some (1, 1)))))

let _two_remove_add3 () =
  Atomic.trace (fun () ->
      Random.init 0;
      let pq = create ~max_height:1 ~compare:Int.compare () in
      let removed1 = ref None in
      let removed2 = ref None in
      let removed3 = ref None in

      add pq 2 2;
      add pq 2 10;

      Atomic.spawn (fun () -> removed1 := remove_min_opt pq);
      Atomic.spawn (fun () ->
          add pq 4 10;
          removed2 := remove_min_opt pq;
          removed3 := remove_min_opt pq);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              !removed1 != None && !removed2 != None && !removed3 != None)))

let () =
  let open Alcotest in
  run "DSCheck_Skiplist"
    [
      ( "basic",
        [
          test_case "max_height_of" `Quick _test_max_height_of;
          test_case "2-mem" `Slow _two_mem;
          test_case "2-add-same" `Slow _two_add_same;
          test_case "2-add" `Slow _two_add;
          test_case "2-remove" `Slow _two_remove;
          test_case "2-remove2" `Slow _two_remove2;
          (* test_case "2-remove-fifo" `Slow _two_remove_fifo; *)
          test_case "2-remove-add" `Slow _two_remove_add;
          test_case "2-remove-add2" `Slow _two_remove_add2;
          test_case "2-remove-add3" `Slow _two_remove_add3;
        ] );
    ]
