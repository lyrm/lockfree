type t = { ready : int Atomic.t; total_domains : int; round : int Atomic.t }

let init ~total_domains =
  { ready = Atomic.make 0; total_domains; round = Atomic.make 0 }
;;

let wait_until_all_ready ?(round=0) { ready; total_domains; _ } =
  while Atomic.get ready < total_domains*(round+1) do
    ()
  done
;;

let worker ({ ready; round; _ } as t) f =
  Atomic.incr ready;
  wait_until_all_ready t;
  (* all domains are up at this point *)
  for i = 1 to Int.max_int do
    (* wait for signal to start work *)
    while Atomic.get round < i do
      ()
    done;
    f ~round:i;
    (* signal that we're done *)
    Atomic.incr ready;
  done
;;

let run ({ ready = _; round; total_domains = _ } as t) ?(drop_first = true) rounds =
  wait_until_all_ready t;
  (* all domains are up, can start benchmarks *)
  let results = ref [] in
  for i = 1 to rounds do

    let start_time = Unix.gettimeofday () in
    Atomic.incr round;
    wait_until_all_ready ~round:i t;
    let end_time = Unix.gettimeofday () in

    let diff = end_time -. start_time in
    if drop_first && i == 1 then () else results := diff :: !results
  done;
  !results
;;