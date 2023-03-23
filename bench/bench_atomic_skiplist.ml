open Lockfree
let num_elems = 200_000

(* A Balanced workload with one thread with reads and another thread with writes (50% adds and 50% removes). *)
let balanced_workload () = 
  let sl = Atomicskiplist.create () in
  (* let elems = Array.init num_elems (fun _ -> Random.int 10000) in  *)
  let push = Domain.spawn ( fun () ->
    let start_time = Unix.gettimeofday ()  in 
    for i = 0 to (num_elems - 1) do ( 
      if (i/2) < num_elems/2 then 
        Atomicskiplist.add sl i |> ignore
    else
        Atomicskiplist.remove sl (i - (num_elems/2)) |> ignore)
    done;
    start_time
  ) in 
  for i = 0 to (num_elems - 1) do 
    Atomicskiplist.find sl i |> ignore
  done;
  let end_time = Unix.gettimeofday () in 
  let start_time = Domain.join push in 
  let time_diff = end_time -. start_time in 
  time_diff

let bench () =
  let results = ref [] in
  for i = 1 to 10 do
    let time = balanced_workload () in
    if i > 1 then results := time :: !results
  done;
  let results = List.sort Float.compare !results in
  let median_time = List.nth results 4 in
  let median_throughput = Float.of_int num_elems /. median_time in
  Benchmark_result.create_generic ~median_time ~median_throughput "atomic_skiplist_balanced"