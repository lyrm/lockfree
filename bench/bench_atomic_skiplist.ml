open Lockfree
let num_elems = 200_000
let num_threads = 2

(* A write heavy workload with threads with 50% adds and 50% removes. *)
let write_heavy_workload () = 
  let sl = Atomicskiplist.create () in
  (* let elems = Array.init num_elems (fun _ -> Random.int 10000) in  *)
  let push = (fun () -> Domain.spawn ( fun () ->
    let start_time = Unix.gettimeofday () in 
    for i = 0 to (num_elems - 1) do ( 
      if (i/2) < num_elems/2 then 
        Atomicskiplist.add sl (i) |> ignore
    else
        Atomicskiplist.remove sl (i - (num_elems/2)) |> ignore)
    done;
    start_time
  )) in 
  let threads = List.init num_threads (fun _ -> push ()) in 
  let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
  let end_time = Unix.gettimeofday () in 
  let time_diff = end_time -. (List.nth start_time_threads 0) in 
  time_diff

(* A regular workload with 90% reads, 9% adds and 1% removes. *)
let read_heavy_workload () = 
  let sl = Atomicskiplist.create () in
  let elems = Array.init num_elems (fun _ -> Random.int 10000) in 
  let push = (fun () -> Domain.spawn ( fun () ->
    let start_time = Unix.gettimeofday () in 
    for i = 0 to (num_elems - 1) do ( 
      if i mod 1000 < 90 then 
        Atomicskiplist.add sl elems.(i) |> ignore
      else if i mod 1000 >= 90 && i mod 1000 < 100 then 
        Atomicskiplist.remove sl elems.(i) |> ignore
      else 
        Atomicskiplist.find sl elems.(i) |> ignore
    )
    done;
    start_time
  )) in 
  let threads = List.init num_threads (fun _ -> push ()) in 
  let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
  let end_time = Unix.gettimeofday () in 
  let time_diff = end_time -. (List.nth start_time_threads 0) in 
  time_diff
  
  
  let moderate_heavy_workload () = 
    let sl = Atomicskiplist.create () in
    let elems = Array.init num_elems (fun _ -> Random.int 10000) in 
    let push = (fun () -> Domain.spawn ( fun () ->
      let start_time = Unix.gettimeofday () in 
      for i = 0 to (num_elems - 1) do ( 
        if i mod 1000 < 200 then 
          Atomicskiplist.add sl elems.(i) |> ignore
        else if i mod 1000 >= 200 && i mod 1000 < 300 then 
          Atomicskiplist.remove sl elems.(i) |> ignore
        else 
          Atomicskiplist.find sl elems.(i) |> ignore
      )
      done;
      start_time
    )) in 
    let threads = List.init num_threads (fun _ -> push ()) in 
    let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
    let end_time = Unix.gettimeofday () in 
    let time_diff = end_time -. (List.nth start_time_threads 0) in 
    time_diff
    


let bench ~workload_type () =
  let workload =
    if workload_type = "read_heavy" then 
      read_heavy_workload
    else if workload_type == "moderate_heavy" then 
      moderate_heavy_workload
    else 
      write_heavy_workload
    in
  let results = ref [] in
  for i = 1 to 10 do
    let time = workload () in
    if i > 1 then results := time :: !results
  done;
  let results = List.sort Float.compare !results in
  let median_time = List.nth results 4 in
  let median_throughput = Float.of_int num_elems /. median_time in
  Benchmark_result.create_generic ~median_time ~median_throughput ("atomic_skiplist_"^workload_type)