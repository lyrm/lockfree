(* ********************************************************************)
(** Push / steal_one *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    test queue
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

(******* steal_one/push seq *)
let test1 queue =
  if Local.push queue 1 then () else failwith "push failed";
  if Local.steal_one queue <> 1 then failwith "steal failed."

let test1o queue =
  if O.Local.push queue 1 then () else failwith "push failed";
  if O.Local.steal_one queue <> 1 then failwith "steal failed."

let nt = 1_000_000

let _ =
  let r = measure nt create test1 in
  Format.printf "Result test 1 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test1o in
  Format.printf "nold: %f\n" r

(******* steal_one/push par *)
let test2 queue =
  let sema = Semaphore.Binary.make false in

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        if Local.push queue 1 then () else failwith "Push has failed.")
  in

  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        try Some (Local.steal_one queue) with Exit -> None)
  in
  Domain.join dp;
  let stolen = Domain.join ds in
  match stolen with
  | Some 1 -> ()
  | None -> if Local.steal_one queue = 1 then () else raise Exit
  | _ -> raise Exit

let test2o queue =
  let sema = Semaphore.Binary.make false in

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        if O.Local.push queue 1 then () else failwith "Push has failed.")
  in

  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        try Some (O.Local.steal_one queue) with Exit -> None)
  in
  Domain.join dp;
  let stolen = Domain.join ds in
  match stolen with
  | Some 1 -> ()
  | None -> if O.Local.steal_one queue = 1 then () else raise Exit
  | _ -> raise Exit

let nt = 10_000

let _ =
  let r = measure nt create test2 in
  Format.printf "Result test 2 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test2o in
  Format.printf "nold: %f\n" r

(******* steal_one/push par with several pushes and steals *)
let test2b nto_push =
  let queue = create ~size_exponent:8 () in
  let sema = Semaphore.Binary.make false in

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        let to_push = List.init nto_push (fun i -> i) in
        List.iter
          (fun elt ->
            let is_pushed = Local.push queue elt in
            if not is_pushed then failwith "Push has failed.")
          to_push)
  in
  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        let s = Array.make nto_push None in
        for i = 0 to nto_push - 1 do
          let stolen = try Some (Local.steal_one queue) with Exit -> None in
          s.(i) <- stolen
        done;
        s)
  in
  Domain.join dp;
  let stolen = Domain.join ds in
  (stolen, queue)

(* ******************************************************************* *)

(** Push / steal *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in
  let local = create ~size_exponent:8 () in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    (test local queue : unit)
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

let test3 local queue =
  if Local.push queue 1 then () else failwith "push failed";
  if Local.steal ~from:queue local <> 1 then failwith "steal failed.";
  if Local.pop local <> Some 1 then failwith "pop failed"

let test3o local queue =
  let queue = O.create ~size_exponent:6 () in
  if O.Local.push queue 1 then () else failwith "push failed";
  if O.Local.steal ~from:queue local <> 1 then failwith "steal failed.";
  if O.Local.pop local <> Some 1 then failwith "pop failed"

let nt = 10_000_000

let _ =
  let r = measure nt create test3 in
  Format.printf "Result test 3 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test3o in
  Format.printf "nold: %f\n" r

(******* steal/push par *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in
  let local = create ~size_exponent:8 () in
  let to_push = List.init 100 (fun i -> i) in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    (test local queue to_push : unit)
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

let test4 queue local to_push =
  let sema = Semaphore.Binary.make false in

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        List.iter
          (fun elt ->
            Domain.cpu_relax ();
            if Local.push queue elt then () else failwith "Push has failed.")
          to_push)
  in

  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        Local.steal ~from:queue local)
  in
  Domain.join dp;
  let nb_stolen = Domain.join ds in
  List.iter
    (fun elt ->
      if elt >= nb_stolen then (
        if Local.pop queue <> Some elt then failwith "pop failed")
      else if Local.pop local <> Some elt then failwith "pop failed2")
    to_push

let test4o queue local to_push =
  let sema = Semaphore.Binary.make false in

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        List.iter
          (fun elt ->
            Domain.cpu_relax ();
            if O.Local.push queue elt then () else failwith "Push has failed.")
          to_push)
  in

  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        O.Local.steal ~from:queue local)
  in
  Domain.join dp;
  let nb_stolen = Domain.join ds in
  List.iter
    (fun elt ->
      if elt >= nb_stolen then (
        if O.Local.pop queue <> Some elt then failwith "pop failed")
      else if O.Local.pop local <> Some elt then failwith "pop failed2")
    to_push

let nt = 10_000

let _ =
  let r = measure nt create test4 in
  Format.printf "Result test 4 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test4o in
  Format.printf "nold: %f\n" r

(* ******************************************************************* *)

(** Pop / steal_one *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    (test queue : unit)
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

let test5 queue =
  if Local.push queue 13 then () else failwith "push failed";
  if Local.steal_one queue <> 13 then failwith "steal failed.";
  if Local.pop queue <> None then failwith "pop failed"

let test5o queue =
  if O.Local.push queue 13 then () else failwith "push failed";
  if O.Local.steal_one queue <> 13 then failwith "steal failed.";
  if O.Local.pop queue <> None then failwith "pop failed"

let nt = 1_000_000

let _ =
  let r = measure nt create test5 in
  Format.printf "Result test 5 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test5o in
  Format.printf "nold: %f\n" r

let test5' queue =
  if Local.push queue 13 then () else failwith "push failed";
  if Local.pop queue <> Some 13 then failwith "pop failed";
  if
    try
      Local.steal_one queue |> ignore;
      true
    with Exit -> false
  then failwith "steal failed."

let test5o' queue =
  if O.Local.push queue 1 then () else failwith "push failed";
  if O.Local.pop queue <> Some 1 then failwith "pop failed";
  if
    try
      O.Local.steal_one queue |> ignore;
      true
    with Exit -> false
  then failwith "steal failed."

let nt = 1_000_000

let _ =
  let r = measure nt create test5' in
  Format.printf "Result test 5' :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test5o' in
  Format.printf "nold: %f\n" r

(* ******************************************************************* *)

(** Pop / steal *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in
  let local = create ~size_exponent:8 () in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    (test local queue : unit)
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

(******* sequential push steal pop *)
let test6 local queue =
  if Local.push queue 13 then () else failwith "push failed";
  if Local.steal ~from:queue local <> 1 then failwith "steal failed.";
  if Local.pop local <> Some 13 then failwith "pop failed";
  if Local.pop queue <> None then failwith "pop failed"

let test6o local queue =
  if O.Local.push queue 13 then () else failwith "push failed";
  if O.Local.steal ~from:queue local <> 1 then failwith "steal failed.";
  if O.Local.pop local <> Some 13 then failwith "pop failed";
  if O.Local.pop queue <> None then failwith "pop failed"

let nt = 1_000_000

let _ =
  let r = measure nt create test6 in
  Format.printf "Result test 6 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test6o in
  Format.printf "nold: %f\n" r

(******* sequential push pop steal *)
let test7 local queue =
  if Local.push queue 13 then () else failwith "push failed";
  if Local.pop queue <> Some 13 then failwith "pop failed";
  if Local.steal ~from:queue local <> 0 then failwith "steal failed."

let test7o local queue =
  if O.Local.push queue 13 then () else failwith "push failed";
  if O.Local.pop queue <> Some 13 then failwith "pop failed";
  if O.Local.steal ~from:queue local <> 0 then failwith "steal failed."

let nt = 1_000_000

let _ =
  let r = measure nt create test7 in
  Format.printf "Result test 7 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test7o in
  Format.printf "nold: %f\n" r

(******* multiple pushes then steal and pop *)
let test8 local queue =
  let l = List.init 100 (fun i -> i) in
  List.iter
    (fun elt -> if Local.push queue elt then () else failwith "push failed")
    l;
  if Local.steal ~from:queue local <> 50 then failwith "steal failed.";
  List.iter
    (fun elt ->
      if elt >= 50 then (
        if Local.pop queue <> Some elt then failwith "pop failed")
      else if Local.pop local <> Some elt then failwith "pop failed2")
    l

let test8o local queue =
  let l = List.init 100 (fun i -> i) in
  List.iter
    (fun elt -> if O.Local.push queue elt then () else failwith "push failed")
    l;

  if O.Local.steal ~from:queue local <> 50 then failwith "steal failed.";
  List.iter
    (fun elt ->
      if elt >= 50 then (
        if O.Local.pop queue <> Some elt then failwith "pop failed")
      else if O.Local.pop local <> Some elt then failwith "pop failed2")
    l

let nt = 100_000

let _ =
  let r = measure nt create test8 in
  Format.printf "Result test 8 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test8o in
  Format.printf "nold: %f\n" r

(*******  pop/steal parallel *)
let measure nt create test =
  let queue = create ~size_exponent:8 () in
  let local = create ~size_exponent:8 () in
  let to_push = List.init 100 (fun i -> i) in

  let t = Unix.gettimeofday () in
  for i = 0 to nt do
    (test local queue to_push : unit)
  done;
  let t1 = Unix.gettimeofday () in
  t1 -. t

let test9 queue local to_push =
  let sema = Semaphore.Binary.make false in
  List.iter
    (fun elt ->
      if Local.push queue elt then () else failwith "Push has failed.")
    to_push;

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        let popped = ref [] in
        let t = ref true in
        while !t do
          let p = Local.pop queue in
          if Option.is_some p then popped := Option.get p :: !popped
          else t := false
        done;
        !popped)
  in
  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        let stolen = ref 0 in
        for i = 0 to 10 do
          stolen := !stolen + try Local.steal ~from:queue local with Exit -> 0
        done;
        !stolen)
  in
  let popped = Domain.join dp in
  let nb_stolen = Domain.join ds in
  let stolen =
    let aux = ref [] in
    for _ = 0 to nb_stolen - 1 do
      aux := Option.get (Local.pop local) :: !aux
    done;
    !aux
  in
  let t1 = List.length popped = 100 - nb_stolen in
  let t2 = List.sort Stdlib.compare popped = List.rev popped in
  let t3 = List.sort Stdlib.compare (popped @ stolen) = to_push in
  let t4 = List.sort Stdlib.compare stolen = List.rev stolen in
  if not (t1 && t2 && t3 && t4) then failwith "something wrong"

let test9o queue local to_push =
  let sema = Semaphore.Binary.make false in
  List.iter
    (fun elt ->
      if O.Local.push queue elt then () else failwith "Push has failed.")
    to_push;

  let dp =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        let popped = ref [] in
        let t = ref true in
        while !t do
          let p = O.Local.pop queue in
          if Option.is_some p then popped := Option.get p :: !popped
          else t := false
        done;
        !popped)
  in
  let ds =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        let stolen = ref 0 in
        for i = 0 to 10 do
          stolen :=
            !stolen + try O.Local.steal ~from:queue local with Exit -> 0
        done;
        !stolen)
  in
  let popped = Domain.join dp in
  let nb_stolen = Domain.join ds in
  let stolen =
    let aux = ref [] in
    for _ = 0 to nb_stolen - 1 do
      aux := Option.get (O.Local.pop local) :: !aux
    done;
    !aux
  in
  let t1 = List.length popped = 100 - nb_stolen in
  let t2 = List.sort Stdlib.compare popped = List.rev popped in
  let t3 = List.sort Stdlib.compare (popped @ stolen) = to_push in
  let t4 = List.sort Stdlib.compare stolen = List.rev stolen in
  if not (t1 && t2 && t3 && t4) then failwith "something wrong"

let nt = 10_000

let _ =
  let r = measure nt create test9 in
  Format.printf "Result test 9 :\nnew: %f\n" r

let _ =
  let r = measure nt O.create test9o in
  Format.printf "nold: %f\n" r

(** ***************************************************************** *)

(** with multiple stealers *)
let main () =
  let num_of_stealers = 4 in
  let num_of_elements = ref 2_000_000 in

  let owner queue =
    let left = ref !num_of_elements in
    while !left > 0 do
      (* insert items as owner*)
      for _ = 0 to Random.int 1024 - 1 do
        if Local.push queue 0 then left := !left - 1
      done;
      (* pop *)
      while Option.is_some (Local.pop queue) do
        ()
      done
    done;
    (* drain whatever remains in the queue *)
    while Option.is_some (Local.pop queue) do
      ()
    done
  in

  let stealer victim_queue =
    let queue = create ~size_exponent:10 () in
    let stolen = ref 0 in
    let tries = ref 0 in
    while !stolen = 0 && !tries < 100 do
      incr tries;
      stolen := Local.steal ~from:victim_queue queue
    done;
    while Option.is_some (Local.pop queue) do
      ()
    done;
    !stolen
  in

  let run () =
    let sema = Semaphore.Counting.make (num_of_stealers + 1) in
    let queue = create ~size_exponent:10 () in

    for _ = 0 to 200 do
      Local.push queue 0 |> ignore
    done;

    let producer =
      Domain.spawn (fun () ->
          Semaphore.Counting.acquire sema;
          while Semaphore.Counting.get_value sema <> 0 do
            Domain.cpu_relax ()
          done;
          owner queue)
    in
    let stealers =
      List.map
        (fun _ ->
          Domain.spawn (fun () ->
              Semaphore.Counting.acquire sema;
              while Semaphore.Counting.get_value sema <> 0 do
                Domain.cpu_relax ()
              done;
              stealer queue))
        (List.init num_of_stealers (fun _ -> ()))
    in
    Domain.join producer;
    let stolen = List.map Domain.join stealers in
    stolen
  in
  run ()

let main_o () =
  let num_of_stealers = 4 in
  let num_of_elements = ref 2_000_000 in

  let owner queue =
    let left = ref !num_of_elements in
    while !left > 0 do
      (* insert items as owner*)
      for _ = 0 to Random.int 1024 - 1 do
        if O.Local.push queue 0 then left := !left - 1
      done;
      (* pop *)
      while Option.is_some (O.Local.pop queue) do
        ()
      done
    done;
    (* drain whatever remains in the queue *)
    while Option.is_some (O.Local.pop queue) do
      ()
    done
  in

  let stealer victim_queue =
    let queue = O.create ~size_exponent:10 () in
    let stolen = ref 0 in
    let tries = ref 0 in
    while !stolen = 0 && !tries < 100 do
      incr tries;
      stolen := O.Local.steal ~from:victim_queue queue
    done;
    while Option.is_some (O.Local.pop queue) do
      ()
    done;
    !stolen
  in

  let run () =
    let sema = Semaphore.Counting.make (num_of_stealers + 1) in
    let queue = O.create ~size_exponent:10 () in

    for _ = 0 to 200 do
      Local.push queue 0 |> ignore
    done;

    let producer =
      Domain.spawn (fun () ->
          Semaphore.Counting.acquire sema;
          while Semaphore.Counting.get_value sema <> 0 do
            Domain.cpu_relax ()
          done;
          owner queue)
    in
    let stealers =
      List.map
        (fun _ ->
          Domain.spawn (fun () ->
              Semaphore.Counting.acquire sema;
              while Semaphore.Counting.get_value sema <> 0 do
                Domain.cpu_relax ()
              done;
              stealer queue))
        (List.init num_of_stealers (fun _ -> ()))
    in
    Domain.join producer;
    let stolen = List.map Domain.join stealers in
    stolen
  in
  run ()

(* trying to change the test so that producer never have to pop*)
let main () =
  let num_of_stealers = 1 in
  let num_of_elements = ref 2_000_000 in

  (* pushing everything *)
  let owner queue =
    let left = ref !num_of_elements in
    while !left > 0 do
      (* insert items as owner*)
      if Local.push queue 0 then left := !left - 1
    done
  in

  let stealer victim_queue =
    let queue = create ~size_exponent:10 () in
    let stolen = ref 0 in
    let tries = ref 0 in
    while !tries < 1000 do
      let s = Local.steal ~from:victim_queue queue in
      if s = 0 then incr tries
      else (
        tries := 0;
        stolen := !stolen + s;
        while Option.is_some (Local.pop queue) do
          ()
        done)
    done;
    !stolen
  in

  let run () =
    let sema = Semaphore.Counting.make (num_of_stealers + 1) in
    let queue = create ~size_exponent:10 () in

    let producer =
      Domain.spawn (fun () ->
          Semaphore.Counting.acquire sema;
          while Semaphore.Counting.get_value sema <> 0 do
            Domain.cpu_relax ()
          done;
          owner queue)
    in
    let stealers =
      List.map
        (fun _ ->
          Domain.spawn (fun () ->
              Semaphore.Counting.acquire sema;
              while Semaphore.Counting.get_value sema <> 0 do
                Domain.cpu_relax ()
              done;
              stealer queue))
        (List.init num_of_stealers (fun _ -> ()))
    in
    Domain.join producer;
    let stolen = List.map Domain.join stealers in
    stolen
  in
  run ()
