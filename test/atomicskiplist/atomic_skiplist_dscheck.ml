let two_producers () = 
  Atomic.trace (fun () -> 
    let sl = Atomicskiplist.create () in 
    let items_total = 4 in 

    for i = 0 to 1 do 
      Atomic.spawn (fun () -> 
        for j = 1 to items_total / 2 do 
          print_int items_total;
          print_int (i+(2*j));
          print_int i;
          print_int j;
          ignore @@ Atomicskiplist.add sl (i + (2*j))
        done
        )
    done;

    Atomic.final (fun () -> 
        let present = ref true in 
        for i = 2 to items_total+1 do 
          present := !present && (Atomicskiplist.find sl i)
        done;
        Atomic.check (fun () -> !present)
      )
  )

let () = 
  let open Alcotest in 
    run "atomic_skiplist_dscheck"
    [
      ("basic", 
        [
          test_case "2-producers" `Slow two_producers;
        ]
      )
    ]