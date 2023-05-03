let _ =
  let h = Lockfree.Atomicskiplist3.create () in
  for i = 0 to 100_000 do
    Lockfree.Atomicskiplist3.add h i |> ignore
  done;
  Gc.major ();
  Gc.print_stat stdout;
  Lockfree.Atomicskiplist3.add h 0
