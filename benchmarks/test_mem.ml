let _ =
  let h = Lockfree.Hshtbl.init ~size_exponent:12 in
  for i = 0 to 100_000 do
    Lockfree.Hshtbl.add i i h |> ignore
  done;
  Gc.major ();
  Gc.print_stat stdout;
Lockfree.Hshtbl.add 0 0 h
(*
let _ =
  let h = Stdlib.Hashtbl.create (1 lsl 12) in
  for i = 0 to 100_000 do
    Hashtbl.add h i i |> ignore
  done;
  Gc.major ();
  Gc.print_stat stdout;
  Hashtbl.add h 0 0
*)
