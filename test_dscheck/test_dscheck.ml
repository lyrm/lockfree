module Atomic = Dscheck.TracedAtomic

let total_checked = ref 0

module C = struct
  let init () = Format.printf "init@."; Atomic.make 0

  let may_incr c a =
    let n =
      Atomic.get
        (Format.printf "1@.";
         c)
    in
    Atomic.compare_and_set c n
      (Format.printf "2@.";
       n + a)

  let may_decr c a =
    let n =
      Atomic.get
        (Format.printf "3@.";
         c)
    in
    Atomic.compare_and_set c n
      (Format.printf "4@.";
       n - a)
end

let _test_dummy0 ()=
  let c = C.init () in
  let _ = C.may_incr c 2 in
  let _ = C.may_decr c 10 in
  Format.printf "HERE %d@." (Atomic.get c);
  Atomic.spawn (fun () -> C.may_incr c 2 |> ignore );
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get c = -6))


let _test_dummy1 init add rm =
  let c = C.init () in
  let rec loop () = if C.may_incr c init then () else loop () in
  loop ();
  let was_incr = ref false in
  let was_decr = ref false in
  Atomic.spawn (fun () -> was_incr := C.may_incr c add);
  Atomic.spawn (fun () -> was_decr := C.may_decr c rm);
  Atomic.final (fun () ->
      total_checked := !total_checked + 1;
      Atomic.check (fun () ->
          let res = Atomic.get c in
          match (!was_incr, !was_decr) with
          | false, false ->
              Format.printf "1. Res = %d \n\n@." res;
              res = init
          | true, false ->
              Format.printf "2. Res = %d \n\n@." res;
              res = init + add
          | false, true ->
              Format.printf "3. Res = %d \n\n@." res;
              res = init - rm
          | true, true ->
              Format.printf "4. Res = %d \n\n@." res;
              res = init + add - rm))

let _test_dummy () =
  let c = C.init () in
  let was_incr = ref false in
  let was_decr = ref false in
  Atomic.spawn (fun () -> was_incr := C.may_incr c 1);
  Atomic.spawn (fun () -> was_decr := C.may_decr c 10);
  Atomic.final (fun () ->
      Atomic.check (fun () ->
          match (!was_incr, !was_decr) with
          | false, false ->
              Format.printf "1. Res = 0 \n\n@.";
              Atomic.get c = 0
          | true, false ->
              Format.printf "2. Res = 1 \n\n@.";
              Atomic.get c = 1
          | false, true ->
              Format.printf "3. Res = -10 \n\n@.";
              Atomic.get c = -10
          | true, true ->
              Format.printf "4. Res = -9 \n\n@.";
              Atomic.get c = -9))

let () =
  Atomic.trace (fun () -> _test_dummy0 ());
  Printf.printf "Total checked: %d\n" !total_checked
