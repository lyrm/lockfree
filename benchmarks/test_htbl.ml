module type HSTBL = sig
  type 'a t

  val init : size_exponent:int -> 'a t
  val add : int -> 'a -> 'a t -> bool
  val find : int -> 'a t -> 'a option
  val mem : int -> 'a t -> bool
  val remove : int -> 'a t -> bool
end

module Hshtbl_seq : HSTBL = struct
  type 'a t = (int, 'a) Hashtbl.t

  let init ~size_exponent =
    let size = Int.shift_left 1 size_exponent in
    Hashtbl.create size

  let add k v t =
    Hashtbl.replace t k v;
    true

  let remove k t =
    if Hashtbl.mem t k then (
      Hashtbl.remove t k;
      true)
    else false

  let find k t = Hashtbl.find_opt t k
  let mem k t = Hashtbl.mem t k
end

(** [measure_and_launch_n_tests n t] launchs [n] times the test [t]
   and  returns the number of seconds required for the [n] tests. *)
let measure_and_launch (test : unit -> unit) =
  let ntries = 10 in
  let l =
    List.init ntries (fun _ ->
        let tstart = Unix.gettimeofday () in
        test ();
        let tend = Unix.gettimeofday () in
        tend -. tstart)
  in
  let l = List.sort compare l in

  List.nth l (ntries / 2) *. 100.

let test_add ntest (module H : HSTBL) =
  let t = H.init ~size_exponent:12 in
  Random.self_init ();
  measure_and_launch (fun () ->
      for _ = 0 to ntest do
        let k = Random.int ntest in
        H.add k k t |> ignore
      done)

let test_add_par ndomain ntest (module H : HSTBL) =
  let t = H.init ~size_exponent:12 in
  Random.self_init ();
  let nt = ntest / ndomain in
  let work () =
    for _ = 0 to nt do
      let k = Random.int ntest in
      H.add k k t |> ignore
    done
  in
  let test () =
    let domains = List.init ndomain (fun _ -> Domain.spawn work) in
    List.iter Domain.join domains
  in
  measure_and_launch test

let test_add_remove ntest (module H : HSTBL) =
  let t = H.init ~size_exponent:12 in
  Random.self_init ();
  let nr = max 100 (ntest / 10) in
  for _ = 0 to 1000 do
    let k = Random.int nr in
    H.add k k t |> ignore
  done;

  measure_and_launch (fun () ->
      for _ = 0 to ntest do
        match Random.int 2 with
        | 0 ->
            let k = Random.int nr in
            H.add k k t |> ignore
        | _ ->
            let k = Random.int nr in
            H.remove k t |> ignore
      done)

let test_add_remove_par ndomain ntest (module H : HSTBL) =
  let t = H.init ~size_exponent:12 in
  Random.self_init ();
  let nr = max 100 (ntest / 10) in
  for _ = 0 to 1000 do
    let k = Random.int nr in
    H.add k k t |> ignore
  done;
  let nt = ntest / ndomain in
  let work () =
    for _ = 0 to nt do
      match Random.int 2 with
      | 0 ->
          let k = Random.int nr in
          H.add k k t |> ignore
      | _ ->
          let k = Random.int nr in
          H.remove k t |> ignore
    done
  in
  let test () =
    let domains = List.init ndomain (fun _ -> Domain.spawn work) in

    List.iter Domain.join domains
  in
  measure_and_launch test

let main () =
  let all_implem =
    [
      ("seq", (module Hshtbl_seq : HSTBL), false);
      ("lf", (module Lockfree.Hshtbl : HSTBL), true);
      ("lf_r", (module Lockfree.Hshtbl_resizable : HSTBL), true);
    ]
  in
  let all_tests =
    [
      ("add", test_add, test_add_par, 100_000);
      ("add remove", test_add_remove, test_add_remove_par, 100_000);
    ]
  in

  List.iter
    (fun (test_name, test_seq, test_par, nt) ->
      Format.printf "*** Test %s ***@." test_name;
      Format.printf " ** Sequential **@.";
      List.iter
        (fun (name, (module H : HSTBL), _thread_safe) ->
          test_seq nt (module H : HSTBL) |> Format.printf "    %s : %.5f@." name)
        all_implem;
      Format.printf " ** Parallel **@.";
      Format.printf "  *** 2 domains ***@.";
      List.iter
        (fun (name, (module H : HSTBL), thread_safe) ->
          if thread_safe then
            test_par 2 nt (module H : HSTBL)
            |> Format.printf "    %s : %.5f@." name)
        all_implem;
      Format.printf "  *** 4 domains ***@.";
      List.iter
        (fun (name, (module H : HSTBL), thread_safe) ->
          if thread_safe then
            test_par 4 nt (module H : HSTBL)
            |> Format.printf "    %s : %.5f@." name)
        all_implem)
    all_tests

let _ = main ()
