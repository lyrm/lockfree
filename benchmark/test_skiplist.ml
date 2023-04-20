module type SKIPLIST = sig
  type t

  val create : ?max_height:int -> unit -> t
  val add : t -> int -> bool
  val find : t -> int -> bool
  val remove : t -> int -> bool
end

module Atomicskiplist_seq : SKIPLIST = struct
  module S = Set.Make (struct
    type t = int

    let compare = compare
  end)

  type t = S.t ref * Mutex.t

  let create ?(max_height = 10) () =
    if max_height = -1 then (ref S.empty, Mutex.create ())
    else (ref S.empty, Mutex.create ())

  let add (t, mutex) k =
    Mutex.lock mutex;
    if S.mem k !t then (
      Mutex.unlock mutex;
      false)
    else (
      t := S.add k !t;
      Mutex.unlock mutex;
      true)

  let remove (t, mutex) k =
    Mutex.lock mutex;
    if S.mem k !t then (
      t := S.remove k !t;
      Mutex.unlock mutex;
      true)
    else (
      Mutex.unlock mutex;
      false)

  let find (t, _) k = S.mem k !t
end

(** [measure_and_launch_n_tests n t] launchs [n] times the test [t]
   and  returns the number of seconds required for the [n] tests. *)
let measure_and_launch (test : unit -> unit) =
  let ntries = 10 in
  let l =
    List.init ntries (fun _ ->
        Gc.major ();
        let tstart = Unix.gettimeofday () in
        test ();
        let tend = Unix.gettimeofday () in
        tend -. tstart)
  in
  let l = List.sort compare l in

  List.nth l (ntries / 2) *. 100.

let _test_add ndomain ntest (module H : SKIPLIST) =
  let t = H.create () in
  Random.self_init ();
  let mutex = Atomic.make 0 in
  let nt = ntest / ndomain in

  let work () =
    Atomic.incr mutex;
    while Atomic.get mutex < ndomain do
      Domain.cpu_relax ()
    done;
    for _ = 0 to nt do
      let k = Random.int ntest in
      H.add t k |> ignore
    done
  in
  let test () =
    Atomic.set mutex 0;
    let domains = List.init (ndomain - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join domains
  in

  measure_and_launch test

let _test_add_remove ndomain ntest (module H : SKIPLIST) =
  let t = H.create () in
  Random.self_init ();
  let nr = max 100 (ntest / 10) in
  for i = 0 to nr / 10 do
    H.add t (i * 10) |> ignore
  done;

  let mutex = Atomic.make 0 in

  let nt = ntest / ndomain in

  let work () =
    Atomic.incr mutex;
    while Atomic.get mutex < ndomain do
      Domain.cpu_relax ()
    done;
    for _ = 0 to nt do
      match Random.int 2 with
      | 0 ->
          let k = Random.int nr in
          H.add t k |> ignore
      | _ ->
          let k = Random.int nr in
          H.remove t k |> ignore
    done
  in
  let test () =
    Atomic.set mutex 0;
    let domains = List.init (ndomain - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join domains
  in
  measure_and_launch test

let _test_find ndomain ntest (module H : SKIPLIST) =
  let t = H.create () in
  Random.self_init ();
  let max_value = 10_000 in
  for _ = 0 to 1000 do
    H.add t (Random.int max_value) |> ignore
  done;

  let mutex = Atomic.make 0 in
  let nt = ntest / ndomain in

  let work () =
    Atomic.incr mutex;
    while Atomic.get mutex < ndomain do
      Domain.cpu_relax ()
    done;
    for _ = 0 to nt do
      H.find t (Random.int max_value) |> ignore
    done
  in
  let test () =
    Atomic.set mutex 0;
    let domains = List.init (ndomain - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join domains
  in
  measure_and_launch test

let main () =
  let all_implem =
    [
      ("seq", (module Atomicskiplist_seq : SKIPLIST), true);
      ("lf", (module Lockfree.Atomicskiplist : SKIPLIST), true);
      ("lf3", (module Lockfree.Atomicskiplist3 : SKIPLIST), true);
    ]
  in
  let all_tests =
    [
      ("add", _test_add, 10_000);
      ("add remove", _test_add_remove, 10_000);
      ("find", _test_find, 100_000);
    ]
  in

  List.iter
    (fun (test_name, test, nt) ->
      Format.printf "*** Test %s ***@." test_name;
      List.iter
        (fun ndomain ->
          Format.printf " ** Number of domain %d **@." ndomain;
          List.iter
            (fun (name, (module H : SKIPLIST), _thread_safe) ->
              test ndomain nt (module H : SKIPLIST)
              |> Format.printf "    %s : %.5f@." name)
            all_implem)
        [ 1; 2; 4; 8 ])
    all_tests

let _ = main ()
