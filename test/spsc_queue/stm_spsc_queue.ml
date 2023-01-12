(** Sequential and Parallel model-based tests of spsc_queue *)

open QCheck
open STM
open Util
module Spsc_queue = Lockfree.Spsc_queue

module WSDConf = struct
  type cmd = Push of int | Pop

  let show_cmd c =
    match c with Push i -> "Push " ^ string_of_int i | Pop -> "Pop"

  type state = int * int list
  type sut = int Spsc_queue.t

  let producer_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd (Gen.map (fun i -> Push i) int_gen)

  let consumer_cmd _s = QCheck.make ~print:show_cmd (Gen.return Pop)

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof [ Gen.return Pop; Gen.map (fun i -> Push i) int_gen ])

  let size_exponent = 4
  let max_size = Int.shift_left 1 size_exponent
  let init_state = (0, [])
  let init_sut () = Spsc_queue.create ~size_exponent
  let cleanup _ = ()

  let next_state c (n, s) =
    match c with
    | Push i -> if n = max_size then (n, s) else (n + 1, i :: s)
    | Pop -> (
        match List.rev s with [] -> (0, s) | _ :: s' -> (n - 1, List.rev s'))

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (result unit exn, protect (fun d -> Spsc_queue.push d i) d)
    | Pop -> Res (result (option int) exn, protect Spsc_queue.pop d)

  let postcond c ((n, s) : state) res =
    match (c, res) with
    | Push _, Res ((Result (Unit, Exn), _), res) -> (
        match res with
        | Error Spsc_queue.Full -> n = max_size
        | Ok () -> n < max_size
        | _ -> false)
    | Pop, Res ((Result (Option Int, Exn), _), res) -> (
        match (res, List.rev s) with
        | Ok None, [] -> true
        | Ok (Some j), x :: _ -> x = j
        | _ -> false)
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

(* The following definitions differ slightly from those in multicoretests:lib/STM.ml.
   This has to do with how this single consumer single producer queue is supposed to be
   used according to spec:
   - [agree_prop_par] differs in that it only spawns one domain ("a consumer domain")
     in parallel with a "producer domain" (it also uses [Semaphore.Binary]) *)
let agree_prop_par (seq_pref, producer, consumer) =
  assume (WSDT_seq.cmds_ok WSDConf.init_state (seq_pref @ producer));
  assume (WSDT_seq.cmds_ok WSDConf.init_state (seq_pref @ consumer));
  let sut = WSDConf.init_sut () in
  let pref_obs = WSDT_dom.interp_sut_res sut seq_pref in
  let sema = Semaphore.Binary.make false in
  let consumer_dom =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        WSDT_dom.interp_sut_res sut consumer)
  in
  while not (Semaphore.Binary.try_acquire sema) do
    Domain.cpu_relax ()
  done;
  let own_obs = WSDT_dom.interp_sut_res sut producer in
  let consumer_obs = Domain.join consumer_dom in
  let res =
    WSDT_dom.check_obs pref_obs own_obs consumer_obs WSDConf.init_state
  in
  let () = WSDConf.cleanup sut in
  res
  || Test.fail_reportf "  Results incompatible with linearized model:\n\n%s"
     @@ Util.print_triple_vertical ~center_prefix:false STM.show_res
          ( List.map snd pref_obs,
            List.map snd own_obs,
            List.map snd consumer_obs )

(* [arb_cmds_par] differs in what each triple component generates:
   "Producer domain" cmds can't be [Pop], "consumer domain" cmds can only be [Pop]. *)
let arb_cmds_par =
  WSDT_dom.arb_triple 20 15 WSDConf.producer_cmd WSDConf.producer_cmd
    WSDConf.consumer_cmd

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par ~count ~name =
  let rep_count = 25 in
  Test.make ~retries:10 ~count ~name arb_cmds_par
    (repeat rep_count agree_prop_par)

(* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
let agree_test_par_negative ~count ~name =
  WSDT_dom.neg_agree_test_par ~count ~name

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count ~name:"STM Lockfree.Spsc_queue test sequential";
      agree_test_par ~count ~name:"STM Lockfree.Spsc_queue test parallel";
      agree_test_par_negative ~count
        ~name:"STM Lockfree.Spsc_queue test parallel, negative";
    ]
