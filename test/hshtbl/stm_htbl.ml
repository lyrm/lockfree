open QCheck
open STM
module Htbl = Lockfree.Hshtbl

module WSDConf = struct
  type cmd = Insert of int * int (*| Remove of int | Find of int | Is_empty*)

  let show_cmd c =
    match c with
    | Insert (k, v) ->
        "Insert (" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")"
  (*| Remove k -> "Remove " ^ string_of_int k
    | Find k -> "Find " ^ string_of_int k
      | Is_empty -> "Is_empty"*)

  type state = int * (int * int) list
  type sut = int Htbl.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Insert (k, v)) int_gen int_gen;
           (*
           Gen.map (fun i -> Remove i) int_gen;
           Gen.map (fun i -> Find i) int_gen;
             Gen.return Is_empty;*)
         ])

  let init_state = (0, [])
  let arr_size = 8
  let max_size = arr_size * 3
  let init_sut () = Htbl.init arr_size
  let cleanup _ = ()

  let next_state c (n, s) =
    match c with
    | Insert (k, v) ->
        if List.mem_assoc k s then (n, s)
        else if n = max_size then (n, s)
        else (n + 1, (k, v) :: s)
  (*| Find _ -> (n, s)
    | Remove k ->
        if List.mem_assoc k s then (n - 1, List.remove_assoc k s) else (n, s)
    | Is_empty -> (n, s)
  *)

  let precond _ _ = true

  let run c t =
    match c with
    | Insert (k, v) ->
        Res (result bool exn, protect (fun t -> Htbl.insert k v t) t)
  (*| Remove k -> Res (bool, Htbl.remove k t)
    | Find k -> Res (option int, Htbl.find k t)
    | Is_empty -> Res (bool, Htbl.is_empty t)
  *)

  let postcond c ((n, s) : state) res =
    match (c, res) with
    | Insert (k, _), Res ((Result (Bool, Exn), _), res) ->
        if List.mem_assoc k s then res = Ok false
        else if n = max_size then res = Error Htbl.Full
        else res = Ok true
    (*| Find _k, Res ((Option Int, _), _res) -> true (*List.assoc_opt k s = res*)
      | Remove _k, Res ((Bool, _), _res) -> true (*
          if List.mem_assoc k s then res else res = false*)
      | Is_empty, Res ((Bool, _), _res) -> true (*(
                                                 match s with [] -> res | _ -> not res)*)*)
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count ~name:"STM Lockfree.Htbl test sequential";
      WSDT_dom.agree_test_par ~count ~name:"STM Lockfree.Htbl test parallel";
    ]
