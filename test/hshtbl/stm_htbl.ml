open QCheck
open STM
module Htbl = Lockfree.Hshtbl

module WSDConf = struct
  type cmd =
    | Add of int * int
    | Replace of int * int
    | Remove of int
    | Find of int
    | Mem of int

  let show_cmd c =
    match c with
    | Add (k, v) -> "Add (" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")"
    | Replace (k, v) ->
        "Replace (" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")"
    | Remove k -> "Remove " ^ string_of_int k
    | Find k -> "Find " ^ string_of_int k
    | Mem k -> "Mem" ^ string_of_int k

  module S = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type state = int S.t
  type sut = int Htbl.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Add (k, v)) int_gen int_gen;
           Gen.map2 (fun k v -> Replace (k, v)) int_gen int_gen;
           Gen.map (fun i -> Remove i) int_gen;
           Gen.map (fun i -> Find i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
         ])

  let init_state = S.empty
  let init_sut () = Htbl.init ~size_exponent:12
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add (k, v) -> if S.mem k s then s else S.add k v s
    | Replace (k, v) -> S.add k v s
    | Find _ -> s
    | Remove k -> if S.mem k s then S.remove k s else s
    | Mem _k -> s

  let precond _ _ = true

  let run c t =
    match c with
    | Add (k, v) -> Res (bool, Htbl.add k v t)
    | Replace (k, v) -> Res (unit, Htbl.replace k v t)
    | Remove k -> Res (bool, Htbl.remove k t)
    | Find k -> Res (option int, Htbl.find k t)
    | Mem k -> Res (bool, Htbl.mem k t)

  let postcond c (s : state) res =
    match (c, res) with
    | Add (k, _), Res ((Bool, _), res) -> S.mem k s = not res
    | Replace (_, _), Res ((Unit, _), ()) -> true
    | Find k, Res ((Option Int, _), res) -> S.find_opt k s = res
    | Remove k, Res ((Bool, _), res) -> S.mem k s = res
    | Mem k, Res ((Bool, _), res) -> S.mem k s = res
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 200 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count ~name:"STM Lockfree.Htbl test sequential";
      WSDT_dom.agree_test_par ~count ~name:"STM Lockfree.Htbl test parallel";
    ]
