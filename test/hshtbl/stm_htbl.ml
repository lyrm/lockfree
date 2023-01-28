open QCheck
open STM
module Htbl = Lockfree.Hshtbl

module WSDConf = struct
  type cmd =
    | Insert_No_Resize of int * int
    | Remove of int
    | Find of int
    | Mem of int
    | Is_empty

  let show_cmd c =
    match c with
    | Insert_No_Resize (k, v) ->
        "Insert_No_Resize (" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")"
    | Remove k -> "Remove " ^ string_of_int k
    | Find k -> "Find " ^ string_of_int k
    | Mem k -> "Mem" ^ string_of_int k
    | Is_empty -> "Is_empty"

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
           Gen.map2 (fun k v -> Insert_No_Resize (k, v)) int_gen int_gen;
           Gen.map (fun i -> Remove i) int_gen;
           Gen.map (fun i -> Find i) int_gen;
           Gen.map (fun i -> Mem i) int_gen;
           Gen.return Is_empty;
         ])

  let init_state = S.empty
  let arr_size = 100
  let init_sut () = Htbl.init arr_size
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Insert_No_Resize (k, v) -> if S.mem k s then s else S.add k v s
    | Find _ -> s
    | Remove k -> if S.mem k s then S.remove k s else s
    | Mem _ -> s
    | Is_empty -> s

  let precond _ _ = true

  let run c t =
    match c with
    | Insert_No_Resize (k, v) -> Res (bool, Htbl.insert_no_resize k v t)
    | Remove k -> Res (bool, Htbl.remove k t)
    | Find k -> Res (option int, Htbl.find k t)
    | Mem k -> Res (bool, Htbl.mem k t)
    | Is_empty -> Res (bool, Htbl.is_empty t)

  let postcond c (s : state) res =
    match (c, res) with
    | Insert_No_Resize (k, _), Res ((Bool, _), res) -> S.mem k s = not res
    | Find k, Res ((Option Int, _), res) -> S.find_opt k s = res
    | Remove k, Res ((Bool, _), res) -> S.mem k s = res
    | Mem k, Res ((Bool, _), res) -> S.mem k s = res
    | Is_empty, Res ((Bool, _), res) -> S.is_empty s = res
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count ~name:"STM Lockfree.Htbl test sequential";
      WSDT_dom.agree_test_par ~count ~name:"STM Lockfree.Htbl test parallel";
    ]
