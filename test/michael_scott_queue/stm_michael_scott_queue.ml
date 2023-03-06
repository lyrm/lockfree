(** Sequential and Parallel model-based tests of michael_scott_queue *)

open QCheck
open STM
module Ms_queue = Lockfree.Michael_scott_queue

module WSDConf = struct
  type cmd = Push of int | Pop | Clean_until of (int -> bool) | Is_empty

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Clean_until _ -> "Clean_until"
    | Is_empty -> "Is_empty"

  type state = int list
  type sut = int Ms_queue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           Gen.return Is_empty;
           Gen.oneof
             [
               Gen.return @@ Clean_until (fun _ -> true);
               Gen.return @@ Clean_until (fun _ -> false);
             ];
         ])

  let init_state = []
  let init_sut () = Ms_queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> ( match List.rev s with [] -> s | _ :: s' -> List.rev s')
    | Is_empty -> s
    | Clean_until pred ->
        let rec clean_until s =
          match s with
          | [] -> []
          | x :: xs -> if pred x then s else clean_until xs
        in
        clean_until (List.rev s) |> List.rev

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Ms_queue.push d i)
    | Pop -> Res (option int, Ms_queue.pop d)
    | Clean_until pred -> Res (unit, Ms_queue.clean_until d pred)
    | Is_empty -> Res (bool, Ms_queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Pop, Res ((Option Int, _), res) -> (
        match List.rev s with [] -> res = None | j :: _ -> res = Some j)
    | Clean_until _, Res ((Unit, _), _) -> true
    | Is_empty, Res ((Bool, _), res) -> (
        match s with [] -> res | _ -> not res)
    | _, _ -> false
end

module WSDT_seq = STM_sequential.Make (WSDConf)
module WSDT_dom = STM_domain.Make (WSDConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      WSDT_seq.agree_test ~count
        ~name:"STM Lockfree.Michael_scott_queue test sequential";
      WSDT_dom.agree_test_par ~count
        ~name:"STM Lockfree.Michael_scott_queue test parallel";
    ]
