(** Sequential and Parallel model-based tests of Treiber stack *)

open QCheck
open STM
module Stack = Saturn_lockfree.Stack

module Spec = struct
  type cmd =
    | Push of int
    | Push_all of int list (* add_seq uses the same function as push_all *)
    | Pop_opt
    (* peek_exn and drop_exn use the same function as pop_exn*)
    | Pop_all
    | Peek_opt
    (* peek_exn uses the same function as peek_exn *)
    | Try_compare_and_pop of int
    | Try_compare_and_set of int * int
    | Try_set of int
    (* set_exn uses the same function as try_set *)
    | Is_empty
    | To_seq

  let string_of_int_list l =
    "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Push_all l -> "Push_all " ^ string_of_int_list l
    | Pop_opt -> "Pop_opt"
    | Pop_all -> "Pop_all"
    | Peek_opt -> "Peek_opt"
    | Try_compare_and_pop i -> "Try_compare_and_pop " ^ string_of_int i
    | Try_compare_and_set (i, j) ->
        "Try_compare_and_set (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"
    | Try_set i -> "Try_set " ^ string_of_int i
    | Is_empty -> "Is_empty"
    | To_seq -> "To_seq"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.map (fun l -> Push_all l) (Gen.list int_gen);
           Gen.return Pop_opt;
           Gen.return Pop_all;
           Gen.return Peek_opt;
           Gen.map (fun i -> Try_compare_and_pop i) int_gen;
           Gen.map2 (fun i j -> Try_compare_and_set (i, j)) int_gen int_gen;
           Gen.map (fun i -> Try_set i) int_gen;
           Gen.return Is_empty;
           Gen.return To_seq;
         ])

  let init_state = []
  let init_sut () = Stack.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push i -> i :: s
    | Push_all l -> List.rev l @ s
    | Pop_opt -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_all -> []
    | Peek_opt -> s
    | Try_compare_and_pop i -> (
        match s with [] -> [] | hd :: tl -> if hd = i then tl else s)
    | Try_compare_and_set (i, j) -> (
        match s with [] -> [] | hd :: tl -> if hd = i then j :: tl else s)
    | Try_set i -> ( match s with [] -> s | _ :: tl -> i :: tl)
    | Is_empty -> s
    | To_seq -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Stack.push d i)
    | Push_all l -> Res (unit, Stack.push_all d l)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | Try_compare_and_pop i -> Res (bool, Stack.try_compare_and_pop d i)
    | Try_compare_and_set (i, j) -> Res (bool, Stack.try_compare_and_set d i j)
    | Try_set i -> Res (bool, Stack.try_set d i)
    | Is_empty -> Res (bool, Stack.is_empty d)
    | To_seq -> Res (seq int, Stack.to_seq d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Push_all _, Res ((Unit, _), _) -> true
    | Try_compare_and_pop i, Res ((Bool, _), res) -> (
        match s with [] -> res = false | hd :: _ -> res = (hd = i))
    | Try_compare_and_set (i, _), Res ((Bool, _), res) -> (
        match s with [] -> res = false | hd :: _ -> res = (hd = i))
    | Try_set _, Res ((Bool, _), res) -> res = (s <> [])
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | To_seq, Res ((Seq Int, _), res) -> List.of_seq res = s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn_lockfree.Bounded_stack" (module Spec) |> exit
