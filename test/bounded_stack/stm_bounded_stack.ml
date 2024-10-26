(** Sequential and Parallel model-based tests of bounded_queue *)

open QCheck
open STM
module Stack = Saturn_lockfree.Bounded_stack

module Spec = struct
  type cmd =
    | Try_push of int
    | Push_exn of int
    | Try_push_all of int list
    (* | Push_all_exn of int list *)
    | Pop_opt
    | Pop_exn
    | Pop_all
    | Peek_opt
    | Peek_exn
    | Is_empty
    | Length

  let string_of_int_list l =
    "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

  let show_cmd c =
    match c with
    | Try_push i -> "Try_push " ^ string_of_int i
    | Push_exn i -> "Push_exn " ^ string_of_int i
    | Try_push_all l -> "Try_push_all " ^ string_of_int_list l
    (* | Push_all_exn l -> "Push_all_exn " ^ string_of_int_list l *)
    | Pop_opt -> "Pop_opt"
    | Pop_exn -> "Pop_exn"
    | Pop_all -> "Pop_all"
    | Peek_opt -> "Peek_opt"
    | Peek_exn -> "Peek_exn"
    | Is_empty -> "Is_empty"
    | Length -> "Length"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Try_push i) int_gen;
           Gen.map (fun i -> Push_exn i) int_gen;
           Gen.map (fun l -> Try_push_all l) (Gen.list int_gen);
           (* Gen.map (fun l -> Push_all_exn l) (Gen.list int_gen); *)
           Gen.return Pop_opt;
           Gen.return Pop_exn;
           Gen.return Pop_all;
           Gen.return Is_empty;
           Gen.return Length;
           Gen.return Peek_opt;
           Gen.return Peek_exn;
           Gen.return Is_empty;
         ])

  let init_state = []
  let capacity = 8
  let init_sut () = Stack.create ~capacity ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_push i -> if List.length s >= capacity then s else i :: s
    | Push_exn i -> if List.length s >= capacity then s else i :: s
    | Try_push_all l ->
        if List.length s + List.length l > capacity then s else List.rev l @ s
    (* | Push_all_exn l ->
        if List.length s + List.length l > capacity then s else List.rev l @ s *)
    | Pop_opt -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_exn -> ( match s with [] -> s | _ :: s' -> s')
    | Pop_all -> []
    | Peek_opt -> s
    | Peek_exn -> s
    | Is_empty -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_push i -> Res (bool, Stack.try_push d i)
    | Push_exn i ->
        Res (result unit exn, protect (fun d -> Stack.push_exn d i) d)
    | Try_push_all l -> Res (bool, Stack.try_push_all d l)
    (* | Push_all_exn l ->
        Res (result unit exn, protect (fun d -> Stack.push_all_exn d l) d) *)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Pop_exn -> Res (result int exn, protect Stack.pop_exn d)
    | Pop_all -> Res (list int, Stack.pop_all d)
    | Peek_opt -> Res (option int, Stack.peek_opt d)
    | Peek_exn -> Res (result int exn, protect Stack.peek_exn d)
    | Is_empty -> Res (bool, Stack.is_empty d)
    | Length -> Res (int, Stack.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_push _, Res ((Bool, _), res) -> List.length s < capacity = res
    | Push_exn _, Res ((Result (Unit, Exn), _), res) -> (
        match res with
        | Error Stack.Full -> List.length s = capacity
        | Ok () -> List.length s < capacity
        | _ -> false)
    | Try_push_all l, Res ((Bool, _), res) ->
        List.length s + List.length l <= capacity = res
    (* | Push_all_exn _, Res ((Result (Unit, Exn), _), res) -> (
        match res with
        | Error Stack.Full -> List.length s = capacity
        | Ok () -> List.length s < capacity
        | _ -> false) *)
    | (Pop_opt | Peek_opt), Res ((Option Int, _), res) -> (
        match s with [] -> res = None | j :: _ -> res = Some j)
    | (Pop_exn | Peek_exn), Res ((Result (Int, Exn), _), res) -> (
        match (res, s) with
        | Error Stack.Empty, [] -> true
        | __, [] -> false
        | Ok j, _ -> j = List.hd s
        | _ -> false)
    | Pop_all, Res ((List Int, _), res) -> res = s
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn_lockfree.Bounded_stack" (module Spec) |> exit
