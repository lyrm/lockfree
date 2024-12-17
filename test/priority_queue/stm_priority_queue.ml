open QCheck
open STM
module Priority_queue = Saturn.Priority_queue

module Spec = struct
  type cmd =
    (* Mem of int  *)
    | Add of int * int
    | Remove_min
  (* | Length *)

  let show_cmd c =
    match c with
    (* | Mem i -> "Mem " ^ string_of_int i *)
    | Add (i, k) -> "Add (" ^ string_of_int i ^ ", " ^ string_of_int k ^ ")"
    | Remove_min -> "Remove_min"
  (* | Length -> "Length" *)

  type state = { min : int; queue : (int * int) list }
  type sut = (int, int) Priority_queue.t

  let arb_cmd _s =
    let priority_gen = Gen.int_bound 10 in
    let val_gen = Gen.int_bound 100 in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun p i -> Add (p, i)) priority_gen val_gen;
           (* Gen.map (fun p -> Mem p) priority_gen; *)
           Gen.return Remove_min;
           (* Gen.return Length; *)
         ])

  let init_state = { min = Int.max_int; queue = [] }

  (* { min = 1; queue = List.init 5 (fun i -> (i + 1, 1)) } *)

  let init_sut () = Priority_queue.create ~compare:Int.compare ()
  (* let sl = Priority_queue.create ~compare:Int.compare () in
    List.iter (fun i -> Priority_queue.add sl i 1)
    @@ List.init 5 (fun i -> i + 1);
    sl *)

  let cleanup _ = ()

  let new_min queue =
    let rec aux curr_min queue =
      match queue with
      | [] -> curr_min
      | (p, _) :: xs -> if p < curr_min then aux p xs else aux curr_min xs
    in
    { min = aux Int.max_int queue; queue }

  let next_state c (s : state) =
    match c with
    | Add (p, i) -> begin
        let min = Int.min s.min p in
        { min; queue = List.rev s.queue |> List.cons (p, i) |> List.rev }
      end
    | Remove_min ->
        let queue = List.remove_assoc s.min s.queue in
        new_min queue
  (* | Mem _ -> s
     | Length -> s *)

  let precond _ _ = true

  let run c d =
    match c with
    | Add (p, i) -> Res (unit, Priority_queue.add d p i)
    | Remove_min ->
        Res
          ( list int,
            match Priority_queue.remove_min_opt d with
            | None -> []
            | Some (p, v) -> [ p; v ] )
  (* | Mem i -> Res (bool, Priority_queue.mem d i)
     | Length -> Res (int, Priority_queue.length d) *)

  let postcond c (s : state) res =
    match (c, res) with
    | Add (_, _), Res ((Unit, _), ()) -> true
    | Remove_min, Res ((List Int, _), res) -> begin
        match List.sort (fun (p1, _) (p2, _) -> Int.compare p1 p2) s.queue with
        | [] -> res = []
        | (p, _) :: _ -> [ p; List.assoc p s.queue ] = res
      end
    (* | Mem i, Res ((Bool, _), res) -> List.mem_assoc i s.queue = res
       | Length, Res ((Int, _), res) -> List.length s.queue = res *)
    | _, _ -> false
end

let () = Stm_run.run ~name:"Saturn.Priority_queue" (module Spec) |> exit
