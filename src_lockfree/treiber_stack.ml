(** Treiber's Lock Free stack *)

type 'a node = Nil | Cons of { value : 'a; tail : 'a node }
type 'a t = 'a node Atomic.t

let create () = Atomic.make_contended Nil
let is_empty t = Atomic.get t == Nil

let of_seq seq =
  Seq.fold_left
    (fun (len, acc) elt -> (len + 1, Cons { value = elt; tail = acc }))
    (1, Nil) seq
  |> snd |> Atomic.make_contended

(* *)

exception Empty

type ('a, _) poly =
  | Option : ('a, 'a option) poly
  | Value : ('a, 'a) poly
  | Unit : ('a, unit) poly

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly -> r =
 fun t backoff poly ->
  match Atomic.get t with
  | Nil -> begin
      match poly with Option -> None | Value | Unit -> raise_notrace Empty
    end
  | Cons cons_r as cons ->
      if Atomic.compare_and_set t cons cons_r.tail then
        match poly with
        | Option -> Some cons_r.value
        | Value -> cons_r.value
        | Unit -> ()
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
let drop_exn t = pop_as t Backoff.default Unit

type ('a, _) poly_peek =
  | Option : ('a, 'a option) poly_peek
  | Value : ('a, 'a) poly_peek

let peek_as : type a r. a t -> (a, r) poly_peek -> r =
 fun t poly ->
  match Atomic.get t with
  | Nil -> begin
      match poly with Option -> None | Value -> raise_notrace Empty
    end
  | Cons cons -> (
      match poly with Option -> Some cons.value | Value -> cons.value)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

let rec pop_all t backoff =
  match Atomic.get t with
  | Nil -> []
  | old_head ->
      if Atomic.compare_and_set t old_head Nil then
        let[@tail_mod_cons] rec aux = function
          | Nil -> []
          | Cons cons -> cons.value :: aux cons.tail
        in
        aux old_head
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default

let to_seq t =
  match Atomic.get t with
  | Nil -> Seq.empty
  | old_head ->
      let rec aux s () =
        match s with
        | Nil -> Seq.Nil
        | Cons cons -> Seq.Cons (cons.value, aux cons.tail)
      in
      aux old_head
(* *)

let rec push t value backoff =
  let tail = Atomic.get t in
  let cons = Cons { value; tail } in
  if not (Atomic.compare_and_set t tail cons) then
    push t value (Backoff.once backoff)

let push t value = push t value Backoff.default

let rec push_all_ t backoff values =
  let rec build_node acc = function
    | [] -> acc
    | x :: xs -> build_node (Cons { tail = acc; value = x }) xs
  in
  match Atomic.get t with
  | Nil ->
      if Atomic.compare_and_set t Nil (build_node Nil values) then ()
      else push_all_ t (Backoff.once backoff) values
  | Cons _ as old_head ->
      if Atomic.compare_and_set t old_head @@ build_node old_head values then ()
      else push_all_ t (Backoff.once backoff) values

let push_all t values = push_all_ t Backoff.default values
let add_seq t seq = push_all_ t Backoff.default (List.of_seq seq)

(* *)
let try_compare_and_pop t value =
  let rec aux backoff =
    match Atomic.get t with
    | Nil -> false
    | Cons cons_r as old_head ->
        if cons_r.value == value then
          if Atomic.compare_and_set t old_head cons_r.tail then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let try_compare_and_set t old_value new_value =
  let rec aux backoff =
    match Atomic.get t with
    | Nil -> false
    | Cons cons_r as old_head ->
        if cons_r.value == old_value then
          if
            Atomic.compare_and_set t old_head
            @@ Cons { cons_r with value = new_value }
          then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let rec set_exn t value backoff =
  match Atomic.get t with
  | Nil -> raise_notrace Empty
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t old_head @@ Cons { cons_r with value } then
        value
      else set_exn t value (Backoff.once backoff)

let set_exn t value = set_exn t value Backoff.default

let rec try_set t value backoff =
  match Atomic.get t with
  | Nil -> false
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t old_head @@ Cons { cons_r with value } then
        true
      else try_set t value (Backoff.once backoff)

let try_set t value = try_set t value Backoff.default
