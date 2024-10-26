type 'a node = Nil | Cons of { value : 'a; tail : 'a node; capacity : int }
type 'a t = { head : 'a node Atomic.t; capacity : int }

(* *)
let create ?(capacity = Int.max_int) () =
  let head = Atomic.make_contended Nil in
  { head; capacity = max capacity 1 } |> Multicore_magic.copy_as_padded

let length t =
  match Atomic.get t.head with Nil -> 0 | Cons cons -> cons.capacity

let is_empty t = Atomic.get t.head = Nil

exception Empty
exception Full

let of_seq ?(capacity = Int.max_int) seq =
  if capacity < Seq.length seq then raise Full
  else
    let head =
      Seq.fold_left
        (fun (len, acc) elt ->
          (len + 1, Cons { value = elt; tail = acc; capacity = len }))
        (1, Nil) seq
      |> snd |> Atomic.make_contended
    in
    { head; capacity } |> Multicore_magic.copy_as_padded

(* *)

type ('a, _) poly_peek =
  | Option : ('a, 'a option) poly_peek
  | Value : ('a, 'a) poly_peek

let peek_as : type a r. a t -> (a, r) poly_peek -> r =
 fun t poly ->
  match Atomic.get t.head with
  | Nil -> begin
      match poly with Option -> None | Value -> raise_notrace Empty
    end
  | Cons cons_r -> (
      match poly with Option -> Some cons_r.value | Value -> cons_r.value)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

type ('a, _) poly =
  | Option : ('a, 'a option) poly
  | Value : ('a, 'a) poly
  | Unit : ('a, unit) poly

(* *)
let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly -> r =
 fun t backoff poly ->
  match Atomic.get t.head with
  | Nil -> begin
      match poly with
      | Option -> None
      | Value -> raise_notrace Empty
      | Unit -> raise_notrace Empty
    end
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head cons_r.tail then
        match poly with
        | Option -> Some cons_r.value
        | Value -> cons_r.value
        | Unit -> ()
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option
let drop_exn t = pop_as t Backoff.default Unit

let rec pop_all t backoff =
  match Atomic.get t.head with
  | Nil -> []
  | old_head ->
      if Atomic.compare_and_set t.head old_head Nil then
        let[@tail_mod_cons] rec aux = function
          | Nil -> []
          | Cons cons -> cons.value :: aux cons.tail
        in
        aux old_head
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default

let to_seq t =
  match Atomic.get t.head with
  | Nil -> Seq.empty
  | old_head ->
      let rec aux s () =
        match s with
        | Nil -> Seq.Nil
        | Cons cons -> Seq.Cons (cons.value, aux cons.tail)
      in
      aux old_head

(* *)

type _ mono = Unit : unit mono | Bool : bool mono

let rec push_as : type r. 'a t -> Backoff.t -> 'a -> r mono -> r =
 fun t backoff value mono ->
  match Atomic.get t.head with
  | Nil ->
      if
        Atomic.compare_and_set t.head Nil
        @@ Cons { value; tail = Nil; capacity = 1 }
      then match mono with Bool -> true | Unit -> ()
      else push_as t (Backoff.once backoff) value mono
  | Cons cons_r as old_head ->
      if cons_r.capacity >= t.capacity then
        match mono with Bool -> false | Unit -> raise Full
      else
        let new_head =
          Cons { value; tail = old_head; capacity = cons_r.capacity + 1 }
        in
        if Atomic.compare_and_set t.head old_head new_head then
          match mono with Bool -> true | Unit -> ()
        else push_as t (Backoff.once backoff) value mono

let push_exn t value = push_as t Backoff.default value Unit
let try_push t value = push_as t Backoff.default value Bool

let rec push_all_as : type r. 'a t -> Backoff.t -> 'a list -> r mono -> r =
 fun t backoff values mono ->
  let len = List.length values in
  if len = 0 then match mono with Unit -> () | Bool -> true
  else if len > t.capacity then
    match mono with Unit -> raise Full | Bool -> false
  else
    let rec build_node len acc = function
      | [] -> acc
      | x :: xs ->
          build_node (len + 1)
            (Cons { capacity = len + 1; tail = acc; value = x })
            xs
    in
    match Atomic.get t.head with
    | Nil ->
        if Atomic.compare_and_set t.head Nil (build_node 0 Nil values) then
          match mono with Bool -> true | Unit -> ()
        else push_all_as t (Backoff.once backoff) values mono
    | Cons cons_r as old_head ->
        if cons_r.capacity + len > t.capacity then
          match mono with Bool -> false | Unit -> raise Full
        else if
          Atomic.compare_and_set t.head old_head
          @@ build_node cons_r.capacity old_head values
        then match mono with Bool -> true | Unit -> ()
        else push_all_as t (Backoff.once backoff) values mono

let try_push_all t values = push_all_as t Backoff.default values Bool
let push_all_exn t values = push_all_as t Backoff.default values Unit
let add_seq_exn t seq = push_all_as t Backoff.default (List.of_seq seq) Unit
let try_add_seq t seq = push_all_as t Backoff.default (List.of_seq seq) Bool

(* *)
let try_compare_and_pop t value =
  let rec aux backoff =
    match Atomic.get t.head with
    | Nil -> false
    | Cons cons_r as old_head ->
        if cons_r.value == value then
          if Atomic.compare_and_set t.head old_head cons_r.tail then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let try_compare_and_set t old_value new_value =
  let rec aux backoff =
    match Atomic.get t.head with
    | Nil -> false
    | Cons cons_r as old_head ->
        if cons_r.value == old_value then
          if
            Atomic.compare_and_set t.head old_head
            @@ Cons { cons_r with value = new_value }
          then true
          else aux (Backoff.once backoff)
        else false
  in
  aux Backoff.default

let rec set_exn t value backoff =
  match Atomic.get t.head with
  | Nil -> raise_notrace Empty
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head @@ Cons { cons_r with value }
      then value
      else set_exn t value (Backoff.once backoff)

let set_exn t value = set_exn t value Backoff.default

let rec try_set t value backoff =
  match Atomic.get t.head with
  | Nil -> false
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head @@ Cons { cons_r with value }
      then true
      else try_set t value (Backoff.once backoff)

let try_set t value = try_set t value Backoff.default
