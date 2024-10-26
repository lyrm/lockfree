type 'a node = Nil | Cons of { value : 'a; tail : 'a node; capacity : int }
type 'a t = { head : 'a node Atomic.t; capacity : int }

let create ?(capacity = Int.max_int) () =
  let head = Atomic.make_contended Nil in
  { head; capacity = max capacity 1 } |> Multicore_magic.copy_as_padded

exception Empty
exception Full

let is_empty t = Atomic.get t.head = Nil

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let peek_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  match Atomic.get t.head with
  | Nil -> begin
      match poly with Option -> None | Value -> raise_notrace Empty
    end
  | Cons cons_r -> (
      match poly with Option -> Some cons_r.value | Value -> cons_r.value)

let peek_exn t = peek_as t Value
let peek_opt t = peek_as t Option

let rec pop_as : type a r. a t -> Backoff.t -> (a, r) poly -> r =
 fun t backoff poly ->
  match Atomic.get t.head with
  | Nil -> begin
      match poly with Option -> None | Value -> raise_notrace Empty
    end
  | Cons cons_r as old_head ->
      if Atomic.compare_and_set t.head old_head cons_r.tail then
        match poly with Option -> Some cons_r.value | Value -> cons_r.value
      else pop_as t (Backoff.once backoff) poly

let pop_exn t = pop_as t Backoff.default Value
let pop_opt t = pop_as t Backoff.default Option

let rec pop_all t backoff =
  match Atomic.get t.head with
  | Nil -> []
  | old_head ->
      if Atomic.compare_and_set t.head old_head Nil then
        let rec aux acc = function
          | Nil -> List.rev acc
          | Cons cons -> aux (cons.value :: acc) cons.tail
        in
        aux [] old_head
      else pop_all t (Backoff.once backoff)

let pop_all t = pop_all t Backoff.default
let to_seq t = pop_all t |> List.to_seq

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

let length t =
  match Atomic.get t.head with Nil -> 0 | Cons cons -> cons.capacity

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
