(* Copyright (c) 2024 Carine Morel

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
    REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
    AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
    INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
    LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE. *)

(* Based largely on the skiplist implementation written by Vesa Karvonen. *)

module Atomic = Multicore_magic.Transparent_atomic

type ('k, 'v, _) node =
  | Null : ('k, 'v, [> `Null ]) node
  | Node : {
      key : 'k;
      value : 'v;
      next : ('k, 'v) links;
      mutable incr : Size.once;
      mutable timestamp : int;
    }
      -> ('k, 'v, [> `Node ]) node
  | Mark : {
      node : ('k, 'v, [< `Null | `Node ]) node;
      decr : Size.once;
    }
      -> ('k, 'v, [> `Mark ]) node

and ('k, 'v) link =
  | Link : ('k, 'v, [< `Null | `Node | `Mark ]) node -> ('k, 'v) link
[@@unboxed]

and ('k, 'v) links = ('k, 'v) link Atomic.t array

type 'k compare = 'k -> 'k -> int
(* Encoding the [compare] function using an algebraic type would allow the
   overhead of calling a closure to be avoided for selected primitive types like
   [int]. *)

type ('k, 'v) t = {
  compare : 'k compare;
  root : ('k, 'v) links;
  size : Size.t;
  timestamp : int Atomic.t;
}

(* *)

(** [get_random_height max_height] gives a random value [n] in the range from
    [1] to [max_height] with the desired distribution such that [n] is twice as
    likely as [n + 1]. *)
let rec get_random_height max_height =
  let m = (1 lsl max_height) - 1 in
  let x = Random.bits () land m in
  if x = 1 then
    (* We reject [1] to get the desired distribution. *)
    get_random_height max_height
  else
    (* We do a binary search for the highest 1 bit.  Techniques in

         Using de Bruijn Sequences to Index a 1 in a Computer Word
         by Leiserson, Prokop, and Randall

       could perhaps speed this up a bit, but this is likely not performance
       critical. *)
    let n = 0 in
    let n, x = if 0xFFFF < x then (n + 0x10, x lsr 0x10) else (n, x) in
    let n, x = if 0x00FF < x then (n + 0x08, x lsr 0x08) else (n, x) in
    let n, x = if 0x000F < x then (n + 0x04, x lsr 0x04) else (n, x) in
    let n, x = if 0x0003 < x then (n + 0x02, x lsr 0x02) else (n, x) in
    let n, _ = if 0x0001 < x then (n + 0x01, x lsr 0x01) else (n, x) in
    max_height - n

(* *)

let[@inline] is_marked = function
  | Link (Mark _) -> true
  | Link (Null | Node _) -> false

(* *)

(** [find_path t key preds succs lowest] search fo the position after all the
    nodes with key [key], updating [preds] and [succs] and removing nodes with
    marked references along the way, and always descending down to [lowest]
    level. The boolean return value is only meaningful when [lowest] is given as
    [0]. *)
let rec find_path t key preds succs lowest =
  let prev = t.root in
  let level = Array.length prev - 1 in
  let prev_at_level = Array.unsafe_get prev level in
  find_path_rec t key prev prev_at_level preds succs level lowest
    (Atomic.get prev_at_level)

and find_path_rec t key prev prev_at_level preds succs level lowest = function
  | Link Null ->
      if level < Array.length preds then begin
        Array.unsafe_set preds level prev_at_level;
        Array.unsafe_set succs level Null
      end;
      if lowest < level then
        let level = level - 1 in
        let prev_at_level = Array.unsafe_get prev level in
        find_path_rec t key prev prev_at_level preds succs level lowest
          (Atomic.get prev_at_level)
  | Link (Node r as curr) -> begin
      let next_at_level = Array.unsafe_get r.next level in
      match Atomic.get next_at_level with
      | Link (Null | Node _) as next ->
          let c = t.compare key r.key in
          if 0 <= c then
            (* key >= r.key *)
            find_path_rec t key r.next next_at_level preds succs level lowest
              next
          else begin
            if level < Array.length preds then begin
              Array.unsafe_set preds level (Array.unsafe_get prev level);
              Array.unsafe_set succs level curr
            end;
            if lowest < level then
              let level = level - 1 in
              let prev_at_level = Array.unsafe_get prev level in
              find_path_rec t key prev prev_at_level preds succs level lowest
                (Atomic.get prev_at_level)
            else begin
              if level = 0 && r.incr != Size.used_once then begin
                Size.update_once t.size r.incr;
                r.incr <- Size.used_once
              end
            end
          end
      | Link (Mark r) ->
          (* The [curr_node] is being removed from the skiplist and we help with
             that. *)
          if level = 0 then Size.update_once t.size r.decr;
          find_path_rec t key prev prev_at_level preds succs level lowest
            (let after = Link r.node in
             if Atomic.compare_and_set prev_at_level (Link curr) after then
               after
             else Atomic.get prev_at_level)
    end
  | Link (Mark _) ->
      (* The node corresponding to [prev] is being removed from the skiplist.
         This means we might no longer have an up-to-date view of the skiplist
         and so we must restart the search. *)
      find_path t key preds succs lowest

(* *)

(** [find_node t key] tries to find the first node with the specified [key],
    removing nodes with marked references along the way, and stopping as soon as
    the node is found. *)
let rec find_node t ?timestamp key =
  let prev = t.root in
  let level = Array.length prev - 1 in
  let prev_at_level = Array.unsafe_get prev level in
  find_node_rec t ~timestamp key prev prev_at_level level
    (Atomic.get prev_at_level)

and find_node_rec t ~timestamp key prev prev_at_level level :
    _ -> (_, _, [< `Null | `Node ]) node = function
  | Link Null ->
      if 0 < level then
        let level = level - 1 in
        let prev_at_level = Array.unsafe_get prev level in
        find_node_rec t ~timestamp key prev prev_at_level level
          (Atomic.get prev_at_level)
      else Null
  | Link (Node r as curr) -> begin
      let next_at_level = Array.unsafe_get r.next level in
      match Atomic.get next_at_level with
      | Link (Null | Node _) as next ->
          let c = t.compare key r.key in
          let go_next_if =
            0 < c
            || c = 0
               && Option.fold ~none:false
                    ~some:(fun ts -> ts <= r.timestamp)
                    timestamp
          in

          if go_next_if then
            find_node_rec t ~timestamp key r.next next_at_level level next
          else if 0 < level then
            let level = level - 1 in
            let prev_at_level = Array.unsafe_get prev level in
            find_node_rec t ~timestamp key prev prev_at_level level
              (Atomic.get prev_at_level)
          else if c = 0 then curr
          else Null
      | Link (Mark r) ->
          if level = 0 then Size.update_once t.size r.decr;
          find_node_rec t ~timestamp key prev prev_at_level level
            (let after = Link r.node in
             if Atomic.compare_and_set prev_at_level (Link curr) after then
               after
             else Atomic.get prev_at_level)
    end
  | Link (Mark _) -> find_node t key

(* *)

let create ?(max_height = 10) ~compare () =
  (* The upper limit of [30] comes from [Random.bits ()] as well as from
     limitations with 32-bit implementations.  It should not be a problem in
     practice. *)
  if max_height < 1 || 30 < max_height then
    invalid_arg "Skiplist: max_height must be in the range [1, 30]";
  let root = Array.init max_height @@ fun _ -> Atomic.make (Link Null) in
  let size = Size.create () in
  { compare; root; size; timestamp = Atomic.make_contended 0 }

let max_height_of t = Array.length t.root

(* *)

let find_opt t key =
  match find_node t key with Null -> None | Node r -> Some r.value

(* *)

let mem t key = match find_node t key with Null -> false | Node _ -> true

(* *)

(* TODO using find_path to remove node is not ideal 

Why not using [find_node]: [add] adds after all identical keys whereas [find_node] 
is searching for the first so this is not possible to use the same function for both. 

*)
let rec add t key value preds succs =
  find_path t key preds succs 0;

  let (Node r as node : (_, _, [ `Node ]) node) =
    let next = Array.map (fun succ -> Atomic.make (Link succ)) succs in
    let incr = Size.new_once t.size Size.incr in
    Node { key; value; incr; next; timestamp = Int.max_int }
  in
  if
    let succ = Link (Array.unsafe_get succs 0) in
    Atomic.compare_and_set (Array.unsafe_get preds 0) succ (Link node)
  then begin
    if r.incr != Size.used_once then begin
      Size.update_once t.size r.incr;
      r.incr <- Size.used_once
    end;
    r.timestamp <- Atomic.fetch_and_add t.timestamp 1;
    (* The node is now considered as added to the skiplist. *)
    let rec update_levels level : unit =
      if Array.length r.next = level then begin
        if is_marked (Atomic.get (Array.unsafe_get r.next (level - 1))) then begin
          (* The node we finished adding has been removed concurrently.  To
             ensure that no references we added to the node remain, we call
             [find_node] which will remove nodes with marked references along
             the way. *)
          find_path t key preds succs level
        end
      end
      else if
        let succ = Link (Array.unsafe_get succs level) in
        Atomic.compare_and_set (Array.unsafe_get preds level) succ (Link node)
      then update_levels (level + 1)
      else
        let _found = find_path t key preds succs level in
        let rec update_nexts level' =
          if level' < level then update_levels level
          else
            let next = Array.unsafe_get r.next level' in
            match Atomic.get next with
            | Link (Null | Node _) as before ->
                let succ = Link (Array.unsafe_get succs level') in
                if before != succ then
                  (* It is possible for a concurrent remove operation to have
                     marked the link. *)
                  if Atomic.compare_and_set next before succ then
                    update_nexts (level' - 1)
                  else update_levels level
                else update_nexts (level' - 1)
            | Link (Mark _) ->
                (* The node we were trying to add has been removed concurrently.
                   To ensure that no references we added to the node remain, we
                   call [find_node] which will remove nodes with marked
                   references along the way. *)
                find_path t key preds succs level
        in
        update_nexts (Array.length r.next - 1)
    in
    update_levels 1
  end
  else add t key value preds succs

let add t key value : unit =
  let height = get_random_height (Array.length t.root) in
  let preds =
    (* Init with [Obj.magic ()] is safe as the array is fully overwritten by
       [find_path] called at the start of the recursive [try_add]. *)
    Array.make height (Obj.magic ())
  in
  let succs = Array.make height Null in
  add t key value preds succs

(* *)

let length t = Size.get t.size

(* *)

let rec find_min t timestamp : (_, _, [< `Node | `Null ]) node =
  let rec find_min_rec prev_at_level0 = function
    | Link (Mark _) -> find_min t timestamp
    | Link Null -> Null
    | Link (Node curr as curr_node) as curr_link -> begin
        let next_at_level_0 = Array.unsafe_get curr.next 0 in
        match Atomic.get next_at_level_0 with
        | Link (Null | Node _) ->
            if curr.timestamp > timestamp then
              (* node is too young *)
              let next_at_level_0 = Array.unsafe_get curr.next 0 in
              find_min_rec next_at_level_0 (Atomic.get next_at_level_0)
            else curr_node
        | Link (Mark next_marked) ->
            Size.update_once t.size next_marked.decr;
            let after = Link next_marked.node in
            if Atomic.compare_and_set prev_at_level0 curr_link after then
              find_min_rec prev_at_level0 after
            else find_min_rec prev_at_level0 (Atomic.get prev_at_level0)
      end
  in
  let prev = t.root in
  let prev_at_level0 = Array.unsafe_get prev 0 in
  find_min_rec prev_at_level0 (Atomic.get prev_at_level0)

let find_min_debug t =
  match find_min t (Atomic.get t.timestamp) with
  | Null -> None
  | Node r -> Some (r.key, r.value)

let rec try_remove t timestamp next level link = function
  | Link (Mark r) ->
      if level = 0 then begin
        Size.update_once t.size r.decr;
        false
      end
      else
        let level = level - 1 in
        let link = Array.unsafe_get next level in
        try_remove t timestamp next level link (Atomic.get link)
  | Link ((Null | Node _) as succ) ->
      let decr =
        if level = 0 then Size.new_once t.size Size.decr else Size.used_once
      in
      let marked_succ = Mark { node = succ; decr } in
      if Atomic.compare_and_set link (Link succ) (Link marked_succ) then
        if level = 0 then
          (* TODO : use find_node  *)
          let _node = find_node ~timestamp t in
          true
        else
          let level = level - 1 in
          let link = Array.unsafe_get next level in
          try_remove t timestamp next level link (Atomic.get link)
      else try_remove t timestamp next level link (Atomic.get link)

let rec remove_min_opt t =
  let curr_timestamp = Atomic.get t.timestamp in
  match find_min t curr_timestamp with
  | Null -> None
  | Node { next; key; value; _ } ->
      let level = Array.length next - 1 in
      let link = Array.unsafe_get next level in
      if try_remove t curr_timestamp next level link (Atomic.get link) then
        Some (key, value)
      else remove_min_opt t

(* match find_node t key with
  | Null -> false
  | Node { next; _ } ->
      let level = Array.length next - 1 in
      let link = Array.unsafe_get next level in
      try_remove t key next level link (Atomic.get link) *)

(* DEBUG *)

let print_debug sl =
  let level_max = Array.length sl.root in

  let rec to_string level acc = function
    | Link Null -> "Null" :: acc |> List.rev
    | Link (Node r) ->
        let str =
          "Node (" ^ Int.to_string r.key ^ ", " ^ Int.to_string r.value ^ ", l="
          ^ (Array.length r.next |> Int.to_string)
          ^ ")"
        in
        to_string level (str :: acc) (Atomic.get r.next.(level))
    | Link (Mark r) -> begin
        match r.node with
        | Null -> "Mark (Null)" :: acc |> List.rev
        | Node r ->
            let str =
              "Mark (" ^ "Node (" ^ Int.to_string r.key ^ ", "
              ^ Int.to_string r.value ^ ")" ^ ")"
            in
            to_string level (str :: acc) (Atomic.get r.next.(level))
      end
  in
  let str =
    List.init level_max (fun i ->
        "Root, "
        ^ (Array.unsafe_get sl.root i |> Atomic.get |> to_string i []
         |> String.concat ", "))
  in
  List.iteri
    (fun i s -> Format.printf "Level %d : %s@." (level_max - i - 1) s)
    (List.rev str)

(* let _test () =
  let pq = create ~max_height:4 ~compare:Int.compare () in
  add pq 1 1;
  add pq 1 2;
  add pq 1 3;
  print_debug pq;
  print_newline ();
  assert (Some (1, 1) = remove_min_opt pq);
  print_debug pq;
  print_newline ();
  assert (Some (1, 2) = remove_min_opt pq);
  print_debug pq;
  print_newline ();
  assert (Some (1, 3) = remove_min_opt pq);
  print_debug pq

let _test2 () =
  let barrier = Barrier.create 2 in
  let pq = create ~max_height:2 ~compare:Int.compare () in
  add pq 1 1;
  add pq 1 2;
  print_debug pq;
  print_newline ();
  let work () =
    Barrier.await barrier;
    remove_min_opt pq |> ignore; Domain.cpu_relax ();
    remove_min_opt pq |> ignore
  in
  let d1 = Domain.spawn work in
  let d2 = Domain.spawn work in
  Domain.join d1;
  Domain.join d2;
  print_debug pq;
  print_newline ()

let _test2 () =
  let barrier = Barrier.create 2 in
  let pq = create ~max_height:2 ~compare:Int.compare () in
  add pq 1 1;
  add pq 1 2;
  let work () =
    Barrier.await barrier;
    remove_min_opt pq |> ignore; Domain.cpu_relax ();
    remove_min_opt pq |> ignore
  in
  let d1 = Domain.spawn work in
  let d2 = Domain.spawn work in
  Domain.join d1;
  Domain.join d2

*)
(* let _test3 () =
  let barrier = Barrier.create 2 in
  let pq = create ~max_height:2 ~compare:Int.compare () in

  add pq 1 1;
  add pq 1 2;

  let work1 () =
    Barrier.await barrier;
    remove_min_opt pq |> ignore;
    Domain.cpu_relax ();
    remove_min_opt pq |> ignore
  in

  let work2 () =
    Barrier.await barrier;
    add pq 1 3;
    Domain.cpu_relax ();
    add pq 1 4
  in

  let d1 = Domain.spawn work1 in
  let d2 = Domain.spawn work2 in
  Domain.join d1;
  Domain.join d2 *)
