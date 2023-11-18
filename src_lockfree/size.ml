(* Copyright (c) 2023 Vesa Karvonen

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

(** ⚠️ Beware that this implementation uses a bunch of low level data
    representation tricks to minimize overheads. *)

module Atomic = Transparent_atomic

let max_value = Int.max_int

module Snapshot = struct
  type t = int Atomic.t array
  (** We use an optimized flat representation where the first element of the
      array holds the status of the snapshot.

        +--------+---------+---------+---------+- - -
        | status | counter | counter | counter | ...
        +--------+---------+---------+---------+- - -

      The status is either {!collecting}, {!computing}, or a non-negative value.

      The counter snapshot values are initialized to a negative value and after
      collecting they will all be non-negative. *)

  let zero = [| Atomic.make 0 |]
  let collecting = -1
  let computing = -2

  let[@inline] is_collecting (s : t) =
    Atomic.get (Array.unsafe_get s 0) = collecting

  let create n = Array.init n @@ fun _ -> Atomic.make collecting

  let[@inline] set s i after =
    let snap = Array.unsafe_get s i in
    let before = Atomic.get snap in
    if
      before = collecting
      || (* NOTE: The condition below accounts for overflow. *)
      (after - before - 1) land max_value < max_value / 2
    then Atomic.compare_and_set snap before after |> ignore

  let[@inline] forward s i after =
    let snap = Array.unsafe_get s i in
    while
      let before = Atomic.get snap in
      (before = collecting
      || (* NOTE: The condition below accounts for overflow. *)
      (after - before - 1) land max_value < max_value / 2)
      && not (Atomic.compare_and_set snap before after)
    do
      ()
    done

  let rec compute s sum i =
    if 0 < i then
      (* NOTE: Operations below are in specific order for performance. *)
      let decr = Array.unsafe_get s i in
      let incr = Array.unsafe_get s (i + 1) in
      let decr = Atomic.get decr in
      let incr = Atomic.get incr in
      compute s (sum - decr + incr) (i - 2)
    else sum land max_value

  let compute s = compute s 0 (Array.length s - 2)

  let compute s =
    let status = Array.unsafe_get s 0 in
    if Atomic.get status = collecting then
      Atomic.compare_and_set status collecting computing |> ignore;
    if Atomic.get status = computing then begin
      let computed = compute s in
      if Atomic.get status = computing then
        Atomic.compare_and_set status computing computed |> ignore
    end;
    Atomic.get status
end

type _ state =
  | Open : { mutable index : int } -> [ `Open ] state
  | Used : [ `Used ] state

let used_index = 0

type tx = { value : int; once : [ `Open ] state }

type t = tx Atomic.t array
(** We use an optimized flat representation where the first element of the array
    holds a reference to the snapshot and the other elements are the counters.

      +----------+------+------+------+------+- - -
      | snapshot | decr | incr | decr | incr | ...
      +----------+------+------+------+------+- - -

    Counters at odd numbered indices are for [decr]ements and the counters at
    even numbered indices are for [incr]ements.

    A counter refers to a unique [tx] record. *)

let[@inline] snapshot_of (t : t) : Snapshot.t Atomic.t =
  Obj.magic (Array.unsafe_get t 0)

(* *)

let zero = { value = 0; once = Open { index = used_index } }

let n_way_max =
  Int.min
    (Domain.recommended_domain_count () * 2)
    (Bits.floor_pow_2 ((Sys.max_array_length - 1) lsr 1))
  |> Bits.ceil_pow_2

let n_way_default = n_way_max |> Int.min 8

let create ?n_way () =
  let n_way =
    match n_way with
    | None -> n_way_default
    | Some n_way -> n_way |> Int.max 1 |> Int.min n_way_max |> Bits.ceil_pow_2
  in
  Array.init ((n_way * 2) + 1) @@ fun i ->
  Atomic.make (if i = 0 then Obj.magic Snapshot.zero else zero)
  |> Multicore_magic.copy_as_padded

let n_way_of t = (Array.length t - 1) lsr 1

(* *)

type once = Once : _ state -> once [@@unboxed]

let get_index (Open r) = r.index
let use_index (Open r) = r.index <- used_index

(* *)

let used_once = Once Used

(* *)

type update = int

let decr = 1
let incr = 2

let new_once t update =
  let mask = Array.length t - 3 in
  let index =
    (Multicore_magic.instantaneous_domain_index () * 2 land mask) + update
  in
  Once (Open { index })

(* *)

let rec update_once t once counter =
  let before = Atomic.get counter in
  let index = get_index once in
  let before_once = before.once in
  if index != used_index && before_once != once then begin
    use_index before_once;
    let value = (before.value + 1) land max_value in
    let after = { value; once } in
    if Atomic.compare_and_set counter before after then begin
      let snapshot = Atomic.get (snapshot_of t) in
      if Snapshot.is_collecting snapshot then
        Snapshot.forward snapshot index value
    end
    else update_once t once (Array.unsafe_get t index)
  end

let update_once t once =
  match once with
  | Once Used -> ()
  | Once (Open _ as once) ->
      let index = get_index once in
      if index != used_index then update_once t once (Array.unsafe_get t index)

(* *)

let get_collecting_snapshot t =
  let snapshot = snapshot_of t in
  let before = Atomic.get snapshot in
  if Snapshot.is_collecting before then before
  else
    let after = Snapshot.create (Array.length t) in
    if Atomic.compare_and_set snapshot before after then after
    else Atomic.get snapshot

let rec collect t snapshot i =
  if 0 < i then begin
    let after = Atomic.get (Array.unsafe_get t i) in
    Snapshot.set snapshot i after.value;
    collect t snapshot (i - 1)
  end

let get t =
  let snapshot = get_collecting_snapshot t in
  collect t snapshot (Array.length t - 1);
  Snapshot.compute snapshot
