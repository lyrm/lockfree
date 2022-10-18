module Llist : sig
  type 'a t

  val init : ?compare:('a -> 'a -> int) -> unit -> 'a t
  val insert : 'a -> 'a t -> bool
  val delete : 'a -> 'a t -> bool
  val mem : 'a -> 'a t -> bool
end

module Htbl : sig
  type 'a t

  val init : int -> 'a t
  val insert : int -> 'a -> 'a t -> bool
  val find : int -> 'a t -> 'a option
  val mem : int -> 'a t -> bool
  val delete : int -> 'a t -> bool
end
