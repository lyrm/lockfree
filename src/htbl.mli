module Llist : sig
  type 'a t

  val init : ?compare:('a -> 'a -> int) -> unit -> 'a t
  val add : 'a -> 'a t -> bool
  val remove : 'a -> 'a t -> bool
  val mem : 'a -> 'a t -> bool
end

module Htbl : sig
  type 'a t

  val init : size_exponent:int -> 'a t
  val add : int -> 'a -> 'a t -> bool
  val find : int -> 'a t -> 'a option
  val mem : int -> 'a t -> bool
  val remove : int -> 'a t -> bool

end

module Htbl_resizable : sig
  type 'a t

  val init : size_exponent:int -> 'a t
  val add : int -> 'a -> 'a t -> bool
  val add_no_resize : int -> 'a -> 'a t -> bool
  val find : int -> 'a t -> 'a option
  val mem : int -> 'a t -> bool
  val remove : int -> 'a t -> bool
  val is_empty : 'a t -> bool

end
