type t

val create : ?max_height:int -> unit -> t
val find : t -> int -> bool
val add : t -> int -> bool
val remove : t -> int -> bool
