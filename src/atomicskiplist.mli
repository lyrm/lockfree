type 'a t
val create : unit -> 'a t
val find: 'a t -> int ->bool
val add: 'a t -> int -> bool
val remove: 'a t -> int -> bool
