type 'a t

val init : unit -> 'a t

val insert : 'a -> 'a t -> bool

val delete : 'a -> 'a t -> bool

val mem : 'a -> 'a t -> bool

val close : 'a t -> unit

val clean_local : 'a t -> unit

val of_list : 'a list -> 'a t
