type 'a t

val init : unit -> 'a t

val insert : 'a -> 'a t -> bool

val delete : 'a -> 'a t -> bool

val mem : 'a -> 'a t -> bool
