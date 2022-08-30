type 'a t

val init : unit ->  'a t

val insert : int -> 'a ->  'a t -> unit

val mem : int -> 'a t -> bool
