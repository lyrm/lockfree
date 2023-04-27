(** Lockfree skiplist from :
     A lock-free concurrent skiplist with wait-free search,
     M.Herlihy, Y. Lev, N. Shavit, 2007.

    This skiplist is a lockfree implementation of a set with logarithm
    time search. It is balanced without requiring any actual
    re-balancing. [mem] is the function for which this data structure
    is optimized.
 *)

type t
(** The type of the skip list *)

val create : ?max_height:int -> unit -> t
(** [create ()] creates a new skip list. The optional parameter
    [max_height] (10 by default) can be used to trick a bit
    performances in case the number of elements in it is well
    contained.  *)

val mem : t -> int -> bool
(** [mem s v] returns [true] is [v] is in [s]. [mem] is logarithmic in
    time and wait free. *)

val add : t -> int -> bool
(** [add s v] returns [true] and adds [v] to [s] if [v] is not already
   in [s]. It returns [false] otherwise. *)

val remove : t -> int -> bool
(** [remove s v] returns [true] is [v] is in [s] and removed it. It returns [false] otherwise. *)
