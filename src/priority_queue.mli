(** A lock-free skiplist. *)

type (!'k, !'v) t
(** The type of a lock-free skiplist containing bindings of keys of type ['k] to
    values of type ['v]. *)

val create : ?max_height:int -> compare:('k -> 'k -> int) -> unit -> ('k, 'v) t
(** [create ~compare ()] creates a new empty skiplist where keys are ordered
    based on the given [compare] function.

    Note that the polymorphic [Stdlib.compare] function has relatively high
    overhead and it is usually better to use a type specific [compare] function
    such as [Int.compare] or [String.compare].

    The optional [max_height] argument determines the maximum height of nodes in
    the skiplist and directly affects the performance of the skiplist.  The
    current implementation does not adjust height automatically. *)

val max_height_of : ('k, 'v) t -> int
(** [max_height_of s] returns the maximum height of nodes of the skiplist [s] as
    specified to {!create}. *)

val find_opt : ('k, 'v) t -> 'k -> 'v option
(** [find_opt s k] tries to find a binding of [k] to [v] from the skiplist [s]
    and returns [Some v] in case such a binding was found or return [None] in
    case no such binding was found. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem s k] determines whether the skiplist [s] contained a binding of [k]. *)

val add : ('k, 'v) t -> 'k -> 'v -> unit
(** [add s k v] tries to add a new binding of [k] to [v] into the skiplist
    [s] and returns [true] on success.  Otherwise the skiplist already contained
    a binding of [k] and [false] is returned. *)

val remove_min_opt : ('k, 'v) t -> ('k * 'v) option
(** [remove_min_opt s] removes the binding with the smallest key from the skiplist
    [s] and returns the key-value pair.  If the skiplist was empty [None] is
    returned. *)

val length : ('k, 'v) t -> int
(** [length s] computes the number of bindings in the skiplist [s]. *)
