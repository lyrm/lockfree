(** Lock-free single-producer, multi-consumer dynamic-size double-ended queue (deque).

      The main strength of a deque in a typical work-stealing setup with a 
      per-core structure, is efficient work distribution. The owner uses [push] 
      and [pop] methods to operate at one end of the deque, while other (free) 
      cores can efficiently steal work from the other side.

      This approach is great for throughput. Stealers and the owner working on
      different sides, reduce contention in work distribution. Further, the 
      local LIFO order, running related tasks one after another, improves locality.

      On the other hand, the local LIFO order does not offer any fairness 
      guarantees. Thus, it is not the best choice when tail latency matters.
*)

(** {1 API} *)

type 'a t
(** Type of work-stealing queue *)

val create : unit -> 'a t
(** [create ()] returns a new empty work-stealing queue. *)

val of_list : 'a list -> 'a t
(** [of_list list] creates a new work-stealing queue from [list].

 {[
        # open Saturn.Work_stealing_deque
        # let t : int t = of_list [1;2;3;4]
        val t : int t = <abstr>
        # pop_opt t
        - : int option = Some 4
        # pop_exn t 
        - : int = 3
        # steal_opt t 
        - : int option = Some 1
 ]}
*)

exception Empty

(** {2 Queue owner functions} *)

val push : 'a t -> 'a -> unit
(** [push queue element] adds [element] at the end of the [queue].
      It should only be invoked by the domain that owns the [queue]. *)

val peek_exn : 'a t -> 'a
(** [peek_exn queue] returns the last element of the [queue] without removing it.
      
    @raises Empty if the [queue] is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt queue] returns [Some] of the last element of the [queue] without
      removing it, or [None] if the [queue] is empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn queue] removes and returns the last element of the [queue]. It 
      should only be invoked by the domain that owns the [queue].

       @raises Empty if the [queue] is empty.
*)

val pop_opt : 'a t -> 'a option
(** [pop_opt queue] removes and returns [Some] of the last element of the 
      [queue], or returns [None] if the [queue] is empty.  *)

val drop_exn : 'a t -> unit
(** [drop_exn queue] removes the last element of the [queue]. 
            
    @raises Empty if the [queue] is empty. *)

(** {2 Stealer functions} *)

val steal_peek_exn : 'a t -> 'a
(** [steal_peek_exn queue] returns the top element of the [queue] without 
      removing it. It should only be invoked by a domain that doesn't own the 
      [queue].
    
  @raises Empty if the [queue] is empty. *)

val steal_peek_opt : 'a t -> 'a option
(** [steal_peek_opt queue] returns [Some] of the top element of the [queue] 
      without removing it, or [None] if the [queue] is empty. It should only be 
      invoked by a domain that doesn't own the [queue]. *)

val steal_exn : 'a t -> 'a
(** [steal_exn queue] removes and returns the top element of the [queue]. 
      It should only be invoked by a domain that doesn't own the [queue].

        @raises Empty if the [queue] is empty.
  *)

val steal_opt : 'a t -> 'a option
(** [steal_opt queue] removes and returns [Some] of the top element of the 
      [queue], or returns [None] if the [queue] is empty. It should only be
      invoked by a domain that doesn't own the [queue]. *)

val steal_drop_exn : 'a t -> unit
(** [steal_drop_exn queue] removes the top element of the [queue]. 
      
    @raises Empty if the [queue] is empty. *)

(** {1 Examples} *)

(** {2 Sequential example} *)

(** {2 Parallel example} *)
