(** Classic multi-producer multi-consumer Treiber stack.

    All function are lockfree. It is the recommended starting point
    when needing LIFO structure. *)

(** {1 API} *)

type 'a t
(** Represents a lock-free Treiber stack holding elements of type ['a]. *)

val create : unit -> 'a t
(** [create ()] creates a new empty Treiber stack. *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false]. *)

(** {2 Consumer functions} *)

exception Empty
(** Raised when {!pop_exn} or {!peek_exn} or {!drop_exn} is applied to an empty
 stack.

  This exception is meant to avoid allocations required by an option type.
  As such, it does not register backtrace information and it is recommended to 
  use the following pattern to catch it.

  {@ocaml skip[
    match pop_exn s with
      | value -> () (* ... *) 
      | exception Empty -> () (* ... *)
  ]} *)

val peek_exn : 'a t -> 'a
(** [peek_exn stack] returns the top element of the [stack] without removing it.
    
  @raises Empty if the [stack] is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the [stack] is empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn stack] removes and returns the top element of the [stack].
 
  @raises Empty if the [stack] is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the [stack],
    or [None] if the [stack] is empty. *)

val drop_exn : 'a t -> unit
(** [drop_exn stack] removes the top element of the [stack]. 

  @raises Empty if the [stack] if empty. *)

val try_compare_and_pop : 'a t -> 'a -> bool
(** [try_compare_and_pop stack before] tries to remove the top element of the 
  [stack] if it is equal to [before]. Returns [true] on success and [false] in 
  case the hash table is empty or if the top element is not equal to [before].

    ℹ️ The values are compared using physical equality, i.e. the [==] operator. *)

val pop_all : 'a t -> 'a list
(** [pop_all stack] removes and returns all elements of the [stack] in the LIFO 
order. 

  {[
    # open Saturn_lockfree.Stack
    # let t : int t = create ()
    val t : int t = <abstr>
    # push t 1
    - : unit = ()
    # push t 2
    - : unit = ()
    # push t 3
    - : unit = ()
    # pop_all t
    - : int list = [3; 2; 1]
  ]}
*)

(** {2 Producer functions} *)

val push : 'a t -> 'a -> unit
(** [push stack element] adds [element] to the top of the [stack]. *)

val push_all : 'a t -> 'a list -> unit
(** [push_all stack elements] adds all [elements] to the top of the [stack]. 
    
  {[
    # let t : int t = create ()
    val t : int t = <abstr>
    # push_all t [1; 2; 3; 4]
    - : unit = ()
    # pop_opt t
    - : int option = Some 4
    # pop_opt t 
    - : int option = Some 3
    # pop_all t
    - : int list = [2; 1]
  ]}
    *)

(** {3 Updating bindings} *)

val try_set : 'a t -> 'a -> bool
(** [try_set stack value] tries to update the top element of the [stack] to
    [value]. Returns [true] on success and [false] in case the [stack] is Empty.
    *)

val try_compare_and_set : 'a t -> 'a -> 'a -> bool
(** [try_compare_and_set stack before after] tries to update the top element of 
the [stack] from the [before] value to the [after] value. Returns [true] on 
success and [false] in case the [stack] is empty or the top element is not equal 
to [before].

    ℹ️ The values are compared using physical equality, i.e. the [==]
    operator. *)

val set_exn : 'a t -> 'a -> 'a
(** [set_exn stack after] tries to update the top element of [stack] from some 
[before] value to the [after] value. Returns the [before] value on success.

    @raise Empty if the [stack] is empty. *)

(** {2 With Sequences }*)
val to_seq : 'a t -> 'a Seq.t
(** [to_seq stack] takes a snapshot of [stack] and returns its value top to 
bottom.

  🐌 This is a linear time operation. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq seq] create a stack from a [seq]. It must be finite *)

val add_seq : 'a t -> 'a Seq.t -> unit
(** [add_seq stack stack seq] adds all elements of [seq] to the top of the 
[stack]. [seq] must be finite. *)

(** {1 Examples}
    An example top-level session:
    {[
      # open Saturn_lockfree.Stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # push t 42
      - : unit = ()
      # push_all t [1; 2; 3]
      - : unit = ()
      # pop_exn t
      - : int = 3
      # peek_opt t
      - : int option = Some 2
      # pop_all t
      - : int list = [2; 1; 42]
      # pop_exn t
      Exception: Saturn_lockfree__Treiber_stack.Empty.]}

    A multicore example: 
    {@ocaml non-deterministic[
      # open Saturn_lockfree.Stack
      # let t :int t = create ()
      val t : int t = <abstr>
      # let barrier =  Atomic.make 2
      val barrier : int Atomic.t = <abstr>
      # let pusher () = 
        Atomic.decr barrier;
        while Atomic.get barrier != 0 do Domain.cpu_relax () done;

        push_all t [1;2;3] |> ignore;
        push t 42;
        push t 12
      val pusher : unit -> unit = <fun>
      # let popper () = 
        Atomic.decr barrier;
        while Atomic.get barrier != 0 do Domain.cpu_relax () done;
        List.init 6 (fun i -> Domain.cpu_relax (); pop_opt t)
      val popper : unit -> int option list = <fun>
      # let domain_pusher = Domain.spawn pusher
      val domain_pusher : unit Domain.t = <abstr>
      # let domain_popper = Domain.spawn popper
      val domain_popper : int option list Domain.t = <abstr>
      # Domain.join domain_pusher
      - : unit = ()
      # Domain.join domain_popper
      - : int option list = [Some 42; Some 3; Some 2; Some 1; None; Some 12]
      ]}
 
    *)
