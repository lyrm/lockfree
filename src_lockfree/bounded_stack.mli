(** Lock-free bounded stack. *)

(** {1 API} *)

type 'a t
(** Represents a lock-free bounded stack holding elements of type ['a]. *)

val create : ?capacity:int -> unit -> 'a t
(** [create ~capacity ()] creates a new empty bounded stack with a maximum 
capacity of [capacity]. Default [capacity] value is [Int.max_int].
*)

val length : 'a t -> int
(** [length stack] returns the number of elements currently in the [stack]. *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false]. *)

(** {2 Consumer functions} *)

exception Empty
(** Raised when {!push_exn} or {!peek_exn} is applied to an empty stack.

  This exception is meant to avoid allocations required by an option type.
  As such, it does not register backtrace information and it is recommended to 
  use the following pattern to catch it.

  {[
    match pop_exn s with
      | value -> (* ... *)
      | exception Empty -> (* ... *)
  ]} *)

val peek_exn : 'a t -> 'a
(** [peek_exn stack] returns the top element of the [stack] without removing it.
    
  @raises Empty if the [stack] is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the [stack] is empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn stack] removes and returns the top element of the [stack].
 
  @raises Empty if the stack [s] is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the [stack],
    or [None] if the stack is empty. *)

val pop_all : 'a t -> 'a list
(** [pop_all stack] removes and returns all elements of the [stack] in the LIFO 
order. 

  {[
    # let t : int Saturn.Bounded_stack.t =
     Saturn.Bounded_stack.create ()
    val t : int Saturn.Bounded_stack.t = <abstr>
    # Saturn.Bounded_stack.try_push t 1
    - : bool = true
    # Saturn.Bounded_stack.try_push t 2
    - : bool = true
    # Saturn.Bounded_stack.try_push t 3
    - : bool = true
    # Saturn.Bounded_stack.pop_all t
    - : int list = Some [3; 2; 1]
  ]}
*)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq stack] returns a sequence of all elements of the [stack] in the LIFO
order. Equivalent to {[ pop_all stack |> List.to_seq ]} *)

(** {2 Producer functions} *)

exception Full
(** Raised when {!push_exn} is applied to a full stack. *)

val push_exn : 'a t -> 'a -> unit
(** [push_exn stack element] adds [element] to the top of the [stack].
    
  @raises Full if the stack [s] is full. *)

val try_push : 'a t -> 'a -> bool
(** [try_push stack element] tries to add [element] to the top of the [stack].
    Returns [true] if the element was successfully added, or [false] if the
    stack is full. *)

val push_all_exn : 'a t -> 'a list -> unit
(** [push_exn stack elements] adds all [elements] to the top of the [stack].
    
  @raises Full if the stack [s] is full. *)

val try_push_all : 'a t -> 'a list -> bool
(** [try_push stack elements] tries to add all [elements] to the top of the [stack].
    Returns [true] if the element was successfully added, or [false] if the
    stack is full. 
    
  {[
    # let t : int Saturn.Bounded_stack.t =
     Saturn.Bounded_stack.create ()
    val t : int Saturn.Bounded_stack.t = <abstr>
    # Saturn.Bounded_stack.try_push_all t [1; 2; 3; 4]
    - : bool = true
    # Saturn.Bounded_stack.pop_opt t
    - : int option = Some 4
    # Saturn.Bounded_stack.pop_opt t 
    - : int option = Some 3
    # Saturn.Bounded_stack.pop_all t
    - : int list = Some [2; 1]
  ]}
    *)

(** {1 Examples}
    An example top-level session:
    {[
      # let t : int Saturn.Bounded_stack.t =
        Saturn.Bounded_stack.create ()
      val t : int Saturn.Bounded_stack.t = <abstr>
      # Saturn.Bounded_stack.try_push t 42
      - : bool = true
      # Saturn.Bounded_stack.push_exn t 1
      - : unit = ()
      # Saturn.Bounded_stack.pop_exn t
      - : int = 1
      # Saturn.Bounded_stack.peek_opt t
      - : int option = Some 42
      # Saturn.Bounded_stack.pop_opt t
      - : int option = Some 42 
      # Saturn.Bounded_stack.pop_opt t
      - : int option = None
      # Saturn.Bounded_stack.pop_exn t
      Exception: Bounded_stack.Empty.]}

    A multicore example: 
 
    *)
