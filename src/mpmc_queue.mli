(**
  Wait-Free queue with multiple producers and consumers.

  + Based on the paper :
     Wait-Free Queues With Multiple Enqueuers and Dequeuers
     A. Kogan, E. Petrank, 2011

     http://www.cs.technion.ac.il/~erez/Papers/wfquque-ppopp.pdf

  + Example with 2 domains :

   let queue = init ~num_domain:2 in
   let wq1 = register queue in

   let domain2 = Domain.spawn (fun () ->
     let wq2 = register queue in
     push wq2 42;
     pop wq2
   ) in

   let n1 = pop wq1 in
   let n2 = Domain.join domain2 in
   n1, n2
*)

type 'a t
(** Wait-free queue for multiple producers and multiples consumers.
 *)

val init : num_domain:int -> 'a t
(** [init ~num_domain:n] creates a wait-free queue that can be
   accessed by at maximum [n] domains. *)

type 'a wt
(** Describes a handler for an ['a t] queue. All domains working on a
   queue must have their own handler for it. *)

val register : 'a t -> 'a wt
(** [register q] builds a handler for [q] that enables the domain
   calling this function to work on it. The resulting handler is own
   by the calling domain and only him can use it. Otherwise, the
   exception [Not_handler_owner] is raised.

   A domain should call this function only once on a specific queue,
   otherwise, it will take an additional slot and reduce the total
   number of domains able to use the queue.

   If a domain calls [register] while there are already [n] registered
   domains, with [n] being the value of the argument [~num_domain]
   passed to [init], an exception [Too_Many_Registered_Domains] is
   raised. *)

exception Not_handler_owner

exception Too_many_registered_domains
(** Exception raises if more domains than the number given at the
   initialization of the queue with [init] try to [register]. *)

val push : 'a wt -> 'a -> unit
(** [push wq elt] adds [elt] at the end of the queue handled by
   [wq]. The current domain must own [wq] otherwise
   [Not_handler_owner] is raised. *)

val pop : 'a wt -> 'a option
(** [pop wq elt] pops the head of the queue handled by [wq]. The
   current domain must own [wq] otherwise [Not_handler_owner] is
   raised. *)
