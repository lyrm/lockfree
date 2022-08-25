(**
  Based on the paper :
   Wait-Free Queues With Multiple Enqueuers and Dequeuers
   A. Kogan, E. Petrank, 2011

   http://www.cs.technion.ac.il/~erez/Papers/wfquque-ppopp.pdf

*)

type 'a t
(** Wait-free queue for multiple producers and multiples consumers. *)

val init : num_domain:int -> 'a t
(** [init ~num_domain:n] creates a wait-free queue that can be
   accessed by at maximum [n] domains. *)

type 'a wt

val register : 'a t -> 'a wt
(** To work on a queue, a domain must register using this function. If
   there are already the maximum number of domains registered (the
   [~num_domain] argument of [init]), the exception
   [Too_Many_Registered_Domains] is raised. If a domain tries to work on
   a type [wt] returns by  *)

exception Not_queue_owner

exception Too_Many_Registered_Domains
(** Exception raises if more domains than the number given at
   initialization of the queue with [init] try to register with
   [register]. *)

val push : 'a wt -> 'a -> unit
(** [push wq elt] adds [elt] at the end of the queue [wq]. The current
   domain must own [wq] i.e. it must be the one that builds it with
   [register]. *)

val pop : 'a wt -> 'a option
(** [pop wq elt] pops the head of the queue [wq]. The current domain
   must own [wq] i.e. it must be the one that builds it with
   [register]. *)
