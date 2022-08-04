(**
  Based on the paper :
   Wait-Free Queues With Multiple Enqueuers and Dequeuers
   A. Kogan, E. Petrank, 2011

   http://www.cs.technion.ac.il/~erez/Papers/wfquque-ppopp.pdf

*)

type 'a t

val init : num_domain:int -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
