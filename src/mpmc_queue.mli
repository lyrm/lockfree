(**
  Based on the paper :
   Wait-Free Queues With Multiple Enqueuers and Dequeuers
   A. Kogan, E. Petrank, 2011

   http://www.cs.technion.ac.il/~erez/Papers/wfquque-ppopp.pdf

*)

type 'a t


(* not enough domains raises Index out of bounds *)
val init : num_domain:int -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
