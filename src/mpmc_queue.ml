(*
 * Copyright (c) 2022, Carine Morel <carine@tarides.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(*
  Based on the paper :
   Wait-Free Queues With Multiple Enqueuers and Dequeuers
   A. Kogan, E. Petrank, 2011

   http://www.cs.technion.ac.il/~erez/Papers/wfquque-ppopp.pdf

*)

type 'a t = {
  head : 'a node Atomic.t;
  tail : 'a node Atomic.t;
  state : 'a op_desc Atomic.t Array.t;
  register : int Atomic.t;
}
(* Type representing the queue as a singly-linked list :

   - [head] : it is actually the "previous" head of the queue, i.e :

     + when poping the returned value is [(head.next |>
   Option.get).value]

     + an empty list is such as [head.next = None]

   - [tail] : where new nodes are attached to form the queue

   - [state] : described the current state of each domain, i.e are
     they currently enqueuing or dequeuing ? And if so, which node ?
     Each domain has its own cell describing its current state, but all
     domains will read and write on it.

   - [register] : each time a domain registers, it gets a number egal
     to the current value of [register] that is then incremented. It
     links a domain to a cell in the array [state].*)

and 'a node = {
  value : 'a option;
  next : 'a node option Atomic.t;
  enq_tid : int option;
  deq_tid : int option Atomic.t;
}
(* Holds one element of the queue.

   - [value] is the actual value of the node

   - [next] is the next node in the queue ([node.next] is popped
     [next] or say otherwise has been pushed after)

   - [enq_tid] is the identifier of this node's enqueuer

   - [deq_tid] is the identifier of this node's dequeuer. Having a
     value > -1 means this node is being popped.

   Each domain is be given an unique identifier when
   [register]ing. These are the identifiers used for [end_tid] and
   [deq_tid]. *)

and 'a op_desc = {
  phase : int;
  pending : bool;
  enqueue : bool;
  node : 'a node option;
}
(* Operation descriptor of a domain.

    - [phase] : describes the age of the current operation (or
   previous, if none are in progress) operation. Younger operations
   have an higher phase.

    - [pending] : is [true] when the corresponding domain is currently
   trying to do an enqueue or a dequeue.

    - [enqueue] : is used only to know if a domain is currently
   enqueuing (i.e. [enqueue] and [pending] are [true])

    - [node] : is used to store either the node to enqueue or the node
   to dequeue (i.e. the head node). *)

let empty_node v =
  {
    value = v;
    next = Atomic.make None;
    enq_tid = None;
    deq_tid = Atomic.make None;
  }

let init ~num_domain =
  let sentinel = empty_node None in
  {
    head = Atomic.make sentinel;
    tail = Atomic.make sentinel;
    state =
      Array.init num_domain (fun _ ->
          Atomic.make
            { phase = -1; pending = false; enqueue = true; node = None });
    register = Atomic.make 0;
  }

type 'a wt = Domain.id * int * 'a t
(* Queue handler (tid, ind, t) :
   - [t] : Queue associated to the handler

   - [tid] : Domain identifier from [Domain.self ()]

   - [ind] : Unique identifier between 0 and [num_domain -1] that
   links a domain to one celle in [t.state] array.
*)

exception Too_many_registered_domains

let rec register t : 'a wt =
  let tid = Atomic.get t.register in
  if tid >= Array.length t.state then raise Too_many_registered_domains;
  if Atomic.compare_and_set t.register tid (tid + 1) then
    (Domain.self (), tid, t)
  else register t

let max_phase t =
  Array.fold_left
    (fun max elt -> Int.max (Atomic.get elt).phase max)
    (Atomic.get t.state.(0)).phase t.state

(* [is_still_pending] checks that the current operation is still
   unfinished. Phase comparison ensures that the [pending] operation
   has not changed. *)
let is_still_pending t tid phase =
  (Atomic.get t.state.(tid)).pending
  && (Atomic.get t.state.(tid)).phase <= phase

exception Not_handler_owner

let rec enq ((tid, ind, t) : 'a wt) value =
  if Domain.self () <> tid then raise Not_handler_owner;
  let phase = max_phase t + 1 in
  Atomic.set t.state.(ind)
  @@ {
       phase;
       pending = true;
       enqueue = true;
       node =
         Some
           {
             value = Some value;
             next = Atomic.make None;
             enq_tid = Some ind;
             deq_tid = Atomic.make None;
           };
     };
  help t phase;
  help_finish_enq t;
  (* Optimization 3: cleaning the operation descriptor.
     TODO : checks this optimization is actually efficient *)
  Atomic.set t.state.(ind)
  @@ { phase; pending = false; enqueue = true; node = None }

and deq ((tid, ind, t) : 'a wt) =
  if Domain.self () <> tid then raise Not_handler_owner;
  let phase = max_phase t + 1 in
  Atomic.set t.state.(ind)
  @@ { phase; pending = true; enqueue = false; node = None };
  help t phase;
  help_finish_deq t;
  (* Finally we can return the popped value *)
  let v =
    match (Atomic.get t.state.(ind)).node with
    | None -> None (* queue is empty*)
    | Some node -> (Option.get (Atomic.get node.next)).value
  in
  (* Optimization 3: cleaning the operation descriptor.
     TODO : checks this optimization is actually efficient *)
  Atomic.set t.state.(ind)
  @@ { phase; pending = false; enqueue = false; node = None };
  v

(* [help] function makes sure that before performing its own works, a
   domain helps other domains perform their older operations. In case
   no operations are pending, a domain calling [help] will just help
   itself and pursue its own current operation. *)
and help t phase =
  Array.iteri
    (fun ind op_desc ->
      let desc = Atomic.get op_desc in
      if desc.pending && desc.phase <= phase then
        (* An older operation is in progress in this domain *)
        if desc.enqueue then (* Enqueue operation *) help_enq t ind phase
        else (* Dequeue operation *) help_deq t ind phase)
    t.state

and help_enq t ind phase =
  if is_still_pending t ind phase then
    let last = Atomic.get t.tail in
    let next = Atomic.get last.next in
    if last = Atomic.get t.tail then (
      match next with
      | None ->
          (* Enqueue can be applied *)
          if is_still_pending t ind phase then
            if
              Atomic.compare_and_set last.next next
                (Atomic.get t.state.(ind)).node
            then help_finish_enq t
            else help_enq t ind phase
          else () (* another domain finished this enqueue *)
      | Some _ ->
          (* Another enqueue is in progress *)
          help_finish_enq t
          (* help finishing the other enqueue first,
             then retry *);
          help_enq t ind phase)
    else help_enq t ind phase
  else ()

and help_finish_enq t =
  let last = Atomic.get t.tail in
  let nextopt = Atomic.get last.next in
  match nextopt with
  | Some next ->
      (* getting the id and state of the domain that called [enq] *)
      let next_enq_tid = Option.get next.enq_tid in
      let curDesc = Atomic.get t.state.(next_enq_tid) in
      if
        (* making sure no one has already done the job *)
        last = Atomic.get t.tail
        && (Atomic.get t.state.(next_enq_tid)).node = nextopt
      then (
        let newDesc =
          { (Atomic.get t.state.(next_enq_tid)) with pending = false }
        in
        (* These two last lines work because until the [tail] value is
           moved to [next] (second line), every helping domains will
           try to do it (and nothing else) (see the [match next with
           Some _] case in [help_enq] for exemple. *)
        ignore (Atomic.compare_and_set t.state.(next_enq_tid) curDesc newDesc);
        ignore (Atomic.compare_and_set t.tail last next))
  | None -> () (* another domain already finished the enqueue *)

and help_deq t ind phase =
  if is_still_pending t ind phase then
    let first = Atomic.get t.head in
    let last = Atomic.get t.tail in
    let next = Atomic.get first.next in
    if first = Atomic.get t.head then
      (* the queue is empty but an enqueue operation may be ongoing *)
      if first = last then (
        match next with
        | None ->
            (* queue is empty *)
            let curDesc = Atomic.get t.state.(ind) in
            (if last = Atomic.get t.tail && is_still_pending t ind phase then
             let newDesc =
               { (Atomic.get t.state.(ind)) with pending = false; node = None }
             in
             Atomic.compare_and_set t.state.(ind) curDesc newDesc |> ignore);
            (* retry in case the previous CAS operation did not work *)
            help_deq t ind phase
        | Some _ ->
            (* an enqueue operation is in progress : help than retry *)
            help_finish_enq t;
            help_deq t ind phase)
      else
        (* queue has at least one element *)
        let curDesc = Atomic.get t.state.(ind) in
        let node = curDesc.node in
        if is_still_pending t ind phase then
          if first = Atomic.get t.head && node <> Some first then
            let newDesc =
              (* registering the popped node in the dequeuer state *)
              { (Atomic.get t.state.(ind)) with node = Some first }
            in
            if Atomic.compare_and_set t.state.(ind) curDesc newDesc then (
              Atomic.compare_and_set first.deq_tid None (Some ind) |> ignore;
              help_finish_deq t;
              help_deq t ind phase)
            else help_deq t ind phase
          else (
            Atomic.compare_and_set first.deq_tid None (Some ind) |> ignore;
            help_finish_deq t;
            help_deq t ind phase)
        else (* another thread has finished the operation *)
          ()
    else help_deq t ind phase
  else ()

and help_finish_deq t =
  let first = Atomic.get t.head in
  let next = Atomic.get first.next in
  let ind = Atomic.get first.deq_tid in
  match ind with
  | None -> ()
  | Some ind -> (
      let curDesc = Atomic.get t.state.(ind) in
      if first = Atomic.get t.head then
        match next with
        | Some next ->
            let newDesc = { (Atomic.get t.state.(ind)) with pending = false } in
            Atomic.compare_and_set t.state.(ind) curDesc newDesc |> ignore;
            Atomic.compare_and_set t.head first next |> ignore
        | None -> print_endline "none")

let push = enq
let pop = deq
