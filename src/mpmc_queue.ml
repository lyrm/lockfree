(**
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
(** Type representing the queue as a singly-linked list :

   - [head] : it is actually the "previous" head of the queue, i.e :

     + when poping the value returned is [(head.next |> Option.get).value]

     + an empty list is such as [head.next = None]

   - [tail] : where new node are attached to form the queue

   - [state] : described the current state of each domain, i.e are they
    currently enqueuing or dequeuing ? And if so, which node ? Each
    domain has its own box but all domains will read and write on it

   - [tid_tbl] : enables to links the domain identifier (returns by
    [Domain.self ()]) and its index in [state] field.
 *)

and 'a node = {
  value : 'a option;
  next : 'a node option Atomic.t;
  enq_tid : int;
  deq_tid : int Atomic.t;
}
(** Holds one element of the queue.

   - [value] is the actual value of the node

   - [next] is the next node in the queue ([node.next] is popped
   [next] or say otherwise has been pushed after)

   - [enq_tid] is the identifier of this node's enqueuer

   - [deq_tid] is the identifier of this node's dequeuer. Having a
   value > -1 means this node is being popped.

   [enq_tid] and [deq_tid] are not directly domain identifier but are
   linked to it by [tid_tbl]. They are used to identify and help the
   identified domain if it is still working on the corresponding
   operation.
*)

and 'a op_desc = {
  phase : int;
  pending : bool;
  enqueue : bool;
  node : 'a node option;
}
(** Operation descriptor of a domain.

    - [phase] : describes the age of the current (or previous, if no
   operation are in progress) operation. Younger operations are higher
   phase value.

    - [pending] : is [true] when the corresponding domain is currently
   trying to do an enqueue or a dequeue.

    - [enqueue] : is used only to know if a domain is currently
   enqueuing (i.e. [enqueue] and [pending] are [true])

    - [node] : is used to store either the node to enqueue or the node
   to dequeue (i.e. the head node).  *)

let init_node value etid : 'a node =
  { value; next = Atomic.make None; enq_tid = etid; deq_tid = Atomic.make (-1) }

let init_op_desc phase pending enqueue node = { phase; pending; enqueue; node }
let empty_node v = init_node v (-1)

let init ~num_domain =
  let sentinel = empty_node None in
  {
    head = Atomic.make sentinel;
    (* None ? *)
    tail = Atomic.make sentinel;
    (* None ? *)
    state =
      Array.init num_domain (fun _ ->
          Atomic.make @@ init_op_desc (-1) false true None);
    register = Atomic.make 0;
  }

type 'a wt = Domain.id * int * 'a t

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
(* let max = ref (-1) in
     for i = 0 to Array.length t.state - 1 do
       max := Int.max (Atomic.get t.state.(i)).phase !max
     done;
   !max*)

let is_still_pending t tid phase =
  (Atomic.get t.state.(tid)).pending
  && (Atomic.get t.state.(tid)).phase <= phase

exception Not_handler_owner

let rec enq ((tid, ind, t) : 'a wt) value =
  if Domain.self () <> tid then raise Not_handler_owner;
  let phase = max_phase t + 1 in
  (* it may seem that evey case of the state array is accessed only by
     the [ind] domain but it is not the case ! An other may come to help. *)
  Atomic.set t.state.(ind)
  @@ init_op_desc phase true true (Some (init_node (Some value) ind));
  help t phase;
  help_finish_enq t

and deq ((tid, ind, t) : 'a wt) =
  if Domain.self () <> tid then raise Not_handler_owner;
  let phase = max_phase t + 1 in
  Atomic.set t.state.(ind) @@ init_op_desc phase true false None;
  help t phase;
  help_finish_deq t;
  match (Atomic.get t.state.(ind)).node with
  | None -> None
  | Some node -> (Option.get (Atomic.get node.next)).value

and help t phase =
  Array.iteri
    (fun ind op_desc ->
      let desc = Atomic.get op_desc in
      if desc.pending && desc.phase <= phase then
        if desc.enqueue then help_enq t ind phase else help_deq t ind phase)
    t.state

and help_enq t ind phase =
  (* why is this wait free ? *)
  if is_still_pending t ind phase then
    let last = Atomic.get t.tail in
    let next = Atomic.get last.next in
    if last = Atomic.get t.tail then (
      match next with
      | None ->
          (* enqueue can be applied *)
          if is_still_pending t ind phase then
            if
              Atomic.compare_and_set last.next next
                (Atomic.get t.state.(ind)).node
            then help_finish_enq t (* go out of the loop *)
            else help_enq t ind phase
          else help_enq t ind phase
      | Some _ ->
          (* some enqueue is in progress *)
          help_finish_enq t (* help it first, then retry *);
          help_enq t ind phase)
    else help_enq t ind phase
  else ()

and help_finish_enq t =
  let last = Atomic.get t.tail in
  let nextopt = Atomic.get last.next in
  match nextopt with
  | Some next ->
      (* working domain can be or not be the enqueuer *)
      let next_enq_tid = next.enq_tid in
      let curDesc = Atomic.get t.state.(next_enq_tid) in
      (* making sure no one has already done the job *)
      if
        last = Atomic.get t.tail
        && (Atomic.get t.state.(next_enq_tid)).node = nextopt
      then (
        let newDesc =
          init_op_desc (Atomic.get t.state.(next_enq_tid)).phase false true
            nextopt
        in
        (* The current thread changes a box in [t.state] that may not be its own *)
        ignore (Atomic.compare_and_set t.state.(next_enq_tid) curDesc newDesc);
        ignore (Atomic.compare_and_set t.tail last next))
  | None -> () (* another domain already finished the enqueue *)

and help_deq t ind phase =
  if is_still_pending t ind phase then
    let first = Atomic.get t.head in
    let last = Atomic.get t.tail in
    let next = Atomic.get first.next in
    if first = Atomic.get t.head then
      if first = last then (
        (* queue may by empty (or a enqueue operation is in progress) *)
        match next with
        | None ->
            (* queue is empty *)
            let curDesc = Atomic.get t.state.(ind) in
            (if last = Atomic.get t.tail && is_still_pending t ind phase then
             let newDesc =
               init_op_desc (Atomic.get t.state.(ind)).phase false false None
             in
             Atomic.compare_and_set t.state.(ind) curDesc newDesc |> ignore);
            (* retry in case the previous CAS operation did not work *)
            help_deq t ind phase
        | Some _ ->
            (* an enqueue operation is in progress : help than retry *)
            help_finish_enq t;
            help_deq t ind phase)
      else
        (* queue is not empty *)
        let curDesc = Atomic.get t.state.(ind) in
        let node = curDesc.node in
        if is_still_pending t ind phase then
          if first = Atomic.get t.head && node <> Some first then
            let newDesc =
              init_op_desc (Atomic.get t.state.(ind)).phase true false
                (Some first)
            in
            if Atomic.compare_and_set t.state.(ind) curDesc newDesc then (
              Atomic.compare_and_set first.deq_tid (-1) ind |> ignore;
              help_finish_deq t;
              help_deq t ind phase)
            else help_deq t ind phase
          else (
            Atomic.compare_and_set first.deq_tid (-1) ind |> ignore;
            help_finish_deq t;
            help_deq t ind phase)
        else
          (* another thread has finished the operation (line 128 in the paper) *)
          ()
    else help_deq t ind phase
  else ()

and help_finish_deq t =
  let first = Atomic.get t.head in
  let next = Atomic.get first.next in
  let ind = Atomic.get first.deq_tid in
  if ind <> -1 then
    let curDesc = Atomic.get t.state.(ind) in
    if first = Atomic.get t.head then
      match next with
      | Some next ->
          let newDesc =
            init_op_desc (Atomic.get t.state.(ind)).phase false false
              (Atomic.get t.state.(ind)).node
          in
          Atomic.compare_and_set t.state.(ind) curDesc newDesc |> ignore;
          Atomic.compare_and_set t.head first next |> ignore
      | None -> print_endline "none"

let push = enq
let pop = deq
