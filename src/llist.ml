open Domain

(*
IDée DLS
- collector avec GC.finalize
- Patricia tree ou inode
pour allouer que les clés que tu utilises
Gain mémoire mais perte en perf la première fois
-> séquentiel parce que pour le patricia tree uniquement
(moins cher que les external ?)

- doc à améliorer
- ajouter un DLS overflow (hardlimit)


*)


type 'a node = {
  key : 'a;
  mark : bool Atomic.t;
  next : 'a node option Atomic.t;
}

type 'a t = {
  head : 'a node option Atomic.t;
  prev : 'a node option Atomic.t DLS.key;
  curr : 'a node option DLS.key;
  next : 'a node option DLS.key;
}


type 'a local =
  { prev : 'a node option Atomic.t;
    curr : 'a node option;
    next : 'a node option}

(* tableau de ref pour éviter contention (car pas de contention
enlecture )

A tester par rapport au false sharing *)

let init () =
  let head = Atomic.make None in
  {
    head;
    prev = DLS.new_key (fun () -> head);
    curr = DLS.new_key (fun () -> None);
    next = DLS.new_key (fun () -> None);
  }

let find key t : bool =
  let rec try_again () : bool =
    DLS.set t.prev t.head;
    DLS.set t.curr (Atomic.get t.head);
    let rec loop () =
      match DLS.get t.curr with
      | None -> false
      | Some { key = ckey; mark = cmark; next } ->
          DLS.set t.next (Atomic.get next);
          if Atomic.get (DLS.get t.prev) != DLS.get t.curr then
            (* Another domain has changed the current node *)
            try_again ()
          else if not (Atomic.get cmark) then
            if ckey >= key then ckey = key
            else (
              DLS.set t.prev next;
              DLS.set t.curr (DLS.get t.next);
              DLS.set t.next None;
              loop ())
          else if
            (* the current node has been marked for deletion
               -> we try to remove it now *)
            Atomic.compare_and_set (DLS.get t.prev) (DLS.get t.curr)
              (DLS.get t.next)
          then (
            (* it worked ! great, let's continue searching our key ! *)
            DLS.set t.curr (DLS.get t.next);
            DLS.set t.next None;
            loop ())
          else
            (* Another domain removed the marked node before us, we
               need to begin again *)
            try_again ()
    in
    loop ()
  in
  let res = match Atomic.get t.head with None -> false | _ -> try_again () in
  res

let reset t =
  DLS.set t.prev (Atomic.make None);
  DLS.set t.curr None;
  DLS.set t.next None

let insert key t =
  let rec loop () =
    if find key t then (* key already in the list *)
      false
    else
      (* not in ! We can add it ! [t.curr] has been set up by [find] to
         be the next node : we have everything to create our new node. *)
      let node =
        { key; mark = Atomic.make false; next = Atomic.make (DLS.get t.curr) }
      in
      (* let's try to add it! *)
      if Atomic.compare_and_set (DLS.get t.prev) (DLS.get t.curr) (Some node)
      then (
        reset t;
        true)
      else (
        reset t;
        loop ())
  in
  let res = loop () in
  res

let delete key t =
  let rec loop () =
    if not (find key t) then false
    else
      let curr = DLS.get t.curr |> Option.get in
      if not (Atomic.compare_and_set curr.mark false true) then loop ()
      else if
        Atomic.compare_and_set (DLS.get t.prev) (DLS.get t.curr)
          (DLS.get t.next)
      then true
      else not (find key t)
  in
  let res = loop () in
  res

let mem key t =
  let res = find key t in
  reset t;
  res

let clean_local t =
  DLS.set t.prev (Atomic.make None);
  DLS.set t.curr None;
  DLS.set t.next None

let close t =
  Atomic.set t.head None;
  reset t

let of_list l =
  let t = init () in
  let lsorted = List.sort_uniq compare l |> List.rev in
  List.iter (fun elt -> ignore @@ insert elt t) lsorted;
  t

(*
open Domain

type 'a node = { key : 'a; next_node : 'a mark_node Atomic.t }
and 'a mark_node = bool * 'a node option

type 'a t = {
  head : 'a mark_node Atomic.t;
  prev : 'a mark_node Atomic.t DLS.key;
  curr : 'a mark_node DLS.key;
  next : 'a mark_node DLS.key;
}

let init () =
  let head = Atomic.make (false, None) in
  {
    head;
    prev = DLS.new_key (fun () -> head);
    curr = DLS.new_key (fun () -> (false, None));
    next = DLS.new_key (fun () -> (false, None));
  }

let node key next = { key; next_node = Atomic.make (false, next) }

let find t key : bool =
  let rec try_again () =
    DLS.set t.prev t.head;
    DLS.set t.curr (Atomic.get t.head);
    let rec loop () =
      match DLS.get t.curr with
      | _, None -> false
      | pmark, Some curr_node -> (
          DLS.set t.next (Atomic.get curr_node.next_node);
          let ckey = curr_node.key in
          if Atomic.get @@ DLS.get t.prev != DLS.get t.curr then
            (* Another domain has changed the current node *)
            try_again ()
          else
            match DLS.get t.next with
            | false, _ ->
                if ckey >= key then ckey = key
                else (
                  DLS.set t.prev curr_node.next_node;
                  DLS.set t.curr (DLS.get t.next);
                  loop ())
            | true, _ ->
                if
                  Atomic.compare_and_set (DLS.get t.prev) (DLS.get t.curr)
                    (DLS.get t.next)
                then (
                  DLS.set t.curr (DLS.get t.next);
                  loop ())
                else try_again ())
    in
    loop ()
  in
  try_again ()

let insert t key =
  let rec loop () =
    if find t key then false
    else
      let node =
        { key; next_node = Atomic.make (false, snd (DLS.get t.curr)) }
      in
      if
        Atomic.compare_and_set (DLS.get t.prev) (DLS.get t.curr)
          (false, Some node)
      then true
      else loop ()
  in
  loop ()

*)
