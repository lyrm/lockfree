module Atomic = Dscheck.TracedAtomic

type 'a node = { key : 'a; next : (bool * 'a node option) Atomic.t }
type 'a t = (bool * 'a node option) Atomic.t

type 'a local = {
  prev : (bool * 'a node option) Atomic.t;
  curr : bool * 'a node option;
  next : bool * 'a node option;
}

let init () : 'a t = Atomic.make (false, None)

let find key (t : 'a t) : bool * 'a local =
  let rec try_again () =
    let local = { prev = t; curr = Atomic.get t; next = (false, None) } in
    let rec loop local =
      match local.curr with
      | _pmark, None -> (false, local)
      | _pmark, Some { key = ckey; next = ato_next } ->
          let next = Atomic.get ato_next in
          let local = { local with next } in
          if Atomic.get local.prev != local.curr then
            (* Another domain has changed the current node *)
            try_again ()
          else if not (fst next) then
            if ckey >= key then (ckey = key, local)
            else
              let local =
                { prev = ato_next; curr = next; next = (false, None) }
              in
              loop local
          else if
            (* the current node has been marked for deletion
               -> we try to remove it now *)
            Atomic.compare_and_set local.prev local.curr (false, snd local.next)
          then
            (* it worked ! great, let's continue searching our key ! *)
            let local =
              {
                local with
                curr = (false, snd local.next);
                next = (false, None);
              }
            in
            loop local
          else
            (* Another domain removed the marked node before us, we
               need to begin again *)
            try_again ()
    in
    loop local
  in
  let res = try_again () in
  res

let insert (key : 'a) (t : 'a t) =
  let rec loop () =
    let is_found, local = find key t in
    if is_found then (* key already in the list *)
      false
    else
      (* not in ! We can add it ! [t.curr] has been set up by [find] to
         be the next node : we have everything to create our new node. *)
      let node = { key; next = Atomic.make local.curr } in
      (* let's try to add it! *)
      if Atomic.compare_and_set local.prev local.curr (false, Some node) then
        true
      else loop ()
  in
  loop ()

let delete (key : 'a) (t : 'a t) =
  let rec loop () =
    let is_found, local = find key t in
    if not is_found then false
    else
      let curr = local.curr |> snd |> Option.get in
      if
        not (Atomic.compare_and_set curr.next local.next (true, snd local.next))
      then loop ()
      else if Atomic.compare_and_set local.prev local.curr local.next then true
      else (
        ignore (find key t);
        true)
  in
  loop ()

let mem key t =
  let is_found, _ = find key t in
  is_found
