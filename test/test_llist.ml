open Lockfree.Llist

let random_list max_size max_elt =
  List.init (Random.int max_size + 1) (fun i -> Random.int max_elt)

let print_list_int = function
  | [] -> Format.printf "[]@."
  | x :: xs ->
      Format.printf "[%d" x;
      List.iter (fun elt -> Format.printf "; %d" elt) xs;
      Format.printf "]@."

let print_list_bool = function
  | [] -> Format.printf "[]@."
  | x :: xs ->
      Format.printf "[%b" x;
      List.iter (fun elt -> Format.printf "; %b" elt) xs;
      Format.printf "]@."

let test n =
  let t = init () in
  let sema = Semaphore.Binary.make false in

  let l = List.init n (fun i -> i) in

  let d1 =
    Domain.spawn (fun () ->
        while not (Semaphore.Binary.try_acquire sema) do
          Domain.cpu_relax ()
        done;
        List.map (fun i -> delete i t) l)
  in
  let d2 =
    Domain.spawn (fun () ->
        Semaphore.Binary.release sema;
        List.map (fun i -> insert i t) l)
  in
  let delete = Domain.join d1 in
  let insert = Domain.join d2 in

  ( l, insert,
    delete,
    List.for_all2
      (fun has_been_deleted k ->
        if has_been_deleted then not (mem k t) else mem k t)
      delete l,
    t )

exception Error of (int list * bool list * bool list * int t) option

let main () =
  Random.self_init ();
  for i = 0 to 10000 do
    Format.printf "###New try##@.";
    let n = 2 in
    let l, insert, delete, test, t = test n in
    if not test then (
      Format.printf "###### ERROR #####@.";
      raise (Error (Some (l, insert, delete, t))))
  done;
  None
