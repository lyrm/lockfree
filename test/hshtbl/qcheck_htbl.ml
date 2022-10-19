module Htbl = Lockfree.Hshtbl

(* making sur the generation int list are small enough (small_nat < 100) *)
let int_list = QCheck.(list_of_size Gen.small_nat small_nat)

let tests_one_domain =
  QCheck.
    [
      (* Add a list of unique elements in a linked list. All
         insertion should success. *)
      Test.make ~name:"seq_insert" int_list (fun l ->
          assume (List.length l <= 32);
          let open Htbl in
          let t = init 32 in
          let l = List.sort_uniq compare l in
          List.for_all (fun elt -> insert elt elt t) l);
      (* Add a list of elements in a linked list and checks :

         - the elements that have already been added, the [insert]
         function returns [false] and [true] otherwise.

         - [mem] on all added elements returns [true].
      *)
      Test.make ~name:"seq_insert2" int_list (fun l ->
          assume (List.length l <= 32);
          let open Htbl in
          let t = init 32 in

          let has_been_added = List.map (fun elt -> insert elt elt t) l in

          let rec loop prev l has_been_added =
            match (l, has_been_added) with
            | [], [] -> true
            | [], _ | _, [] -> false
            | x :: xs, added :: has_been_added ->
                if added = not (List.mem x prev) then
                  loop (x :: prev) xs has_been_added
                else false
          in

          loop [] l has_been_added
          &&
          let uniq = List.sort compare l in
          List.for_all (fun elt -> mem elt t) uniq);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Hshtbl"
    [
      ("one_domain", to_alcotest tests_one_domain);
    ]
;;

main ()
