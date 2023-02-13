module type HSTBL = sig
  type 'a t

  val init : size_exponent:int -> 'a t
  val add : int -> 'a -> 'a t -> bool
  val find : int -> 'a t -> 'a option
  val mem : int -> 'a t -> bool
  val remove : int -> 'a t -> bool
end

module Hshtbl_seq : HSTBL = struct
  type 'a t = (int, 'a) Hashtbl.t

  let init ~size_exponent =
    let size = Int.shift_left 1 size_exponent in
    Hashtbl.create size

  let add k v t =
    if Hashtbl.mem t k then false
    else (
      Hashtbl.add t k v;
      true)

  let remove k t =
    if Hashtbl.mem t k then (
      Hashtbl.remove t k;
      true)
    else false

  let find k t = Hashtbl.find_opt t k
  let mem k t = Hashtbl.mem t k
end

let test_add ntest (module H : HSTBL) =
  let t = H.init ~size_exponent:12 in
  Random.self_init ();
  for _ = 0 to ntest do
    let k = Random.int ntest in
    H.add k k t |> ignore
  done

(* Command line *)
let usage_msg = "test_perf -m <module_name> -n <number_of_runs>"
let nb_test = ref 100_000
let hstbl_name = ref "lf"

let speclist =
  [
    ( "-m",
      Arg.Set_string hstbl_name,
      "Hashtable name (<seg>, <lf> or <lf_r>). Default is <lf>" );
    ("-n", Arg.Set_int nb_test, "Number of runs. Default is 10 000.");
  ]

let all_implem =
  [
    ("seq", (module Hshtbl_seq : HSTBL));
    ("lf", (module Lockfree.Hshtbl : HSTBL));
    ("lf_r", (module Lockfree.Hshtbl_resizable : HSTBL));
  ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  match List.assoc_opt !hstbl_name all_implem with
  | None -> Format.printf "%s @." usage_msg
  | Some (module H : HSTBL) -> test_add !nb_test (module H : HSTBL)
