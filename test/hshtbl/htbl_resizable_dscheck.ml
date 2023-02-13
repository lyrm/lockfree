module Htbl = Htbl_resizable.Htbl_resizable

let two_producers () =
  Atomic.trace (fun () ->
      let htbl = Htbl.init ~size_exponent:8 in
      let items_by_domain = 2 in
      let nb_domain = 2 in

      for i = 0 to nb_domain - 1 do
        Atomic.spawn (fun () ->
            for j = 0 to items_by_domain - 1 do
              let elt = j + (i * items_by_domain) in
              Htbl.add elt elt htbl |> ignore
            done)
      done;

      Atomic.final (fun () ->
          let is_here =
            List.init (nb_domain * items_by_domain) (fun i -> Htbl.mem i htbl)
          in

          Atomic.check (fun () -> List.for_all (fun i -> i) is_here)))

let () =
  let open Alcotest in
  run "hshtbl_dscheck"
    [ ("basic", [ test_case "2-producers" `Slow two_producers ]) ]
