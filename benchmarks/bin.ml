let ntest = 100;;
let n = 12 ;;

let t = H.init ~size_exponent:12;;

let l =
  List.init n (fun _ ->
      let tstart = Unix.gettimeofday () in
      for _i = 0 to (ntest / n) - 1 do
        let k = Random.int 1000 in
        H.insert k k t |> ignore
      done;
      let tend = Unix.gettimeofday () in
      tend -. tstart);;


let l = List.sort compare l;;
List.nth l ((n / 2) - 1)
