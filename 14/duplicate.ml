let duplicate l =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (h :: h :: acc) t
  in helper [] l |> List.rev

let%test _ = duplicate [1; 2] = [1; 1; 2; 2]
let%test _ = duplicate ["a"; "b"; "c"; "c"; "d"] =
               ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
