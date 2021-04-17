let length l =
  let rec helper acc = function
  | [] -> acc
  | _ :: t -> helper (acc + 1) t
  in helper 0 l

let%test _ = length [] = 0
let%test _ = length ["a"; "b"; "c"] = 3
let%test _ = length [1; 2; 3; 45; 6] = 5
