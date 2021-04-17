let reverse l =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (h :: acc) t
  in helper [] l

let%test _ = reverse [1; 2; 3; 4] = [4; 3; 2; 1]
let%test _ = reverse [1] = [1]
let%test _ = reverse [] = []
