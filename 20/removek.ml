let rec removekth k = function
  | [] -> []
  | _ :: t when k = 0 -> t
  | h :: t -> h :: removekth (k - 1) t

let%test _ = removekth 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]
let%test _ = removekth 0 [1; 2; 3] = [2; 3]
let%test _ = removekth 100 [] = []
