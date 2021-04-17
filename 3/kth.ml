let rec kth k = function
  | [] -> None
  | h :: t ->
     if k = 1 then Some h
     else if k < 1 then None
     else kth (k - 1) t

let test_list = [1; 2; 3; 4; 5]

let%test _ = kth 1 test_list = Some 1
let%test _ = kth 4 test_list = Some 4
let%test _ = kth 10 test_list = None
let%test _ = kth 1 [] = None
