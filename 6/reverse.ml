let palindrome l =
  let reverse = List.rev l in
  let rec helper l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | _ :: _, [] | [], _ :: _ -> false
    | h1 :: t1, h2 :: t2 ->
       if h1 = h2 then helper t1 t2
       else false
  in helper l reverse

let%test _ = palindrome [1; 2; 3] = false
let%test _ = palindrome [1; 2; 3; 2; 1] = true
let%test _ = palindrome [] = true
let%test _ = palindrome [1; 2] = false
let%test _ = palindrome [1] = true
