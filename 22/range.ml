let range l r =
  let rec helper curr finish update =
    if curr = finish then [curr]
    else curr :: helper (update curr) finish update
  in
  if l < r then helper l r (fun x -> x + 1) else
    helper l r (fun x -> x - 1)

let%test _ = range 4 9 = [4; 5; 6; 7; 8; 9]
let%test _ = range 9 4 = [9; 8; 7; 6; 5; 4]
let%test _ = range 10 10 = [10]
