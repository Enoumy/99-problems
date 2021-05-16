let split list idx =
  let rec helper i left right =
    if i = 0 then (List.rev left), right else
      match right with
      | [] -> (List.rev left), right
      | h :: t -> helper (i - 1) (h :: left) t
  in
  helper idx [] list

let rotate l n =
  let open Base in
  let length = List.length l in
  let real_n = n % length in
  let first, second = split l real_n in
  second @ first

let%test _ = rotate [1; 2; 3; 4] 2 = [3; 4; 1; 2]
let%test _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
let%test _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
