let slice l i k =
  let rec helper acc index = function
    | [] -> acc
    | h :: t -> if index > k then acc
                else if index < i then
                  helper acc (index + 1) t
                else helper (h :: acc) (index + 1) t
  in helper [] 0 l |> List.rev

let%test _ = slice [0; 1; 2; 3; 4] 1 3 = [1; 2; 3]
let%test _ = slice [0; 1; 2; 3; 4] 1 10 = [1; 2; 3; 4]
let%test _ = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 =
               ["c"; "d"; "e"; "f"; "g"]
