let drop l n =
  let rec helper acc count = function
    | [] -> acc
    | h :: t -> if count = 0 then helper acc ((count + 1) mod n) t
                else helper (h :: acc) ((count + 1) mod n) t
  in helper [] 1 l |> List.rev

let%test _ = drop [1; 2; 3; 4; 5] 2 = [1; 3; 5]
let%test _ = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 =
               ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
