let encode l =
  let rec helper acc curr_length = function
    | [] -> acc
    | [h] -> (curr_length, h) :: acc
    | first :: (second :: _ as t) ->
       if first = second then helper acc (curr_length + 1) t
       else helper ((curr_length, first) :: acc) 1 t
  in
  helper [] 1 l |> List.rev

let%test _ = encode ["a"; "b"; "c"; "c"] = [(1, "a"); (1, "b"); (2, "c")]
let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
               [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
