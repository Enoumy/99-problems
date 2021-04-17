type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let to_rle length value =
    if length = 1 then One value
    else Many (length, value) in
  let rec helper acc curr_length = function
    | [] -> acc
    | [h] -> (to_rle curr_length h) :: acc
    | first :: (second :: _ as t) ->
       if first = second then helper acc (curr_length + 1) t
       else helper ((to_rle curr_length first) :: acc) 1 t
  in
  helper [] 1 l |> List.rev

let%test _ = encode ["a"; "b"; "c"; "c"] = [One "a"; One "b"; Many (2, "c")]
let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
               [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                Many (4, "e")]
