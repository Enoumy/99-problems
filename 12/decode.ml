type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode l =
  let rec repeat_append n value acc =
    if n <= 0 then acc
    else repeat_append (n - 1) value (value :: acc)
  in
  let rec helper acc = function
    | [] -> acc
    | One value :: t -> helper (value :: acc) t
    | Many (count, value) :: t -> helper (repeat_append count value acc) t
  in
  helper [] l |> List.rev

let%test _ = decode [Many (2, "a"); One "b"] = ["a"; "a"; "b"]
let%test _ = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] =
               ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
