let eliminate_duplicates l =
  let rec helper acc = function
    | [] -> acc
    | [h] -> h :: acc
    | first :: ((second :: _) as remaining) ->
       if first = second then helper acc remaining
       else helper (first :: acc) remaining
  in helper [] l |> List.rev

let%test _ = eliminate_duplicates
               ["a";
                "a";
                "a";
                "a";
                "b";
                "c";
                "c";
                "a";
                "a";
                "d";
                "e";
                "e";
                "e";
                "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
