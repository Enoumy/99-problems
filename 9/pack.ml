let pack l =
  let rec helper acc curr = function
    | [] -> (
      match curr with
      | [] -> acc
      | _ :: _ as rem -> rem :: acc
    )
    | h :: t -> (
      match curr with
      | [] -> helper acc (h :: curr) t
      | curr_head :: _ ->
         if curr_head = h then helper acc (h :: curr) t
         else helper (curr :: acc) [h] t
    )
  in helper [] [] l |> List.rev

let%test _ = pack ["a"; "b"; "c"; "c"] = [["a"]; ["b"]; ["c"; "c"]]
let%test _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] =
               [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                ["e"; "e"; "e"; "e"]];
