let split l n =
  let rec helper acc count = function
    | [] -> (List.rev acc), []
    | h :: t as whole -> if count <= 0 then (List.rev acc), whole
                         else helper (h :: acc) (count - 1) t
  in helper [] n l

let%test _ = split ["a"; "b"; "c"] 2 = (["a"; "b"], ["c"])
let%test _ = split ["a"; "b"; "c"] 5 = (["a"; "b"; "c"], [])
let%test _ = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 =
               (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
let%test _ = split ["a"; "b"; "c"; "d"] 5 =
               (["a"; "b"; "c"; "d"], [])
