let rec insert_at element i list =
  if i = 0 then element :: list
  else match list with
       | [] -> [element]
       | h :: t -> h :: insert_at element (i - 1) t

let%test _ = insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"]
let%test _ = insert_at "alfa" 3 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "alfa"; "d"]
let%test _ = insert_at "alfa" 100 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"]
