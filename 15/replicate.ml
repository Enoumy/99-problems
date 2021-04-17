let replicate l n =
  let rec append_n_times acc count value =
    if count <= 0 then acc
    else append_n_times (value :: acc) (count - 1) value
  in
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (append_n_times acc n h) t
  in helper [] l |> List.rev

let%test _ = replicate [1; 2; 3] 3 = [1; 1; 1; 2; 2; 2; 3; 3; 3]
let%test _ = replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
