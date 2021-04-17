let rec last_element = function
  | [] -> None
  | [h] -> Some h
  | _ :: t -> last_element t

let%test _ = last_element [] = None
let%test _ = last_element [1] = Some 1
let%test _ = last_element [1; 2; 3; 4; 5] = Some 5
