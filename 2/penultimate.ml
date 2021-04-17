let rec penultimate = function
  | [] | [_] -> None
  | [h; _] -> Some h
  | _ :: t -> penultimate t

let%test _ = penultimate [] = None
let%test _ = penultimate [1] = None
let%test _ = penultimate [1; 2] = Some 1
let%test _ = penultimate [3; 4; 1; 45] = Some 1
