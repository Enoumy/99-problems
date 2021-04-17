type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten l =
  let rec helper acc = function
    | [] -> acc
    | One value :: t -> helper (value :: acc) t
    | Many values :: t -> helper (helper acc values) t
  in helper [] l |> List.rev

let%test _ = flatten
               [One "a";
                Many
                  [One "b";
                   Many [
                       One "c";
                       One "d"
                     ];
                   One "e"
                  ]
               ] = ["a"; "b"; "c"; "d"; "e"]
