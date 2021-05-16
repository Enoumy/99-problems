(*  From the previous problem.*)
let range l r =
  let rec helper curr finish update =
    if curr = finish then [curr]
    else curr :: helper (update curr) finish update
  in
  if l < r then helper l r (fun x -> x + 1) else
    helper l r (fun x -> x - 1)

let shuffle l =
  Random.self_init ();
  let new_order = List.map (fun _ -> Random.int 1_000_000_000) l in
  let open Base in
  List.zip_exn l new_order
  |> List.sort ~compare:(fun (_, order1) (_, order2) -> Int.compare order1 order2)
  |> List.map ~f:(fun (x, _) -> x)

let rand_select list number =
  let length = List.length list in
  let chosen = shuffle (range 0 (length - 1)) in
  let rec helper i acc = function
    | [] -> acc
    | (h :: t) -> if i = 0 then acc else helper (i - 1) (h :: acc) t in
  helper number [] chosen;;

print_string (String.concat " " (List.map string_of_int (rand_select ["a"; "b"; "c"; "d"; "e"; "f"] 3)));
print_string "End of transmission. Don't panic!";
