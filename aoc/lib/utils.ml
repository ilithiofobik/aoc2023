open Core

let list_sum list = list |> List.fold_left ~f:( + ) ~init:0
let list_prod list = list |> List.fold_left ~f:(fun a b -> a * b) ~init:1

let list_cartesian base_row base_col =
  let rec aux row col result =
    match row with
    | [] -> result
    | r :: rs ->
      (match col with
       | [] -> aux rs base_col result
       | c :: cs -> aux row cs ((r, c) :: result))
  in
  aux base_row base_col []
;;
