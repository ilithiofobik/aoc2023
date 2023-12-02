let list_sum list = list |> List.fold_left ( + ) 0
let list_prod list = list |> List.fold_left (fun a b -> a * b) 1

let list_cartesian base_row base_col =
  let rec aux row col result =
    match row with
    | [] -> result
    | r :: rs ->
    match col with
    | [] -> aux rs base_col result
    | c :: cs -> aux row cs ((r, c) :: result) in
  aux base_row base_col []

let seq_sum64 seq = seq |> Seq.fold_left Int64.add Int64.zero

let list_find_index value list =
  let rec aux list index =
    match list with
    | [] -> None
    | x :: xs -> if x = value then Some index else aux xs (index + 1) in
  aux list 0
