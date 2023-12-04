open Core

let string_to_nums str =
  str
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> not (String.is_empty x))
  |> List.map ~f:int_of_string
;;

let line_to_value line =
  let pair =
    let splitted = line |> String.split ~on:':' in
    List.nth_exn splitted 1 |> String.split ~on:'|' |> List.map ~f:string_to_nums
  in
  let winning = List.nth_exn pair 0 in
  let my = List.nth_exn pair 1 in
  let f x = List.mem winning x ~equal:( = ) in
  my |> List.count ~f
;;

let safe_pow num =
  match num with
  | 0 -> 0
  | n -> Int.pow 2 (n - 1)
;;

let scratch_count values =
  let rec aux values counts idx =
    match values with
    | [] -> counts |> Utils.list_sum
    | v :: vs ->
      let new_counts =
        let c = List.nth_exn counts idx in
        let f i x = if idx < i && i <= idx + v then x + c else x in
        counts |> List.mapi ~f
      in
      aux vs new_counts (idx + 1)
  in
  let counts = List.init (List.length values) ~f:(const 1) in
  aux values counts 0
;;

let lines = In_channel.read_lines "../../../data/day4.txt"
let values = lines |> List.map ~f:line_to_value
let result1 = values |> List.map ~f:safe_pow |> Utils.list_sum
let result2 = scratch_count values
