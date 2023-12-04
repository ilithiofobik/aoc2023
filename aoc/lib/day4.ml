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
  let counts = Array.init (List.length values) ~f:(const 1) in
  let rec aux values idx =
    match values with
    | [] -> counts |> Utils.array_sum
    | v :: vs ->
      let c = idx |> Array.get counts in
      List.init v ~f:(fun x -> x + idx + 1)
      |> List.iter ~f:(fun x -> counts.(x) <- counts.(x) + c);
      aux vs (idx + 1)
  in
  aux values 0
;;

let lines = In_channel.read_lines "../../../data/day4.txt"
let values = lines |> List.map ~f:line_to_value
let result1 = values |> List.map ~f:safe_pow |> Utils.list_sum
let result2 = scratch_count values
