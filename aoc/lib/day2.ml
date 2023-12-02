open Core

let list_to_pair l =
  match l with
  | [num; color] -> (int_of_string num, color)
  | _ -> failwith "Invalid list"

let line_to_pairs line =
  let splitted = line |> String.split ~on:':' in
  List.nth_exn splitted 1
  |> String.tr ~target:';' ~replacement:','
  |> String.split ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:list_to_pair

let legal_color (num, color) =
  let max_num =
    match color with
    | "red" -> 12
    | "green" -> 13
    | "blue" -> 14
    | _ -> failwith "Unknown color" in
  num <= max_num

let legal_line line = line_to_pairs line |> List.for_all ~f:legal_color

let line_to_power line =
  let rec aux red blue green ps =
    match ps with
    | [] -> red * blue * green
    | (num, color) :: ps' ->
    match color with
    | "red" -> aux (max red num) blue green ps'
    | "blue" -> aux red (max blue num) green ps'
    | "green" -> aux red blue (max green num) ps'
    | _ -> failwith "Unknown color" in
  line_to_pairs line |> aux 0 0 0

let task1 lines =
  let f i line = (i + 1, legal_line line) in
  lines
  |> List.mapi ~f
  |> List.filter ~f:snd
  |> List.map ~f:fst
  |> Utils.list_sum

let task2 lines = lines |> List.map ~f:line_to_power |> Utils.list_sum
let lines = In_channel.read_lines "../../../data/day2.txt"
let result1 = task1 lines
let result2 = task2 lines
