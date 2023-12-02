open Core

let list_to_pair l =
  match l with
  | [ num; color ] -> (int_of_string num, color)
  | _ -> failwith "Invalid list";;

let line_to_pairs line =
  let splitted = String.split line ~on:':' in
  let games = List.nth_exn splitted 1 in
  let normalized = String.tr ~target:';' ~replacement:',' games in
  let pairs = String.split normalized ~on:',' in
  let trimmed = List.map pairs ~f:String.strip in 
  let rounds = List.map trimmed ~f:(String.split ~on:' ') in 
  List.map rounds ~f:list_to_pair;;

let legal_color (num, color) =
  let max_num =
    match color with 
    | "red" -> 12 
    | "green" -> 13
    | "blue" -> 14
    | _ -> failwith "Unknown color" in
  num <= max_num;;

let legal_line line =
  let pairs = line_to_pairs line in
  List.for_all ~f:legal_color pairs;;

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
  aux 0 0 0 (line_to_pairs line);;

let task1 lines =
  let mapped = List.mapi lines ~f:(fun i line -> (i + 1, legal_line line)) in
  let filtered = List.filter mapped ~f:snd in 
  List.map filtered ~f:fst 
  |> Utils.list_sum;;

let task2 lines =
  List.map ~f:line_to_power lines
  |> Utils.list_sum;;

let lines =
  Utils.file_to_lines ("../../../data/day2.txt");;
let result1 = task1 lines;;
let result2 = task2 lines;;
