let list_to_pair l =
  match l with
  | [ num; color ] -> (int_of_string num, color)
  | _ -> failwith "Invalid list";;

let line_to_pairs line =
  line 
  |> String.split_on_char ':' 
  |> List.rev 
  |> List.hd 
  |> String.map (fun c -> if c = ';' then ',' else c)
  |> String.split_on_char ',' 
  |> List.map String.trim 
  |> List.map (String.split_on_char ' ') 
  |> List.map list_to_pair;;

let legal_color num color =
  let max_num =
    match color with 
    | "red" -> 12 
    | "green" -> 13
    | "blue" -> 14
    | _ -> failwith "Unknown color" in
  num <= max_num;;

let legal_line line =
  line 
  |> line_to_pairs
  |> List.for_all (fun (num, color) -> legal_color num color);;

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
  lines
  |> List.mapi (fun i line -> (i + 1, line))
  |> List.filter (fun (_, line) -> legal_line line)
  |> List.map fst 
  |> Utils.list_sum;;

let task2 lines =
  lines
  |> List.map line_to_power
  |> Utils.list_sum;;

let lines =
  Utils.file_to_lines ("input/day2.txt") in
let result1 = task1 lines in
let result2 = task2 lines in
  Printf.printf "Task1: %d\n" result1;
  Printf.printf "Task2: %d\n" result2;
  assert (result1 = 2149);
  assert (result2 = 71274);