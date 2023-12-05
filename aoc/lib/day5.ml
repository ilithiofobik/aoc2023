open Core

type range_mapping =
  { destination_start : int
  ; source_start : int
  ; length : int
  }

type input =
  { seeds : int list
  ; maps : range_mapping array array
  }

let lines_to_maps lines =
  let cmp a b = Int.compare a.source_start b.source_start in
  let rec aux lines (map_acc : range_mapping list) (maps_acc : range_mapping array list) =
    match lines with
    | [] ->
      let sorted = map_acc |> Array.of_list in
      Array.sort ~compare:cmp sorted;
      sorted :: maps_acc |> List.rev |> Array.of_list
    | line :: lines ->
      if String.is_empty line
      then (
        let sorted = map_acc |> Array.of_list in
        Array.sort ~compare:cmp sorted;
        aux lines [] (sorted :: maps_acc))
      else if String.is_suffix line ~suffix:":"
      then aux lines map_acc maps_acc
      else
        line
        |> String.split ~on:' '
        |> (function
         | [ destination_start; source_start; length ] ->
           let destination_start = int_of_string destination_start in
           let source_start = int_of_string source_start in
           let length = int_of_string length in
           let map = { destination_start; source_start; length } in
           aux lines (map :: map_acc) maps_acc
         | _ -> failwith "Invalid input")
  in
  aux lines [] []
;;

let line_to_seed line =
  let splitted = line |> String.split ~on:':' in
  List.nth_exn splitted 1
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:int_of_string
;;

let lines_to_input lines =
  match lines with
  | seeds :: _ :: maps ->
    let seeds = seeds |> line_to_seed in
    let maps = maps |> lines_to_maps in
    { seeds; maps }
  | _ -> failwith "Invalid input"
;;

let map_value value map =
  let rec bin_search left right =
    if left > right
    then value
    else (
      let mid = (left + right) / 2 in
      let r_map = map.(mid) in
      if r_map.source_start <= value && value < r_map.source_start + r_map.length
      then value + r_map.destination_start - r_map.source_start
      else if value < r_map.source_start
      then bin_search left (mid - 1)
      else bin_search (mid + 1) right)
  in
  bin_search 0 (Array.length map - 1)
;;

let seed_to_location maps value =
  maps 
  |> Array.fold ~init:value ~f:map_value
;;

let ranges_to_min_loc input =
  let rec aux ranges cmin =
    match ranges with
    | [] -> cmin
    | r_start :: r_len :: rest ->
      List.init r_len ~f:(( + ) r_start)
      |> List.map ~f:(seed_to_location input.maps)
      |> Utils.list_min
      |> min cmin
      |> aux rest
    | _ -> failwith "Invalid input"
  in
  aux input.seeds Int.max_value
;;

let lines = In_channel.read_lines "../../../data/day5.txt"
let input1 = lines |> lines_to_input

let task input =
  input.seeds |> List.map ~f:(seed_to_location input.maps) |> Utils.list_min
;;

let result1 = input1 |> task
let result2 = input1 |> ranges_to_min_loc

let () =
  Stdio.printf "Result1: %d\n" result1;
  Stdio.printf "Result2: %d\n" result2
;;
