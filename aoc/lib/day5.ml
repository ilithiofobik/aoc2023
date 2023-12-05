open Core

type range_mapping =
  { destination_start : int
  ; source_start : int
  ; length : int
  }

type input =
  { seeds : int list
  ; maps : range_mapping list list
  }

let lines_to_maps lines =
  let rec aux lines map_acc maps_acc =
    match lines with
    | [] -> map_acc :: maps_acc |> List.rev
    | line :: lines ->
      if String.is_empty line
      then aux lines [] (map_acc :: maps_acc)
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

let seed_to_location maps seed =
  let rec aux curr maps =
    match maps with
    | [] -> curr
    | map :: maps ->
      let f x =
        if x.source_start <= curr && curr < x.source_start + x.length
        then Some (curr + x.destination_start - x.source_start)
        else None
      in
      let new_curr = map |> List.find_map ~f |> Option.value ~default:curr in
      aux new_curr maps
  in
  aux seed maps
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
