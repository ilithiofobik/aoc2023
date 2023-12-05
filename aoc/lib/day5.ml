open Core

type range_mapping =
  { destination_start : int
  ; source_start : int
  ; length : int
  }

type input =
  { seeds : int list
  ; mmaps : range_mapping array array
  }

type range =
  { start : int
  ; length : int
  }

let lines_to_mmaps lines =
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
  | seeds :: _ :: mmaps ->
    let seeds = seeds |> line_to_seed in
    let mmaps = mmaps |> lines_to_mmaps in
    { seeds; mmaps }
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

let seed_to_location maps value = maps |> Array.fold ~init:value ~f:map_value

let seeds_to_ranges seeds =
  let rec aux seeds acc =
    match seeds with
    | [] -> acc
    | start :: length :: rest -> aux rest ({ start; length } :: acc)
    | _ -> failwith "Invalid input"
  in
  aux seeds []
;;

let map_range map r =
  let r_start = r.start in
  let src_start = map.source_start in
  let dst_start = map.destination_start in
  let r_end = r.start + r.length in
  let src_end = map.source_start + map.length in
  let before =
    if r_start < src_start
    then [ { start = r_start; length = src_start - r_start } ]
    else []
  and after =
    if r_end > src_end then [ { start = src_end; length = r_end - src_end } ] else []
  and common =
    if (src_start <= r_start && r_start < src_end) || (src_end < r_end && r_end <= src_end)
    then (
      let offset = dst_start - src_start in
      let nstart = max r_start src_start in
      let nend = min r_end src_end in
      let start = nstart + offset in
      let length = nend - nstart in
      [ { start; length } ])
    else []
  in
  before @ after, common
;;

let maps_ranges ranges maps =
  let n = maps |> Array.length in
  let rec aux unmapped mapped pos =
    match pos with
    | k when k = n -> mapped @ unmapped
    | k ->
      let pairs = List.map ~f:(map_range maps.(k)) unmapped in
      let new_unmapped, new_mapped = List.unzip pairs in
      aux (new_unmapped |> List.concat) (new_mapped |> List.concat) (k + 1)
  in
  aux ranges [] 0
;;

let ranges_to_min_loc input =
  let ranges = input.seeds |> seeds_to_ranges in
  let mmaps = input.mmaps in
  mmaps
  |> Array.fold ~init:ranges ~f:maps_ranges
  |> List.map ~f:(fun r -> r.start)
  |> Utils.list_min
;;

let lines = In_channel.read_lines "../../../data/day5.txt"
let input = lines |> lines_to_input
let result1 = input.seeds |> List.map ~f:(seed_to_location input.mmaps) |> Utils.list_min
let result2 = input |> ranges_to_min_loc

let () =
  Stdio.printf "Result1: %d\n" result1;
  Stdio.printf "Result2: %d\n" result2
;;
