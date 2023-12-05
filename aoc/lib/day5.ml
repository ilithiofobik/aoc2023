open Core

type range =
  { rstart : int
  ; rend : int
  }

type range_mapping =
  { offset : int
  ; dst : range
  ; src : range
  }

type input =
  { seeds : int list
  ; mmaps : range_mapping array array
  }

let lines_to_mmaps lines =
  let cmp a b = Int.compare a.src.rstart b.src.rstart in
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
         | [ dst_start; src_start; len ] ->
           let dst_start = int_of_string dst_start in
           let src_start = int_of_string src_start in
           let len = int_of_string len in
           let dst = { rstart = dst_start; rend = dst_start + len } in
           let src = { rstart = src_start; rend = src_start + len } in
           let offset = dst_start - src_start in
           let map = { offset; dst; src } in
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
      if r_map.src.rstart <= value && value < r_map.src.rend
      then value + r_map.offset
      else if value < r_map.src.rstart
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
    | rstart :: len :: rest -> aux rest ({ rstart; rend = rstart + len } :: acc)
    | _ -> failwith "Invalid input"
  in
  aux seeds []
;;

let map_range m r =
  let a = r.rstart in
  let b = max r.rstart m.src.rstart in
  let c = min r.rend m.src.rend in
  let d = r.rend in
  let before = if a < b then [ { rstart = a; rend = b } ] else []
  and common = if b < c then [ { rstart = b + m.offset; rend = c } ] else []
  and after = if c < d then [ { rstart = c; rend = d } ] else [] in
  before @ after, common
;;

let maps_ranges (ranges : range list) (maps : range_mapping array) =
  let n = maps |> Array.length in
  let rec aux unmapped mapped pos =
    match pos with
    | k when k = n -> mapped @ unmapped
    | k ->
      let pairs = List.map ~f:(map_range maps.(k)) unmapped in
      let nu, nm = List.unzip pairs in
      let new_unmapped = nu |> List.concat in
      let new_mapped = (nm |> List.concat) @ mapped in
      aux new_unmapped new_mapped (k + 1)
  in
  aux ranges [] 0
;;

let ranges_to_min_loc input =
  let ranges = input.seeds |> seeds_to_ranges in
  let mmaps = input.mmaps in
  mmaps
  |> Array.fold ~init:ranges ~f:maps_ranges
  |> List.map ~f:(fun r -> r.rstart)
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
