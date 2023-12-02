open Core

let digits1 = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]

let digits2 =
  ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
  @ digits1

let str_to_digit str =
  if str |> List.mem digits1 ~equal:String.equal then
    str |> int_of_string
  else
    digits2 |> Utils.list_find_index str |> Option.value_exn |> succ

let line_to_num digits line =
  let n = String.length line in
  let pred pos d = line |> String.is_substring_at ~pos ~substring:d in
  let get_digit pos_map =
    let rec aux pos =
      if pos >= n then
        0
      else
        digits |> List.find ~f:(pos_map pos |> pred) |> function
        | Some d -> str_to_digit d
        | None -> aux (pos + 1) in
    aux 0 in

  let pos_map2 pos = n - 1 - pos in
  let digit1 = get_digit Fun.id in
  let digit2 = get_digit pos_map2 in

  (10 * digit1) + digit2

let task digits lines =
  lines |> List.map ~f:(line_to_num digits) |> Utils.list_sum

let lines = In_channel.read_lines "../../../data/day1.txt"
let result1 = task digits1 lines
let result2 = task digits2 lines