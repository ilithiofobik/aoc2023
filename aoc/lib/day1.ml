open Core

let digits1 = Array.init 9 ~f:(fun i -> i + 1 |> Int.to_string)

let digits2 =
  Array.append
    [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
    digits1
;;

let str_to_digit str =
  let f i s = if String.equal s str then (i mod 9) + 1 |> Option.some else None in
  digits2 |> Array.find_mapi ~f |> Option.value_exn
;;

let line_to_num digits line =
  let n = String.length line in
  let pred pos d = line |> String.is_substring_at ~pos ~substring:d in
  let f pos = digits |> Array.find ~f:(pred pos) |> Option.map ~f:str_to_digit in
  let get_digit indices = indices |> List.find_map ~f |> Option.value_exn in
  let indices1 = List.init n ~f:Fun.id in
  let indices2 = indices1 |> List.rev in
  let digit1 = get_digit indices1 in
  let digit2 = get_digit indices2 in
  (10 * digit1) + digit2
;;

let task digits lines = lines |> List.map ~f:(line_to_num digits) |> Utils.list_sum
let lines = In_channel.read_lines "../../../data/day1.txt"
let result1 = task digits1 lines
let result2 = task digits2 lines
