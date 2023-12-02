open Core

let digits1 = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]

let digits2 =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ] @ digits1
;;

let str_to_digit str =
  if str |> List.mem digits1 ~equal:String.equal
  then str |> int_of_string
  else (
    let f _ s = String.equal s str in
    digits2 |> List.findi ~f |> Option.value_exn |> fst |> succ)
;;

let line_to_num digits line =
  let n = String.length line in
  let get_digit pred =
    let rec aux pos =
      if pos >= n
      then 0
      else
        digits
        |> List.find ~f:(pred pos)
        |> function
        | Some d -> str_to_digit d
        | None -> aux (pos + 1)
    in
    aux 0
  in
  let pred1 pos d = line |> String.is_substring_at ~pos ~substring:d in
  let pred2 pos d = pred1 (n - 1 - pos) d in
  let digit1 = get_digit pred1 in
  let digit2 = get_digit pred2 in
  (10 * digit1) + digit2
;;

let task digits lines = lines |> List.map ~f:(line_to_num digits) |> Utils.list_sum
let lines = In_channel.read_lines "../../../data/day1.txt"
let result1 = task digits1 lines
let result2 = task digits2 lines
