open Core

let digits1 =  [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ];;

let digits2 = 
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ] @ digits1;;

let str_to_digit str =
  if List.mem digits1 str ~equal:String.equal then 
    str |> int_of_string
  else
    digits2 |> Utils.list_find_index str |> Option.value_exn |> succ;;

let line_to_num digits line =
  let get_digit s len trans pred = 
    let rec aux s len = 
      match len with 
      | 0 -> 0 
      | n -> 
        digits 
        |> List.find ~f:(pred s)
        |> function
        | Some d -> str_to_digit d
        | None -> 
          let new_len = n - 1 in
          let new_str = trans s new_len in
          aux new_str new_len in
    aux s len in

  let line_len = String.length line in 
  let first_digit = get_digit line line_len String.suffix (fun s d -> String.is_prefix s ~prefix:d) in
  let second_digit = get_digit line line_len String.prefix (fun s d -> String.is_suffix s ~suffix:d) in
  
  10 * first_digit + second_digit
;;

let task digits lines =
  List.map lines ~f:(line_to_num digits)
  |> Utils.list_sum;;
let lines  = Utils.file_to_lines ("../../../data/day1.txt");;
let result1 = task digits1 lines;;
let result2 = task digits2 lines;;

