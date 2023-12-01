let digits1 =  [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ];;

let digits2 = 
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ] @ digits1;;

let str_to_digit str =
  if digits1 |> List.mem str then 
    str |> int_of_string
  else
    digits2 |> Utils.list_find_index str |> Option.get |> succ;;

let line_to_num digits line =
  let get_digit s len pos pred = 
    let rec aux s len = 
      match len with 
      | 0 -> 0 
      | n -> 
        digits 
        |> List.find_opt (pred s)
        |> function
        | Some d -> str_to_digit d
        | None -> 
          let new_len = n - 1 in
          let new_str = String.sub s pos new_len in
          aux new_str new_len in
    aux s len in

  let line_len = String.length line in 
  let first_digit = get_digit line line_len 1 (fun s d -> String.starts_with ~prefix:d s) in
  let second_digit = get_digit line line_len 0 (fun s d -> String.ends_with ~suffix:d s) in
  
  10 * first_digit + second_digit
;;

let task lines digits =
  lines
  |> List.map (line_to_num digits)
  |> Utils.list_sum;;

let lines  = Utils.file_to_lines ("input/day1.txt") in
let result1 = task lines digits1 in
let result2 = task lines digits2 in
  Printf.printf "Task1: %d\n" result1;
  Printf.printf "Task2: %d\n" result2;
  assert (result1 = 55488);
  assert (result2 = 55614);