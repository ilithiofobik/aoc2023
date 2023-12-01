let digits1 = 
  [ 
    "1"; 
    "2" ; 
    "3" ;
    "4" ;
    "5" ;
    "6" ;
    "7" ;
    "8" ;
    "9" 
];;

let digits2 = 
  [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine"
  ] @ digits1;;

let str_to_digit str =
  match str with
  | "1" | "one" -> 1
  | "2" | "two" -> 2
  | "3" | "three" -> 3
  | "4" | "four" -> 4
  | "5" | "five" -> 5
  | "6" | "six" -> 6
  | "7" | "seven" -> 7
  | "8" | "eight" -> 8
  | "9" | "nine" -> 9
  | _ -> failwith "Invalid digit";;

let line_to_num digits line =
  let rec get_first_digit s len =
    match len with 
    | 0 -> 0 
    | n -> 
      let new_len = n - 1 in
      let new_digit = 
        digits 
        |> List.find_opt (fun d -> String.starts_with ~prefix:d s) 
        |> Option.map str_to_digit in 
      match new_digit with
      | Some d -> d
      | None -> get_first_digit (String.sub s 1 new_len) new_len in

  let rec get_last_digit s len =
    match len with 
    | 0 -> 0 
    | n -> 
      let new_len = n - 1 in
      let new_digit = 
        digits 
        |> List.find_opt (fun d -> String.ends_with ~suffix:d s) 
        |> Option.map str_to_digit in 
      match new_digit with
      | Some d -> d
      | None -> get_last_digit (String.sub s 0 new_len) new_len in

    let line_len = String.length line in 
    10 * (get_first_digit line line_len) + (get_last_digit line line_len)
;;

let task lines digits =
  lines
  |> List.map (line_to_num digits)
  |> List.fold_left (+) 0;;

  let lines  = Utils.file_to_lines ("input/day1.txt") in
  let result1 = task lines digits1 in
  let result2 = task lines digits2 in
    Printf.printf "Task1: %d\n" result1;
    Printf.printf "Task2: %d\n" result2;
    assert (result1 = 55488);
    assert (result2 = 55614);