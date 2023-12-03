open Core

type symbol =
  { row : int
  ; col : int
  ; sym : char
  }

type number =
  { row : int
  ; col : int
  ; len : int
  ; num : int
  }

let neighbours (s : symbol) (n : number) =
  let row_geq = s.row >= n.row - 1 in
  let row_leq = s.row <= n.row + 1 in
  let col_geq = s.col >= n.col - 1 in
  let col_leq = s.col <= n.col + n.len in
  row_geq && row_leq && col_geq && col_leq
;;

let num_to_value syms num =
  if syms |> List.exists ~f:(fun s -> neighbours s num) then Some num.num else None
;;

let sym_to_value nums s =
  match s.sym with
  | '*' ->
    nums
    |> List.filter ~f:(neighbours s)
    |> (function
     | [ num1; num2 ] -> num1.num * num2.num |> Option.some
     | _ -> None)
  | _ -> None
;;

let linei_to_num_sym i line =
  let rec aux cnum cnum_len j nums syms input =
    let new_nums =
      match cnum_len with
      | 0 -> nums
      | _ ->
        let new_num = { row = i; col = j - cnum_len; len = cnum_len; num = cnum } in
        new_num :: nums
    in
    match input with
    | [] -> new_nums, syms
    | c :: cs ->
      (match c with
       | '0' .. '9' as c ->
         let new_cnum = (cnum * 10) + (Char.to_int c - Char.to_int '0') in
         aux new_cnum (cnum_len + 1) (j + 1) nums syms cs
       | _ ->
         let new_syms =
           match c with
           | '.' -> syms
           | _ -> { row = i; col = j; sym = c } :: syms
         in
         aux 0 0 (j + 1) new_nums new_syms cs)
  in
  aux 0 0 0 [] [] line
;;

let task1 nums syms = nums |> List.filter_map ~f:(num_to_value syms) |> Utils.list_sum
let task2 nums syms = syms |> List.filter_map ~f:(sym_to_value nums) |> Utils.list_sum
let lines = In_channel.read_lines "../../../data/day3.txt"
let pairs = lines |> List.map ~f:String.to_list |> List.mapi ~f:linei_to_num_sym
let nums = List.concat_map pairs ~f:fst
let syms = List.concat_map pairs ~f:snd
let result1 = task1 nums syms
let result2 = task2 nums syms
