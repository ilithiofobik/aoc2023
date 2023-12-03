open Core

let neighbours s_row s_col n_row n_col len =
  let row_geq = s_row >= n_row - 1 in
  let row_leq = s_row <= n_row + 1 in
  let col_geq = s_col >= n_col - 1 in
  let col_leq = s_col <= n_col + len in
  row_geq && row_leq && col_geq && col_leq
;;

let num_to_value syms (a, b, len, num) =
  if syms |> List.exists ~f:(fun (i, j, _) -> neighbours i j a b len) then num else 0
;;

let sym_to_value nums (a, b, sym) =
  match sym with
  | '*' ->
    nums
    |> List.filter ~f:(fun (i, j, len, _) -> neighbours a b i j len)
    |> List.map ~f:(fun (_, _, _, num) -> num)
    |> (function
     | [ num1; num2 ] -> num1 * num2
     | _ -> 0)
  | _ -> 0
;;

let linei_to_num_sym i line =
  let rec aux cnum cnum_len j nums syms input =
    match input with
    | [] ->
      let new_nums =
        if cnum_len = 0 then nums else (i, j - cnum_len, cnum_len, cnum) :: nums
      in
      new_nums, syms
    | c :: cs ->
      (match c with
       | '0' .. '9' as c ->
         let new_cnum = (cnum * 10) + (Char.to_int c - Char.to_int '0') in
         aux new_cnum (cnum_len + 1) (j + 1) nums syms cs
       | _ ->
         let new_syms =
           match c with
           | '.' -> syms
           | _ -> (i, j, c) :: syms
         in
         let new_nums =
           match cnum_len with
           | 0 -> nums
           | _ -> (i, j - cnum_len, cnum_len, cnum) :: nums
         in
         aux 0 0 (j + 1) new_nums new_syms cs)
  in
  aux 0 0 0 [] [] line
;;

let task1 nums syms = nums |> List.map ~f:(num_to_value syms) |> Utils.list_sum
let task2 nums syms = syms |> List.map ~f:(sym_to_value nums) |> Utils.list_sum
let lines = In_channel.read_lines "../../../data/day3.txt"
let pairs = lines |> List.map ~f:String.to_list |> List.mapi ~f:linei_to_num_sym
let nums = List.concat_map pairs ~f:fst
let syms = List.concat_map pairs ~f:snd
let result1 = task1 nums syms
let result2 = task2 nums syms
