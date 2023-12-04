open Core

let string_to_nums str =
  str
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> not (String.is_empty x))
  |> List.map ~f:int_of_string
;;

let line_to_value line =
  let pair =
    let splitted = line |> String.split ~on:':' in
    List.nth_exn splitted 1 |> String.split ~on:'|' |> List.map ~f:string_to_nums
  in
  match pair with
  | [ winning; my ] ->
    let f x = List.mem winning x ~equal:( = ) in
    my |> List.count ~f
  | _ -> failwith "Invalid input"
;;

let safe_pow num =
  match num with
  | 0 -> 0
  | n -> Int.pow 2 (n - 1)
;;

let scratch_count values =
  let counts = Array.init (List.length values) ~f:(const 1) in
  let f i v =
    let c = i |> Array.get counts in
    List.init v ~f:(( + ) (i + 1)) |> List.iter ~f:(fun x -> counts.(x) <- counts.(x) + c)
  in
  values |> List.iteri ~f;
  counts |> Utils.array_sum
;;

let lines = In_channel.read_lines "../../../data/day4.txt"
let values = lines |> List.map ~f:line_to_value
let result1 = values |> List.map ~f:safe_pow |> Utils.list_sum
let result2 = scratch_count values
