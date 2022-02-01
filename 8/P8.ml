let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input =
  read_file "P8.txt" |> String.split_on_char '\n' |> List.fold_left ( ^ ) ""

let int_list_of_int n =
  let rec int_list_of_int' n acc =
    if n < 10 then n :: acc
    else
      let acc = (n mod 10) :: acc in
      let n = n / 10 in
      int_list_of_int' n acc
  in
  int_list_of_int' n []

let solution s =
  let rec solution' i acc =
    if i > String.length s - 13 then acc
    else
      let prod =
        String.sub s i 13
        |> int_of_string
        |> int_list_of_int
        |> List.fold_left ( * ) 1
      in
      if prod > acc then solution' (i + 1) prod else solution' (i + 1) acc
  in
  solution' 0 0

let () = solution input |> string_of_int |> print_endline
