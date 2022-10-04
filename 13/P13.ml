let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input =
  read_file "P13.txt"
  |> String.split_on_char '\n'
  |> List.filter (fun x -> x <> "")

let solution =
  input
  |> List.map Big_int_Z.big_int_of_string
  |> List.fold_left Big_int_Z.add_big_int Big_int_Z.zero_big_int
  |> Big_int_Z.string_of_big_int
  |> fun s -> String.sub s 0 10

let () = solution |> print_endline
