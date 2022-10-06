let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input =
  read_file "P22.txt"
  |> String.split_on_char ','
  |> List.map (fun s -> String.sub s 1 (String.length s - 1))
  |> List.map (fun s -> String.sub s 0 (String.length s - 1))

let solution =
  input
  |> List.fast_sort String.compare
  |> List.mapi (fun i s ->
         (i + 1) * String.fold_left (fun acc c -> acc + (Char.code c - 64)) 0 s)
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
