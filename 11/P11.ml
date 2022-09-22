let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input =
  read_file "P11.txt"
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ' ')
  |> List.flatten
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string

let solution input =
  let input = Array.of_list input in
  let rec loop init i =
    if i + 4 >= Array.length input then init
    else
      let window = input.(i) * input.(i + 1) * input.(i + 2) * input.(i + 3) in
      let init = if window > init then window else init in
      loop init (i + 1)
  in
  let horizontal = loop 0 0 in
  let rec loop init i =
    if i + 60 >= Array.length input then init
    else
      let window =
        input.(i) * input.(i + 20) * input.(i + 40) * input.(i + 60)
      in
      let init = if window > init then window else init in
      loop init (i + 1)
  in
  let vertical = loop 0 0 in
  let rec loop init i =
    if i + 63 >= Array.length input then init
    else
      let window =
        input.(i) * input.(i + 20 + 1) * input.(i + 40 + 2) * input.(i + 60 + 3)
      in
      let init = if window > init then window else init in
      loop init (i + 1)
  in
  let diagonal1 = loop 0 0 in
  let rec loop init i =
    if i + 57 >= Array.length input then init
    else
      let window =
        input.(i) * input.(i + 20 - 1) * input.(i + 40 - 2) * input.(i + 60 - 3)
      in
      let init = if window > init then window else init in
      loop init (i + 1)
  in
  let diagonal2 = loop 0 0 in
  List.fold_left max 0 [ horizontal; vertical; diagonal1; diagonal2 ]

let () = solution input |> string_of_int |> print_endline
