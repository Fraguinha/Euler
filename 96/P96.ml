let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch - 1) in
  close_in ch;
  s

let group_by_puzzle l =
  let rec loop n puzzle puzzles l =
    match l with
    | [] -> puzzles
    | h :: t -> (
        match h with
        | [ _; _; _; _; _; _; _; _; _ ] as line ->
            let puzzle = puzzle @ line in
            if n = 1 then
              let puzzles = puzzles @ [ puzzle ] in
              loop 9 [] puzzles t
            else loop (n - 1) puzzle puzzles t
        | _ -> raise (Invalid_argument "Malformed list of lines"))
  in
  loop 9 [] [] l

let puzzles =
  read_file "P96.txt"
  |> String.split_on_char '\n'
  |> List.filter (fun s -> not (String.starts_with ~prefix:"Grid" s))
  |> List.map (fun s -> List.init (String.length s) (fun i -> String.sub s i 1))
  |> List.map (List.map int_of_string)
  |> group_by_puzzle
  |> List.map Sudoku.make

let solution =
  List.mapi
    (fun i p ->
      print_string "Solving puzzle: ";
      print_int (i + 1);
      print_newline ();
      let s = Sudoku.solve p in
      Sudoku.print s;
      s)
    puzzles
  |> List.map Sudoku.top_left
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
