let solution =
  List.init 1000 (fun x -> x)
  |> List.filter (fun x -> x mod 3 = 0 || x mod 5 = 0)
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
