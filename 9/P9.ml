let solution () =
  let rec solution' a b c =
    if a + b + c = 1000 && (a * a) + (b * b) = c * c then a * b * c
    else if a + b + (c + 1) <= 1000 then solution' a b (c + 1)
    else if a + (b + 1) + (b + 2) <= 1000 then solution' a (b + 1) (b + 2)
    else if a + 1 + (a + 2) + (a + 3) <= 1000 then
      solution' (a + 1) (a + 2) (a + 3)
    else 0
  in
  solution' 1 2 3

let () = solution () |> string_of_int |> print_endline
