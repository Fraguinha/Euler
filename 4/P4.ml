let int_list_of_int n =
  let rec int_list_of_int' n acc =
    if n < 10 then n :: acc
    else
      let acc = (n mod 10) :: acc in
      let n = n / 10 in
      int_list_of_int' n acc
  in
  int_list_of_int' n []

let is_palindrome n =
  let l = int_list_of_int n in
  l = List.rev l

let solution () =
  let rec solution' a b acc =
    let n = a * b in
    let acc = if is_palindrome n && n > acc then n else acc in
    if b < 1000 then solution' a (b + 1) acc
    else if a < 1000 then solution' (a + 1) 0 acc
    else acc
  in
  solution' 0 0 0

let () = solution () |> string_of_int |> print_endline
