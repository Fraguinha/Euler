let is_prime n =
  let upper_bound = float_of_int n |> sqrt |> floor |> int_of_float in
  let rec is_prime' i =
    if i > upper_bound then true
    else if n mod i = 0 then false
    else is_prime' (i + 1)
  in
  is_prime' 2

let solution =
  List.init 2_000_000 (fun x -> x)
  |> List.filter is_prime
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
