let is_prime n =
  let upper_bound = float_of_int n |> sqrt |> floor |> int_of_float in
  let rec is_prime' i =
    if i > upper_bound then true
    else if n mod i = 0 then false
    else is_prime' (i + 1)
  in
  is_prime' 2

let solution n =
  let rec solution' i n =
    if is_prime i then if n = 0 then i else solution' (i + 1) (pred n)
    else solution' (i + 1) n
  in
  solution' 1 n

let () = solution 10001 |> string_of_int |> print_endline
