let is_prime n =
  let upper_bound = float_of_int n |> sqrt |> floor |> int_of_float in
  let rec is_prime' i =
    if i > upper_bound then true
    else if n mod i = 0 then false
    else is_prime' (i + 1)
  in
  is_prime' 2

let solution () =
  let divisors = List.init 20 (fun x -> x + 1) in
  let rec solution' n =
    if List.for_all (fun d -> n mod d = 0) divisors then n else solution' (n + 1)
  in
  solution' 1

let () = solution () |> string_of_int |> print_endline
