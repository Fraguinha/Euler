let n = 600_851_475_143

let is_prime n =
  let upper_bound = float_of_int n |> sqrt |> floor |> int_of_float in
  let rec is_prime' i =
    if i > upper_bound then true
    else if n mod i = 0 then false
    else is_prime' (i + 1)
  in
  is_prime' 2

let solution n =
  let upper_bound = float_of_int n |> sqrt |> floor |> int_of_float in
  let rec find_biggest_prime_factor' i acc =
    if i > upper_bound then acc
    else
      let r = n mod i in
      if is_prime i && r = 0 then
        let q = n / i in
        if is_prime q then q else find_biggest_prime_factor' (i + 1) i
      else find_biggest_prime_factor' (i + 1) acc
  in
  find_biggest_prime_factor' 1 1

let () = solution n |> string_of_int |> print_endline
