let rec factorial acc n =
  if n = 0 then Big_int_Z.string_of_big_int acc
  else
    let big_n = Big_int_Z.big_int_of_int n in
    factorial (Big_int_Z.mult_big_int acc big_n) (n - 1)

let fact_100 = factorial Big_int_Z.unit_big_int 100

let solution =
  String.fold_left (fun acc c -> acc + int_of_char c - 48) 0 fact_100

let () = solution |> string_of_int |> print_endline
