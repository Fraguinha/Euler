let rec fib n a b acc =
  let a = Big_int_Z.add_big_int a b in
  let b = Big_int_Z.sub_big_int a b in
  if String.length (Big_int_Z.string_of_big_int a) >= n then acc
  else fib n a b (acc + 1)

let solution = fib 1000 Big_int_Z.unit_big_int Big_int_Z.unit_big_int 3
let () = solution |> string_of_int |> print_endline
