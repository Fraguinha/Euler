let rec fib n a b acc =
  let a = a + b in
  let b = a - b in
  if a <= n then
    let acc = a :: acc in
    fib n a b acc
  else acc

let solution =
  fib 4_000_000 2 1 [ 2; 1 ]
  |> List.filter (fun x -> x mod 2 = 0)
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
