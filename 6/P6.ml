let numbers = List.init 100 (fun x -> x + 1)
let sum_of_squares = List.fold_left (fun acc x -> (x * x) + acc) 0 numbers

let square_of_sum =
  let sum = List.fold_left ( + ) 0 numbers in
  sum * sum

let solution = square_of_sum - sum_of_squares
let () = solution |> string_of_int |> print_endline
