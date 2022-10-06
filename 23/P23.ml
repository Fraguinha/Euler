module Int_Set = Set.Make (Int)

let divisors n =
  let rec divisors' n i acc =
    if i >= n then acc
    else if n mod i = 0 then divisors' n (i + 1) (i :: acc)
    else divisors' n (i + 1) acc
  in
  divisors' n 1 []

let abundant n =
  let rec abundant' n i acc =
    if i >= n then acc
    else
      let sum = List.fold_left ( + ) 0 (divisors i) in
      if sum > i then abundant' n (i + 1) (Int_Set.add i acc)
      else abundant' n (i + 1) acc
  in
  abundant' n 1 Int_Set.empty

let abundant_numbers = abundant 28123

let is_sum_of_abundant n =
  Int_Set.fold
    (fun x acc -> if Int_Set.mem (n - x) abundant_numbers then true else acc)
    abundant_numbers false

let solution =
  List.init 28123 (fun x -> x + 1)
  |> List.filter (fun x -> not (is_sum_of_abundant x))
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
