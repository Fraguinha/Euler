module Int_Set = Set.Make (Int)

let divisors n =
  let rec divisors' n i acc =
    if i >= n then acc
    else if n mod i = 0 then divisors' n (i + 1) (i :: acc)
    else divisors' n (i + 1) acc
  in
  divisors' n 1 []

let cache = Hashtbl.create 997

let amicable_numbers n =
  let rec amicable_numbers' n i acc =
    if i > n then acc
    else
      let sum_of_divisors =
        if Hashtbl.mem cache i then Hashtbl.find cache i
        else
          let sum = List.fold_left ( + ) 0 (divisors i) in
          Hashtbl.add cache i sum;
          sum
      in
      let sum_of_divisors_of_sum_of_divisors =
        if Hashtbl.mem cache sum_of_divisors then
          Hashtbl.find cache sum_of_divisors
        else
          let sum = List.fold_left ( + ) 0 (divisors sum_of_divisors) in
          Hashtbl.add cache sum_of_divisors sum;
          sum
      in
      if sum_of_divisors_of_sum_of_divisors = i && i <> sum_of_divisors then
        amicable_numbers' n (i + 1) (Int_Set.add i acc)
      else amicable_numbers' n (i + 1) acc
  in
  amicable_numbers' n 1 Int_Set.empty

let solution = Int_Set.fold ( + ) (amicable_numbers 10_000) 0
let () = solution |> string_of_int |> print_endline
