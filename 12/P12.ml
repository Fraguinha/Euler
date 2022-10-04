let extend_triangle_nums nums =
  match nums with
  | [] -> [ 1 ]
  | nums ->
      let last = List.hd nums in
      let next = List.length nums + 1 in
      (next + last) :: nums

let factors n =
  let rec loop i facts =
    if i > (n |> float_of_int |> sqrt |> floor |> int_of_float) then facts
    else if n mod i = 0 then
      if i = n / i then loop (i + 1) [ i ]
      else loop (i + 1) (i :: (n / i) :: facts)
    else loop (i + 1) facts
  in
  loop 1 []

let rec solution nums =
  let nums = extend_triangle_nums nums in
  let n = List.hd nums in
  let f = factors n in
  let num_factors = List.length f in
  if num_factors > 500 then n else solution nums

let () = solution [] |> string_of_int |> print_endline
