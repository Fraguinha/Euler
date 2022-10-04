let translate_20 n =
  match n with
  | 0 -> ""
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | 10 -> "ten"
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | _ -> raise (Invalid_argument "n must be less than 20")

let translate_100 n =
  match n with
  | i when i < 20 -> translate_20 i
  | i when i > 19 && i < 30 -> "twenty" ^ translate_20 (n mod 10)
  | i when i > 29 && i < 40 -> "thirty" ^ translate_20 (n mod 10)
  | i when i > 39 && i < 50 -> "forty" ^ translate_20 (n mod 10)
  | i when i > 49 && i < 60 -> "fifty" ^ translate_20 (n mod 10)
  | i when i > 59 && i < 70 -> "sixty" ^ translate_20 (n mod 10)
  | i when i > 69 && i < 80 -> "seventy" ^ translate_20 (n mod 10)
  | i when i > 79 && i < 90 -> "eighty" ^ translate_20 (n mod 10)
  | i when i > 89 && i < 100 -> "ninety" ^ translate_20 (n mod 10)
  | _ -> raise (Invalid_argument "n must be less than 100")

let translate n =
  if n < 20 then translate_20 n
  else if n >= 100 then
    let hundreds = n / 100 in
    if hundreds = 10 then "onethousand"
    else
      let tens = n mod 100 in
      translate_20 hundreds
      ^ "hundred"
      ^ if tens > 0 then "and" ^ translate_100 tens else ""
  else translate_100 n

let solution =
  List.init 1000 (fun i -> i + 1)
  |> List.map translate
  |> List.map String.length
  |> List.fold_left ( + ) 0

let () = solution |> string_of_int |> print_endline
