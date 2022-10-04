let n = Big_int_Z.power (Big_int_Z.big_int_of_string "2") 1000
let s = Big_int_Z.string_of_big_int n
let solution = String.fold_left (fun acc c -> acc + int_of_char c - 48) 0 s
let () = solution |> string_of_int |> print_endline
