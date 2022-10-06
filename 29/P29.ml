module Big_int_Set = Set.Make (struct
  type t = Big_int_Z.big_int

  let compare = Big_int_Z.compare_big_int
end)

let numbers = List.init 99 (fun i -> Big_int_Z.big_int_of_int (i + 2))

let set =
  List.fold_left
    (fun set a ->
      List.fold_left
        (fun set b ->
          Big_int_Set.add (Big_int_Z.power_big_int_positive_big_int a b) set)
        set numbers)
    Big_int_Set.empty numbers

let solution = Big_int_Set.cardinal set
let () = solution |> string_of_int |> print_endline
