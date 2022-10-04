let cache = Hashtbl.create 997

let collatz n =
  let rec collatz' n i acc =
    if Hashtbl.mem cache i then Hashtbl.find cache i @ acc
    else if i = 1 then (
      let result = 1 :: acc in
      Hashtbl.add cache n result;
      result)
    else if i mod 2 = 0 then collatz' n (i / 2) (i :: acc)
    else collatz' n ((3 * i) + 1) (i :: acc)
  in
  collatz' n n []

let solution =
  List.init 1_000_000 (fun i -> i + 1)
  |> List.rev_map (fun i -> (i, collatz i))
  |> List.rev_map (fun t ->
         let i, res = t in
         (i, List.length res))
  |> List.fold_left
       (fun acc t ->
         let res' = snd t in
         let res = snd acc in
         if res' > res then t else acc)
       (0, 0)
  |> fst

let () = solution |> string_of_int |> print_endline
