let cache = Hashtbl.create 199

let rec options down right =
  if down = 0 then (
    let result = 1 in
    Hashtbl.add cache (down, right) result;
    result)
  else if right = 0 then (
    let result = 1 in
    Hashtbl.add cache (down, right) result;
    result)
  else if Hashtbl.mem cache (down, right) then Hashtbl.find cache (down, right)
  else
    let result = options (down - 1) right + options down (right - 1) in
    Hashtbl.add cache (down, right) result;
    result

let () = options 20 20 |> string_of_int |> print_endline
