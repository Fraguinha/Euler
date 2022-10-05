let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input =
  read_file "P67.txt"
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "")
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)

let triangle n =
  let arr = Array.make_matrix n n 0 in
  input |> List.iteri (fun i l -> List.iteri (fun j n -> arr.(i).(j) <- n) l);
  arr

let t = triangle 100
let cache = Hashtbl.create 997

let rec solution t i j acc =
  if i = Array.length t - 1 then (
    let result = acc + t.(i).(j) in
    Hashtbl.add cache (i, j) result;
    result)
  else
    let left =
      if Hashtbl.mem cache (i + 1, j) then Hashtbl.find cache (i + 1, j)
      else solution t (i + 1) j acc
    in
    let right =
      if Hashtbl.mem cache (i + 1, j + 1) then Hashtbl.find cache (i + 1, j + 1)
      else solution t (i + 1) (j + 1) acc
    in
    let result = acc + t.(i).(j) + max left right in
    Hashtbl.add cache (i, j) result;
    result

let () = solution t 0 0 0 |> string_of_int |> print_endline
