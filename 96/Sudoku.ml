type el = Tile.t
type t = { board : el list; decisions : el list; missing : el list }

let print t =
  List.iter
    (fun el ->
      let line, column, _ = Tile.pos el in
      if line > 1 && column = 1 then (
        print_newline ();
        print_int (Tile.value el);
        print_string "  ")
      else (
        print_int (Tile.value el);
        print_string "  "))
    t.board;
  print_newline ()

let numbers = List.init 9 (fun x -> x + 1)

let valid t x n =
  let line, column, block = Tile.pos x in
  List.for_all
    (fun el ->
      let l, c, b = Tile.pos el in
      let v = Tile.value el in
      not (n = v && (line = l || column = c || block = b)))
    t.board

let backtrack t =
  let missing =
    List.filter (fun el -> Tile.value el = 0) t.board
    |> List.sort (fun a b ->
           let va = List.filter (fun i -> valid t a i) numbers in
           let vb = List.filter (fun i -> valid t b i) numbers in
           List.length va - List.length vb)
  in
  let rec backtrack' t =
    if List.length t.missing = 0 then t
    else
      try
        let x = List.hd t.missing in
        let v = List.find (fun i -> valid t x i) numbers in
        let x = Tile.set x v in
        let board =
          List.map
            (fun el -> if Tile.pos el = Tile.pos x then x else el)
            t.board
        in
        let decisions = x :: t.decisions in
        let missing = List.tl t.missing in
        backtrack' { board; decisions; missing }
      with Not_found ->
        let x = List.hd t.decisions in
        let v = Tile.value x in
        let v = List.find (fun i -> i > v && valid t x i) numbers in
        let x = Tile.set x v in
        let board =
          List.map
            (fun el -> if Tile.pos el = Tile.pos x then x else el)
            t.board
        in
        let decisions = x :: List.tl t.decisions in
        let t = { t with board; decisions } in
        backtrack' t
  in
  backtrack' { t with missing }

let solve t = backtrack t

let top_left t =
  match t.board with
  | e1 :: e2 :: e3 :: _ ->
      (Tile.value e1 * 100) + (Tile.value e2 * 10) + Tile.value e3
  | _ -> assert false

let make l =
  if List.length l <> 9 * 9 then raise (Invalid_argument "Invalid Sudoku Size")
  else
    let board =
      List.mapi
        (fun i x ->
          let line = (i / 9) + 1 in
          let column = (i mod 9) + 1 in
          let block =
            match (line, column) with
            | l, c when l >= 1 && l <= 3 && c >= 1 && c <= 3 -> 1
            | l, c when l >= 1 && l <= 3 && c >= 4 && c <= 6 -> 2
            | l, c when l >= 1 && l <= 3 && c >= 7 && c <= 9 -> 3
            | l, c when l >= 4 && l <= 6 && c >= 1 && c <= 3 -> 4
            | l, c when l >= 4 && l <= 6 && c >= 4 && c <= 6 -> 5
            | l, c when l >= 4 && l <= 6 && c >= 7 && c <= 9 -> 6
            | l, c when l >= 7 && l <= 9 && c >= 1 && c <= 3 -> 7
            | l, c when l >= 7 && l <= 9 && c >= 4 && c <= 6 -> 8
            | l, c when l >= 7 && l <= 9 && c >= 7 && c <= 9 -> 9
            | _ -> assert false
          in
          Tile.make (line, column, block) x (x <> 0))
        l
    in
    let decisions = [] in
    let missing = List.filter (fun el -> Tile.value el = 0) board in
    { board; decisions; missing }
