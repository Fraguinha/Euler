type weekday =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  | Unknown

let next_weekday weekday =
  match weekday with
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Saturday
  | Saturday -> Sunday
  | Sunday -> Monday
  | Unknown -> Unknown

type month =
  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

let next_month month =
  match month with
  | January -> February
  | February -> March
  | March -> April
  | April -> May
  | May -> June
  | June -> July
  | July -> August
  | August -> September
  | September -> October
  | October -> November
  | November -> December
  | December -> January

type date = { year : int; month : month; day : int; weekday : weekday }

let compare_date a b =
  if a.year < b.year then -1
  else if a.year > b.year then 1
  else if a.month < b.month then -1
  else if a.month > b.month then 1
  else if a.day < b.day then -1
  else if a.day > b.day then 1
  else 0

let days_in_month year month =
  match month with
  | January | March | May | July | August | October | December -> 31
  | April | June | September | November -> 30
  | February ->
      if year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) then 29
      else 28

let next_date date =
  let days = days_in_month date.year date.month in
  let day' = if date.day < days then date.day + 1 else 1 in
  let month' = if date.day < days then date.month else next_month date.month in
  let year' =
    if date.day = days && date.month = December then date.year + 1
    else date.year
  in
  let weekday' = next_weekday date.weekday in
  { year = year'; month = month'; day = day'; weekday = weekday' }

let reference = { year = 1900; month = January; day = 1; weekday = Monday }
let start = { year = 1901; month = January; day = 1; weekday = Unknown }
let finish = { year = 2000; month = December; day = 31; weekday = Unknown }

let rec solution acc date =
  if compare_date date finish > 0 then acc
  else if compare_date date start >= 0 && date.weekday = Sunday && date.day = 1
  then solution (acc + 1) (next_date date)
  else solution acc (next_date date)

let () = solution 0 reference |> string_of_int |> print_endline
