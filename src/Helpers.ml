open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false
let parseNum = take_while1 is_digit >>| int_of_string

(* let parseLines parseLine = many (parseLine <* end_of_line) *)
let parseLines parseLine =
  sep_by1 end_of_line parseLine
  <* (option () end_of_line >>| fun _ -> ())
  <* end_of_input

let runParser parser input = parse_string ~consume:Consume.All parser input
let getDiff a b = a - b |> abs
let id x = x
