open Angstrom
open Helpers

let input = [%blob "Day3.txt"]

let parseMulInstr =
  ( and+ ) (string "mul(" *> parseInt) (string "," *> parseInt <* char ')')
  >>| fun pair -> Some pair

let parseAnyChar = any_char >>| fun _ -> None
let parseInstrOrAnyChar = parseMulInstr <|> parseAnyChar
let parseInstrs = many parseInstrOrAnyChar >>| List.filter_map id

let multiplyAndSum pairs =
  pairs |> List.map (fun (a, b) -> a * b) |> List.fold_left ( + ) 0

let part1Parsed = runParser parseInstrs input |> Result.get_ok
let solve_part1 () = multiplyAndSum part1Parsed |> string_of_int
let () = assert (solve_part1 () = "189600467")
