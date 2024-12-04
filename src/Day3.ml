open Angstrom
open Helpers

let input = [%blob "Day3.txt"]

let parseMulInstr =
  ( and+ ) (string "mul(" *> parseInt) (string "," *> parseInt <* char ')')
  >>| fun pair -> Some pair

let parseInstrOrAnyChar = parseMulInstr <|> parseAnyChar
let parseMuls = many parseInstrOrAnyChar >>| List.filter_map id

let multiplyAndSum pairs =
  pairs |> List.map (fun (a, b) -> a * b) |> List.fold_left ( + ) 0

let part1Parsed = runParser parseMuls input |> Result.get_ok
let solve_part1 () = multiplyAndSum part1Parsed |> string_of_int
let () = assert (solve_part1 () = "189600467")

type instruction = Mul of int * int | Do | Dont

let parseDo = string "do()" >>| fun _ -> Some Do
let parseDont = string "don't()" >>| fun _ -> Some Dont

let parseInstrOrAny =
  parseMulInstr
  >>| Option.map (fun (a, b) -> Mul (a, b))
  <|> parseDo <|> parseDont <|> parseAnyChar

let parseInstrs = many parseInstrOrAny >>| List.filter_map id
let parsedPart2 = runParser parseInstrs input |> Result.get_ok

type state = Enable | Disable

let runInstructions instructions =
  instructions
  |> List.fold_left
       (fun (state, acc) instr ->
         match instr with
         | Do -> (Enable, acc)
         | Dont -> (Disable, acc)
         | Mul (a, b) -> (
             match state with
             | Enable -> (state, acc + (a * b))
             | Disable -> (state, acc)))
       (Enable, 0)

let solve_part2 () = runInstructions parsedPart2 |> snd |> string_of_int
let () = assert (solve_part2 () = "107069718")
