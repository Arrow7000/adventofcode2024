open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false
let parseNum = Angstrom.take_while1 is_digit >>| int_of_string
let parseGap = Angstrom.string "   "
let parsePair = lift2 (fun a c -> (a, c)) parseNum (parseGap *> parseNum)
let parseFile = many (parsePair <* Angstrom.end_of_line)
let textFile = [%blob "Day1.txt"]
let sort = List.fast_sort compare
let getDiff a b = a - b |> abs
let listSum = List.fold_left ( + ) 0

let solve_part1 () =
  match Angstrom.parse_string ~consume:Consume.All parseFile textFile with
  | Ok listOfPairs ->
      let as_, bs = listOfPairs |> List.split in
      let sortedAs = as_ |> sort in
      let sortedBs = bs |> sort in
      let merged = List.combine sortedAs sortedBs in
      let diffs = List.map (fun (a, b) -> getDiff a b) merged in
      let diffSum = listSum diffs in
      diffSum |> string_of_int
  | Error e -> failwith e

let () = assert (solve_part1 () = "1189304")
