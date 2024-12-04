open Angstrom
open Helpers

let input = [%blob "Day4.txt"]
let parseXmas = string "XMAS"
let parseSamx = string "SAMX"
let parseBoth = parseXmas <|> parseSamx >>| fun _ -> Some ()
let parseSeq = many (parseBoth <|> parseAnyChar)
let combineCharsList chars = String.of_seq (List.to_seq chars)
let combineStrs = String.concat "\n"
let inputRows = input |> String.split_on_char '\n'

(* let () = print_endline (String.concat "\n" inputRows) *)
let inputRowsChars = inputRows |> List.map (String.to_seq >> List.of_seq)
let inputColsChars = transpose inputRowsChars
let inputCols = List.map combineCharsList inputColsChars
(* let () = print_endline (String.concat "\n" inputCols) *)

(** Take the first item of the list and append it to the whole list *)
let rotateBackward list =
  match list with head :: rest -> rest @ [ head ] | [] -> []

(** Take the last item of the list and cons it to the whole list *)
let rotateForward list = list |> List.rev |> rotateBackward |> List.rev

let rec repeatFunc n f input =
  match n with 0 -> input | _ -> repeatFunc (n - 1) f (f input)

(* uuuhh yeah that's not how diagonal lines work. diagonal lines will always have one item in the first list, 2 in the second, 3 in the third, and so on, up to a maximum number in the middle lists, before declining back down to 1 again. that's what the output structure should look like, which my diagonalisation functions definitely don't do. *)
let diagonaliseForward listOfLists =
  listOfLists |> List.mapi (fun i list -> repeatFunc i rotateForward list)

let diagonaliseBackward listOfLists =
  listOfLists |> List.mapi (fun i list -> repeatFunc i rotateBackward list)

let parseList inputList =
  (* inputList |> List.map (runParser parseSeq >> Result.get_ok) *)
  runParser (parseLines parseSeq) inputList |> Result.get_ok

let rowResults = parseList (combineStrs inputRows)
let colResults = parseList (combineStrs inputCols)

let diagForwardResults =
  inputRowsChars |> diagonaliseForward |> transpose |> List.map combineCharsList
  |> combineStrs |> parseList

let diagBackwardResults =
  inputRowsChars |> diagonaliseBackward |> transpose
  |> List.map combineCharsList |> combineStrs |> parseList

let solve_part1 () =
  rowResults @ colResults @ diagForwardResults @ diagBackwardResults
  |> List.flatten |> List.filter_map id |> List.length |> string_of_int
