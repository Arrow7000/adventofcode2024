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

let parsedLists =
  match Angstrom.parse_string ~consume:Consume.All parseFile textFile with
  | Ok listOfPairs ->
      let as_, bs = listOfPairs |> List.split in
      (as_, bs)
  | Error e -> failwith e

let firstList, sndList = parsedLists

let solve_part1 () =
  let sortedAs = firstList |> sort in
  let sortedBs = sndList |> sort in
  let merged = List.combine sortedAs sortedBs in
  let diffs = List.map (fun (a, b) -> getDiff a b) merged in
  let diffSum = listSum diffs in
  diffSum |> string_of_int

let _part1_assert = assert (solve_part1 () = "1189304")

(* Part 2 *)

module IntMap = Map.Make (Int)

let mapOfSndList =
  sndList
  |> List.fold_left
       (fun map locationId ->
         match IntMap.find_opt locationId map with
         | Some count -> IntMap.add locationId (count + 1) map
         | None -> IntMap.add locationId 1 map)
       IntMap.empty

let getCountForFirstCol locationId =
  IntMap.find_opt locationId mapOfSndList |> Option.value ~default:0

let firstListCounts =
  firstList
  |> List.map (fun locId -> locId * getCountForFirstCol locId)
  |> listSum

let solve_part2 () = firstListCounts |> string_of_int
