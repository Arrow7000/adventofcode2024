open Angstrom
open Helpers

let parseGap = Angstrom.string "   "
let parsePair = lift2 (fun a c -> (a, c)) parseInt (parseGap *> parseInt)
let parseFile = parseLines parsePair
let textFile = [%blob "Day1.txt"]
let sort = List.fast_sort compare
let listSum = List.fold_left ( + ) 0

let parsedLists =
  match runParser parseFile textFile with
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

let () = assert (solve_part1 () = "1189304")

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
let () = assert (solve_part2 () = "24349736")
