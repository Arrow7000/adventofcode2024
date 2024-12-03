open Angstrom
open Helpers

let textFile = [%blob "Day2.txt"]
let parseSpace = char ' ' >>| fun _ -> ()
let parseLine = sep_by1 parseSpace parseInt
let parseAll = parseLines parseLine
let parsed = runParser parseAll textFile |> Result.get_ok

let isSafe a b =
  let diff = getDiff a b in
  diff >= 1 && diff <= 3

module Part1 = struct
  type state =
    | NotStarted
    | Init of int
    | Increasing of int
    | Decreasing of int
    | Unsafe

  let checkIsSafe ints =
    ints
    |> List.fold_left
         (fun state level ->
           match state with
           | NotStarted -> Init level
           | Init n ->
               if isSafe n level then
                 if n < level then Increasing level else Decreasing level
               else Unsafe
           | Increasing n ->
               if isSafe n level && n < level then Increasing level else Unsafe
           | Decreasing n ->
               if isSafe n level && n > level then Decreasing level else Unsafe
           | Unsafe -> Unsafe)
         NotStarted
end

let solve_part1 () =
  parsed |> List.map Part1.checkIsSafe
  |> List.filter_map (function Part1.Unsafe -> None | _ -> Some ())
  |> List.length |> string_of_int

let () = assert (solve_part1 () = "564")

module Part2 = struct
  let part1IsSafe ints = Part1.checkIsSafe ints <> Part1.Unsafe

  let getDampenedLists ints =
    let len = List.length ints in
    let indexList = List.init len id in
    indexList
    |> List.map (fun indexToRemove ->
           ints |> List.filteri (fun i _ -> i <> indexToRemove))

  let loopUntilSafe (intsList : int list list) =
    let rec loop (listsRemaining : int list list) =
      match listsRemaining with
      | [] -> false
      | head :: rest -> part1IsSafe head || loop rest
    in
    loop intsList

  let checkIsSafe ints =
    part1IsSafe ints || loopUntilSafe (getDampenedLists ints)
end

let solve_part2 () =
  parsed |> List.filter Part2.checkIsSafe |> List.length |> string_of_int

let () = assert (solve_part2 () = "604")
