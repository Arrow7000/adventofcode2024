open Angstrom
open Helpers

let textFile = [%blob "Day2.txt"]
let parseSpace = char ' ' >>| fun _ -> ()
let parseLine = sep_by1 parseSpace parseNum
let parseAll = parseLines parseLine
let parsed = runParser parseAll textFile |> Result.get_ok

type state =
  | NotStarted
  | Init of int
  | Increasing of int
  | Decreasing of int
  | Unsafe

let isSafe a b =
  let diff = getDiff a b in
  diff >= 1 && diff <= 3

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

let solve_part1 () =
  parsed |> List.map checkIsSafe
  |> List.filter_map (function Unsafe -> None | _ -> Some ())
  |> List.length |> string_of_int

let () = assert (solve_part1 () = "564")
