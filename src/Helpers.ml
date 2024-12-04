open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false
let parseInt = take_while1 is_digit >>| int_of_string
let parseAnyChar = any_char >>| fun _ -> None

(* let parseLines parseLine = many (parseLine <* end_of_line) *)
let parseLines parseLine =
  sep_by1 end_of_line parseLine
  <* (option () end_of_line >>| fun _ -> ())
  <* end_of_input

let runParser parser input = parse_string ~consume:Consume.All parser input
let getDiff a b = a - b |> abs
let id x = x

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

(* let transpose m =
   if m = [] then []
   else List.(fold_right (map2 cons) m @@ map (fun _ -> []) (hd m)) *)

let ( >> ) f g x = g (f x)
