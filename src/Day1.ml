let textFile = [%blob "text.txt"]
let result = "Hello " ^ textFile |> print_endline
