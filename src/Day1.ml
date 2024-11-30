let textFile = [%blob "text.txt"]
let result = "Hello " ^ textFile |> print_endline
let () = assert (1 + 2 == 3)
