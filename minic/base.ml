let rec join s = function
    | [] -> ""
    | [x] -> x
    | x :: xs -> x ^ s ^ join s xs

let string_of_bool b = if b then "true" else "false"
