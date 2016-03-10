let rec join s = function
    | [] -> ""
    | [x] -> x
    | x :: xs -> x ^ s ^ join s xs
