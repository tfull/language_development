open Base

let _ =
    let tree = Parser.parse Tokenizer.tokenize (Lexing.from_channel (open_in Sys.argv.(1))) in
    print_string (join "\n" (List.map Syntax.string_of_global tree) ^ "\n")
