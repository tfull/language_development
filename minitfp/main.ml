let _ =
    let tree = Parser.parse Tokenizer.tokenize (Lexing.from_channel (open_in Sys.argv.(1))) in
    print_string (Syntax.string_of_body tree ^ "\n")
