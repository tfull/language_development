open Base

let _ =
    let tree = Parser.parse Tokenizer.tokenize (Lexing.from_channel (open_in Sys.argv.(1))) in
    print_string ("[" ^ join "; " (List.map Syntax.string_of_funstate tree) ^ "]\n\n");
    print_string (join "\n\n" (List.map KNormal.debug_of_funstate (KNormal.check_functions tree)) ^ "\n")
