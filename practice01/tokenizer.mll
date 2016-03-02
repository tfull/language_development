{
    open Parser

    exception TokenizeError of string
}

let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']

rule tokenize = parse
    | [' ' '\t' '\n'] { tokenize lexbuf }
    | digit+ as v { INTLI (int_of_string v) }
    | digit+ ['l' 'L'] as v { LONGLI (Int64.of_string (String.sub v 0 (String.length v - 1))) }
    | digit+ "." digit+ ['f' 'F'] as v { FLOATLI (float_of_string (String.sub v 0 (String.length v - 1))) }
    | "{" { LMPAR }
    | "}" { RMPAR }
    | ";" { SCOLON }
    | "=" { EQUAL }
    | "(" { LPAR }
    | ")" { RPAR }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { STAR }
    | "/" { SLASH }
    | "%" { PERCENT }
    | "," { COMMA }
    | "<" { LT }
    | ">" { GT }
    | "<=" { LE }
    | ">=" { GE }
    | "==" { EQ }
    | "True" { BOOLLI true }
    | "False" { BOOLLI false }
    | "return" { RETURN }
    | lower (lower | upper | digit | "_")* as v { LOWLI v }
    | upper (lower | upper | digit | "_") * as v { UPLI v }
    | eof { EOF }
    | _ { raise (TokenizeError "illegal character") }
