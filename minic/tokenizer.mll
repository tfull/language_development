{
    open Parser

    exception TokenizeError of string
}

let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let top = lower | upper | '_'

rule tokenize = parse
    | [' ' '\t' '\n'] { tokenize lexbuf }
    | digit+ as v { INTLI (int_of_string v) }
    | digit+ "." digit+ as v { FLOATLI (float_of_string v) }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { STAR }
    | "/" { SLASH }
    | "%" { PERCENT }
    | "(" { LPAR }
    | ")" { RPAR }
    | "{" { LMPAR }
    | "}" { RMPAR }
    | ";" { SCOLON }
    | "," { COMMA }
    | "=" { EQUAL }
    | "==" { EQ }
    | "!=" { NE }
    | "<" { LT }
    | ">" { GT }
    | ">=" { LE }
    | "<=" { GE }
    | "||" { OR }
    | "&&" { AND }
    | "int" { INT }
    | "float" { FLOAT }
    | "bool" { BOOL }
    | "true" { BOOLLI true }
    | "false" { BOOLLI false }
    | "return" { RETURN }
    | top (top | digit)* as v { VAR v }
    | eof { EOF }
    | _ { raise (TokenizeError "illegal character") }
