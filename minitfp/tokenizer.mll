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
    | digit+ "." digit+ ['f' 'F'] as v { FLOATLI (float_of_string (String.sub v 0 (String.length v - 1))) }
    | "{" { LMPAR }
    | "}" { RMPAR }
    | "(" { LPAR }
    | ")" { RPAR }
    | ";" { SCOLON }
    | "," { COMMA }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { STAR }
    | "/" { SLASH }
    | "%" { PERCENT }
    | "=" { EQUAL }
    | "<" { LT }
    | ">" { GT }
    | "<=" { LE }
    | ">=" { GE }
    | "==" { EQ }
    | "def" { DEF }
    | "var" { VAR }
    | "ret" { RET }
    | "True" { BOOLLI true }
    | "False" { BOOLLI false }
    | eof { EOF }
    | (lower | "_") (lower | upper | digit | "_")* as v { LVAR v }
    | upper (lower | upper | digit | "_")* as v { UVAR v }
    | _ { raise (TokenizeError "illegal character") }
