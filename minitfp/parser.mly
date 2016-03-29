%{
    open Syntax
    exception ParseError of string
    let parse_error s = raise (ParseError (s ^ " " ^ (string_of_int (Parsing.symbol_start ())) ^ " - " ^ (string_of_int (Parsing.symbol_end ()))))
%}

%token <int> INTLI
%token <float> FLOATLI
%token <bool> BOOLLI
%token <string> LVAR UVAR
%token LMPAR RMPAR LPAR RPAR
%token SCOLON COMMA
%token EQUAL
%token IF THEN ELSE
%token PLUS MINUS STAR SLASH PERCENT
%token LT GT LE GE EQ NE
%token AND OR
%token VAR DEF
%token RET
%token EOF

%start parse
%type <Syntax.body> parse

%%

parse:
    | global_states EOF { $1 }
global_states:
    | global_state global_states { $1 :: $2 }
    | { [] }
global_state:
    | VAR LVAR EQUAL exp SCOLON { GVar($2, $4) }
    | DEF LVAR global_state_args EQUAL exp SCOLON { GDefExp($2, $3, $5) }
    | DEF LVAR global_state_args LMPAR states RMPAR { GDefState($2, $3, $5) }
global_state_args:
    | LVAR global_state_args { $1 :: $2 }
    | { [] }
states:
    | state states { $1 :: $2 }
    | { [] }
state:
    | exp SCOLON { SExp $1 }
exp:
    | IF exp THEN exp ELSE exp { EIf($2, $4, $6) }
    | binop1 { $1 }
binop1:
    | binop1 OR binop2 { EIf($1, EBool true, $3) }
    | binop2 { $1 }
binop2:
    | binop2 AND binop3 { EIf($1, $3, EBool false) }
    | binop3 { $1 }
binop3:
    | binop4 NE binop4 { ENe($1, $3) }
    | binop4 EQ binop4 { EEq($1, $3) }
    | binop4 LE binop4 { ELe($1, $3) }
    | binop4 LT binop4 { ELt($1, $3) }
    | binop4 GE binop4 { EGe($1, $3) }
    | binop4 GT binop4 { EGt($1, $3) }
    | binop4 { $1 }
binop4:
    | binop4 STAR binop5 { EStar($1, $3) }
    | binop4 SLASH binop5 { ESlash($1, $3) }
    | binop4 PERCENT binop5 { EPercent($1, $3) }
    | binop5 { $1 }
binop5:
    | binop5 PLUS singleop { EPlus($1, $3) }
    | binop5 MINUS singleop { EMinus($1, $3) }
    | singleop { $1 }
singleop:
    | PLUS app { EPositive $2 }
    | MINUS app { ENegative $2 }
    | app { $1 }
app:
    | app atom { EApp($1, $2) }
    | atom { $1 }
atom:
    | LVAR { EVar $1 }
    | INTLI { EInt $1 }
    | FLOATLI { EFloat $1 }
    | BOOLLI { EBool $1 }
    | LPAR exp RPAR { $2 }
