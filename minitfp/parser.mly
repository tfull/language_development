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
%token SCOLON COMMA EQUAL
%token PLUS MINUS STAR SLASH PERCENT
%token LT GT LE GE EQ
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
    | global_state { [$1] }
global_state:
    | VAR LVAR EQUAL exp SCOLON { GVar($2, $4) }
    | DEF LVAR EQUAL exp SCOLON { GDefExp($2, [], $4) }
    | DEF LVAR global_state_args EQUAL exp SCOLON { GDefExp($2, $3, $5) }
    | DEF LVAR LMPAR states RMPAR { GDefState($2, [], $4) }
    | DEF LVAR global_state_args LMPAR states RMPAR { GDefState($2, $3, $5) }
global_state_args:
    | LVAR global_state_args { $1 :: $2 }
    | LVAR { [$1] }
states:
    | state states { $1 :: $2 }
    | state { [$1] }
state:
    | exp SCOLON { SExp $1 }
exp:
    | INTLI { EInt $1 }
