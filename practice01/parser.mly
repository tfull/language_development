%{
    open Syntax
    exception ParseError of string
    let parse_error s = raise (ParseError (s ^ " " ^ (string_of_int (Parsing.symbol_start ())) ^ " - " ^ (string_of_int (Parsing.symbol_end ()))))
%}

%token <int> INTLI
%token <float> FLOATLI
%token <string> UPLI LOWLI
%token <bool> BOOLLI
%token <int64> LONGLI
%token LT GT LE GE EQ
%token LMPAR RMPAR LPAR RPAR
%token SCOLON EQUAL COMMA
%token PLUS MINUS STAR PERCENT SLASH
%token RETURN
%token EOF

%start parse
%type <Syntax.funstate list> parse

%%

parse:
    | EOF { [] }
    | funstates EOF { $1 }
funstates:
    | funstate funstates { $1 :: $2 }
    | funstate { [$1] }
funstate:
    | UPLI LOWLI LPAR RPAR LMPAR states RMPAR { Func($1, $2, [], $6) }
    | UPLI LOWLI LPAR funargs RPAR LMPAR states RMPAR { Func($1, $2, $4, $7) }
funargs:
    | UPLI LOWLI COMMA funargs { ($1, $2) :: $4 }
    | UPLI LOWLI { [($1, $2)] }
states:
    | state states { $1 :: $2 }
    | state { [$1] }
state:
    | UPLI LOWLI SCOLON { Let($1, $2) }
    | LOWLI EQUAL expp0 SCOLON { Assign($1, $3) }
    | RETURN expp0 SCOLON { Return $2 }
    | expp0 SCOLON { Exp $1 }
expp0:
    | exp0 LT exp0 { Lt($1, $3) }
    | exp0 GT exp0 { Gt($1, $3) }
    | exp0 LE exp0 { Le($1, $3) }
    | exp0 GE exp0 { Ge($1, $3) }
    | exp0 EQ exp0 { Eq($1, $3) }
    | exp0 { $1 }
exp0:
    | exp0 PLUS exp1 { Add($1, $3) }
    | exp0 MINUS exp1 { Sub($1, $3) }
    | exp1 { $1 }
exp1:
    | exp1 STAR exp2 { Mul($1, $3) }
    | exp1 PERCENT exp2 { Mod($1, $3) }
    | exp1 SLASH exp2 { Div($1, $3) }
    | exp2 { $1 }
exp2:
    | MINUS exp3 { Minus($2) }
    | exp3 { $1 }
exp3:
    | LOWLI LPAR RPAR { App($1, []) }
    | LOWLI LPAR exps RPAR { App($1, $3) }
    | exp4 { $1 }
exp4:
    | LPAR expp0 RPAR { $2 }
    | INTLI { Int $1 }
    | LOWLI { Var $1 }
    | FLOATLI { Float $1 }
    | BOOLLI { Bool $1 }
    | LONGLI { Long $1 }
exps:
    | expp0 COMMA exps { $1 :: $3 }
    | expp0 { [$1] }
;
