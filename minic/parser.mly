%{
    open Syntax
    exception ParseError of string
    let parse_error s = raise (ParseError (s ^ " " ^ (string_of_int (Parsing.symbol_start ())) ^ " - " ^ (string_of_int (Parsing.symbol_end ()))))
%}

%token <int> INTLI
%token <float> FLOATLI
%token <bool> BOOLLI
%token <string> VAR
%token INT FLOAT BOOL
%token LMPAR RMPAR LPAR RPAR
%token SCOLON COMMA
%token EQUAL
%token OR AND
%token PLUS MINUS STAR SLASH PERCENT
%token LT GT LE GE EQ NE
%token VAR
%token RETURN
%token EOF

%start parse
%type <Syntax.global list> parse

%%

parse:
    | global_states EOF { $1 }
global_states:
    | global_state global_states { $1 :: $2 }
    | { [] }
global_state:
    | datatype VAR SCOLON { GVar($1, $2, None) }
    | datatype VAR EQUAL exp SCOLON { GVar($1, $2, Some $4) }
    | datatype VAR LPAR args RPAR LMPAR states RMPAR { GDef($1, $2, $4, $7) }
args:
    | { [] }
    | args_plus { $1 }
args_plus:
    | datatype VAR COMMA args_plus { ($1, $2) :: $4 }
    | datatype VAR { [($1, $2)] }
states:
    | state states { $1 :: $2 }
    | { [] }
state:
    | datatype VAR SCOLON { SVar($1, $2, None) }
    | datatype VAR EQUAL exp SCOLON { SVar($1, $2, Some $4) }
    | VAR EQUAL exp SCOLON { SAssign($1, $3) }
    | RETURN exp SCOLON { SRet $2 }
    | exp SCOLON { SExp $1 }
exp:
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
    | PLUS atom { EPositive $2 }
    | MINUS atom { ENegative $2 }
    | atom { $1 }
atom:
    | VAR { EVar $1 }
    | INTLI { EInt $1 }
    | FLOATLI { EFloat $1 }
    | BOOLLI { EBool $1 }
    | LPAR exp RPAR { $2 }
    | app { $1 }
app:
    | VAR LPAR RPAR { EApp($1, []) }
    | VAR LPAR exps RPAR { EApp($1, $3) }
exps:
    | exp { [$1] }
    | exp COMMA exps { $1 :: $3 }
datatype:
    | INT { TInt }
    | FLOAT { TFloat }
    | BOOL { TBool }
