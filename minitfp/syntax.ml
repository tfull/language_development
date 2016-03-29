open Base

type exp =
    | EInt of int
    | EFloat of float
    | EBool of bool
    | EPlus of exp * exp
    | EMinus of exp * exp
    | EStar of exp * exp
    | ESlash of exp * exp
    | EPercent of exp * exp
    | ENegative of exp
    | EPositive of exp
    | ELt of exp * exp
    | ELe of exp * exp
    | EGt of exp * exp
    | EGe of exp * exp
    | EEq of exp * exp
    | ENe of exp * exp
    | EIf of exp * exp * exp
    | EApp of exp * exp
    | EVar of string

type state =
    | SDeclare of string
    | SExp of exp
    | SRet of exp
    | SAssign of string * exp

type global_state =
    | GVar of string * exp
    | GDefExp of string * string list * exp
    | GDefState of string * string list * state list

type body = global_state list

let rec string_of_exp = function
    | EInt i -> "EInt " ^ string_of_int i
    | EFloat f -> "EFloat " ^ string_of_float f
    | EBool b -> "EBool " ^ (if b then "true" else "false")
    | EVar s -> "EVar " ^ s
    | EPlus(e1, e2) -> "EPlus(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EMinus(e1, e2) -> "EMinus(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EStar(e1, e2) -> "EStar(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | ESlash(e1, e2) -> "ESlash(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EPercent(e1, e2) -> "EPercent(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | ENegative e -> "ENegative(" ^ string_of_exp e ^ ")"
    | EPositive e -> "EPositive(" ^ string_of_exp e ^ ")"
    | ELt(e1, e2) -> "ELt(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | ELe(e1, e2) -> "ELe(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EGt(e1, e2) -> "EGt(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EGe(e1, e2) -> "EGe(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EEq(e1, e2) -> "EEq(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | ENe(e1, e2) -> "ENe(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | EIf(e1, e2, e3) -> "EIf(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ", " ^ string_of_exp e3 ^ ")"
    | EApp(e1, e2) -> "EApp(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"

let string_of_state = function
    | SDeclare s -> "SDeclare " ^ s
    | SExp e -> "SExp(" ^ string_of_exp e ^ ")"
    | SRet e -> "SRet(" ^ string_of_exp e ^ ")"
    | SAssign(s, e) -> "SAssign(" ^ s ^ ", " ^ string_of_exp e ^ ")"

let string_of_global_state = function
    | GVar(s, e) -> "GVar(" ^ s ^ ", " ^ string_of_exp e ^ ")"
    | GDefExp(s, xs, e) -> "GDefExp(" ^ s ^ ", [" ^ join "; " xs ^ "], " ^ string_of_exp e ^ ")"
    | GDefState(s, xs, states) -> "GDefState(" ^ s ^ ", [" ^ join "; " xs ^ "], [" ^ join "; " (List.map string_of_state states) ^ "])"

let string_of_body b = "[" ^ join "; " (List.map string_of_global_state b) ^ "]"
