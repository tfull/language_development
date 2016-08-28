open Base

type data_type =
    | TInt
    | TFloat
    | TBool

type expression =
    | EIf of expression * expression * expression
    | ENe of expression * expression
    | EEq of expression * expression
    | ELe of expression * expression
    | ELt of expression * expression
    | EGe of expression * expression
    | EGt of expression * expression
    | EStar of expression * expression
    | ESlash of expression * expression
    | EPercent of expression * expression
    | EPlus of expression * expression
    | EMinus of expression * expression
    | EPositive of expression
    | ENegative of expression
    | EVar of string
    | EInt of int
    | EFloat of float
    | EBool of bool
    | EApp of string * expression list

type state =
    | SVar of data_type * string * expression option
    | SAssign of string * expression
    | SRet of expression
    | SExp of expression

type global =
    | GVar of data_type * string * expression option
    | GDef of data_type * string * (data_type * string) list * state list

let string_of_data_type = function
    | TInt -> "TInt"
    | TFloat -> "TFloat"
    | TBool -> "TBool"

let rec string_of_expression = function
    | EIf (e1, e2, e3) -> "EIf(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ", " ^ string_of_expression e3 ^ ")"
    | ENe (e1, e2) -> "ENe(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EEq (e1, e2) -> "EEq(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | ELe (e1, e2) -> "ELe(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | ELt (e1, e2) -> "ELt(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EGe (e1, e2) -> "EGe(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EGt (e1, e2) -> "EGt(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EStar (e1, e2) -> "EStar(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | ESlash (e1, e2) -> "ESlash(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EPercent (e1, e2) -> "EPercent(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EPlus (e1, e2) -> "EPlus(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EMinus (e1, e2) -> "EMinus(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
    | EPositive e -> "EPositive(" ^ string_of_expression e ^ ")"
    | ENegative e -> "ENegative(" ^ string_of_expression e ^ ")"
    | EVar s -> "EVar(" ^ s ^ ")"
    | EInt i -> "EInt " ^ string_of_int i
    | EFloat f -> "EFloat " ^ string_of_float f
    | EBool b -> "EBool " ^ string_of_bool b
    | EApp (f, args) -> "EApp(" ^ f ^ ", [" ^ join "; " (List.map string_of_expression args) ^ "])"

let string_of_state = function
    | SVar (d, s, Some e) -> "SVar(" ^ string_of_data_type d ^ ", " ^ s ^ ", " ^ "Some(" ^ string_of_expression e ^ "))"
    | SVar (d, s, None) -> "SVar(" ^ string_of_data_type d ^ ", " ^ s ^ ", " ^ "None)"
    | SAssign (s, e) -> "SAssign(" ^ s ^ ", " ^ string_of_expression e ^ ")"
    | SRet e -> "SRet(" ^ string_of_expression e ^ ")"
    | SExp e -> "SExp(" ^ string_of_expression e ^ ")"

let string_of_global = function
    | GVar (d, s, Some e) -> "GVar(" ^ string_of_data_type d ^ ", " ^ s ^ ", Some(" ^ string_of_expression e ^ "))"
    | GVar (d, s, None) -> "GVar(" ^ string_of_data_type d ^ ", " ^ s ^ ", None)"
    | GDef (d, s, vs, states) -> "GDef(" ^ string_of_data_type d ^ ", " ^ s ^ ", [" ^ (join "; " (List.map (fun (d, s) -> "(" ^ string_of_data_type d ^ ", " ^ s ^ ")") vs)) ^ "], [" ^ join "; " (List.map string_of_state states) ^ "])"
