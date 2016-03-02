open Base

type exp =
    | Int of int
    | Float of float
    | Bool of bool
    | Long of int64
    | Add of exp * exp
    | Sub of exp * exp
    | Mul of exp * exp
    | Div of exp * exp
    | Mod of exp * exp
    | Minus of exp
    | Var of string
    | App of string * exp list
    | Le of exp * exp
    | Ge of exp * exp
    | Lt of exp * exp
    | Gt of exp * exp
    | Eq of exp * exp

type state =
    | Let of string * string
    | Assign of string * exp
    | Return of exp
    | Exp of exp

type funstate =
    | Func of string * string * (string * string) list * state list

let rec string_of_exp = function
    | Int i -> "Int " ^ string_of_int i
    | Float f -> "Float " ^ string_of_float f
    | Bool b -> "Bool " ^ (if b then "true" else "false")
    | Long l -> "Long " ^ Int64.to_string l
    | Var s -> "Var " ^ s
    | Add(e1, e2) -> "Add(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Sub(e1, e2) -> "Sub(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Mul(e1, e2) -> "Mul(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Div(e1, e2) -> "Div(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Mod(e1, e2) -> "Mod(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Le(e1, e2) -> "Le(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Ge(e1, e2) -> "Ge(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Lt(e1, e2) -> "Lt(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Gt(e1, e2) -> "Gt(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Eq(e1, e2) -> "Eq(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Minus e -> "Minus(" ^ string_of_exp e ^ ")"
    | App(s, es) -> "App(" ^ s ^ ", [" ^  join "; " (List.map string_of_exp es) ^ "])"

let string_of_state = function
    | Let(u, s) -> "Let(" ^ u ^ ", " ^ s ^ ")"
    | Assign(s, e) -> "Assign(" ^ s ^ ", " ^ string_of_exp e ^ ")"
    | Return e -> "Return(" ^ string_of_exp e ^ ")"
    | Exp e -> "Exp(" ^ string_of_exp e ^ ")"

let string_of_funstate = function
    | Func(u, s, args, states) -> "Func(" ^ u ^ ", " ^ s ^ ", [" ^ join "; " (List.map (fun (x, y) -> "(" ^ x ^ ", " ^ y ^ ")") args) ^ "], " ^ ("[" ^ join "; " (List.map string_of_state states) ^ "]") ^ ")"
