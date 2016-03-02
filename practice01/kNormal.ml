open Base

exception Error of string

type datatype =
    | Bool
    | Int
    | Long
    | Float
    | Fun of datatype list * datatype

type var = datatype * string

type state =
    | LoadInt of var * int
    | LoadBool of var * bool
    | LoadFloat of var * float
    | LoadLong of var * int64
    | Add of var * var * var
    | Sub of var * var * var
    | Mul of var * var * var
    | Div of var * var * var
    | Mod of var * var * var
    | Neg of var * var
    | FAdd of var * var * var
    | FSub of var * var * var
    | FMul of var * var * var
    | FDiv of var * var * var
    | FMod of var * var * var
    | FNeg of var * var
    | Le of var * var * var
    | Ge of var * var * var
    | Lt of var * var * var
    | Gt of var * var * var
    | Eq of var * var * var
    | Mov of var * var
    | App of var * string * var list
    | Ret of var

type funstate =
    | Func of datatype * string * (datatype * string) list * state list

let string_of_datatype = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Float -> "Float"
    | Long -> "Long"
    | _ -> raise (Error "string_of_datatype")

let string_to_datatype = function
    | "Bool" -> Bool
    | "Int" -> Int
    | "Float" -> Float
    | "Long" -> Long
    | _ -> raise (Failure "type not exist")

let string_of_var (t, s) = string_of_datatype t ^ " " ^ s

let debug_of_state = function
    | LoadInt(s, i) -> "LoadInt " ^ (string_of_var s) ^ " " ^ string_of_int i
    | LoadBool(s, b) -> "LoadBool " ^ (string_of_var s) ^ " " ^ (if b then "true" else "false")
    | LoadFloat(s, f) -> "LoadFloat " ^ (string_of_var s) ^ " " ^ string_of_float f
    | LoadLong(s, l) -> "LoadLong " ^ (string_of_var s) ^ " " ^ Int64.to_string l
    | Add(s0, s1, s2) -> "Add " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Sub(s0, s1, s2) -> "Sub " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Mul(s0, s1, s2) -> "Mul " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Div(s0, s1, s2) -> "Div " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Mod(s0, s1, s2) -> "Mod " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Neg(s0, s1) -> "Neg " ^ join " " (List.map string_of_var [s0; s1])
    | FAdd(s0, s1, s2) -> "FAdd " ^ join " " (List.map string_of_var [s0; s1; s2])
    | FSub(s0, s1, s2) -> "FSub " ^ join " " (List.map string_of_var [s0; s1; s2])
    | FMul(s0, s1, s2) -> "FMul " ^ join " " (List.map string_of_var [s0; s1; s2])
    | FDiv(s0, s1, s2) -> "FDiv " ^ join " " (List.map string_of_var [s0; s1; s2])
    | FMod(s0, s1, s2) -> "FMod " ^ join " " (List.map string_of_var [s0; s1; s2])
    | FNeg(s0, s1) -> "FNeg " ^ join " " (List.map string_of_var [s0; s1])
    | Le(s0, s1, s2) -> "Le " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Ge(s0, s1, s2) -> "Ge " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Lt(s0, s1, s2) -> "Lt " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Gt(s0, s1, s2) -> "Gt " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Eq(s0, s1, s2) -> "Eq " ^ join " " (List.map string_of_var [s0; s1; s2])
    | Mov(s0, s1) -> "Mov " ^ join " " (List.map string_of_var [s0; s1])
    | App(s0, s1, xs) -> "App " ^ (string_of_var s0) ^ " " ^ s1 ^ "(" ^ join "," (List.map string_of_var xs) ^ ")"
    | Ret s -> "Ret " ^ string_of_var s

let debug_of_funstate = function
    | Func(d, s, vs, xs) ->
        string_of_datatype d ^ " " ^ s ^ " " ^ "(" ^ join "," (List.map string_of_var vs) ^ "){\n" ^ join "\n" (List.map (fun x -> "  " ^ debug_of_state x) xs) ^ "\n}"

let make_variable i = "%" ^ string_of_int i

let rec gather_functions = function
    | [] -> []
    | Syntax.Func(t, f, args, states) :: xs -> (f, Fun(List.map (fun x -> string_to_datatype (fst x)) args, string_to_datatype t), f) :: gather_functions xs

let rec find_variable_sub z = function
    | [] -> None
    | (v, t, x) :: xs -> if z = v then Some (t, x) else find_variable_sub z xs

let rec find_variable z = function
    | [] -> None
    | l :: ls -> (match find_variable_sub z l with | Some x -> Some x | None -> find_variable z ls)

let rec check_exp i env = function
    | Syntax.Int n ->
        let s = make_variable i in
        ([LoadInt((Int, s), n)], (Int, s), i + 1)
    | Syntax.Long n ->
        let s = make_variable i in
        ([LoadLong((Long, s), n)], (Long, s), i + 1)
    | Syntax.Float n ->
        let s = make_variable i in
        ([LoadFloat((Float, s), n)], (Float, s), i + 1)
    | Syntax.Bool b ->
        let s = make_variable i in
        ([LoadBool((Bool, s), b)], (Bool, s), i + 1)
    | Syntax.Add(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long) && t0 = t1 then
            (l @ [Add((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            if t0 = Float && t0 = t1 then
                (l @ [FAdd((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
            else
                raise (Error "type invalid for Add")
    | Syntax.Sub(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long) && t0 = t1 then
            (l @ [Sub((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            if t0 = Float && t0 = t1 then
                (l @ [FSub((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
            else
                raise (Error "type invalid for Sub")
    | Syntax.Mul(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long) && t0 = t1 then
            (l @ [Mul((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            if t0 = Float && t0 = t1 then
                (l @ [FMul((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
            else
                raise (Error "type invalid for Mul")
    | Syntax.Div(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long) && t0 = t1 then
            (l @ [Div((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            if t0 = Float && t0 = t1 then
                (l @ [FDiv((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
            else
                raise (Error "type invalid for Div")
    | Syntax.Mod(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long) && t0 = t1 then
            (l @ [Mod((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            if t0 = Float && t0 = t1 then
                (l @ [FMod((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
            else
                raise (Error "type invalid for Mod")
    | Syntax.Minus e ->
        let (l, (t, x), j) = check_exp i env e in
        let s = make_variable j in
        if t = Int || t = Long then
            (l @ [Neg((t, s), (t, x))], (t, s), j + 1)
        else
            if t = Float then
                (l @ [FNeg((t, s), (t, x))], (t, s), j + 1)
            else
                raise (Error "type invalid for Minus")
    | Syntax.Le(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long || t0 = Float) && t0 = t1 then
            (l @ [Le((Bool, s), (t0, s0), (t1, s1))], (Bool, s), j + 1)
        else
            raise (Error "type invalid for Le")
    | Syntax.Ge(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long || t0 = Float) && t0 = t1 then
            (l @ [Ge((Bool, s), (t0, s0), (t1, s1))], (Bool, s), j + 1)
        else
            raise (Error "type invalid for Ge")
    | Syntax.Lt(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long || t0 = Float) && t0 = t1 then
            (l @ [Lt((Bool, s), (t0, s0), (t1, s1))], (Bool, s), j + 1)
        else
            raise (Error "type invalid for Lt")
    | Syntax.Gt(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long || t0 = Float) && t0 = t1 then
            (l @ [Gt((Bool, s), (t0, s0), (t1, s1))], (Bool, s), j + 1)
        else
            raise (Error "type invalid for Gt")
    | Syntax.Eq(e1, e2) ->
        let (l, vs, j) = check_exps i env [e1; e2] in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable j in
        if (t0 = Int || t0 = Long || t0 = Float) && t0 = t1 then
            (l @ [Eq((Bool, s), (t0, s0), (t1, s1))], (Bool, s), j + 1)
        else
            raise (Error "type invalid for Eq")
    | Syntax.Var s ->
        begin
            match find_variable s env with
                | Some (t, v) -> ([], (t, v), i)
                | None -> raise (Error "no such variable to refer")
        end
    | Syntax.App(s, es) ->
        let (l, vs, j) = check_exps i env es in
        match find_variable s env with
            | Some (Fun (args, target), f) ->
                if args = List.map fst vs then
                    let nv = make_variable j in
                    (l @ [App((target, nv), f, vs)], (target, nv), j + 1)
                else
                    raise (Error "illegal type for App")
            | _ -> raise (Error "no such function")
and check_exps i env = function
    | [] -> ([], [], i)
    | x :: xs ->
        let (l, s, j) = check_exp i env x in
        let (ls, ss, k) = check_exps j env xs in
        (l @ ls, s :: ss, k)

let rec check_states ty i env = function
    | [] -> []
    | Syntax.Let(t, s) :: xs ->
        if List.exists (fun (f, _, _) -> s = f) (List.hd env) then
            raise (Failure "deplicate variable")
        else
            check_states ty (i + 1) (((s, string_to_datatype t, make_variable i) :: (List.hd env)) :: List.tl env) xs
    | Syntax.Assign(s, e) :: xs ->
        let (l, (t, x), j) = check_exp i env e in
        begin
            match find_variable s env with
                | Some (c, d) ->
                    if c = t then
                        l @ [Mov((c, d), (t, x))] @ check_states ty j env xs
                    else
                        raise (Error "type error for Mov")
                | None -> raise (Error "no such variable for assign")
        end
    | Syntax.Return e :: xs ->
        let (l, (t, x), j) = check_exp i env e in
        if t = ty then
            l @ [Ret((t, x))] @ check_states ty j env xs
        else
            raise (Error "illegal type for return")
    | Syntax.Exp e :: xs ->
        let (l, _, j) = check_exp i env e in
        l @ check_states ty j env xs

let rec read_args i = function
    | [] -> []
    | (t, s) :: xs -> (s, string_to_datatype t, make_variable i) :: read_args (i + 1) xs

let check env = function
    | Syntax.Func(t, f, args, states) -> Func(string_to_datatype t, f, List.map (fun (t, s) -> (string_to_datatype t, s)) args, check_states (string_to_datatype t) (List.length args) ((read_args 0 args) :: env) states)

let check_functions funs =
    let env = [gather_functions funs] in
    List.map (check env) funs
