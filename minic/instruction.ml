exception TypeError of string

type datatype =
    | TInt
    | TFloat
    | TBool
    | TFun of datatype list * datatype

type variable = datatype * string

type instruction =
    | LoadInt of variable * int
    | LoadFloat of variable * float
    | LoadBool of variable * bool
    | Add of variable * variable * variable
    | Sub of variable * variable * variable
    | Mul of variable * variable * variable
    | Div of variable * variable * variable
    | Mod of variable * variable * variable
    | FAdd of variable * variable * variable
    | FSub of variable * variable * variable
    | FMul of variable * variable * variable
    | FDiv of variable * variable * variable
    | FMod of variable * variable * variable
    | Neg of variable * variable
    | FNeg of variable * variable

let rec gather_functions = function
    | [] -> []
    | Syntax.GDef(t, f, args, _) :: xs -> (f, (List.map fst args, t)) :: gather_functions xs
    | _ :: xs -> gather_functions xs

let make_variable_name i = "%" ^ string_of_int i

let rec normalize_expression i env = function
    | Syntax.EInt n ->
        let s = make_variable_name i in
        ([LoadInt((TInt, s), n)], (TInt, s), i + 1)
    | Syntax.EFloat r ->
        let s = make_variable_name i in
        ([LoadFloat((TFloat, s), r)], (TFloat, s), i + 1)
    | Syntax.EBool b ->
        let s = make_variable_name i in
        ([LoadBool((TBool, s), b)], (TBool, s), i + 1)
    | Syntax.EPositive e -> normalize_expression i env e
    | Syntax.ENegative e ->
        let (l, (t, s), j) = normalize_expression i env e in
        let x = make_variable_name j in
        if t = TInt then
            (l @ [Neg((t, x), (t, s))], (t, x), j + 1)
        else if t = TFloat then
            (l @ [FNeg((t, x), (t, s))], (t, x), j + 1)
        else
            raise (TypeError "mismatched type for ENegative")
    | Syntax.EPlus (e1, e2) ->
        let (l, vs, j) = normalize_expressions i env in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable_name j in
        if t0 = TInt && t1 = TInt then
            (l @ [Add((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else if t0 = TFloat && t1 = TFloat then
            (l @ [FAdd((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            raise (TypeError "mismatched type for EPlus")
    | Syntax.EMinus (e1, e2) ->
        let (l, vs, j) = normalize_expressions i env in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable_name j in
        if t0 = TInt && t1 = TInt then
            (l @ [Sub((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else if t0 = TFloat && t1 = TFloat then
            (l @ [FSub((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            raise (TypeError "mismatched type for EMinus")
    | Syntax.EStar (e1, e2) ->
        let (l, vs, j) = normalize_expressions i env in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable_name j in
        if t0 = TInt && t1 = TInt then
            (l @ [Mul((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else if t0 = TFloat && t1 = TFloat then
            (l @ [FMul((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            raise (TypeError "mismatched type for EStar")
    | Syntax.ESlash (e1, e2) ->
        let (l, vs, j) = normalize_expressions i env in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable_name j in
        if t0 = TInt && t1 = TInt then
            (l @ [Div((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else if t0 = TFloat && t1 = TFloat then
            (l @ [FDiv((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            raise (TypeError "mismatched type for ESlash")
    | Syntax.EPercent (e1, e2) ->
        let (l, vs, j) = normalize_expressions i env in
        let (t0, s0) = List.nth vs 0 in
        let (t1, s1) = List.nth vs 1 in
        let s = make_variable_name j in
        if t0 = TInt && t1 = TInt then
            (l @ [Mod((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else if t0 = TFloat && t1 = TFloat then
            (l @ [FMod((t0, s), (t0, s0), (t1, s1))], (t0, s), j + 1)
        else
            raise (TypeError "mismatched type for EPercent")
and normalize_expressions i env = function
    | [] -> ([], [], i)
    | x :: xs ->
        let (l, s, j) = normalize_expression i env x in
        let (ls, ss, k) = normalize_expressions j env xs in
        (l @ ls, s :: ss, k)

let read_variable i = function
    | Syntax.GVar(t, v, e) -> 

let read gs =
    let funs = gather_functions gs in
