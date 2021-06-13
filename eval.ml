open Syntax

exception Unbound

(* let num_add = ref 0 *)
let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec find_match p v =
  match p,v with
  | PInt p1,VInt v1 -> if p1 = v1 then Some [] else None
  | PBool p1,VBool v1 -> if p1 = v1 then Some [] else None
  | PVar p1,s -> Some [(p1,s)]
  | PPair (p1,p2),VPair (v1,v2) ->
              (match find_match p1 v1,find_match p2 v2 with
               | Some left,Some right -> Some (left @ right)
               | _,_ -> None)
  | PNil,VNil -> Some []
  | PCons (p1,p2),VCons (v1,v2) ->
              (match find_match p1 v1,find_match p2 v2 with
               | Some left,Some right -> Some (left @ right)
               | _,_ -> None)
  | _,_ -> None

let rec check_match case v =
match case with
| [] -> None
| (p,e) :: cs ->
    match find_match p v with
    | Some lst -> Some (lst,e)
    | None -> check_match cs v

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       lookup x env
     with
     | Unbound -> failwith "EvalErr")
  | EAdd (e1,e2) ->
    (* num_add := !num_add + 1; *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> failwith "EvalErr")
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> failwith "EvalErr")
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> failwith "EvalErr")
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> if i2 = 0 then failwith "EvalErr" else VInt (i1 / i2)
     | _ -> failwith "EvalErr")
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | VBool i1,  VBool i2  -> VBool (i1 = i2)
     | _ -> failwith "EvalErr")
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> failwith "EvalErr")
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> failwith "EvalErr")
  | ELet (e1,e2,e3) ->
    eval_expr (extend e1 (eval_expr env e2) env) e3
  | EFun (x,e1) -> VFun (x, e1, env)
  | EApp (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
     | VFun(x,e,oenv) -> eval_expr (extend x v2 oenv) e
     | VRecFun(f,x,e,oenv) ->
        let env' = extend x v2 (extend f (VRecFun(f,x,e,oenv)) oenv) in
        eval_expr env' e
     | _ -> failwith "EvalErr")
  | ELetRec (f,x,e1,e2) ->
    let env' = extend f (VRecFun(f,x,e1,env)) env in
    eval_expr env' e2
  | ENil -> VNil
  | EPair (e1,e2) -> VPair (eval_expr env e1,eval_expr env e2)
  | ECons (e1,e2) -> VCons (eval_expr env e1,eval_expr env e2)
  | EMatch (e1,e2) -> match check_match e2 (eval_expr env e1) with
                      | None -> failwith "EvalErr"
                      | Some (ee,ex) -> eval_expr (ee @ env) ex

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1, e2) -> let newval = eval_expr env e2 in
                      (e1, (extend e1 newval env), newval)
  | CRecDecl (f,x,e) -> let v = VRecFun (f,x,e,env) in
                        (f, (extend f v env), v)
