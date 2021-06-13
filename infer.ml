open TySyntax
open Syntax
open ConstraintSolver

exception Unbound
exception TypeErr

type constraints = (ty * ty) list
type tyenv = (name * ty) list

let empty_tyenv = []
let extendty x v tyenv = (x, v) :: tyenv

let rec lookup x tyenv =
  try List.assoc x tyenv with Not_found -> raise Not_found

let rec infer_pattern p con extyenv =
  match p with
  | PInt _ -> (TyInt,con,extyenv)
  | PBool _ -> (TyBool,con,extyenv)
  | PVar p1 -> let a = TyVar (new_tyvar ()) in
               (a,con,(extendty p1 a extyenv))
  | PPair (p1,p2) -> let (ty1,con1,env1) = infer_pattern p1 con extyenv in
                     let (ty2,con2,env2) = infer_pattern p2 con extyenv in
                     (TyPair (ty1,ty2),con1 @ con2 @ con,env1 @ env2 @ extyenv)
  | PNil -> let a = TyVar (new_tyvar ()) in
            (TyList a,con,extyenv)
  | PCons (p1,p2) -> let a = TyVar (new_tyvar ()) in
                     let (ty1,con1,env1) = infer_pattern p1 con extyenv in
                     let (ty2,con2,env2) = infer_pattern p2 con extyenv in
                     (TyList a,extendty a ty1 (extendty (TyList a) ty2 con),env1 @ env2 @ extyenv)

let rec intfun tyenv e1 e2 =
  let (ty1, con1) = infer_expr tyenv e1 in
  let (ty2, con2) = infer_expr tyenv e2 in
  (TyInt, (extendty ty1 TyInt (extendty ty2 TyInt (con1 @ con2))))
and infer_match plist t a tyenv =
  match plist with
  | [] -> []
  | (p,e) :: ps-> let (ty1,con1,env1) = infer_pattern p [] [] in
                  let (ty2,con2) = infer_expr (env1 @ tyenv) e in
                  (t,ty1) :: (a,ty2) :: con1 @ con2 @ infer_match ps t a tyenv
and infer_expr tyenv e =
  match e with
  | EConstInt i -> (TyInt, [])
  | EConstBool b -> (TyBool, [])
  | EVar x ->
    (try
       (lookup x tyenv, [])
     with
     | Not_found -> failwith "Unbound")
  | EAdd (e1,e2) -> intfun tyenv e1 e2
  | ESub (e1,e2) -> intfun tyenv e1 e2  
  | EMul (e1,e2) -> intfun tyenv e1 e2
  | EDiv (e1,e2) -> intfun tyenv e1 e2
  | EEq (e1,e2) ->
    let (ty1, con1) = infer_expr tyenv e1 in
    let (ty2, con2) = infer_expr tyenv e2 in
    (TyBool, (extendty ty1 ty2 (con1 @ con2)))
  | ELt (e1,e2) ->
    let (ty1, con1) = infer_expr tyenv e1 in
    let (ty2, con2) = infer_expr tyenv e2 in
    (TyBool, (extendty ty1 TyInt (extendty ty2 TyInt (con1 @ con2))))
  | EIf (e1,e2,e3) ->
    let (ty1, con1) = infer_expr tyenv e1 in
    let (ty2, con2) = infer_expr tyenv e2 in
    let (ty3, con3) = infer_expr tyenv e3 in
    (ty2, (extendty ty1 TyBool (extendty ty2 ty3 (con1 @ con2 @ con3))))
  | ELet (e1,e2,e3) ->
    let (ty2, con2) = infer_expr tyenv e2 in
    let newtyenv = extendty e1 ty2 tyenv in
    let (ty3, con3) = infer_expr newtyenv e3 in
    (ty3, con2 @ con3)
  | EFun (x,e1) ->
    let a = TyVar (new_tyvar ()) in
    let newtyenv = extendty x a tyenv in
    let (ty1, con1) = infer_expr newtyenv e1 in
    (TyFun (a, ty1), con1)
  | EApp (e1,e2) ->
    let (ty1, con1) = infer_expr tyenv e1 in
    let (ty2, con2) = infer_expr tyenv e2 in
    let a = TyVar (new_tyvar ()) in
    (a, (extendty ty1 (TyFun (ty2, a)) (con1 @ con2)))
  | ELetRec(f,x,e1,e2) ->
    let a = TyVar (new_tyvar ()) in
    let b = TyVar (new_tyvar ()) in
    let newtyenv = extendty f (TyFun (a,b)) tyenv in
    let (ty1, con1) = infer_expr (extendty x a newtyenv) e1 in
    let (ty2, con2) = infer_expr newtyenv e2 in
    (ty2, (extendty ty1 b (con1 @ con2)))
  | ENil -> let a = TyVar (new_tyvar ()) in (TyList a, [])
  | EPair (e1,e2) -> let (ty1, con1) = infer_expr tyenv e1 in
                     let (ty2, con2) = infer_expr tyenv e2 in
                     (TyPair (ty1,ty2),con1 @ con2)
  | ECons (e1,e2) -> let (ty1, con1) = infer_expr tyenv e1 in
                     let (ty2, con2) = infer_expr tyenv e2 in
                     (ty2,extendty (TyList ty1) ty2 (con1 @ con2))
  | EMatch (e1,e2) -> let a = TyVar (new_tyvar ()) in
                      let (ty1,con1) = infer_expr tyenv e1 in
                      let con2 = infer_match e2 ty1 a tyenv in
                      (a,con1 @ con2)

let rec infer_cmd tyenv c =
  match c with
  | CExp e -> let (ty, con) = infer_expr tyenv e in
              (try 
                let t = ty_subst (unify con) ty in (t,tyenv)
               with
               | TyError -> failwith "TypeErr")
  | CDecl (e1, e2) -> let (ty2, con2) = infer_expr tyenv e2 in
                      (try 
                        let ty  = ty_subst (unify con2) ty2 in
                        (ty,(extendty e1 ty tyenv))
                       with
                       | TyError -> failwith "TypeErr")
  | CRecDecl (f,x,e1) -> let a = TyVar (new_tyvar ()) in
                         let b = TyVar (new_tyvar ()) in
                         let newtyenv = extendty f (TyFun (a,b)) (extendty x a tyenv) in
                         let (ty1, con1) = infer_expr newtyenv e1 in
                         (try
                            let subst = unify con1 in
                            let be  = ty_subst subst a in
                            let af = ty_subst subst ty1 in
                            (TyFun (be,af),(extendty f (TyFun (be,af)) tyenv))
                          with
                          | TyError -> failwith "TypeErr")