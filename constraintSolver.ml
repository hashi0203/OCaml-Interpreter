open TySyntax

exception TyError

type subst = (tyvar * ty) list
type constraints = (ty * ty) list

let rec ty_subst subst ty =
  match ty with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (ty1,ty2) -> TyFun (ty_subst subst ty1, ty_subst subst ty2)
  | TyVar tyvar -> (try ty_subst subst (List.assoc tyvar subst) with
                    | Not_found -> TyVar tyvar)
  | TyList ty1 -> TyList (ty_subst subst ty1)
  | TyPair (ty1,ty2) -> TyPair (ty_subst subst ty1,ty_subst subst ty2)

(* 各sigma2の二項目を取ってきてsigma1で置き換える *)
let rec compose sigma1 sigma2 =
  match sigma2 with
  | [] -> sigma1
  | (tyvar,ty) :: sigmas -> (tyvar, ty_subst sigma1 ty) :: (compose sigma1 sigmas)

let rec v2ty c subst =
  match c with
  | [] -> []
  | (ty1,ty2) :: cs -> ((ty_subst subst ty1),(ty_subst subst ty2)) :: v2ty cs subst

let rec checktina v ty =
  match ty with
  | TyFun (ty1,ty2) -> if checktina v ty1 then true else checktina v ty2
  | TyVar x -> if v = x then true else false
  | _ -> false

let rec unify c =
  match c with
  | [] -> []
  | (ty1,ty2) :: cs ->
     if ty1 = ty2 then unify cs
     else match ty1,ty2 with
        | TyFun (s1,t1),TyFun (s2,t2) -> unify ((s1,s2) :: (t1,t2) :: cs)
        | TyVar v,_ -> if checktina v ty2 then raise TyError else compose (unify (v2ty cs [(v,ty2)])) [(v,ty2)]
        | _,TyVar v -> if checktina v ty1 then raise TyError else compose (unify (v2ty cs [(v,ty1)])) [(v,ty1)]
        | TyList s1,TyList s2 -> unify ((s1,s2) :: cs)
        | TyPair (s1,t1),TyPair (s2,t2) -> unify ((s1,s2) :: (t1,t2) :: cs)
        | _,_ -> raise TyError
