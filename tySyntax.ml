type tyvar = string
	      
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyList of ty
  | TyPair of ty * ty

let cur_tyvar = ref 0
let new_tyvar () = cur_tyvar := !cur_tyvar + 1; "a" ^ (string_of_int !cur_tyvar) 

let rec print_type ty =
  match ty with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyFun (a,b) -> print_type a;
                   print_string " -> ";
                   print_type b
  | TyVar x -> print_string x
  | TyList l -> print_type l;
                print_string " list"
  | TyPair (a,b) -> print_type a;
                    print_string " * ";
                    print_type b