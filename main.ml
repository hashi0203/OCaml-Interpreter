open TySyntax
open Syntax
open Infer
open Eval
       
let rec read_eval_print tyenv env =
  print_string "# ";
  flush stdout;
  (try
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (ty,newtyenv) = infer_cmd tyenv cmd in
  let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s : " id;
   print_type ty;
   print_string " = ";
   print_value v 0;
   print_newline ();
   (* print_string "Number of 'EAdd' called is ";
   print_int !num_add;
   print_newline ();
   num_add := 0; *)
   read_eval_print newtyenv newenv)
  with Failure errmsg -> (print_string errmsg;
                          print_newline ();
                          read_eval_print tyenv env))

let initial_tyenv =
extendty "i"  TyInt
 (extendty "v" TyInt
   (extendty "x" TyInt
     empty_tyenv))

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))
    
let _ = read_eval_print initial_tyenv initial_env
