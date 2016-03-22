type identifier = string
datatype let_type = LET | LET_STAR | LET_REC

datatype value = NIL
               | BOOL of bool
               | NUM of int
               (*
               | S_EXP_LIT of value
               | S_EXP_SYM of identifier
               | S_EXP_LIST of value list
               | PAIR of value * value
               | CLOSURE of ((identifier list) * exp) * ((identifier * exp)
               list)
               | PRIMITIVE of string
               *)
and

exp = LIT of value
             | VAR of identifier
             (*
             | SET of identifier * exp
             | IFX of exp * exp * exp
             | WHILEX of exp * exp
             | BEGIN of exp list
             | APPLY of exp * (exp list)
             | LETX of let_type * (identifier list) * (exp list) * exp
             | LAMBDA of lambda
             *)
             withtype lambda = (identifier list) * exp

datatype def = VAL of identifier * exp
             | EXP of exp
             | DEFINE of identifier * lambda

fun value_to_string (NIL) = "[value: NIL]"
  | value_to_string (BOOL(true)) = "[value: #t]"
  | value_to_string (BOOL(false)) = "[value: #f]"
  | value_to_string (NUM(int)) = "[value: " ^ (Int.toString int) ^ "]"

fun exp_to_string (LIT(value)) = value_to_string value
  | exp_to_string (VAR(var)) = "[var " ^ var ^ "]"

fun print_def (VAL(ident, exp)) =
  (print ("(val " ^ ident ^ exp_to_string exp ^ "\n"))
  | print_def (EXP(exp)) = print ((exp_to_string exp) ^ "\n")
  | print_def (DEFINE(ident, _)) =
  print ("(define " ^ ident ^ ")\n")
