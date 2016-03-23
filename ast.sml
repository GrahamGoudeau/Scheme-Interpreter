type identifier = string
datatype let_type = LET | LET_STAR | LET_REC

datatype value = NIL
               | BOOL of bool
               | NUM of int
               | CLOSURE of
                   ((identifier list) * exp) * ((identifier * value) list)
               (*
               | S_EXP_LIT of value
               | S_EXP_SYM of identifier
               | S_EXP_LIST of value list
               | PAIR of value * value
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
  | value_to_string (CLOSURE(lambda, env)) = "[value: closure]"

fun exp_to_string (LIT(value)) = value_to_string value
  | exp_to_string (VAR(var)) = "[var " ^ var ^ "]"

fun print_def (VAL(ident, exp)) =
  (print ("(val " ^ ident ^ " " ^ exp_to_string exp ^ ")\n"))
  | print_def (EXP(exp)) = print ((exp_to_string exp) ^ ")\n")
  | print_def (DEFINE(ident, _)) =
  print ("(define " ^ ident ^ ")\n")

type error_message = string

exception VariableNotBound

datatype runtime_Error = VAR_NOT_BOUND of error_message

fun raise_runtime_error error =
  let
    val runtime_err_msg = "Runtime error encountered:\n\t - \""
    fun get_msg msg = runtime_err_msg ^ msg ^ "\"\n"
    fun handle_error (VAR_NOT_BOUND(msg)) =
      (print (get_msg msg); raise VariableNotBound)
  in handle_error error
  end

datatype env = ENV of (identifier * value) list

val init_env = (ENV([]))

fun print_all_env (ENV(xs)) =
  (print "=== Environment state: ===\n";
   List.map
     (fn (ident, value) => print ("{" ^ ident ^ ", " ^
                                  (value_to_string value) ^ "}\n"))
     xs;
   print "\n")

fun bind_env key value (ENV(xs)) = (ENV((key, value)::xs))
fun find_env key (ENV([])) =
      raise_runtime_error (VAR_NOT_BOUND("Var \"" ^ key ^ "\" not bound"))
  | find_env key (ENV((ident, value)::xs)) =
      if key = ident then value
      else find_env key (ENV(xs))


fun eval (LIT(value)) env = (value, env)
  | eval (VAR(ident)) env = ((find_env ident env), env)

fun execute defs =
      let
        fun execute_def (VAL((ident, exp))) env =
              let
                val (result, new_env) = eval exp env
              in
                (bind_env ident result env)
              end
          | execute_def (EXP(exp)) env =
              let
                val (result, new_env) = eval exp env
              in
                new_env
              end
          (*| execute_def (DEFINE(ident, (ident_list, exp))) =*)
      in
        List.foldl (fn (def, old_env) => execute_def def old_env) init_env defs
      end
