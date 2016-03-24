type identifier = string
datatype let_type = LET | LET_STAR | LET_REC

datatype value = NIL
               | BOOL of bool
               | NUM of int
               | CLOSURE of
                   ((identifier list) * exp) * ((identifier * value) list)
               | PRIMITIVE of string
               (*
               | S_EXP of s_exp
               | S_EXP_LIT of value
               | S_EXP_SYM of identifier
               | S_EXP_LIST of value list
               | PAIR of value * value
               list)
               | PRIMITIVE of string
               *)
and

s_exp = S_EXP_INT of int
      | S_EXP_BOOL of bool
      | S_EXP_SYM of identifier
      | S_EXP_LIST of s_exp list

and

exp = LIT of value
             | VAR of identifier
             | APPLY of exp * (exp list)
             | LAMBDA of lambda
             (*
             | SET of identifier * exp
             | IFX of exp * exp * exp
             | WHILEX of exp * exp
             | BEGIN of exp list
             | LETX of let_type * (identifier list) * (exp list) * exp
             *)
             withtype lambda = (identifier list) * exp

datatype def = VAL of identifier * exp
             | EXP of exp
             | DEFINE of identifier * lambda

val primitive_funcs_arity =
  [("=", 2), ("+", 2), ("-", 2), ("*", 2), ("/", 2), ("print", 1),
   ("if", 3), ("lambda", 2)]

val primitive_funcs = List.map (fn (oper, _) => oper) primitive_funcs_arity

fun member_string (elem:string) (xs:string list) =
  List.exists (fn x => x = elem) xs

fun value_to_string (NIL) = "[value: NIL]"
  | value_to_string (BOOL(true)) = "[value: #t]"
  | value_to_string (BOOL(false)) = "[value: #f]"
  | value_to_string (NUM(int)) = "[value: " ^ (Int.toString int) ^ "]"
  | value_to_string (CLOSURE(lambda, env)) = "[value: closure]"
  | value_to_string (PRIMITIVE(ident)) = "[primitive op: " ^ ident ^ "]"

fun exp_to_string (LIT(value)) = value_to_string value
  | exp_to_string (VAR(var)) = "[var " ^ var ^ "]"
  | exp_to_string (APPLY(ident, arg_list)) =
      "[apply " ^ exp_to_string ident ^ " args: " ^
        String.concat (List.map (fn arg => ((exp_to_string arg) ^ " "))
        arg_list) ^ "]"

fun print_def (VAL(ident, exp)) =
  (print ("(val " ^ ident ^ " " ^ exp_to_string exp ^ ")\n"))
  | print_def (EXP(exp)) = print ((exp_to_string exp) ^ ")\n")
  | print_def (DEFINE(ident, _)) =
  print ("(define " ^ ident ^ ")\n")

type error_message = string

exception VariableNotBound
exception UndefinedMethod
exception InvalidMethodName
exception MismatchFunctionArity
exception TypeError
exception DivideByZero
exception UnexpectedRuntimeError

datatype runtime_Error = VAR_NOT_BOUND of error_message
                       | INVALID_METHOD of error_message
                       | UNDEFINED_METHOD of error_message
                       | MISMATCH_ARITY of error_message
                       | TYPE_ERROR of error_message
                       | DIV_BY_ZERO of error_message
                       | UNEXPECTED of error_message

fun raise_runtime_error error =
  let
    val runtime_err_msg = "Runtime error:\n\t - \""
    fun get_msg msg = runtime_err_msg ^ msg ^ "\"\n"
    fun handle_error (VAR_NOT_BOUND(msg)) =
          (print (get_msg msg); raise VariableNotBound)
      | handle_error (INVALID_METHOD(msg)) =
          (print (get_msg msg); raise InvalidMethodName)
      | handle_error (UNDEFINED_METHOD(msg)) =
          (print (get_msg msg); raise UndefinedMethod)
      | handle_error (MISMATCH_ARITY(msg)) =
          (print (get_msg msg); raise MismatchFunctionArity)
      | handle_error (TYPE_ERROR(msg)) =
          (print (get_msg msg); raise TypeError)
      | handle_error (DIV_BY_ZERO(msg)) =
          (print (get_msg msg); raise DivideByZero)
      | handle_error (UNEXPECTED(msg)) =
          (print (get_msg msg); raise UnexpectedRuntimeError)
  in handle_error error
  end

fun get_param_list_from_closure (CLOSURE(((ident_list, _), _))) = ident_list
  | get_param_list_from_closure _ =
      raise_runtime_error
        (UNEXPECTED("Unexpected error while getting parameter list"))
fun get_body_from_closure (CLOSURE(((_, body), _))) = body
  | get_body_from_closure _ =
      raise_runtime_error
        (UNEXPECTED("Unexpected error while getting closure body"))

fun get_env_from_closure (CLOSURE((_, env))) = env
  | get_env_from_closure _ =
      raise_runtime_error
        (UNEXPECTED("Unexpected error while getting closure environment"))


(*datatype env = ENV of (identifier * value) list*)
type env = (identifier * value) list

(*val init_env = (ENV([]))*)
val init_env = [] : env

fun combine_envs env1 env2 = env1 @ env2

fun print_all_env (xs: env) =
  (print "=== Environment state: ===\n";
   List.map
     (fn (ident, value) => print ("{" ^ ident ^ ", " ^
                                  (value_to_string value) ^ "}\n"))
     xs;
   print "\n")

fun bind_env key value (xs: env) = (key, value)::xs

fun find_env key [] =
      raise_runtime_error (VAR_NOT_BOUND("Var \"" ^ key ^ "\" not bound"))
  | find_env key ((ident, value)::xs) =
      if key = ident then value
      else find_env key xs

fun eval_primitive op_str exp_list env =
      let
        val list_len = List.length exp_list
        val arity = (case (List.find (fn ((oper, ar)) => oper = op_str)
        primitive_funcs_arity)
                        of
                         SOME((found_op, arity)) => arity
                       | NONE =>
                           raise_runtime_error
                            (UNDEFINED_METHOD("Unrecognized primitive \"" ^
                                            op_str ^ "\"")))
        val arity_error = fn (oper) =>
          raise_runtime_error
            (MISMATCH_ARITY("Mismatch primitive arity with: \"" ^
                            oper ^ "\""))
        val type_error = fn oper =>
          raise_runtime_error
            (TYPE_ERROR("Invalid types to primitive function: \"" ^
                        oper ^ "\""))

        fun print_line str = print (str ^ "\n")
        fun compute_primitive "=" [x, y] env = ((BOOL(x = y)), env)
          | compute_primitive "+" [(NUM(x)), (NUM(y))] env = ((NUM(x + y)), env)
          | compute_primitive "+" [x, y] env = type_error "+"
          | compute_primitive "-" [(NUM(x)), (NUM(y))] env = ((NUM(x - y)), env)
          | compute_primitive "-" [x, y] env = type_error "-"
          | compute_primitive "*" [(NUM(x)), (NUM(y))] env = ((NUM(x * y)), env)
          | compute_primitive "*" [x, y] env = type_error "*"
          | compute_primitive "/" [(NUM(x)), (NUM(y))] env =
              if y = 0 then
                raise_runtime_error (DIV_BY_ZERO("Divide by zero error"))
              else ((NUM(round(real(x) / real(y)))), env)
          | compute_primitive "print" [NIL] env = (print_line "()"; (NIL, env))
          | compute_primitive "print" [(BOOL(x))] env =
              (if x then print_line "true"
              else print_line "false";
              ((BOOL(x)), env))
          | compute_primitive "print" [(NUM(x))] env =
              (print_line (Int.toString x); ((NUM(x)), env))
          | compute_primitive "print" [(CLOSURE(s))] env =
              (print_line "{closure}";
                ((CLOSURE(s)), env))
          | compute_primitive "print" _ env = arity_error "print"
          | compute_primitive oper _ env = arity_error oper
        fun eval_args [] env arg_values = (arg_values, env)
          | eval_args (arg::args) env arg_values =
              let val (value, value_state) = eval arg env
              in
                eval_args args value_state (arg_values @ [value])
              end
        (*val (arg_list, arg_env) = eval_args exp_list env []*)
      in
        if op_str = "lambda" then
          let fun get_params_and_body [x] param_list = ((List.rev param_list), x)
                | get_params_and_body ((VAR(x))::xs) param_list =
                    get_params_and_body xs (x::param_list)
                | get_params_and_body (_::xs) param_list = raise Match
                | get_params_and_body [] _ = raise Match
            val (params, body) = get_params_and_body exp_list []
          in
            ((CLOSURE((params, body), env)), env)
          end
        else
          let
            val(arg_list, arg_env) = eval_args exp_list env []
          in compute_primitive op_str arg_list arg_env
          end
      end
and
    eval (LIT(value)) env = (value, env)
  | eval (VAR(ident)) env = ((find_env ident env), env)
  | eval (LAMBDA(lambda)) env = ((CLOSURE(lambda, env)), env)
  | eval (APPLY((VAR(ident)), exp_list)) env = 
      if member_string ident primitive_funcs then
        eval_primitive ident exp_list env
      else
      let
        fun bind_args [] [] env = env
          | bind_args (arg::args) (param::params) env =
              let val (value, value_state) = eval arg env
              in
                bind_args args params (bind_env param value value_state)
              end
          | bind_args _ _ _ =
              raise_runtime_error
                (MISMATCH_ARITY("Mismatch in arity for method \"" ^
                                ident ^ "\""))
        val bound_value = find_env ident env
        (*val (CLOSURE(((ident_list, body), captured_env))) = (case bound_value
        * of*)
        fun user_apply closure exp_list env =
              let
                val ident_list = get_param_list_from_closure closure
                val body = get_body_from_closure closure
                val new_env = bind_args exp_list ident_list env
                val combined =
                  combine_envs new_env (get_env_from_closure closure)
              in eval body combined end

        val (value, _) = (case bound_value of
          (CLOSURE(((ident_list, body), captured_env))) =>
            (user_apply (CLOSURE(((ident_list, body), captured_env))) exp_list
              env)
          | (PRIMITIVE(oper)) => eval_primitive oper exp_list env
          | _ =>
            raise_runtime_error
              (UNDEFINED_METHOD("Method \"" ^ ident ^ "\" not found")))
      in
        (value, env)
      end
  | eval (APPLY(exp, _)) env =
      raise_runtime_error (INVALID_METHOD("Invalid method name: \"" ^
                           exp_to_string exp ^ "\""))

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
          | execute_def (DEFINE((ident, lambda))) env =
              (bind_env
                ident
                (CLOSURE((lambda: (identifier list * exp),
                         env)))
                env)
      in
        List.foldl (fn (def, old_env) => execute_def def old_env) init_env defs
      end
