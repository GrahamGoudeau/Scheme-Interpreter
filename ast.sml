type identifier = string
datatype let_type = LET | LET_STAR | LET_REC

datatype value = NIL
               | BOOL of bool
               | NUM of int
               | CLOSURE of
                   ((identifier list) * exp) * ((identifier * int) list)
               | PRIMITIVE of identifier
               | UNDEFINED
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
   ("if", 3), ("lambda", 2), (">", 2), ("<", 2)]

val reserved_idents = ["if", "lambda"]
val primitive_funcs =
  ["=", "+", "-", "*", "/", "<", ">", "print", "check-expect"]
(*val primitive_funcs = List.map (fn (oper, _) => oper) primitive_funcs_arity*)
val primitive_funcs = reserved_idents @ primitive_funcs

fun member_string (elem:string) (xs:string list) =
  List.exists (fn x => x = elem) xs

fun value_to_string (NIL) = "[value: NIL]"
  | value_to_string (BOOL(true)) = "[value: #t]"
  | value_to_string (BOOL(false)) = "[value: #f]"
  | value_to_string (NUM(int)) = "[value: " ^ (Int.toString int) ^ "]"
  | value_to_string (CLOSURE(lambda, env)) = "[value: closure]"
  | value_to_string (PRIMITIVE(ident)) = "[primitive op: " ^ ident ^ "]"
  | value_to_string UNDEFINED = "[value: <undefined>]"

fun exp_to_string (LIT(value)) = value_to_string value
  | exp_to_string (VAR(var)) = "[var " ^ var ^ "]"
  | exp_to_string (APPLY(ident, arg_list)) =
      "[apply " ^ exp_to_string ident ^ " args: " ^
        String.concat (List.map (fn arg => ((exp_to_string arg) ^ " "))
        arg_list) ^ "]"
  | exp_to_string (LAMBDA(ident_list, exp)) =
      "[lambda (" ^ (String.concat ident_list) ^ ")]"

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
exception ReservedKeyword

datatype runtime_Error = VAR_NOT_BOUND of error_message
                       | INVALID_METHOD of error_message
                       | UNDEFINED_METHOD of error_message
                       | MISMATCH_ARITY of error_message
                       | TYPE_ERROR of error_message
                       | DIV_BY_ZERO of error_message
                       | UNEXPECTED of error_message
                       | RESERVED_KEYWORD of error_message

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
      | handle_error (RESERVED_KEYWORD(msg)) =
          (print (get_msg msg); raise ReservedKeyword)
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

val mem_size = 10000
val memory = Array.fromList (List.tabulate (mem_size, (fn _ => UNDEFINED)))
val mem_undefined_loc = 0

(* return the memory address of the stored value *)
local
    val next_mem = ref 1
    fun new_store_value value = Array.update(memory, !next_mem, value) before
                              next_mem := !next_mem + 1
  in
  fun bind_memory t = !next_mem before new_store_value t
  fun update_memory t index = Array.update(memory, index, t)
  end

type env = (identifier * int) list

val init_env : env = []

fun combine_envs env1 env2 = env1 @ env2

fun print_all_env (xs: env) =
  (print "=== Environment state: ===\n";
   List.map
     (fn (ident, index) => print ("{" ^ ident ^ ", " ^
              (value_to_string (Array.sub(memory, index))) ^ "}\n"))
     xs;
   print "\n")

fun bind_env key value (xs: env) = (key, (bind_memory value))::xs

fun find_env_index key [] =
      raise_runtime_error (VAR_NOT_BOUND("Var \"" ^ key ^ "\" not bound"))
  | find_env_index key ((ident, index)::xs) =
      if key = ident then index
      else find_env_index key xs

fun find_env_val key [] =
      raise_runtime_error (VAR_NOT_BOUND("Var \"" ^ key ^ "\" not bound"))
  | find_env_val key ((ident, index)::xs) =
      if key = ident then Array.sub(memory, index)
      else find_env_val key xs

exception EvalCase
exception ApplyCase
exception ApplyCase2
exception ApplyCase3
exception ApplyCase4
fun type_error oper = 
  raise_runtime_error (TYPE_ERROR("Invalid types to function " ^ oper))
fun arity_error oper =
  raise_runtime_error (MISMATCH_ARITY("Mismatch in arity for function " ^ oper))

fun value_equal(NUM(a), NUM(b)) = a = b
  | value_equal(BOOL(a), BOOL(b)) = a = b
  | value_equal(NIL, NIL) = true
  | value_equal(_, _) = false

fun get_bool_value(BOOL(truth), _) = truth
  | get_bool_value(_, oper) = type_error oper

fun print_ln str = print (str ^ "\n")

fun eval_primitive("+", [x, y], env) =
      let
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) =
              (NUM(a + b), env)
          | apply(f, g) =
              type_error "primitive +"
      in apply(operand1, operand2)
      end
  | eval_primitive("+", _, env) =
      arity_error "primitive +"
  | eval_primitive("-", [x, y], env) =
      let
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) =
              (NUM(a - b), env)
          | apply(f, g) = type_error "primitive -"
      in apply(operand1, operand2)
      end
  | eval_primitive("-", _, env) =
      arity_error "primitive -"
  | eval_primitive("*", [x, y], env) =
      let
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) =
              (NUM(a * b), env)
          | apply(f, g) = type_error "primitive *"
      in apply(operand1, operand2)
      end
  | eval_primitive("*", _, env) =
      arity_error "primitive *"
  | eval_primitive("/", [x, y], env) =
      let
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) =
              if (not (b = 0)) then
                  (NUM(floor(real(a) / real(b))), env)
              else raise_runtime_error
                (DIV_BY_ZERO("Divide by zero error"))
          | apply(f, g) = type_error "primitive /"
      in apply(operand1, operand2)
      end
  | eval_primitive("/", _, env) =
      arity_error "primitive /"
  | eval_primitive("=", [x, y], env) =
      let
        val (operand1, state1) = eval(x, env)
        val (operand2, state2) = eval(y, state1)
        fun apply(NUM(a), NUM(b)) = (BOOL(a = b), state2)
          | apply(BOOL(a), BOOL(b)) = (BOOL(a = b), state2)
          | apply(_, _) = type_error "primitive ="
      in apply(operand1, operand2)
      end
  | eval_primitive(">", [x, y], env) =
      let
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) = (BOOL(a > b), env)
          | apply(_, _) = type_error "primitive >"
      in apply(operand1, operand2)
      end
  | eval_primitive(">", _, env) = arity_error "primitive >"
  | eval_primitive("<", [x, y], env) =
      let
        (* TODO: (< 3 #t) reports type error for '>' *)
        val (operand1, _) = eval(x, env)
        val (operand2, _) = eval(y, env)
        fun apply(NUM(a), NUM(b)) = (BOOL(a < b), env)
          | apply(_, _) = type_error "primitive <"
      in apply(operand1, operand2)
      end
  | eval_primitive("<", _, env) = arity_error "primitive <"
  | eval_primitive("check-expect", [x, y], env) =
      let
        val (result1, _) = eval(x, env)
        val (result2, _) = eval(y, env)
      in
        if value_equal(result1, result2) then
          (print_ln "Test passed"; (NIL, env))
        else
          ((print_ln ("Test failed:\n\t " ^ (value_to_string result1) ^
            " != " ^ (value_to_string result2))); (NIL, env))
      end
  | eval_primitive("print", [x], env) =
      let
        val (result, result_state) = eval(x, env)
        fun handle_print NIL =
              (print_ln "()"; NIL)
          | handle_print UNDEFINED =
              (print_ln "<undefined>"; UNDEFINED)
          | handle_print (BOOL(x)) =
              ((if x then (print_ln "#t") else (print_ln "#f")); (BOOL(x)))
          | handle_print (NUM(x)) =
              if x < 0 then
                (print "-"; print_ln (Int.toString (~x)); (NUM(x)))
              else
                (print_ln (Int.toString x); (NUM(x)))
          | handle_print (CLOSURE(c)) = (print_ln "<lambda>"; (CLOSURE(c)))
          | handle_print (PRIMITIVE(p)) =
              (print_ln ("<primitive " ^ p ^ ">"); (PRIMITIVE(p)))
      in (handle_print result, env)
      end
  | eval_primitive("if", [cond, true_exp, false_exp], env) =
      let
        val (cond_val, _) = eval(cond, env)
        val (result, _) =
          if get_bool_value(cond_val, "if") then
            eval(true_exp, env)
          else
            eval(false_exp, env)
      in (result, env) end
  | eval_primitive("if", _, env) =
      raise_runtime_error(MISMATCH_ARITY("Wrong number of components in "^
        "'if' expression"))
  | eval_primitive(oper, _, _) = (print oper; raise ApplyCase3)

and eval((LIT(v)), env) =
      (v, env)
  | eval(VAR(ident), env) =
      if member_string ident primitive_funcs then (PRIMITIVE(ident), env)
      else (find_env_val ident env, env)
  | eval(APPLY(main_exp, arg_list), env) =
      let
        val (closure, closure_state) = eval(main_exp, env)
        val main_str = exp_to_string main_exp
        fun apply(CLOSURE((ident_list, body), captured_env)) =
            let
              fun bind_args [] [] env _ = env
                | bind_args (a::args) (p::params) env ref_env =
                    let
                      val (value, _) = eval(a, ref_env)
                      val new_state : env = (bind_env p value env)
                    in bind_args args params new_state ref_env
                    end
                | bind_args _ _ _ _ = arity_error (exp_to_string main_exp)
              val bound_env = bind_args arg_list ident_list captured_env env
              val (result, _) = eval(body, bound_env)
            in (result, env)
            end
          | apply(PRIMITIVE(ident)) =
          let
            val (result, res_env) = eval_primitive(ident, arg_list, env)
          in
            (result, res_env)
          end

          | apply(value) = raise_runtime_error
              (INVALID_METHOD("Value " ^ (value_to_string value) ^
                " does not evaluate to a closure"))

      in apply(closure)
      end
  | eval(LAMBDA((ident_list, body)), env) =
      (CLOSURE((ident_list, body), env), env)
fun execute defs =
      let
        fun check_reserved (PRIMITIVE(p)) =
          if member_string p reserved_idents then
            raise_runtime_error(RESERVED_KEYWORD("Reserved keyword: \"" ^
                p ^ "\""))
          else true
          | check_reserved _ = true
        fun execute_def (VAL((ident, exp))) env =
              let
                val (result, new_env) = eval(exp, env)
                val _ = check_reserved result
              in
                (bind_env ident result env)
              end
          | execute_def (EXP(exp)) env =
              let
                val (result, new_env) = eval(exp, env)
                val _ = check_reserved result
              in
                new_env
              end
          | execute_def (DEFINE((ident, lambda))) env =
              let
                val rho' = bind_env ident UNDEFINED env
                val (closure, result_env) = eval(LAMBDA lambda, rho')
                val _ = update_memory closure (find_env_index ident rho')
              in
                rho'
              end
      in
        List.foldl (fn (def, old_env) => execute_def def old_env) init_env defs
      end
