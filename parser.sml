(*TODO: remove "fail" conditions from functions;
* add try_parse_* functions, and let main ones fail
*)

val do_test = false

(* tuples of: unparsed text, line, column *)
type line = int
type column = int
type text = char list
datatype ParseState = STATE of text * line * column

fun init_state text = (STATE(text, 1, 1))

fun get_unparsed_text (STATE(text, _, _)) = text
fun get_line_num (STATE(_, l, _)) = l
fun get_col_num (STATE(_, _, c)) = c

exception TestFailed
exception TabCharacterFound
exception LiteralNotFound
exception ExpectedIdentifier
exception ExpectedExpression
exception ExpectedDefinition
exception ExpectedInt
exception ExpectedBool
exception ExpectedLiteral
exception UnexpectedException

datatype syntax_error = TAB of error_message
               | LIT_NOT_FOUND of error_message
               | EXPECTED_IDENT of error_message
               | TEST_FAILED of error_message
               | EXPECTED_EXPR of error_message
               | EXPECTED_DEF of error_message
               | EXPECTED_INT of error_message
               | EXPECTED_BOOL of error_message
               | EXPECTED_LIT of error_message
               | UNEXPECTED

(* expects a list of SOME(chars), returns a list of just
 * the chars.  Raises exception if there is a NONE in the list *)
fun strip_option_list [] = []
  | strip_option_list ((SOME(c)::cs)) = (c::(strip_option_list cs))
  | strip_option_list (NONE::cs) = raise Match

fun raise_syntax_error (STATE(_, l, c)) error =
  let
    val parse_err_msg = "Error during parsing at line: " ^
                        (Int.toString l) ^ ", column: " ^
                        (Int.toString c) ^ "\n\t - \""
    fun get_msg msg = parse_err_msg ^ msg ^ "\"\n"
    fun handle_error (TAB(msg)) =
          (print (get_msg msg); raise TabCharacterFound)
      | handle_error (LIT_NOT_FOUND(msg)) =
          (print (get_msg msg); raise LiteralNotFound)
      | handle_error (EXPECTED_IDENT(msg)) =
          (print (get_msg msg); raise ExpectedIdentifier)
      | handle_error (TEST_FAILED(msg)) =
          (print (get_msg msg); raise TestFailed)
      | handle_error (EXPECTED_EXPR(msg)) =
          (print (get_msg msg); raise ExpectedExpression)
      | handle_error (EXPECTED_DEF(msg)) =
          (print (get_msg msg); raise ExpectedDefinition)
      | handle_error (EXPECTED_INT(msg)) =
          (print (get_msg msg); raise ExpectedInt)
      | handle_error (EXPECTED_BOOL(msg)) =
          (print (get_msg msg); raise ExpectedBool)
      | handle_error (EXPECTED_LIT(msg)) =
          (print (get_msg msg); raise ExpectedLiteral)
      | handle_error (UNEXPECTED) =
          (print (get_msg "Unexpected error occurred"); raise UnexpectedException)

  in
    handle_error error
  end


fun strip_option _ (SOME(c)) = c
  | strip_option state NONE = raise_syntax_error state UNEXPECTED

  (* should rewrite as general strip comment function *)
fun skip_comment (STATE([], l, c)) = STATE([], l, c)
  | skip_comment (STATE((#";"::cs), l, c)) =
    let
      fun skip_until_newline (STATE([], l, c)) = STATE([], l, c)
        | skip_until_newline (STATE((#"\n" :: cs), l, c)) = STATE(cs, l + 1, 1)
        | skip_until_newline (STATE((c :: cs), l, col)) =
            skip_until_newline (STATE(cs, l, (col + 1)))
    in
      skip_until_newline (STATE(cs, l, (c+1)))
    end
  | skip_comment state = state

fun skip_whitespace (STATE([], l, c)) = STATE([], l, c)
  | skip_whitespace (STATE((#" "::cs), l, c)) =
      skip_whitespace (STATE(cs, l, (c + 1)))
  | skip_whitespace (STATE((#"\n"::cs), l, c)) =
      skip_whitespace (STATE(cs, (l + 1), 1))
  | skip_whitespace (STATE((#"\t"::cs), l, c)) =
      let
        val tab_line_str = Int.toString l
        val tab_line_col = Int.toString c
      in
        raise_syntax_error (STATE(cs, l, c)) (TAB("Tab char found at line: " ^
                         tab_line_str ^
                         ", column: " ^
                         tab_line_col))
      end
  | skip_whitespace state = state

(* expects a string literal as a string *)
fun parse_str_literal original_state literal fail =
  let
    val lit = String.explode literal

    (* produces as much of the literal in acc as possible *)
    fun consume_literal (STATE((t::ts), line, col)) (l :: ls) acc =
      if t = l then consume_literal (STATE(ts, line, (col + 1))) ls (l::acc)
      else (false, original_state, (List.rev (t::acc)))

      | consume_literal (STATE([], line, col)) (l :: ls) acc =
          (false, original_state, (List.rev acc))
      | consume_literal state [] acc = (true, state, (List.rev acc))

    val (result, new_state, acc) = consume_literal original_state lit []

  in
    if result then (SOME(lit), new_state)
    else
      if fail then
        raise_syntax_error original_state (LIT_NOT_FOUND("Literal \"" ^
                                  literal ^
                                  "\" expected but not found"))
      else (NONE, original_state)
  end

fun parse_open_paren state fail =
  parse_str_literal state "(" fail

fun parse_close_paren state fail =
  parse_str_literal state ")" fail

fun member_char (elem : char) (xs : char list) = List.exists (fn x => x = elem) xs
fun member_string (elem:string) (xs:string list) =
  List.exists (fn x => x = elem) xs

fun char_list_is_int [] = false
  | char_list_is_int (c::cs) =
    let
      val rest_is_int = List.all (fn d => Char.isDigit d) cs
    in ((Char.isDigit c) andalso rest_is_int) orelse
       ((c = #"-") andalso rest_is_int)
    end

(* reflects Ramsey 137 of Build, Prove, Compare *)
(* returns (option(ident), state) *)
fun parse_identifier (STATE([], line, col)) fail =
  if fail then raise_syntax_error (STATE([], line, col))
                           (EXPECTED_IDENT("Expected identifier"))
  else (NONE, (STATE([], line, col)))
  | parse_identifier state fail =
  let
    val invalid_chars = [#"(", #")", #";"]
    fun accumulate (STATE([], line, col)) acc =
          ((List.rev acc), (STATE([],line, col)))
      | accumulate (STATE((c::cs), line, col)) acc =
          if (not (Char.isSpace c)) andalso
            (Char.isAlpha c orelse (not (member_char c invalid_chars))) then
            accumulate (STATE(cs, line, col + 1)) (c::acc)
          else ((List.rev acc), (STATE((c::cs), line, col)))
    val (ident, ident_state) = accumulate state []
  in
    if ident = [] orelse (char_list_is_int ident) then
      if fail then
        raise_syntax_error ident_state (EXPECTED_IDENT("Expected identifier"))
      else
        (NONE, ident_state)
    else
      (SOME(ident), ident_state)
  end

fun try_parse_identifier state =
      let
        val (ident, ident_state) = parse_identifier state false
      in (not (ident = NONE))
      end

fun parse_integer (STATE([], line, col)) fail =
  if fail then
    raise_syntax_error (STATE([], line, col)) (EXPECTED_INT("Expected int"))
  else (NONE, (STATE([], line, col)))
  | parse_integer (STATE((c::cs), line, col)) fail =
let
  fun accumulate (STATE([], line, col)) acc =
        (List.rev acc, (STATE([], line, col)))
    | accumulate (STATE((c::cs), line, col)) acc =
        if Char.isDigit c then
          accumulate (STATE(cs, line, col + 1)) (c::acc)
        else (List.rev acc, (STATE((c::cs), line, col)))
  val (integer, int_state) = accumulate (STATE((cs), line, col + 1)) []
  val full_int = (c::integer)
in
  if (not (char_list_is_int full_int)) then
    if fail then
      raise_syntax_error (STATE((c::cs), line, col)) (EXPECTED_INT("Expected int"))
    else (NONE, (STATE((c::cs), line, col)))
  else
    let
      val converted_int =
        strip_option int_state (Int.fromString (String.implode full_int))
    in
      (SOME(NUM(converted_int)), int_state)
    end
end

fun try_parse_integer state =
let
  val (integer, int_state) = parse_integer state false
in
  not (integer = NONE)
end

fun try_parse_boolean state =
    let
      val (true_lit, true_state) =
        parse_str_literal state "#t" false
      val (false_lit, false_state) =
        parse_str_literal state "#f" false
    in
      (not (true_lit = NONE)) orelse (not (false_lit = NONE))
    end

fun parse_boolean state fail =
    let
      val (true_lit, true_state) =
        parse_str_literal state "#t" false
      val (false_lit, false_state) =
        parse_str_literal state "#f" false
        (*
      val (bool_lit, bool_state) =
        if (not (true_lit = NONE)) then (true, true_state)
        else if (not (false_lit = NONE)) then (false, false_state)
        else
            raise_syntax_error state (EXPECTED_BOOL("Expected boolean"))
            *)
    (*in ((BOOL(bool_lit)), bool_state)*)
    in
      if (not (true_lit = NONE)) then ((SOME(BOOL(true))), true_state)
      else if (not (false_lit = NONE)) then ((SOME(BOOL(false))), false_state)
      else if fail then raise_syntax_error state (EXPECTED_BOOL("Expected boolean"))
      else (NONE, state)
    end

    (*
fun parse_s_exp state =
let
  val is_literal = try_parse_grammar_literal state
  val is_ident = try_parse_identifier state
  val (is_sub_s_exp_option, sub_s_exp_state) = parse_open_paren state false
  val is_sub_s_exp = (not (is_sub_s_exp_option = NONE))
  val (ast_node, ast_state) =
    if is_literal then
      parse_grammar_literal state true
    else if is_ident then
      parse_identifier state true
    else if is_sub_s_exp then
      *)

fun parse_grammar_literal state fail =
let
  val is_integer = try_parse_integer state
  val is_boolean = try_parse_boolean state
  val (quote_lit, quote_state) = parse_str_literal state "'" false
  val is_s_exp = (not (quote_lit = NONE))
  val (ast_node, ast_state) =
    if is_integer then
      parse_integer state true
    else if is_boolean then
      parse_boolean state true
      (*
    else if is_s_exp then
      parse_s_exp state
      *)
    else(* if fail then*)
      raise_syntax_error state (EXPECTED_LIT("Expected literal (int, bool, or S-exp)"))
         (*else (NONE, state)*)
in
  0
end

fun try_parse_grammar_literal state = 3

(* returns (VAR(...), new state) *)
fun parse_expression state =
  let val skip_ws_state = skip_whitespace state
      val (open_paren, open_state) = parse_open_paren skip_ws_state false
  in
    if try_parse_identifier skip_ws_state andalso
        (not (try_parse_boolean skip_ws_state)) then
      let val (ident_option, ident_state) = parse_identifier skip_ws_state false
          val ident = strip_option ident_state ident_option
      in ((VAR(String.implode ident)), ident_state)
      end
    else if try_parse_integer skip_ws_state then
      let val (num_option, int_state) = parse_integer skip_ws_state true
          val num = strip_option int_state num_option
      in ((LIT(num)), int_state)
      end
    else if try_parse_boolean skip_ws_state then
      let val (boolean_option, bool_state) = parse_boolean skip_ws_state true
          val boolean = strip_option bool_state boolean_option
      in
        ((LIT(boolean), bool_state))
      end
    else
      raise_syntax_error
        skip_ws_state
        (EXPECTED_EXPR("Expected expression"))
  end

(* returns true if the next tokens are "(define" *)
fun try_parse_val_or_func_definition val_or_func_str state =
  let
    val skip_ws_state1 = skip_whitespace state
    val (open_paren, open_paren_state) =
      parse_open_paren skip_ws_state1 false
    val skip_ws_state2 = skip_whitespace open_paren_state
    val (define_lit, define_lit_state) =
      if open_paren = NONE then (NONE, state)
      else parse_str_literal skip_ws_state2 val_or_func_str false

  in (not (define_lit = NONE))
  end

fun try_parse_val_definition state =
  try_parse_val_or_func_definition "val" state

fun try_parse_func_definition state =
  try_parse_val_or_func_definition "define" state

(* returns (VAL(ident, exp), state) *)
fun parse_val_definition state =
    let
      val (open_paren, open_paren_state) =
        parse_open_paren state true
      val skip_ws_state1 = skip_whitespace open_paren_state
      val (define_lit, define_lit_state) =
        parse_str_literal skip_ws_state1 "val" true
      val skip_ws_state2 = skip_whitespace define_lit_state
      val (ident_option, ident_state) =
        parse_identifier skip_ws_state2 true
      val ident = strip_option ident_state ident_option
      val skip_ws_state3 = skip_whitespace ident_state
      val (expr, expr_state) =
        parse_expression skip_ws_state3
      val skip_ws_state4 = skip_whitespace expr_state
      val (close_paren, close_paren_state) =
        parse_close_paren skip_ws_state4 true
    in ((VAL(String.implode ident, expr)), close_paren_state)
    end

fun parse_func_definition state =
    let
      val (open_paren, open_paren_state) =
        parse_open_paren state true
      val skip_ws_state1 = skip_whitespace open_paren_state
      val (define_lit, define_lit_state) =
        parse_str_literal skip_ws_state1 "define" true
      val skip_ws_state2 = skip_whitespace define_lit_state
      val (ident_option, ident_state) =
        parse_identifier skip_ws_state2 true
      val ident = strip_option ident_state ident_option
      val skip_ws_state3 = skip_whitespace ident_state
      val (open_paren2, open_paren_state2) =
        parse_open_paren skip_ws_state3 true
      fun accumulate_params (STATE([], line, col)) params =
        (List.rev params, (STATE([], line, col)))
        | accumulate_params (STATE((c::cs), line, col)) params =
            if Char.isSpace c then accumulate_params (STATE(cs, line, col + 1)) params
            else if c = #")" then (List.rev params, (STATE(cs, line, col + 1)))
            else
          let
            val skip_ws_state = skip_whitespace (STATE((c::cs), line, col))
            val (ident_option, ident_state) =
              parse_identifier skip_ws_state true
            val ident = strip_option ident_state ident_option
          in accumulate_params ident_state (ident::params)
          end
      val (params, param_state) = accumulate_params open_paren_state2 []
      val skip_ws_state4 = skip_whitespace param_state
      val (exp, exp_state) =
        parse_expression skip_ws_state4
      val skip_ws_state5 = skip_whitespace exp_state
      val (closed_paren, closed_paren_state) =
        parse_close_paren skip_ws_state5 true
    in ((DEFINE(String.implode ident,
                (((List.map (fn s => String.implode s) params): identifier list),
                exp))),
        closed_paren_state)
    end

(* returns (def, state) *)
fun parse_def state =
      let
        val skipped_ws_state = skip_whitespace state
        val is_val_def = try_parse_val_definition skipped_ws_state
        val is_func_def = try_parse_func_definition skipped_ws_state
        (* val is_check-expect *)
        val (ast_node, ast_state) =
          if is_val_def then parse_val_definition skipped_ws_state
          else if is_func_def then parse_func_definition skipped_ws_state
          else
            let
              val (expr, expr_state) = parse_expression skipped_ws_state
            in
              ((EXP(expr)), expr_state)
            end
      in (ast_node, ast_state)
      end


(* expects a list of characters, not a string *)
fun parse state =
  let
    val skipped = skip_whitespace state
  in
    parse_def skipped
  end

fun test_suite do_run =
  if do_run then
    let
      val default_state = (STATE([], 1, 1))
      val (test_ident1, _) = parse_identifier (STATE((String.explode "    \n de.finex0? := 4;\n"), 1,
        1)) true
      val (test_ident2, _) = parse_identifier (STATE((String.explode "  (let"), 1, 1))
        false
    in
      (if not (test_ident1 = (SOME([#"d", #"e"]))) then
        raise_syntax_error default_state (TEST_FAILED("Identifier test 1 fail"))
       else 1;
       if not (test_ident2 = NONE) then
         raise_syntax_error default_state (TEST_FAILED("Ident test 2 fail"))
       else 1)
    end
 else 1

(*
val x = test_suite do_test
val s = String.explode
val a = parse_boolean (STATE(s "#t   \n", 1, 1))
val b = parse_def (STATE(s "(define f (z) #t)", 1, 1))
*)
