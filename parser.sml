(*TODO: remove "fail" conditions from functions;
* add try_parse_* functions, and let main ones fail
*)

(* tuples of: unparsed text, line, column *)
type line = int
type column = int
type text = char list
datatype ParseState = STATE of text * line * column

fun get_unparsed_text (STATE(text, _, _)) = text
fun get_line_num (STATE(_, l, _)) = l
fun get_col_num (STATE(_, _, c)) = c

exception TestFailed
exception TabCharacterFound
exception LiteralNotFound
exception ExpectedIdentifier
exception ExpectedExpression
type error_message = string
datatype error = TAB of error_message
               | LIT_NOT_FOUND of error_message
               | EXPECTED_IDENT of error_message
               | TEST_FAILED of error_message
               | EXPECTED_EXPR of error_message

(* expects a list of SOME(chars), returns a list of just
 * the chars.  Raises exception if there is a NONE in the list *)
fun strip_option_list [] = []
  | strip_option_list ((SOME(c)::cs)) = (c::(strip_option_list cs))
  | strip_option_list (NONE::cs) = raise Match

fun strip_option_char (SOME(c)) = c
  | strip_option_char _ = raise Match

fun raise_error (STATE(_, l, c)) error =
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
  in
    handle_error error
  end

fun skip_comment (STATE([], l, c)) = STATE([], l, c)
  | skip_comment (STATE((#";"::cs), l, c)) =
    let
      fun skip_until_newline (STATE([], l, c)) = STATE([], l, c)
        | skip_until_newline (STATE((#"\n" :: cs), l, c)) = STATE(cs, l, c)
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
        raise_error (STATE(cs, l, c)) (TAB("Tab char found at line: " ^
                         tab_line_str ^
                         ", column: " ^
                         tab_line_col))
      end
  | skip_whitespace state = state

(* expects a string literal as a string *)
fun parse_literal original_state literal fail =
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
        raise_error original_state (LIT_NOT_FOUND("Literal \"" ^
                                  (String.implode lit) ^
                                  "\" expected but not found"))
      else (NONE, original_state)
  end

fun parse_open_paren state fail =
  parse_literal state "(" fail

fun parse_close_paren state fail =
  parse_literal state ")" fail

fun member_char (elem : char) (xs : char list) = List.exists (fn x => x = elem) xs

(* reflects Ramsey 137 of Build, Prove, Compare *)
fun parse_identifier (STATE([], line, col)) fail =
  if fail then raise_error (STATE([], line, col))
                           (EXPECTED_IDENT("Expected identifier"))
  else (NONE, (STATE([], line, col)))
  | parse_identifier state fail =
  let
    val invalid_chars = [#"(", #")", #";"]
    fun accumulate (STATE([], line, col)) acc = ((List.rev acc), (STATE([],
      line, col)))
      | accumulate (STATE((c::cs), line, col)) acc =
          if (not (Char.isSpace c)) andalso
            (Char.isAlpha c orelse (not (member_char c invalid_chars))) then
            accumulate (STATE(cs, line, col + 1)) (c::acc)
          else ((List.rev acc), (STATE((c::cs), line, col)))
    val (ident, ident_state) = accumulate state []
  in
    if ident = [] then
      if fail then
        raise_error ident_state (EXPECTED_IDENT("Expected identifier"))
      else
        (NONE, ident_state)
    else
      (SOME(ident), ident_state)
  end
(*
fun parse_identifier (STATE([], line, col)) fail =
  if fail then
    raise_error
      (STATE([], line, col))
      (EXPECTED_IDENT("Expected valid identifier"))
  else
    (NONE, (STATE([], line, col)))
  | parse_identifier (STATE((c::cs), line, col)) fail =
  let
    val accepted_initials = [#"!", #"$", #"%", #"&", #"*", #"/", #":",
                             #"<", #"=", #">", #"?", #"~", #"_", #"^"]

    fun parse_initial (STATE([], line, col)) fail =
          if fail then
            raise_error
              ((STATE([], line, col)))
              (EXPECTED_IDENT("Expected valid identifier, reached EOF"))
          else (NONE, (STATE([], line, col)))
      | parse_initial (STATE((c::cs), line, col)) fail =
          if Char.isAlpha c orelse (List.exists (fn ch => ch = c)
                                    accepted_initials)
          then (SOME(c), (STATE(cs, line, col + 1)))
          else
            if fail then
              raise_error
                (STATE((c::cs), line, col))
                (EXPECTED_IDENT("Expected valid identifier, got invalid first char \"" ^
                  String.implode [c] ^ "\""))
            else (NONE, (STATE((c::cs), line, col)))

    fun parse_subsequent (STATE([], line, col)) fail =
          if fail then
              raise_error
                ((STATE([], line, col)))
                (EXPECTED_IDENT("Expected valid identifier, reached EOF"))
          (*else (NONE, (STATE([], line, col)))*)
          else ([], (STATE([], line, col)))
      | parse_subsequent (STATE((c::cs), line, col)) fail =
          let
            fun parse_all (STATE([], line, col)) acc fail =
                  ((List.rev acc), (STATE([], line, col)))
              | parse_all (STATE((c::cs), line, col)) acc fail =
              let
                val (try_init, init_state) =
                  parse_initial (STATE((c::cs), line, col)) false
              in
              if (not (try_init = NONE)) orelse (Char.isDigit c) then
                parse_all (STATE(cs, line, col + 1)) (c::acc) fail
              else
                if fail then
                raise_error
                  (STATE((c::cs), line, col))
                  (EXPECTED_IDENT("Expected valid identifier, got invalid \"" ^
                  (String.implode [c]) ^ "\""))
                else
                  ((List.rev acc), (STATE((c::cs), line, col)))
              end
          in
            parse_all (STATE((c::cs), line, col)) [] fail
          end
    val skip_ws_state = skip_whitespace(STATE((c::cs), line, col))
    val (init, init_state) = parse_initial skip_ws_state fail
    val (rest, rest_state) =
      if init = NONE then ([], init_state)
      else parse_subsequent init_state false
    val ident = if init = NONE then NONE
                else (SOME((strip_option_char init)::rest))
  in
    if ident = NONE then
      if fail then
        raise_error
          (STATE((c::cs), line, col))
          (EXPECTED_IDENT("Expected valid identifier"))
      else (NONE, (STATE((c::cs), line, col)))
    else
      (ident, rest_state)
  end
  *)

fun try_parse_identifier state =
      let
        val (ident, ident_state) = parse_identifier state false
      in (not (ident = NONE))
      end


fun parse_expression state =
  (SOME(0), state)

fun parse_definition state =
  let
    val skip_ws_state1 = skip_whitespace state
    val (open_paren, open_paren_state) =
      parse_open_paren skip_ws_state1 true
    val skip_ws_state2 =
      skip_whitespace open_paren_state
    val (define_lit, define_lit_state) =
      parse_literal skip_ws_state2 "define" true
    val skip_ws_state3 =
      skip_whitespace define_lit_state
    val (ident, ident_state) =
      parse_identifier skip_ws_state3 true
    val skip_ws_state4 =
      skip_whitespace ident_state
    val (expr, expr_state) =
      parse_expression skip_ws_state4
  in
    (SOME(0), ident_state)
end

(* returns true if the next tokens are "(define" *)
fun try_parse_definition state =
  let
    val skip_ws_state1 = skip_whitespace state
    val (open_paren, open_paren_state) =
      parse_open_paren skip_ws_state1 false
    val skip_ws_state2 = skip_whitespace open_paren_state
    val (define_lit, define_lit_state) =
      if open_paren = NONE then (NONE, state)
      else parse_literal skip_ws_state2 "define" false

  in (not (define_lit = NONE))
  end

fun parse_forms (STATE([], line, col)) = []
  | parse_forms state =
      let
        fun accumulate_forms (STATE([], _, _)) forms =
              List.rev forms
          | accumulate_forms state forms =
          let
            val (def_node, def_state) = parse_definition state
            val (expr_node, expr_state) = (*parse_expression state*)
                  (SOME(0), (STATE([], 1, 1)))
            val (final_node, final_state) = if def_node = NONE then
                                              (expr_node, expr_state)
                                            else (def_node, def_state)
          in
            accumulate_forms final_state (final_node :: forms)
          end
      in
        accumulate_forms state []
      end


(* expects a list of characters, not a string *)
fun parse text =
  let
    val init_state = STATE(text, 1, 1)
    val skipped = skip_whitespace init_state
  in
    (parse_forms init_state;
    print (String.implode (get_unparsed_text skipped)))
  end

val x = parse_identifier (STATE((String.explode "    \n de.finex0? := 4;\n"), 1, 1)) true
val y = try_parse_definition (STATE((String.explode " ( \ndefine  x\n"), 1, 1))
