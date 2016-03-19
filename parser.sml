(* tuples of: unparsed text, line, column *)
datatype ParseState = STATE of char list * int * int

fun get_unparsed_text (STATE(text, _, _)) = text
fun get_line_num (STATE(_, l, _)) = l
fun get_col_num (STATE(_, _, c)) = c

datatype form = DEFN of defn_type * identifier
and identifier = string
and defn_type = int

exception TabCharacterFound
exception LiteralNotFound
datatype error = TAB of string
               | LIT_NOT_FOUND of string

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

(* expects a string literal as a string; the function will explode it *)
fun parse_literal original_state literal fail =
  let
    val lit = String.explode literal

    fun consume_literal (STATE((t::ts), line, col)) (l :: ls) acc =
      if t = l then consume_literal (STATE(ts, line, (col + 1))) ls (l::acc)
      else (false, original_state, (List.rev acc))
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
                                  "\" expected but not found; " ^
                                  "\"" ^ 
                                  (String.implode acc) ^
                                  "\" found instead"))
      else (NONE, original_state)
  end

fun parse_open_paren state fail =
  parse_literal state "(" fail

(*fun parse_definition _ = (SOME(0), STATE([], 1, 1))*)
fun parse_definition state fail =
  let
    val skip_ws_state1 = skip_whitespace state
    val (open_paren, open_paren_state) =
      parse_open_paren skip_ws_state1 fail
    val skip_ws_state2 =
      skip_whitespace open_paren_state
    val (define_lit, define_lit_state) =
      parse_literal skip_ws_state2 "define" fail
  in
    (SOME(0), define_lit_state)
end

(*fun parse_expression _ = (SOME(0), STATE([], 1, 1))*)

fun parse_forms (STATE([], line, col)) = []
  | parse_forms state =
      let
        fun accumulate_forms (STATE([], _, _)) forms =
              List.rev forms
          | accumulate_forms state forms =
          let
            val (def_node, def_state) = parse_definition state true
            val (expr_node, expr_state) = (*parse_expression state*)
                  (SOME(0), STATE([], 1, 1))
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

val x = parse_definition (STATE((String.explode "( define x0 := 4;\n"), 1, 1)) true
