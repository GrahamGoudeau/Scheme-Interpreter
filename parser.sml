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
        raise TabCharacterFound(String.concat["Tab char found at line: ",
                                              tab_line_str,
                                              ", column: ",
                                              tab_line_col])
      end
  | skip_whitespace state = state

fun parse_forms (STATE([], line, col)) = STATE([], line, col)
  | parse_forms (STATE((c::cs), line, col)) = STATE(cs, line, col)

fun parse text =
  let
    val init_state = STATE(text, 1, 1)
    val skipped = skip_whitespace init_state
  in
    parse_forms init_state
  end

