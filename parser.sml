(* tuples of: unparsed text, line, column *)
datatype ParseState = STATE of char list * int * int

exception TabCharacterFound of string

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

