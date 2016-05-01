exception ProblemWithFile
fun file_problem name =
  (print ("Problem reading from file '" ^ name ^ "'\n"); raise ProblemWithFile)

fun get_chars_from_filestream in_file filename =
  let
    val default_state = (STATE([], ~1, ~1))
    fun accumulate NONE = []
        | accumulate c = ((strip_option default_state c) :: (accumulate (TextIO.input1 in_file)))
  in accumulate (TextIO.input1 in_file)
      handle Io => file_problem filename
  end

exception ImproperUsage

fun print_usage () =
  print "Usage: ./{script} filename [-gc=no (disable garbage collection)]\n\n"

fun parse_args [] = (print_usage(); raise ImproperUsage)
  | parse_args [x] = (x, true)
  | parse_args [x, "-gc=no"] = (x, false)
  | parse_args _ = (print_usage(); raise ImproperUsage)

fun parse_loop text =
let
  fun accumulate_defs (STATE([], line, col)) defs =
    List.rev defs
    | accumulate_defs state defs =
    let 
        val (def, def_state) = parse state
        val skip_ws_state = skip_whitespace def_state
    in
      accumulate_defs skip_ws_state (def::defs)
    end
in
  accumulate_defs (init_state text) []
end


fun main () =
  let val sys_argv = CommandLine.arguments()
      val (input_filename, do_garbage_collect) = parse_args sys_argv
      val input_stream = TextIO.openIn input_filename
      val text = get_chars_from_filestream input_stream
      val _ = TextIO.closeIn input_stream
      val defs = parse_loop text
      val final_env = execute defs do_garbage_collect
  in ()
  end

val _ = main ()
