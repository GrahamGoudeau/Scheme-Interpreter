fun get_chars_from_filestream in_file =
  let fun accumulate NONE = []
        | accumulate c = (c :: (accumulate (TextIO.input1 in_file)))
      val some_chars = accumulate (TextIO.input1 in_file)
  in List.map (fn SOME c => c | NONE => #"0") some_chars
  end

exception ImproperUsage

fun print_usage () = print "Usage: ./{script} [filename]\n\n"

fun parse_args [] = (print_usage(); raise ImproperUsage)
  | parse_args [x] = x
  | parse_args _ = (print_usage(); raise ImproperUsage)

fun main () =
  let val sys_argv = CommandLine.arguments()
      val input_filename = parse_args sys_argv
      val input_stream = TextIO.openIn input_filename
      val text = get_chars_from_filestream input_stream
    val _ = TextIO.closeIn input_stream
    val _ = parse text
  in text
  end

val t = main()
val y = get_chars_from_filestream (TextIO.openIn "b.txt")
val z = CommandLine.arguments()
