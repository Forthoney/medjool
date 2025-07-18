signature COMMAND =
sig
  type ret
  val helpMsg: string
  val parse: string list -> ret list
  val run: string list -> ret list
end

functor CommandFn
  (structure Flag: FLAG type a val desc: string val flags: a Flag.t list):
  COMMAND =
struct
  type ret = a
  exception Help

  val help = {usage = Flag.helpUsage, arg = Argument.None (fn () => raise Help)}

  val helpMsg = String.concatWith "\n"
    (desc :: map (fn fl => "  " ^ Flag.toHelpMsg fl) (flags @ [help]))

  fun parsePartial args =
    let
      val args = Flag.Token.tokenize args
      fun loop acc =
        fn (_, []) => (rev acc, [])
         | ([], rest) => (rev acc, rest)
         | (flag :: rest, args) =>
          case Flag.search flag args of
            (SOME action, args) => loop (action :: acc) (flag :: rest, args)
          | (NONE, args) => loop acc (rest, args)
      val (actions, remaining) = loop [] (help :: flags, args)
    in
      (List.map (fn action => action ()) actions, remaining)
    end

  fun parse args =
    case parsePartial args of
      (actions, []) => actions
    | (actions, arg :: _) => raise Fail ("unmatched " ^ Flag.Token.toString arg)

  fun run args =
    let
      val eprint = print
    in
      parse args
      handle
        Help => (print (helpMsg ^ "\n"); OS.Process.exit OS.Process.success)
      | Argument.Conversion {expected, actual} =>
          ( TextIO.output
              ( TextIO.stdErr
              , "Expected argument of type " ^ expected
                ^ ", but found argument \"" ^ String.toString actual ^ "\"\n"
              )
          ; OS.Process.exit OS.Process.failure
          )
    end
end
