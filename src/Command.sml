functor CommandFn(Opts: COMMAND_OPTS):
sig
  exception Help
  val helpMsg: string
  (* Parse all arguments and return the results of actions. Raises and does NOT handle errors. *)
  val parse: Opts.Flag.token list -> Opts.action list
  (* Parse as many arguments as possible and return the results of actions. Raises and does NOT handle errors.
     The nuance between parse and parsePartial is that parse considered unmatched arguments as errors *)
  val parsePartial: Opts.Flag.token list
                    -> Opts.action list * Opts.Flag.token list
  (* Parse all arguments and return the results of actiosn. Gracefully handles errors and exits *)
  val run: string list -> Opts.action list
end =
struct
  exception Help
  structure Flag = Opts.Flag

  val help = {usage = Flag.helpUsage, arg = Argument.None (fn () => raise Help)}

  val helpMsg = String.concatWith "\n"
    (Opts.desc :: map (fn fl => "  " ^ Flag.toHelpMsg fl) (Opts.flags @ [help]))

  fun parsePartial args =
    let
      fun loop acc =
        fn (_, []) => (rev acc, [])
         | ([], rest) => (rev acc, rest)
         | (flag :: rest, args) =>
          case Flag.search flag args of
            (SOME action, args) => loop (action :: acc) (flag :: rest, args)
          | (NONE, args) => loop acc (rest, args)
      val (actions, remaining) = loop [] (help :: Opts.flags, args)
    in
      (List.map (fn action => action ()) actions, remaining)
    end

  fun parse args =
    case parsePartial args of
      (actions, []) => actions
    | (actions, arg :: _) => raise Fail ("unmatched " ^ Flag.tokToString arg)

  fun run args =
    let
      fun fail msg =
        (TextIO.output (TextIO.stdErr, msg); OS.Process.exit OS.Process.failure)
    in
      parse (Flag.tokenize args)
      handle
        Help => (print (helpMsg ^ "\n"); OS.Process.exit OS.Process.success)
      | Argument.Conversion {expected, actual} =>
          fail
            ("Expected argument of type " ^ expected ^ ", but found argument \""
             ^ String.toString actual ^ "\"\n")

      | Argument.Validation {condition, actual} =>
          fail
            (condition ^ ", but found argument \"" ^ String.toString actual
             ^ "\"\n")
    end
end
