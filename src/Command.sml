functor CommandFn
  (structure Token: TOKEN
   structure Flag: FLAG_CONVENTION

   type action
   val desc: string
   val flags: action Flag.flag list
   val anonymous: action Argument.arg):
sig
  exception Help
  val helpMsg: string
  (* Parse all arguments and return the results of actions. Raises and does NOT handle errors. *)
  val parse: Token.token list -> action list
  (* Parse as many arguments as possible and return the results of actions. Raises and does NOT handle errors.
     The nuance between parse and parsePartial is that parse considered unmatched arguments as errors *)
  (* Parse all arguments and return the results of actiosn. Gracefully handles errors and exits *)
  val run: string list -> action list
end =
struct
  exception Help

  val help = {usage = Flag.helpUsage, arg = Argument.None (fn () => raise Help)}

  val helpMsg = String.concatWith "\n"
    (desc :: map (fn fl => "  " ^ Flag.toHelpMsg fl) (flags @ [help]))

  fun parse args =
    let
      fun search flag =
        Token.search (Flag.match flag) (#arg flag)
      fun loop acc =
        fn (_, []) => (acc, [])
         | ([], rest) => (acc, rest)
         | (flag :: rest, args) =>
          case search flag args of
            SOME (action, args) => loop (action :: acc) (flag :: rest, args)
          | NONE => loop acc (rest, args)
      val (actions, remaining) = loop [] (help :: flags, args)
    in
      case Token.match (anonymous, remaining) of
        (action, []) => map (fn f => f ()) (action :: actions)
      | _ => raise Fail "unmatched"
    end

  fun run args =
    let
      fun fail msg =
        (TextIO.output (TextIO.stdErr, msg); OS.Process.exit OS.Process.failure)
    in
      parse (Token.tokenize args)
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
