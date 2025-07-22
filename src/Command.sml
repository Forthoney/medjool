functor CommandFn
  (structure Token: TOKEN
   structure Flag: FLAG

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

  fun parse toks =
    let
      val flags = help :: flags
      fun findMatch _ [] = NONE
        | findMatch toks (f :: fs) =
          case Token.match (Flag.match f) (#arg f) toks of
            SOME v => SOME v
          | NONE => findMatch toks fs
      fun loop acc [] = acc
        | loop (actions, seen) (toks as t :: ts) =
          case findMatch toks flags of
            SOME (action, rest) => loop (action :: actions, seen) rest
          | NONE => loop (actions, t :: seen) ts
      val (actions, remaining) = loop ([], []) toks
    in
      case Token.matchArg (anonymous, rev remaining) of
        (action, []) => action :: actions
      | (_, remaining :: _) => raise Fail ("unmatched " ^ Token.toString remaining)
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
