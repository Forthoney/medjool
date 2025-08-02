functor CommandFn
  (structure Parser: PARSER
   (* The return type of actions *)
   type action
   val desc: string
   val flags: action Parser.flag list
   (* Arguments a command takes without flags preceding it. E.g. rm _FILE_  or cat _FILE_ *)
   val anonymous: action Argument.arg):
sig
  exception Help
  val helpMsg: string
  (* Parse all arguments and return the results of actions. Raises and does NOT handle errors. *)
  val parse: Parser.token list -> action list
  (* Parse all arguments and return the results of actions. Gracefully handles errors and exits *)
  val run: string list -> action list
end =
struct
  exception Help
  exception Unmatched of string

  val help =
    {usage = Parser.helpUsage, arg = Argument.None (fn () => raise Help)}

  val helpMsg =
    let
      val usage =
        let
          val words =
            case flags of
              [] => [CommandLine.name ()]
            | _ => ["[FLAG]...", CommandLine.name ()]
          val words =
            let
              open Argument
            in
              case anonymous of
                None _ => words
              | One {metavar, ...} => metavar :: words
              | Optional {metavar, ...} => ("[" ^ metavar ^ "]") :: words
              | AtLeastOne {metavar, ...} => (metavar ^ "...") :: words
              | Any {metavar, ...} => ("[" ^ metavar ^ "]...") :: words
            end
        in
          String.concatWith " " ("Usage:" :: rev words)
        end
    in
      String.concatWith "\n"
        (usage :: desc
         :: map (fn fl => "  " ^ Parser.toHelpMsg fl) (flags @ [help]))
    end

  fun parse toks =
    let
      val flags = help :: flags
      fun findMatch _ [] = NONE
        | findMatch toks (f :: fs) =
            case Parser.match f toks of
              SOME v => SOME v
            | NONE => findMatch toks fs
      fun loop acc [] = acc
        | loop (actions, seen) (toks as t :: ts) =
            case findMatch toks flags of
              SOME (action, rest) => loop (action :: actions, seen) rest
            | NONE => loop (actions, t :: seen) ts
      val (actions, remaining) = loop ([], []) toks
    in
      case Parser.matchArg (anonymous, rev remaining) of
        (action, []) => action :: actions
      | (_, remaining :: _) => raise Unmatched (Parser.toString remaining)
    end

  fun run args =
    let
      fun fail msg =
        (TextIO.output (TextIO.stdErr, msg); OS.Process.exit OS.Process.failure)
    in
      parse (Parser.tokenize args)
      handle
        Help => (print (helpMsg ^ "\n"); OS.Process.exit OS.Process.success)
      | Unmatched tok =>
          fail ("Unmatched flag or argument: \"" ^ String.toString tok ^ "\"")
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
