functor Parser_PrefixFn (val prefix: string): PARSER =
struct
  type usage = {name: string, desc: string}
  type 'a flag = {usage: usage, arg: 'a Argument.arg}

  fun toHelpMsg {usage = {name, desc}, arg} =
    let
      open Argument
      val metavar =
        case arg of
          None _ => ""
        | One {metavar, ...} => " <" ^ metavar ^ ">"
        | Optional {metavar, ...} => " [<" ^ metavar ^ ">]"
        | AtLeastOne {metavar, ...} => " <" ^ metavar ^ ">..."
        | Any {metavar, ...} => " [<" ^ metavar ^ ">]..."
    in
      prefix ^ name ^ metavar ^ "\t" ^ desc
    end

  val helpUsage = {name = "help", desc = "Print help"}

  datatype token = Flag of string | Arg of string

  val rec tokenize =
    fn [] => []
     | "--" :: rest => map Arg rest
     | x :: rest =>
      if String.isPrefix prefix x then Flag x :: tokenize rest
      else Arg x :: tokenize rest

  fun toString (Arg a) = a
    | toString (Flag f) = f

  fun splitOnArg (Arg v :: rest) =
        let val (l, r) = splitOnArg rest
        in (v :: l, r)
        end
    | splitOnArg otherwise = ([], otherwise)

  open Argument
  val matchArg =
    fn (None action, args) => (action (), args)
     | (One {action, ...}, Arg a :: rest) => (action a, rest)
     | (Optional {action, ...}, Arg a :: rest) => (action (SOME a), rest)
     | (Optional {action, ...}, args) => (action NONE, args)
     | (Any {action, ...}, args) => let val (l, r) = splitOnArg args
                                    in (action l, r)
                                    end
     | (AtLeastOne {action, ...}, Arg v :: rest) =>
      let val (l, r) = splitOnArg rest
      in (action (v :: l), r)
      end
     | _ => raise Fail "arity"

  fun match {usage = {name, desc}, arg} =
    fn [] => NONE
     | Arg a :: rest => NONE
     | Flag other :: rest =>
      if prefix ^ name = other then SOME (matchArg (arg, rest)) else NONE
end
