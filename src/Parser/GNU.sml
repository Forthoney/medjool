structure GNUParser: PARSER =
struct
  type usage = {short: char option, long: string, desc: string}
  type 'a flag = {usage: usage, arg: 'a Argument.arg}

  fun shortToString c = "-" ^ Char.toString c

  fun toHelpMsg {usage = {short, long, desc}, arg} =
    let
      val metavar =
        case (short, arg) of
          (_, Argument.None _) => ""
        | (SOME _, Argument.One {metavar, ...}) => " <" ^ metavar ^ ">"
        | (SOME _, Argument.Optional {metavar, ...}) => " [<" ^ metavar ^ ">]"
        | (SOME _, Argument.AtLeastOne {metavar, ...}) =>
            " <" ^ metavar ^ ">..."
        | (SOME _, Argument.Any {metavar, ...}) => " [<" ^ metavar ^ ">]..."
        | (NONE, Argument.One {metavar, ...}) => "=<" ^ metavar ^ ">"
        | (NONE, Argument.Optional {metavar, ...}) => "[=<" ^ metavar ^ ">]"
        | (NONE, Argument.AtLeastOne {metavar, ...}) => "=<" ^ metavar ^ ">..."
        | (NONE, Argument.Any {metavar, ...}) => "=[<" ^ metavar ^ ">]..."
      val short = Option.getOpt (Option.map shortToString short, "  ")
    in
      short ^ ", " ^ "--" ^ long ^ metavar ^ "\t" ^ desc
    end

  val helpUsage = {short = SOME #"h", long = "help", desc = "Print help"}

  datatype token =
    Flag of string
  | FlagArg of string * string list
  | Arg of string

  open Substring
  val rec tokenize =
    fn [] => []
     | "--" :: xs => map Arg xs
     | "-" :: xs => Arg "-" :: tokenize xs
     | x :: xs =>
      if String.isPrefix "--" x then
        let
          val (flag, arg) = splitl (fn c => c <> #"=") (full x)
        in
          if isEmpty arg then
            Flag x :: tokenize xs
          else
            FlagArg
              ( string flag
              , (String.tokens (fn c => c = #",") o string o triml 1) arg
              ) :: tokenize xs
        end
      else if String.isPrefix "-" x then
        (map (fn c => Flag ("-" ^ Char.toString c)) o explode o triml 1 o full)
          x @ tokenize xs
      else
        Arg x :: tokenize xs

  fun toString (Flag f) = f
    | toString (FlagArg (f, args)) =
        f ^ "=" ^ String.concatWith "," args
    | toString (Arg a) = a

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

  fun match {usage = {short, long, desc}, arg} =
    fn [] => NONE
     | Arg a :: rest => NONE
     | FlagArg (other, vs) :: rest =>
      if "--" ^ long <> other then
        NONE
      else
        SOME
          ( case (arg, vs) of
              (One {action, ...}, [v]) => action v
            | (Optional {action, ...}, [v]) => action NONE
            | (Any {action, ...}, _) => action vs
            | (AtLeastOne {action, ...}, _) => action vs
            | _ => raise Fail "arity"
          , rest
          )
     | Flag other :: rest =>
      if
        "--" ^ long <> other
        andalso
        Option.getOpt
          (Option.map (fn s => shortToString s <> other) short, true)
      then
        NONE
      else
        SOME
          (case (arg, rest) of
             (None action, _) => (action (), rest)
           | (One {action, ...}, Arg a :: rest) => (action a, rest)
           | (Optional {action, ...}, Arg a :: rest) => (action (SOME a), rest)
           | (Optional {action, ...}, args) => (action NONE, args)
           | (Any {action, ...}, args) =>
               let val (l, r) = splitOnArg args
               in (action l, r)
               end
           | (AtLeastOne {action, ...}, Arg v :: rest) =>
               let val (l, r) = splitOnArg rest
               in (action (v :: l), r)
               end
           | _ => raise Fail "arity")
end
