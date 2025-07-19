signature FLAG =
sig
  (* information on using the flag. Should include information such as the flag's name and the help message *)
  type usage
  type 'a t = {usage: usage, arg: 'a Argument.t}

  (* Tokens produced when parsing according to this convention *)
  type token
  val tokenize: string list -> token list
  val tokToString: token -> string

  val toHelpMsg: 'a t -> string
  (* The usage metadata for the help flag *)
  val helpUsage: usage
  val search: 'a t -> token list -> ((unit -> 'a) option * token list)
end

functor PrefixedFn (val prefix: string) :> FLAG =
struct
  type usage = {name: string, desc: string}
  type 'a t = {usage: usage, arg: 'a Argument.t}

  fun toHelpMsg {usage = {name, desc}, arg} =
    let
      val metavar =
        case arg of
          Argument.None _ => ""
        | Argument.One {metavar, ...} => " <" ^ metavar ^ ">"
        | Argument.Optional {metavar, ...} => " [<" ^ "metavar" ^ ">]"
    in
      prefix ^ name ^ metavar ^ "\t" ^ desc
    end

  val helpUsage = {name = "help", desc = "Print help"}

  datatype token = Flag of string | Arg of string
  val rec tokenize =
    fn [] => []
     | ("--" :: xs) => map Arg xs
     | (x :: xs) =>
      if String.isPrefix prefix x then Flag x :: tokenize xs
      else Arg x :: tokenize xs

  fun tokToString (Arg v) = v
    | tokToString (Flag v) = v

  fun search {usage = {name, desc}, arg} toks =
    let
      open Argument
      fun loop acc =
        fn [] => (NONE, toks)
         | (Arg x :: xs) => loop (Arg x :: acc) xs
         | (Flag other :: xs) =>
          if prefix ^ name <> other then
            loop (Flag other :: acc) xs
          else
            case arg of
              None action => (SOME action, List.revAppend (acc, xs))
            | One {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action a), List.revAppend (acc, xs))
                 | _ => raise Fail "arity")
            | Optional {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action (SOME a)), List.revAppend (acc, xs))
                 | _ => (SOME (fn () => action NONE), List.revAppend (acc, xs)))
    in
      loop [] toks
    end
end

structure Conventions =
struct
  structure SingleDash = PrefixedFn(val prefix = "-")
  structure DoubleDash = PrefixedFn(val prefix = "--")

  structure GNU: FLAG =
  struct
    type usage = {short: char option, long: string, desc: string}
    type 'a t = {usage: usage, arg: 'a Argument.t}

    fun shortToString c = "-" ^ Char.toString c

    fun toHelpMsg {usage = {short, long, desc}, arg} =
      let
        val metavar =
          case (short, arg) of
            (_, Argument.None _) => ""
          | (SOME _, Argument.One {metavar, ...}) => " <" ^ metavar ^ ">"
          | (NONE, Argument.One {metavar, ...}) => "=<" ^ metavar ^ ">"
          | (SOME _, Argument.Optional {metavar, ...}) => " [<" ^ metavar ^ ">]"
          | (NONE, Argument.Optional {metavar, ...}) => "[=<" ^ metavar ^ ">]"
        val short = Option.getOpt (Option.map shortToString short, "  ")
      in
        short ^ ", " ^ "--" ^ long ^ metavar ^ "\t" ^ desc
      end

    val helpUsage = {short = SOME #"h", long = "help", desc = "Print help"}

    datatype token = Flag of string | FlagArg of string * string | Arg of string

    open Substring
    val rec tokenize =
      fn [] => []
       | ("--" :: xs) => map Arg xs
       | (x :: xs) =>
        if String.isPrefix "--" x then
          let
            val (flag, arg) = splitl (fn c => c <> #"=") (full x)
          in
            if isEmpty arg then Flag x :: tokenize xs
            else FlagArg (string flag, string (triml 1 arg)) :: tokenize xs
          end
        else if String.isPrefix "-" x then
          (map (Flag o shortToString) o explode o triml 1 o full) x
          @ tokenize xs
        else
          Arg x :: tokenize xs

    fun tokToString (Flag fl) = fl
      | tokToString (FlagArg (fl, arg)) = fl ^ "=" ^ arg
      | tokToString (Arg v) = v

    fun search {usage = {short, long, desc}, arg} toks =
      let
        open Argument
        fun loop acc =
          fn [] => (NONE, toks)
           | (Arg v :: rest) => loop (Arg v :: acc) rest
           | (FlagArg (other, v) :: rest) =>
            if "--" ^ long = other then
              case arg of
                None action => raise Fail "arity"
              | One {action, ...} =>
                  (SOME (fn () => action v), List.revAppend (acc, rest))
              | Optional {action, ...} =>
                  (SOME (fn () => action (SOME v)), List.revAppend (acc, rest))
            else
              loop (FlagArg (other, v) :: acc) rest
           | (Flag other :: rest) =>
            case
              ( "--" ^ long = other
              , Option.map (fn c => shortToString c = other) short
              )
            of
              (false, SOME false) => loop (Flag other :: acc) rest
            | (false, NONE) => loop (Flag other :: acc) rest
            | _ =>
                (case arg of
                   None action => (SOME action, List.revAppend (acc, rest))
                 | One {action, ...} =>
                     (case rest of
                        Arg v :: rest =>
                          (SOME (fn () => action v), List.revAppend (acc, rest))
                      | _ => raise Fail "arity")
                 | Optional {action, ...} =>
                     (case rest of
                        Arg v :: rest =>
                          ( SOME (fn () => action (SOME v))
                          , List.revAppend (acc, rest)
                          )
                      | _ =>
                          ( SOME (fn () => action NONE)
                          , List.revAppend (acc, rest)
                          )))
      in
        loop [] toks
      end
  end
end
