signature FLAG =
sig
  type usage
  type 'a t = {usage: usage, arg: 'a Argument.t}

  structure Token: TOKEN

  val toHelpMsg: 'a t -> string
  val helpUsage: usage
  val search: 'a t -> Token.t list -> ((unit -> 'a) option * Token.t list)
end

functor PrefixedFn (val prefix: string): FLAG =
struct
  type usage = {name: string, desc: string}
  type 'a t = {usage: usage, arg: 'a Argument.t}

  fun toHelpMsg {usage = {name, desc}, arg} =
    let
      val metavar =
        case Argument.toHelpMsg arg of
          "" => ""
        | other => " " ^ other
    in
      prefix ^ name ^ metavar ^ "\t" ^ desc
    end

  val helpUsage = {name = "help", desc = "Print help"}

  datatype token = Flag of string | Arg of string
  structure Token =
  struct
    type t = token
    val rec tokenize =
      fn [] => []
       | ("--" :: xs) => map Arg xs
       | (x :: xs) =>
        if String.isPrefix prefix x then
          Flag x :: tokenize xs
        else Arg x :: tokenize xs

    fun toString (Arg v) = v
      | toString (Flag v) = v
  end
  
  fun search {usage = {name, desc}, arg} toks =
    let
      open Token
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
        val short = Option.getOpt (Option.map shortToString short, "  ")
        val metavar =
          case Argument.toHelpMsg arg of
            "" => ""
          | other => "=" ^ other
      in
        short ^ ", " ^ "--" ^ long ^ metavar ^ "\t" ^ desc
      end

    val helpUsage = {short = SOME #"h", long = "help", desc = "Print help"}

    datatype token =
      Flag of string
    | FlagArg of string * string
    | Arg of string    

    structure Token =
    struct
      type t = token
      open Substring
      val rec tokenize =
        fn [] => []
         | ("--" :: xs) => map Arg xs
         | (x :: xs) =>
          if String.isPrefix "--" x then
            let
              val (flag, arg) = splitl (fn c => c <> #"=") (full x)
            in
              if isEmpty arg then
                Flag x :: tokenize xs
              else
                FlagArg (string flag, string (triml 1 arg)) :: tokenize xs
            end
          else if String.isPrefix "-" x then
            (map (Flag o shortToString) o explode o triml 1 o full) x @ tokenize xs
          else
            Arg x :: tokenize xs

      fun toString (Flag fl) = fl
        | toString (FlagArg (fl, arg)) = fl ^ "=" ^ arg
        | toString (Arg v) = v
    end

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
              | One {action, ...} => (SOME (fn () => action v), List.revAppend (acc, rest))
              | Optional {action, ...} => (SOME (fn () => action (SOME v)), List.revAppend (acc, rest))
            else
              loop (FlagArg (other, v) :: acc) rest
           | (Flag other :: rest) =>
            if "--" ^ long = other then
              case arg of
                None action => (SOME action, List.revAppend (acc, rest))
              | One {action, ...} => raise Fail "arity"
              | Optional {action, ...} => (SOME (fn () => action NONE), List.revAppend (acc, rest))
            else
              case short of
                NONE => loop (Flag other :: acc) rest
              | SOME c =>
                if shortToString c = other then
                  case arg of
                    None action => (SOME action, List.revAppend (acc, rest))
                  | One {action, ...} =>
                      (case rest of
                         Arg v :: rest =>
                           (SOME (fn () => action v), List.revAppend (acc, rest))
                       | _ => raise Fail "arity")
                  | Optional {action, ...} =>
                      (case rest of
                         Arg v :: rest =>
                           (SOME (fn () => action (SOME v)), List.revAppend (acc, rest))
                       | _ => (SOME (fn () => action NONE), List.revAppend (acc, rest)))
                else
                  loop (Flag other :: acc) rest
      in
        loop [] toks
      end
  end
end
