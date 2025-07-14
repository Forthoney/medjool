signature FLAG =
sig
  type 'a t
  val isMatch: ('a t * string) -> bool
  val toHelpMsg: 'a t -> string
  val preprocess: string -> string list
  val isFlag: string -> bool
  val getArg: 'a t -> 'a Argument.t

  val help: 'a t
end

functor PrefixedFn (val prefix: string): FLAG =
struct
  type 'a t = {name: string, desc: string, arg: 'a Argument.t}

  fun toHelpMsg {name, desc, arg} =
    let
      val metavar =
        case Argument.toHelpMsg arg of
          "" => ""
        | other => " " ^ other
    in
      prefix ^ name ^ metavar ^ "\t" ^ desc
    end

  val isFlag = String.isPrefix prefix

  fun isMatch ({name, desc, arg}, other) = prefix ^ name = other

  fun preprocess s = [s]

  fun getArg {name, desc, arg} = arg

  val help =
    { name = "help"
    , desc = "Print help"
    , arg = Argument.None (fn () => raise Argument.Help)
    }
end

structure Conventions =
struct
  structure SingleDash = PrefixedFn(val prefix = "-")
  structure DoubleDash = PrefixedFn(val prefix = "--")
end
