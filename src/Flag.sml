signature FLAG_CONVENTION =
sig
  (* information on using the flag. Should include information such as the flag's name and the help message *)
  type usage
  type 'a flag = {usage: usage, arg: 'a Argument.arg}
  structure Token: TOKEN

  val toHelpMsg: 'a flag -> string
  (* The usage metadata for the help flag *)
  val helpUsage: usage
  val match: 'a flag -> string -> bool
end

functor PrefixedFn (val prefix: string): FLAG_CONVENTION =
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

  structure Token = Token_PrefixFn(val prefix = prefix)

  fun match {usage = {name, desc}, arg} =
    (fn other => prefix ^ name = other)
end

structure FlagConvention =
struct
  structure SingleDash = PrefixedFn(val prefix = "-")
  structure DoubleDash = PrefixedFn(val prefix = "--")

  structure GNU: FLAG_CONVENTION =
  struct
    type usage = {short: char option, long: string, desc: string}
    type 'a flag = {usage: usage, arg: 'a Argument.arg}
    structure Token = GNUToken

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
          | (NONE, Argument.AtLeastOne {metavar, ...}) =>
              "=<" ^ metavar ^ ">..."
          | (NONE, Argument.Any {metavar, ...}) => "=[<" ^ metavar ^ ">]..."
        val short = Option.getOpt (Option.map shortToString short, "  ")
      in
        short ^ ", " ^ "--" ^ long ^ metavar ^ "\t" ^ desc
      end

    val helpUsage = {short = SOME #"h", long = "help", desc = "Print help"}

    fun match {usage = {short, long, desc}, arg} =
      (fn other =>
         "--" ^ long = other
         orelse
         Option.getOpt
           (Option.map (fn s => shortToString s = other) short, false))
  end
end
