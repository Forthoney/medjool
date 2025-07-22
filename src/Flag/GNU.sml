structure GNUFlag: FLAG =
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

  fun match {usage = {short, long, desc}, arg} =
    (fn other =>
       "--" ^ long = other
       orelse
       Option.getOpt (Option.map (fn s => shortToString s = other) short, false))
end
