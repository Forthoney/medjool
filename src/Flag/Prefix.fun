functor Flag_PrefixFn (val prefix: string): FLAG =
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

  fun match {usage = {name, desc}, arg} =
    (fn other => prefix ^ name = other)
end
