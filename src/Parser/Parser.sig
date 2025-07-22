signature PARSER =
sig
  (* information on using the flag. Should include information such as the flag's name and the help message *)
  type usage
  type 'a flag = {usage: usage, arg: 'a Argument.arg}

  val toHelpMsg: 'a flag -> string
  (* The usage metadata for the help flag *)
  val helpUsage: usage

  type token
  val tokenize: string list -> token list
  val toString: token -> string
  val matchArg: 'a Argument.arg * token list -> ('a * token list)
  val match: 'a flag
             -> token list
             -> ('a * token list) option
end
