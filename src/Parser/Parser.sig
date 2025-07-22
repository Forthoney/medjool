(*
  The parsing algorithm to use for the arguments.
  Unintuitively, this also requires the scaffolding to create the help flag.
  These should be addressed
*)
signature PARSER =
sig
  (* information on using the flag. Should include information such as the flag's name and the help message *)
  type usage
  type 'a flag = {usage: usage, arg: 'a Argument.arg}

  val toHelpMsg: 'a flag -> string
  (* The usage record for creating the help flag *)
  val helpUsage: usage

  type token
  val tokenize: string list -> token list
  val toString: token -> string
  val matchArg: 'a Argument.arg * token list -> ('a * token list)
  val match: 'a flag -> token list -> ('a * token list) option
end
