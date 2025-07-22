signature FLAG =
sig
  (* information on using the flag. Should include information such as the flag's name and the help message *)
  type usage
  type 'a flag = {usage: usage, arg: 'a Argument.arg}

  val toHelpMsg: 'a flag -> string
  (* The usage metadata for the help flag *)
  val helpUsage: usage

  (* Match the given flag against a string *)
  val match: 'a flag -> string -> bool
end
