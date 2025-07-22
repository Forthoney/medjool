signature TOKEN =
sig
  type token
  val tokenize: string list -> token list
  val toString: token -> string
  val matchArg: 'a Argument.arg * token list -> ((unit -> 'a) * token list)
  val match: (string -> bool)
             -> 'a Argument.arg
             -> token list
             -> ((unit -> 'a) * token list) option
end
