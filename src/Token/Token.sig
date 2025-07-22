signature TOKEN =
sig
  type token
  val tokenize: string list -> token list
  val toString: token -> string
  val match: 'a Argument.arg * token list -> ((unit -> 'a) * token list)
  val search: (string -> bool)
              -> 'a Argument.arg
              -> token list
              -> ((unit -> 'a) * token list) option
end
