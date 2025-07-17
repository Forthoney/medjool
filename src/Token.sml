signature TOKEN =
sig
  type t
  val tokenize: string list -> t list 
  val toString: t -> string
end

functor PrefixedTokenFn(val prefix: string): TOKEN =
struct
  datatype t = Flag of string | Arg of string
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
