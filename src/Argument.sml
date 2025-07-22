signature ARGUMENT =
sig
  exception Arity
  exception Conversion of {expected: string, actual: string}
  exception Validation of {condition: string, actual: string}
  datatype 'a arg =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}
  | AtLeastOne of {metavar: string, action: string list -> 'a}
  | Any of {metavar: string, action: string list -> 'a}

  val asInt: string -> int
  val asReal: string -> real
  val asBool: string -> bool
  val satisfies: string -> (string -> bool) -> string -> string
  val includedIn: string list -> string -> string
end

structure Argument =
struct
  exception Arity
  exception Conversion of {expected: string, actual: string}
  exception Validation of {condition: string, actual: string}

  datatype 'a arg =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}
  | AtLeastOne of {metavar: string, action: string list -> 'a}
  | Any of {metavar: string, action: string list -> 'a}

  fun asType tyName scanner s =
    case scanner Substring.getc (Substring.full s) of
      SOME (i, rest) =>
        if Substring.isEmpty rest then i
        else raise Conversion {expected = tyName, actual = s}
    | NONE => raise Conversion {expected = tyName, actual = s}

  val asInt = asType "int" (Int.scan StringCvt.DEC)
  val asReal = asType "real (float)" Real.scan
  val asBool = asType "bool" Bool.scan

  fun satisfies msg predicate s =
    case Option.filter predicate s of
      SOME v => v
    | NONE => raise Validation {condition = msg, actual = s}

  fun includedIn choices =
    satisfies ("Must select value among " ^ String.concatWith ", " choices)
      (fn s => List.exists (fn el => el = s) choices)
end
