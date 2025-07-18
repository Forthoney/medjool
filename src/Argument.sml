structure Argument:
sig
  exception Arity
  exception Conversion of {expected: string, actual: string}
  datatype 'a t =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}

  val asInt: string -> int
  val asReal: string -> real
  val asBool: string -> bool
end =
struct
  exception Arity
  exception Conversion of {expected: string, actual: string}
  datatype 'a t =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}

  fun asType tyName scanner s =
    case scanner Substring.getc (Substring.full s) of
      SOME (i, rest) =>
        if Substring.isEmpty rest then i
        else raise Conversion {expected = tyName, actual = s}
    | NONE => raise Conversion {expected = tyName, actual = s}

  val asInt = asType "int" (Int.scan StringCvt.DEC)
  val asReal = asType "real (float)" Real.scan
  val asBool = asType "bool" Bool.scan
end
