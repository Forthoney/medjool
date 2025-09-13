structure Argument: ARGUMENT =
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

  fun asType {typeName, scanner} s =
    case scanner Substring.getc (Substring.full s) of
      SOME (i, rest) =>
        if Substring.isEmpty rest then i
        else raise Conversion {expected = typeName, actual = s}
    | NONE => raise Conversion {expected = typeName, actual = s}

  fun asType' {typeName, fromString} s =
    case fromString s of
      SOME v => v
    | NONE => raise Conversion {expected = typeName, actual = s}

  val asInt = asType {typeName = "int", scanner = Int.scan StringCvt.DEC}
  val asReal = asType {typeName = "real (float)", scanner = Real.scan}
  val asBool = asType {typeName = "bool", scanner = Bool.scan}

  fun satisfies msg predicate s =
    case Option.filter predicate s of
      SOME v => v
    | NONE => raise Validation {condition = msg, actual = s}

  fun includedIn choices =
    satisfies ("Must select value among " ^ String.concatWith ", " choices)
      (fn s => List.exists (fn el => el = s) choices)
end
