signature ARGUMENT =
sig
  (* Errors in argument parsing *)
  exception Arity
  exception Conversion of {expected: string, actual: string}
  exception Validation of {condition: string, actual: string}

  datatype 'a arg =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}
  | AtLeastOne of {metavar: string, action: string list -> 'a}
  | Any of {metavar: string, action: string list -> 'a}

  (* Defining custom converter which provides helpful error messages *)
  val asType: {typeName: string, scanner: (char, substring) StringCvt.reader -> ('a, substring) StringCvt.reader} -> string -> 'a
  val asType': {typeName: string, fromString: string -> 'a option} -> string -> 'a

  val asInt: string -> int
  val asReal: string -> real
  val asBool: string -> bool
  val satisfies: string -> (string -> bool) -> string -> string
  val includedIn: string list -> string -> string
end
