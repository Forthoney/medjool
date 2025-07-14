structure Argument =
struct
  datatype 'a t =
    None of unit -> 'a
  | One of {metavar: string, action: string -> 'a}
  | Optional of {metavar: string, action: string option -> 'a}

  val toHelpMsg =
    fn None _ => ""
     | One {metavar, ...} => metavar
     | Optional {metavar, ...} => "[" ^ metavar ^ "]"

  exception Help
end
