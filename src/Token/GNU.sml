structure GNUToken: TOKEN =
struct
  datatype token =
    Flag of string
  | FlagArg of string * string list
  | Arg of string

  open Substring
  val rec tokenize =
    fn [] => []
     | ("--" :: xs) => map Arg xs
     | (x :: xs) =>
      if String.isPrefix "--" x then
        let
          val (flag, arg) = splitl (fn c => c <> #"=") (full x)
        in
          if isEmpty arg then
            Flag x :: tokenize xs
          else
            FlagArg
              ( string flag
              , (String.tokens (fn c => c = #",") o string o triml 1) arg
              ) :: tokenize xs
        end
      else if String.isPrefix "-" x then
        (map (fn c => Flag ("-" ^ Char.toString c)) o explode o triml 1 o full)
          x @ tokenize xs
      else
        Arg x :: tokenize xs

  fun toString (Flag f) = f
    | toString (FlagArg (f, args)) =
        f ^ "=" ^ String.concatWith "," args
    | toString (Arg a) = a

  fun splitOnArg (Arg v :: rest) =
        let val (l, r) = splitOnArg rest
        in (v :: l, r)
        end
    | splitOnArg otherwise = ([], otherwise)

  open Argument
  val matchArg =
    fn (None action, args) => (action, args)
     | (One {action, ...}, Arg a :: rest) => (fn () => action a, rest)
     | (Optional {action, ...}, Arg a :: rest) =>
      (fn () => action (SOME a), rest)
     | (Optional {action, ...}, args) => (fn () => action NONE, args)
     | (Any {action, ...}, args) => let val (l, r) = splitOnArg args
                                    in (fn () => action l, r)
                                    end
     | (AtLeastOne {action, ...}, Arg v :: rest) =>
      let val (l, r) = splitOnArg rest
      in (fn () => action (v :: l), r)
      end
     | _ => raise Fail "arity"

  fun matchFlag pred arg =
    fn [] => NONE
     | (Arg a :: rest) => NONE
     | (FlagArg (other, vs) :: rest) =>
      if pred other then
        SOME
          ( case (arg, vs) of
              (One {action, ...}, [v]) => (fn () => action v)
            | (Optional {action, ...}, [v]) => (fn () => action NONE)
            | (Any {action, ...}, _) => (fn () => action vs)
            | (AtLeastOne {action, ...}, _) => (fn () => action vs)
            | _ => raise Fail "arity"
          , rest
          )
      else
        NONE
     | (Flag other :: rest) =>
      if pred other then
        SOME
          (case (arg, rest) of
             (None action, _) => (action, rest)
           | (One {action, ...}, Arg a :: rest) => (fn () => action a, rest)
           | (Optional {action, ...}, Arg a :: rest) =>
               (fn () => action (SOME a), rest)
           | (Optional {action, ...}, args) => (fn () => action NONE, args)
           | (Any {action, ...}, args) =>
               let val (l, r) = splitOnArg args
               in (fn () => action l, r)
               end
           | (AtLeastOne {action, ...}, Arg v :: rest) =>
               let val (l, r) = splitOnArg rest
               in (fn () => action (v :: l), r)
               end
           | _ => raise Fail "arity")
      else
        NONE
end
