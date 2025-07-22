functor Token_PrefixFn (val prefix: string): TOKEN =
struct
  datatype token = Flag of string | Arg of string

  val rec tokenize =
    fn [] => []
     | "--" :: rest => map Arg rest
     | x :: rest =>
      if String.isPrefix prefix x then Flag x :: tokenize rest
      else Arg x :: tokenize rest

  fun toString (Arg a) = a
    | toString (Flag f) = f

  fun splitOnArg (Arg v :: rest) =
        let val (l, r) = splitOnArg rest
        in (v :: l, r)
        end
    | splitOnArg otherwise = ([], otherwise)

  open Argument
  val matchArg =
    fn (None action, args) => (action (), args)
     | (One {action, ...}, Arg a :: rest) => (action a, rest)
     | (Optional {action, ...}, Arg a :: rest) => (action (SOME a), rest)
     | (Optional {action, ...}, args) => (action NONE, args)
     | (Any {action, ...}, args) => let val (l, r) = splitOnArg args
                                    in (action l, r)
                                    end
     | (AtLeastOne {action, ...}, Arg v :: rest) =>
      let val (l, r) = splitOnArg rest
      in (action (v :: l), r)
      end
     | _ => raise Fail "arity"

  fun match pred arg =
    fn [] => NONE
     | Arg a :: rest => NONE
     | Flag other :: rest =>
      if pred other then SOME (matchArg (arg, rest)) else NONE
end
