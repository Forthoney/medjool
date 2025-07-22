functor Token_PrefixFn (val prefix: string): TOKEN =
struct
  datatype token = Flag of string | Arg of string

  val rec tokenize =
    fn [] => []
     | ("--" :: rest) => map Arg rest
     | (x :: rest) =>
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
  val match =
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

  fun search pred arg =
    let
      fun loop acc =
        fn [] => NONE
         | (Arg a :: rest) => loop (Arg a :: acc) rest
         | (Flag other :: rest) =>
          if pred other then
            let val (action, rest) = match (arg, rest)
            in SOME (action, List.revAppend (acc, rest))
            end
          else
            loop (Flag other :: acc) rest
    in
      loop []
    end
end
