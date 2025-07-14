functor TokenizerFn(F: FLAG):
sig
  type token
  val tokenize: string list -> token list
  val search: 'a F.t -> token list -> ((unit -> 'a) option * token list)
end =
struct
  datatype token = Flag of string | Arg of string

  val tokenize =
    let
      fun loop acc [] = rev acc
        | loop acc ("--" :: xs) =
            List.revAppend (acc, map Arg xs)
        | loop acc (x :: xs) =
            if F.isFlag x then loop (Flag x :: acc) xs
            else loop (Arg x :: acc) xs
    in
      loop [] o List.concat o map F.preprocess
    end

  fun search flag =
    let
      fun loop acc =
        fn [] => (NONE, rev acc)
         | (Flag x :: xs) =>
          if F.isMatch (flag, x) then
            case F.getArg flag of
              Argument.None action => (SOME action, List.revAppend (acc, xs))
            | Argument.One {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action a), List.revAppend (acc, xs))
                 | _ => raise Fail "arity")
            | Argument.Optional {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action (SOME a)), List.revAppend (acc, xs))
                 | _ => (SOME (fn () => action NONE), List.revAppend (acc, xs)))
          else
            loop (Flag x :: acc) xs
         | (other :: xs) => loop (other :: acc) xs
    in
      loop []
    end
end
