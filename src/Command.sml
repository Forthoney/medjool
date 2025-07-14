signature COMMAND =
sig
  type ret
  val helpMsg: string
  val parse: string list -> ret list
  val run: string list -> ret list
end

functor CommandFn
  (structure Flag: FLAG type a val desc: string val flags: a Flag.t list):
  COMMAND =
struct
  type ret = a

  datatype token = Flag of string | Arg of string
  structure Token =
  struct
    val fromStrings =
      let
        fun loop acc =
          fn [] => rev acc
           | ("--" :: xs) => List.revAppend (acc, map Arg xs)
           | (x :: xs) =>
            if Flag.isFlag x then loop (Flag x :: acc) xs
            else loop (Arg x :: acc) xs
      in
        loop [] o List.concat o map Flag.preprocess
      end

    fun toString (Arg v) = v
      | toString (Flag v) = v
  end

  fun search flag =
    let
      open Argument
      fun loop acc =
        fn [] => (NONE, rev acc)
         | (Arg x :: xs) => loop (Arg x :: acc) xs
         | (Flag x :: xs) =>
          if Flag.isMatch (flag, x) then
            case Flag.getArg flag of
              None action => (SOME action, List.revAppend (acc, xs))
            | One {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action a), List.revAppend (acc, xs))
                 | _ => raise Fail "arity")
            | Optional {action, ...} =>
                (case xs of
                   Arg a :: xs =>
                     (SOME (fn () => action (SOME a)), List.revAppend (acc, xs))
                 | _ => (SOME (fn () => action NONE), List.revAppend (acc, xs)))
          else
            loop (Flag x :: acc) xs
    in
      loop []
    end

  val helpMsg = String.concatWith "\n"
    (desc :: map (fn fl => "  " ^ Flag.toHelpMsg fl) (flags @ [Flag.help]))

  fun parsePartial args =
    let
      val args = Token.fromStrings args
      fun loop acc =
        fn (_, []) => (rev acc, [])
         | ([], rest) => (rev acc, rest)
         | (flag :: rest, args) =>
          case search flag args of
            (SOME action, args) => loop (action :: acc) (flag :: rest, args)
          | (NONE, args) => loop acc (rest, args)
      val (actions, remaining) = loop [] (Flag.help :: flags, args)
    in
      (List.map (fn action => action ()) actions, remaining)
    end

  fun parse args =
    case parsePartial args of
      (actions, []) => actions
    | (actions, arg :: _) => raise Fail ("unmatched " ^ Token.toString arg)

  fun run args =
    parse args
    handle Argument.Help =>
      (print (helpMsg ^ "\n"); OS.Process.exit OS.Process.success)
end
