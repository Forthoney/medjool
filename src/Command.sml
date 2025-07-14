functor CommandFn(
  structure Flag: FLAG
  type a
  val desc: string
  val flags: a Flag.t list
) =
struct
  structure Tokenizer = TokenizerFn(Flag)

  val helpMsg = String.concatWith "\n"
    (desc :: map (fn fl => "  " ^ Flag.toHelpMsg fl) (flags @ [Flag.help]))

  fun parse args =
    let
      val args = Tokenizer.tokenize args
      fun loop acc =
        fn (_, []) => List.map (fn action => action ()) (rev acc)
         | ([], _) => raise Fail "unmatched"
         | (flag :: rest, args) =>
          case Tokenizer.search flag args of
            (SOME action, args) => loop (action :: acc) (flag :: rest, args)
          | (NONE, args) => loop acc (rest, args)
    in
      loop [] (Flag.help :: flags, args)
      handle Argument.Help => raise Fail helpMsg
    end
end
