structure Command =
  CommandFn
    (structure Flag = Conventions.GNU
     type a = unit
     val desc = "demo command thingy"
     val flags =
       [{ usage = {long = "verbose", desc = "verbosity control", short = SOME #"v"}
        , arg = Argument.Optional
            {action = fn arg => print ("foobar" ^ Option.getOpt (arg, "<default>")), metavar = "THING"}
        }])
val _ = Command.run (CommandLine.arguments ())
