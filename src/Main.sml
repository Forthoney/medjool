val verbose = ref false
structure Command =
  CommandFn
    (structure Flag = Conventions.GNU
     type a = unit
     val desc = "demo command thingy"
     val flags =
       [ { usage =
             {long = "verbose", desc = "verbosity control", short = SOME #"v"}
         , arg = Argument.One
             { action = (fn arg => verbose := arg) o Argument.asBool
             , metavar = "BOOL"
             }
         }
       , { usage = {long = "version", desc = "version thing", short = NONE}
         , arg = Argument.One
             {action = fn arg => print ("baz" ^ arg), metavar = "HELMET"}
         }
       ])
val _ = Command.run (CommandLine.arguments ())
