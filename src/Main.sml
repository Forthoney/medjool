val verbose = ref false
structure Medjool =
  CommandFn
    (structure Token = GNUToken
     structure Flag = GNUFlag

     type action = unit
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
       ]
     val anonymous = Argument.None (fn _ => ()))

val _ = Medjool.run (CommandLine.arguments ())
