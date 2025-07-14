structure Command =
  CommandFn
    (structure Flag = Conventions.SingleDash
     type a = unit
     val desc = "demo command thingy"
     val flags =
       [{ name = "poop"
        , desc = "poopy"
        , arg = Argument.One
            {action = fn arg => print ("foobar" ^ arg), metavar = "THING"}
        }])
val _ = Command.run (CommandLine.arguments ())
