signature COMMAND_OPTS =
sig
  (* The flag convention to use *)
  structure Flag: FLAG
  (* The return type of the action *)
  type action
  val desc: string
  val flags: action Flag.t list
end
