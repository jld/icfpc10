type gate
and pipe
and port = X | L of gate | R of gate | P of pipe
val reset : unit -> unit
val gate : unit -> gate
val pipe : unit -> pipe
val wire : port -> port -> unit
val bprint : Buffer.t -> unit
val sprint : unit -> string
val print : unit -> unit
val fgate : port -> port -> (port * port)
