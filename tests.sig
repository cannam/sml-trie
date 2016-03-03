
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TESTS = sig

    type test = string * (unit -> bool)

    val name : string
    val tests : unit -> test list

end
                      
