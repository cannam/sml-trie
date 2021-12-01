
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TESTS = sig

    type test = string * (unit -> bool)

    val name : string
    val tests : unit -> test list

end
                      
