
structure TestSupport = struct

    type test = string * (unit -> bool)
    type test_suite = string * test list

    fun report converter (exp,obt) =
        print ("--- Expected " ^ (converter exp)
               ^ "\n--- Obtained " ^ (converter obt) ^ "\n")
                                    
    fun check converter (a,b) =
        if a = b then true
        else (report converter (a,b); false)

    (*!!! "check_pairs"? check_all maybe should compare two lists *)
    fun check_all converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal => (app (report converter) unequal; false)

end
