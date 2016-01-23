
structure TestSupport = struct

    type test = string * (unit -> bool)
    type test_suite = string * test list

    fun report converter (a,b) =
        print ("--- Expected " ^ (converter b)
               ^ "\n--- Obtained " ^ (converter a) ^ "\n")
                                    
    fun check converter (a,b) =
        if a = b then true
        else (report converter (a,b); false)
                                    
    fun check_all converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal => (app (report converter) unequal; false)

end
