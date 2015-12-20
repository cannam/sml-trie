
structure TestSupport = struct

    type test = string * (unit -> bool)
    type test_suite = string * test list

    fun check_all converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal =>
            (app (fn (a,b) =>
                     print ("--- Expected \"" ^ (converter b)
                            ^ "\", obtained \"" ^ (converter a) ^ "\"\n"))
                 unequal;
             false)

end
