
structure TestSupport = struct

    type test = string * (unit -> bool)
    type test_suite = string * test list

    fun report converter (exp, obt) =
        print ("--- Expected " ^ (converter exp)
               ^ "\n--- Obtained " ^ (converter obt) ^ "\n")
                                    
    fun check converter (a, b) =
        if a = b then true
        else (report converter (a, b); false)

    fun check_pairs converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal => (app (report converter) unequal; false)

    fun check_lists converter (a, b) =
        let fun check_lists' ([], []) = true
              | check_lists' (a', b') =
                check converter (hd a', hd b') andalso
                check_lists' (tl a', tl b')
        in
            if (List.length a <> List.length b)
            then 
                (print ("--- Lists have differing lengths (expected " ^
                        (Int.toString (List.length b)) ^
                        ", obtained " ^
                        (Int.toString (List.length a)) ^
                        ")\n");
                 false)
            else
                check_lists' (a, b)
        end

    fun check_sets converter greater (a, b) =
        check_lists converter
                    (ListMergeSort.sort greater a,
                     ListMergeSort.sort greater b)
                           
end
