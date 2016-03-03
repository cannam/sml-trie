
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure TestSupport = struct

    type test = string * (unit -> bool)
    type test_suite = string * test list

    fun report converter (obtained, expected) =
        print ("--- Expected " ^ (converter expected)
               ^ "\n--- Obtained " ^ (converter obtained) ^ "\n")
                                    
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
                        (Int.toString (List.length a)) ^ ": [" ^
			(String.concatWith "," (List.map converter a)) ^
                        "])\n");
                 false)
            else
                check_lists' (a, b)
        end

    fun check_sets converter greater (a, b) =
        check_lists converter
                    (ListMergeSort.sort greater a,
                     ListMergeSort.sort greater b)
         
    fun report_exception name msg =
        (print ("*** Caught exception in test \"" ^ name ^ "\": " ^ msg ^ "\n");
         false)
          
    fun run_test_suite (suite_name, tests) =
        case
            List.mapPartial
                (fn (test_name, test) =>
                    if (test ()
                        handle Fail msg => report_exception test_name msg
                             | IO.Io { name, ... } =>
                               (*!!! can we get more info from Exception? *)
                               report_exception test_name ("IO failure: " ^ name)
                             | ex => report_exception test_name "Exception caught")
                    then NONE
                    else (print ("*** Test \"" ^ test_name ^ "\" failed\n");
                          SOME test_name))
                tests
         of failed =>
            let val n = length tests
                val m = length failed
            in
                print (suite_name ^ ": " ^
                       (Int.toString (n - m)) ^ "/" ^ (Int.toString n) ^
                       " tests passed\n");
                if m > 0
                then print (suite_name ^
                            ": Failed tests [" ^ (Int.toString m) ^ "]: " ^
                            (String.concatWith " " failed) ^ "\n")
                else ()
            end
                      
end
