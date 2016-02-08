
structure TestTrie :> TESTS = struct

    open TestSupport

    structure T = StringTrie

    val name = "trie"

    val strings = [ "poot", "parp", "par",
		    "alligator", "zebra",
		    "alliance", "aardvark",
		    "a" ]

    val cutdown_strings = [ "poot", "parp",
			    "alligator",
			    "alliance", "aardvark",
			    "a" ]

    fun sorted s = ListMergeSort.sort String.> s

    fun make_test_trie () = List.foldl (fn (s, t) => T.add (t, s))
				       T.empty
				       strings

    fun id x = x
				       
    fun test_enumerate () =
      check_lists id (T.enumerate (make_test_trie ()),
		      sorted strings)

    fun test_contains () =
      let val t = make_test_trie ()
      in
	  check_pairs Bool.toString
		      [(T.contains (t, "pa"), false),
		       (T.contains (t, "par"), true),
		       (T.contains (t, "parp"), true),
		       (T.contains (t, "part"), false)]
      end
	  
    fun test_remove () =
      check_lists
	  id (T.enumerate
		  (T.remove
		       (T.remove
			    (T.remove (make_test_trie (), "zebra"),
			     "zebra"),
			"par")),
	      sorted cutdown_strings)
		  
    fun test_prefix_match () =
      check_lists id (T.prefix_match (make_test_trie (), "pa"),
		      [ "par", "parp" ])
      andalso
      check_lists id (T.prefix_match (make_test_trie (), "par"),
		      [ "par", "parp" ])
      andalso
      check_lists id (T.prefix_match (make_test_trie (), ""),
		      sorted strings)

    fun test_prefix_of () =
      let val t = make_test_trie ()
      in
	  check_pairs id
		      [(T.prefix_of (t, "pa"), ""),
		       (T.prefix_of (t, "par"), "par"),
		       (T.prefix_of (t, "parp"), "parp"),
		       (T.prefix_of (t, "part"), "par")]
      end
	  
    fun tests () = [
	( "enumerate", test_enumerate ),
	( "contains", test_contains ),
	( "remove", test_remove ),
	( "prefix_match", test_prefix_match ),
	( "prefix_of", test_prefix_of )
	    (*!!! + pattern match *)
    ]
	  
(*		      
    fun test () =
        let
            val t = 
            val t = StringTrie.remove (t, "poot")
	    val contents = StringTrie.enumerate t
            val match = StringTrie.prefix_match (t, "pa")
        in
	    print ("contents: (" ^ (String.concatWith "," contents) ^ ")\n");
	    print ("match: (" ^ (String.concatWith "," match) ^ ")\n");
            print ("contains pa: " ^ (Bool.toString (StringTrie.contains (t, "pa"))) ^ "\n");
            print ("contains par: " ^ (Bool.toString (StringTrie.contains (t, "par"))) ^ "\n");
            print ("contains parp: " ^ (Bool.toString (StringTrie.contains (t, "parp"))) ^ "\n");
            print ("contains part: " ^ (Bool.toString (StringTrie.contains (t, "part"))) ^ "\n");
            print ("prefix_of pa: " ^ (StringTrie.prefix_of (t, "pa")) ^ "\n");
            print ("prefix_of par: " ^ (StringTrie.prefix_of (t, "par")) ^ "\n");
            print ("prefix_of parp: " ^ (StringTrie.prefix_of (t, "parp")) ^ "\n");
            print ("prefix_of part: " ^ (StringTrie.prefix_of (t, "part")) ^ "\n");
            print ("prefix_of \"\": " ^ (StringTrie.prefix_of (t, "")) ^ "\n");
            print ("prefix_of allia: " ^ (StringTrie.prefix_of (t, "allia")) ^ "\n")
        end
      
end
*)		   

end

