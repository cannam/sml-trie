
structure TestTrie :> TESTS = struct

    open TestSupport

    val name = "trie"

    val strings = [ "poot", "parp", "par",
		    "alligator", "zebra",
		    "alliance", "aardvark",
		    "a" ]

    val sorted_strings = ListMergeSort.sort String.> strings

    fun make_test_trie () = List.foldl (fn (s, t) => StringTrie.add (t, s))
				       StringTrie.empty
				       strings

    fun id x = x
				       
    fun test_enumerate () =
      check_lists id (StringTrie.enumerate (make_test_trie ()),
		      sorted_strings)

    fun test_prefix_match () =
      check_lists id (StringTrie.prefix_match (make_test_trie (), "pa"),
		      [ "par", "parp" ])
      andalso
      check_lists id (StringTrie.prefix_match (make_test_trie (), "par"),
		      [ "par", "parp" ])
      andalso
      check_lists id (StringTrie.prefix_match (make_test_trie (), ""),
		      sorted_strings)
      
    fun tests () = [
	( "enumerate", test_enumerate ),
	( "prefix_match", test_prefix_match )
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

