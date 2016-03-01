
structure TestTrie :> TESTS = struct

    open TestSupport

    structure T = StringTrie

    val name = "trie"

    val strings = [ "poot", "parp", "par",
		    "alligator", "zebra",
		    "alliance", "abrasive",
		    "a" ]

    val cutdown_strings = [ "poot", "parp",
			    "alligator",
			    "alliance", "abrasive",
			    "a" ]

    fun sorted s = ListMergeSort.sort String.> s

    fun make_test_trie () = List.foldl (fn (s, t) => T.insert (t, s))
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
      let val t = make_test_trie ()
      in
	  check_lists id (T.prefix_match (t, "pa"),
			  [ "par", "parp" ])
	  andalso
	  check_lists id (T.prefix_match (t, "par"),
			  [ "par", "parp" ])
	  andalso
	  check_lists id (T.prefix_match (t, ""),
			  sorted strings)
      end
	  
    fun test_prefix_of () =
      let val t = make_test_trie ()
      in
	  check_pairs id
		      [(T.prefix_of (t, "pa"), ""),
		       (T.prefix_of (t, "par"), "par"),
		       (T.prefix_of (t, "parp"), "parp"),
		       (T.prefix_of (t, "part"), "par")]
      end

    fun test_pattern_match () =
      let val t = make_test_trie ()
      in
	  check_lists id
		      (T.pattern_match (t, [SOME #"p", NONE, SOME #"r"]),
		       ["par"])
	  andalso
	  check_lists id
		      (T.pattern_match (t, [SOME #"a", SOME #"l", SOME #"l"]),
		       [])
	  andalso
	  check_lists id
		      (T.pattern_match (t, [SOME #"a", NONE, NONE, NONE, NONE,
					    NONE, NONE, SOME #"e"]),
		       ["abrasive", "alliance"])
      end
	  
    fun tests () = [
	( "enumerate", test_enumerate ),
	( "contains", test_contains ),
	( "remove", test_remove ),
	( "prefix_match", test_prefix_match ),
	( "prefix_of", test_prefix_of ),
	( "pattern_match", test_pattern_match )
    ]

end

fun main () =
    let open TestSupport
    in
        app run_test_suite [ ("trie", TestTrie.tests ()) ]
    end

