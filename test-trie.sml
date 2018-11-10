
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TEST_TRIE_FN_ARG = sig
    structure T : STRING_TRIE
    val name : string
end

functor TestTrieFn (ARG : TEST_TRIE_FN_ARG) :> TESTS = struct

    open TestSupport

    structure T = ARG.T
    val name = ARG.name

    val strings = [ "poot", "parp", "par",
		    "alligator", "zebra",
		    "alliance", "abrasive",
		    "a" ]

    val cutdown_strings = [ "poot", "parp",
			    "alligator",
			    "alliance", "abrasive",
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
		  
    fun test_prefixMatch () =
      let val t = make_test_trie ()
      in
	  check_lists id (T.prefixMatch (t, "pa"),
			  [ "par", "parp" ])
	  andalso
	  check_lists id (T.prefixMatch (t, "par"),
			  [ "par", "parp" ])
	  andalso
	  check_lists id (T.prefixMatch (t, ""),
			  sorted strings)
      end
	  
    fun test_prefixOf () =
      let val t = make_test_trie ()
      in
	  check_pairs id
		      [(T.prefixOf (t, "pa"), ""),
		       (T.prefixOf (t, "par"), "par"),
		       (T.prefixOf (t, "parp"), "parp"),
		       (T.prefixOf (t, "part"), "par")]
      end

    fun test_patternMatch () =
      let val t = make_test_trie ()
      in
	  check_lists id
		      (T.patternMatch (t, [SOME #"p", NONE, SOME #"r"]),
		       ["par"])
	  andalso
	  check_lists id
		      (T.patternMatch (t, [SOME #"a", SOME #"l", SOME #"l"]),
		       [])
	  andalso
	  check_lists id
		      (T.patternMatch (t, [SOME #"a", NONE, NONE, NONE, NONE,
					    NONE, NONE, SOME #"e"]),
		       ["abrasive", "alliance"])
      end
	  
    fun tests () = [
	( "enumerate", test_enumerate ),
	( "contains", test_contains ),
	( "remove", test_remove ),
	( "prefixMatch", test_prefixMatch ),
	( "prefixOf", test_prefixOf ),
	( "patternMatch", test_patternMatch )
    ]

end

structure StringMapTrieTest = TestTrieFn(struct
                                          structure T = StringMapTrie
                                          val name = "string-map-trie"
                                          end)
structure StringArrayTrieTest = TestTrieFn(struct
                                            structure T = StringArrayTrie
                                            val name = "string-array-trie"
                                            end)
                                                    
fun main () =
    let open TestSupport
    in
        app run_test_suite [
            (StringMapTrieTest.name, StringMapTrieTest.tests ()),
            (StringArrayTrieTest.name, StringArrayTrieTest.tests ())
        ]
    end

