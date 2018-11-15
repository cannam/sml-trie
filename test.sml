
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

    fun test_trie () = List.foldl (fn (s, t) => T.add (t, s))
				  T.empty
				  strings

    fun id x = x

    fun tests () = [
	( "enumerate-empty",
          fn () => check_lists id (T.enumerate T.empty, [])
        ),
        ( "enumerate",
          fn () => check_lists id (T.enumerate (test_trie ()), sorted strings)
        ),
        ( "contains-empty",
          fn () => check_pairs Bool.toString
                               [(T.contains (T.empty, ""), false),
                                (T.contains (T.empty, "parp"), false)]
        ),
        ( "contains",
          fn () => let val t = test_trie ()
                   in
                       check_pairs Bool.toString
		                   [(T.contains (t, "pa"), false),
		                    (T.contains (t, "par"), true),
		                    (T.contains (t, "parp"), true),
		                    (T.contains (t, "part"), false),
		                    (T.contains (t, "quiz"), false),
		                    (T.contains (t, ""), false)]
                   end
        ),
	( "remove-empty",
          fn () => check_lists id (T.enumerate (T.remove (T.empty, "parp")), [])
        ),
        ( "remove",
          fn () => check_lists
	               id (T.enumerate
		               (T.remove
		                    (T.remove
			                 (T.remove (test_trie (), "zebra"),
			                  "zebra"),
			             "par")),
	                   sorted cutdown_strings)
        ),
        ( "remove-all",
          fn () => let val t = test_trie ()
                       val t0 = foldl (fn (e, t) => T.remove (t, e))
                                      t (T.enumerate t)
                   in
                       check_lists id (T.enumerate t0, [])
                   end
        ),
	( "isEmpty-empty",
          fn () => T.isEmpty T.empty
        ),
        ( "isEmpty-nonempty",
          fn () => not (T.isEmpty (test_trie ()))
        ),
        ( "isEmpty-after-removals",
          fn () => let val t = test_trie ()
                       val e1 = foldl (fn (e, t) => T.remove (t, e))
                                      t (T.enumerate t)
                       val e2 = foldl (fn (e, t) => T.remove (t, e))
                                      t (rev (T.enumerate t))
                   in
                       T.isEmpty e1 andalso T.isEmpty e2
                   end
        ),
        ( "prefixMatch-empty",
          fn () => check_lists id (T.prefixMatch (T.empty, "parp"), [])
        ),
        ( "prefixMatch-matches",
          fn () => check_lists id (T.prefixMatch (test_trie (), "pa"),
			           [ "par", "parp" ])
	           andalso
	           check_lists id (T.prefixMatch (test_trie (), "par"),
			           [ "par", "parp" ])
        ),
        ( "prefixMatch-no-matches",
          fn () => check_lists id (T.prefixMatch (test_trie (), "quiz"), [ ])
        ),
        ( "prefixMatch-all-matches",
          fn () => check_lists id (T.prefixMatch (test_trie (), ""),
                                   sorted strings)
        ),
        ( "prefixOf-empty",
          fn () => check_pairs id [(T.prefixOf (T.empty, "parp"), ""),
                                   (T.prefixOf (T.empty, ""), "")
                                  ]
        ),
	( "prefixOf",
          fn () => check_pairs id [(T.prefixOf (test_trie (), "par"), "par"),
		                   (T.prefixOf (test_trie (), "parp"), "parp"),
		                   (T.prefixOf (test_trie (), "part"), "par")
                                  ]
        ),
	( "prefixOf-no-matches",
          fn () => check_pairs id [(T.prefixOf (test_trie (), "pa"), ""),
		                   (T.prefixOf (test_trie (), "quiz"), "")
                                  ]
        ),
        ( "patternMatch-empty",
          fn () => check_lists id (T.patternMatch (T.empty, []), [])
                   andalso
                   check_lists id (T.patternMatch (T.empty, [SOME #"p"]), [])
                   andalso
                   check_lists id (T.patternMatch (T.empty, [NONE]), [])
        ),
        ( "patternMatch-nil",
          fn () => check_lists id (T.patternMatch (test_trie (), []), [])
        ),
        ( "patternMatch-literal",
          fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [SOME #"p", SOME #"a", SOME #"r"]),
		                   ["par"])
        ),
        ( "patternMatch-one",
          fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [SOME #"p", NONE, SOME #"r"]),
		                   ["par"])
        ),
        ( "patternMatch-some",
	  fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [SOME #"a", NONE, NONE, NONE, NONE,
					 NONE, NONE, SOME #"e"]),
		                   ["abrasive", "alliance"])
        ),
        ( "patternMatch-no-literal",
          fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [SOME #"a", SOME #"l", SOME #"l"]),
		                   [])
        ),
        ( "patternMatch-none",
          fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [SOME #"q", NONE]),
		                   [])
                   andalso
                   check_lists id (T.patternMatch
                                       (test_trie (),
                                        [NONE, SOME #"q"]),
		                   [])
        ),
        ( "patternMatch-overlong",
          fn () => check_lists id (T.patternMatch
                                       (test_trie (),
                                        [NONE, NONE, NONE, NONE, NONE,
                                         NONE, NONE, NONE, NONE, NONE]),
		                   [])
        )
    ]

end

structure StringMTrieTest = TestTrieFn(struct
                                        structure T = StringMTrie
                                        val name = "string-mtrie"
                                        end)
structure StringATrieTest = TestTrieFn(struct
                                        structure T = StringATrie
                                        val name = "string-atrie"
                                        end)
                                                    
fun main () =
    let open TestSupport
    in
        app run_test_suite [
            (StringMTrieTest.name, StringMTrieTest.tests ()),
            (StringATrieTest.name, StringATrieTest.tests ())
        ]
    end

