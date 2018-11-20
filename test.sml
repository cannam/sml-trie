
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
		                   [(T.contains (t, "aaa"), false),
		                    (T.contains (t, "pa"), false),
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
        ( "remove-one",
          fn () => check Int.toString
	                 (List.length
                              (T.enumerate
                                   (T.remove (test_trie (), "zebra"))),
                          List.length
                              (T.enumerate
                                   (test_trie ())) - 1)
        ),
        ( "remove-many",
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
                   andalso
                   check_lists id (T.prefixMatch (test_trie (), "aaa"), [ ])
                   andalso
                   check_lists id (T.prefixMatch (test_trie (), "zzz"), [ ])
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
          fn () => check_pairs id [(T.prefixOf (test_trie (), "AAA"), ""),
		                   (T.prefixOf (test_trie (), "pa"), ""),
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
                   andalso
                   check_lists id (T.patternMatch
                                       (test_trie (),
                                        [NONE, SOME #"A"]),
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

structure BitMappedVectorTest :> TESTS = struct

    open TestSupport

    structure V = BitMappedVector
    val name = "bit-mapped-vector"

    fun id x = x

    fun stringOptToString NONE = "<none>"
      | stringOptToString (SOME s) = s
                   
    fun tests () = [
        ( "new",
          fn () => check Int.toString (V.length (V.new 4), 4)
                   andalso
                   check Int.toString (V.length (V.new 0), 0)
        ),
        ( "isEmpty-empty",
          fn () => check Bool.toString (V.isEmpty (V.new 4), true)
        ),
        ( "isEmpty-nonempty",
          fn () => check Bool.toString
                         (V.isEmpty (V.update (V.new 4, 2, "hello")),
                          false)
        ),
        ( "contains",
          fn () => check_pairs
                       Bool.toString
                       [(V.contains (V.new 4, 2), false),
                        (V.contains (V.update (V.new 4, 2, "hello"), 2), true)]
        ),
        ( "find",
          fn () => V.find (V.new 4, 2) = NONE
                   andalso
                   V.find (V.update (V.new 4, 2, "hello"), 2) = SOME "hello"
        ),
        ( "tabulate-empty",
          fn () => check Bool.toString
                         (V.isEmpty (V.tabulate (0, fn _ => NONE)), true)
        ),
        ( "tabulate-length",
          fn () => let val v = V.tabulate
                                   (4,
                                    fn 0 => SOME "hello" | 1 => NONE
                                     | 2 => SOME "world" | _ => NONE)
                   in
                       check Int.toString (V.length v, 4)
                   end
        ),
        ( "tabulate-contents",
          fn () => let val v = V.tabulate
                                   (4,
                                    fn 0 => SOME "hello" | 1 => NONE
                                     | 2 => SOME "world" | _ => NONE)
                   in
                       check_pairs id [(V.sub (v, 0), "hello"),
                                       (V.sub (v, 2), "world")]
                       andalso
                       V.find (v, 1) = NONE
                       andalso
                       V.find (v, 3) = NONE
                   end
        ),
        ( "enumerate-empty",
          fn () => check Int.toString
                         (List.length (V.enumerate (V.new 0)), 0)
                   andalso
                   check_lists stringOptToString
                               (V.enumerate (V.new 3),
                                [ NONE, NONE, NONE ])
        ),
        ( "enumerate",
          fn () => let val v = V.tabulate
                                   (4,
                                    fn 0 => SOME "hello" | 1 => NONE
                                     | 2 => SOME "world" | _ => NONE)
                   in check_lists (fn NONE => "*" | SOME s => s)
                                  (V.enumerate v,
                                   [ SOME "hello", NONE,
                                     SOME "world", NONE ])
                   end
        ),
        ( "update",
          fn () => let val v = V.new 4
                   in
                       check_lists stringOptToString
                                   (V.enumerate (V.update (v, 0, "0")),
                                    [ SOME "0", NONE, NONE, NONE ])
                       andalso
                       check_lists stringOptToString
                                   (V.enumerate
                                        (V.update
                                             (V.update
                                                  (V.update (v, 2, "2"),
                                                   1, "1"),
                                              3, "3")),
                                    [ NONE, SOME "1", SOME "2", SOME "3" ])
                   end
        ),
        ( "erase-empty",
          fn () => let val v = V.new 4
                   in
                       check_lists stringOptToString
                                   (V.enumerate (V.erase (v, 2)),
                                    [ NONE, NONE, NONE, NONE ])
                   end
        ),
        ( "erase",
          fn () => let val v = V.tabulate
                                   (4,
                                    fn 0 => SOME "hello" | 1 => NONE
                                     | 2 => SOME "world" | _ => NONE)
                   in
                       check_lists stringOptToString
                                   (V.enumerate (V.erase (v, 2)),
                                    [ SOME "hello", NONE, NONE, NONE ])
                       andalso
                       check_lists stringOptToString
                                   (V.enumerate (V.erase (v, 0)),
                                    [ NONE, NONE, SOME "world", NONE ])
                       andalso
                       check Bool.toString
                             (V.isEmpty (V.erase (V.erase (v, 0), 2)), true)
                   end
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
structure StringBTrieTest = TestTrieFn(struct
                                        structure T = StringBTrie
                                        val name = "string-btrie"
                                        end)
                                                    
fun main () =
    let open TestSupport
    in
        app run_test_suite [
            (StringMTrieTest.name, StringMTrieTest.tests ()),
            (StringATrieTest.name, StringATrieTest.tests ()),
            (BitMappedVectorTest.name, BitMappedVectorTest.tests ()),
            (StringBTrieTest.name, StringBTrieTest.tests ())
        ]
    end

