
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE_TEST_FN_ARG = sig
    structure T : STRING_TRIE
    val name : string
end

functor TrieTestFn (ARG : TRIE_TEST_FN_ARG) :> TESTS = struct

    open TestSupport

    structure T = ARG.T
    val name = ARG.name

    val strings = [ "poot", "parp", "par", "po",
		    "alligator", "zebra",
		    "alliance", "abrasive",
		    "a" ]

    val cutdown_strings = [ "poot", "parp", "po",
			    "alligator",
			    "alliance", "abrasive",
			    "a" ]
                              
    fun sorted s = ListMergeSort.sort String.> s

    fun test_trie () = List.foldl (fn (s, t) => T.add (t, s))
				  (T.empty)
				  strings

    fun tests () = [
	( "enumerate-empty",
          fn () => check_lists id (T.enumerate (T.empty), [])
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
        ( "remove-absent",
          fn () => check_lists id
                               (T.enumerate
                                    (T.remove
                                         (T.remove
                                              (T.remove (test_trie (), "zebr"),
                                               "zebras"),
                                          "flarp")),
                                T.enumerate
                                    (test_trie ()))
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
          fn () => T.isEmpty (T.empty)
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
		                   (T.prefixOf (test_trie (), "part"), "par"),
		                   (T.prefixOf (test_trie (), "abras"), "a"),
		                   (T.prefixOf (test_trie (), "abrasiveness"), "abrasive")
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

structure StringMTrieTest = TrieTestFn(struct
                                        structure T = StringMTrie
                                        val name = "string-mtrie"
                                        end)
structure StringATrieTest = TrieTestFn(struct
                                        structure T = StringATrie
                                        val name = "string-atrie"
                                        end)
structure StringBTrieTest = TrieTestFn(struct
                                        structure T = StringBTrie
                                        val name = "string-btrie"
                                        end)

functor TrieRangeTestFn (ARG : TRIE_TEST_FN_ARG) :> TESTS = struct

    open TestSupport

    structure T = ARG.T
    val name = ARG.name

    val strings = [ "a", "abrasive", "alliance", "alligator",
                    "asterisk", "asterix", "par", "parp", "po", "poot"
                  ]
                              
    fun test_trie () = List.foldl (fn (s, t) => T.add (t, s))
				  (T.empty)
				  strings

    val testdata = [
        ("mid-present-endpoints", SOME "alligator", SOME "parp",
         ["alligator", "asterisk", "asterix", "par", "parp"]),
        ("mid-absent-endpoints", SOME "abrade", SOME "aster",
         ["abrasive", "alliance", "alligator"]),
        ("mid-empty", SOME "allied", SOME "allies",
         []),
        ("from-start", NONE, SOME "attic",
         ["a", "abrasive", "alliance", "alligator", "asterisk", "asterix"]),
        ("to-end", SOME "pat", NONE,
         ["po", "poot"]),
        ("from-start-empty", NONE, SOME "",
         []),
        ("to-end-empty", SOME "port", NONE,
         []),
        ("all", NONE, NONE,
         strings)
    ]

    fun enumerateRange trie (from, to) =
        rev (T.foldlRange (fn (e, acc) => e :: acc) [] (trie, from, to))
                                  
    fun tests () =
        map (fn (name, from, to, expected) =>
                (name,
                 fn () => check_lists
                              id
                              (enumerateRange (test_trie ()) (from, to),
                               expected)))
            testdata
                                  
end

structure StringMTrieRangeTest = TrieRangeTestFn(struct
                                                  structure T = StringMTrie
                                                  val name = "string-mtrie-range"
                                                  end)
structure StringATrieRangeTest = TrieRangeTestFn(struct
                                                  structure T = StringATrie
                                                  val name = "string-atrie-range"
                                                  end)
structure StringBTrieRangeTest = TrieRangeTestFn(struct
                                                  structure T = StringBTrie
                                                  val name = "string-btrie-range"
                                                  end)
                                                
structure BitMappedVectorTest :> TESTS = struct

    open TestSupport

    structure V = BitMappedVector
    val name = "bitmapped-vector"

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
        ( "remove-empty",
          fn () => let val v = V.new 4
                   in
                       check_lists stringOptToString
                                   (V.enumerate (V.remove (v, 2)),
                                    [ NONE, NONE, NONE, NONE ])
                   end
        ),
        ( "remove",
          fn () => let val v = V.tabulate
                                   (4,
                                    fn 0 => SOME "hello" | 1 => NONE
                                     | 2 => SOME "world" | _ => NONE)
                   in
                       check_lists stringOptToString
                                   (V.enumerate (V.remove (v, 2)),
                                    [ SOME "hello", NONE, NONE, NONE ])
                       andalso
                       check_lists stringOptToString
                                   (V.enumerate (V.remove (v, 0)),
                                    [ NONE, NONE, SOME "world", NONE ])
                       andalso
                       check Bool.toString
                             (V.isEmpty (V.remove (V.remove (v, 0), 2)), true)
                   end
        )
    ]
                       
end

structure HashMapTest :> TESTS = struct

    (* Simple string hash that depends only on the string length, so
       that we can engineer collisions easily *)
    structure K :> HASH_KEY where type hash_key = string = struct
        fun hashString s = Word32.fromInt (String.size s * String.size s);
        type hash_key = string
        val hashVal = hashString
        val sameKey = op=
    end

    structure M :> PERSISTENT_HASH_MAP where type hash_key = string =
        PersistentHashMapFn(K)

    open TestSupport

    val name = "persistent-hash-map"

    fun test_map () = M.insert
                          (M.insert
                               (M.insert
                                    (M.empty, "squid", "octopus"),
                                "doughnut", "croissant"),
                           "dog", "wolf")
                   
    fun tests () = [
        ( "empty",
          fn () => M.isEmpty (M.empty)
                   andalso
                   not (M.contains (M.empty, "parp"))
                   andalso
                   M.enumerate (M.empty) = []
        ),
        ( "insert-simple",
          fn () => check Int.toString
                         (M.lookup (M.insert (M.empty, "poot", 5), "poot"), 5)
                   andalso
                   not (M.contains (M.insert (M.empty, "poot", 5), "parp"))
        ),
        ( "insert-multiple",
          fn () =>
             let val m = test_map ()
             in
                 check_pairs id
                             [(M.lookup (m, "squid"), "octopus"),
                              (M.lookup (m, "doughnut"), "croissant"),
                              (M.lookup (m, "dog"), "wolf")]
             end
        ),
        ( "insert-colliding",
          fn () =>
             let val m = test_map ()
                 val m = M.insert (m, "cat", "leopard")
                 val m = M.insert (m, "bread", "pizza")
                 val m = M.insert (m, "apple", "bilberry")
             in
                 check_pairs id
                             [(M.lookup (m, "squid"), "octopus"),
                              (M.lookup (m, "doughnut"), "croissant"),
                              (M.lookup (m, "dog"), "wolf"),
                              (M.lookup (m, "bread"), "pizza"),
                              (M.lookup (m, "apple"), "bilberry"),
                              (M.lookup (m, "cat"), "leopard")]
             end
        ),
        ( "insert-duplicate",
          fn () =>
             let val m = test_map ()
                 val m = M.insert (m, "squid", "octopus")
             in
                 check_sets (fn (k, v) => k ^ ":" ^ v)
                            (fn ((k, v), (k', v')) => k ^ v > k' ^ v')
                            (M.enumerate m,
                             [("squid", "octopus"),
                              ("doughnut", "croissant"),
                              ("dog", "wolf")])
             end
        ),                 
        ( "remove-only",
          fn () => M.isEmpty (M.remove (M.insert (M.empty, "poot", 5), "poot"))
        ),
        ( "remove-empty",
          fn () => M.isEmpty (M.remove (M.empty, "poot"))
        ),
        ( "remove-absent",
          fn () => M.enumerate (M.remove (M.insert (M.empty, "poot", 5), "pop"))
                   = [ ("poot", 5) ]
        ),
        ( "remove-one",
          fn () =>
             let val m = test_map ()
                 val m = M.remove (m, "doughnut")
             in
                 check_pairs id
                             [(M.lookup (m, "squid"), "octopus"),
                              (M.lookup (m, "dog"), "wolf")]
                 andalso
                 M.find (m, "doughnut") = NONE
             end
        ),
        ( "isEmpty-after-removals",
          fn () => let val m = test_map ()
                       val e1 = foldl (fn ((k, v), m) => M.remove (m, k))
                                      m (M.enumerate m)
                       val e2 = foldl (fn ((k, v), m) => M.remove (m, k))
                                      m (rev (M.enumerate m))
                   in
                       M.isEmpty e1 andalso M.isEmpty e2
                   end
        ),                                             
        ( "remove-colliding",
          fn () =>
             let val m = test_map ()
                 val m = M.insert (m, "cat", "leopard")
                 val m = M.insert (m, "bread", "pizza")
                 val m = M.insert (m, "apple", "bilberry")
                 val m = M.remove (m, "squid")
                 (* not in map - collides with multiple keys that are: *)
                 val m = M.remove (m, "table")
                 val m = M.remove (m, "dog")
                 (* not in map - collides with one key (doughnut) that is: *)
                 val m = M.remove (m, "semolina")
             in
                 check_pairs id
                             [(M.lookup (m, "doughnut"), "croissant"),
                              (M.lookup (m, "bread"), "pizza"),
                              (M.lookup (m, "apple"), "bilberry"),
                              (M.lookup (m, "cat"), "leopard")]
                 andalso
                 M.find (m, "squid") = NONE
                 andalso
                 M.find (m, "dog") = NONE
             end
        )
    ]
                       
end

signature PERSISTENT_ARRAY_TEST_FN_ARG = sig
    structure A : PERSISTENT_ARRAY
    val name : string
end
    
functor PersistentArrayTestFn (ARG : PERSISTENT_ARRAY_TEST_FN_ARG) :> TESTS = struct

    open TestSupport

    structure A = ARG.A
    val name = ARG.name
                   
    fun tests () = [
        ( "empty",
          fn () => A.isEmpty (A.empty)
                   andalso
                   A.length (A.empty) = 0
        ),
        ( "toList-empty",
          fn () => check_lists id (A.toList A.empty, [])
        ),
        ( "fromList",
          fn () => check_lists id (A.toList (A.fromList []), [])
                   andalso
                   check_lists id (A.toList (A.fromList [ "hello", "world" ]),
                                   [ "hello", "world" ])
        ),
        ( "append",
          fn () => check_lists id (A.toList (A.append (A.empty, "hello")),
                                   [ "hello" ])
                   andalso
                   check_lists id (A.toList (A.append
                                                 (A.append
                                                      (A.empty, "hello"),
                                                  "world")),
                                   [ "hello", "world" ])
                               
        ),
        ( "popEnd",
          fn () =>
             let val a = A.fromList [ "a", "b", "c", "d" ]
             in
                 check_lists id
                             (A.toList (#1 (A.popEnd a)), [ "a", "b", "c" ])
                 andalso 
                 check_pairs id
                             [((#2 (A.popEnd a)), "d")]
                 andalso
                 check_lists id
                             (A.toList (#1 (A.popEnd (A.fromList [ "a" ]))),
                              [])
                 andalso
                 ((A.popEnd (A.fromList []); false) handle Size => true)
             end
        ),
        ( "foldli",
          fn () => check_lists (fn (i, s) => Int.toString i ^ ":" ^ s)
                               (A.foldli (fn (i, x, acc) => (i, x) :: acc)
                                         []
                                         (A.fromList [ "hello", "world" ]),
                                [ (1, "world"), (0, "hello") ])
        ),
        ( "map",
          fn () => check_lists id
                               (A.toList (A.map (implode o rev o explode)
                                                (A.fromList [ "hello", "world" ])),
                                [ "olleh", "dlrow" ])
        ),
        ( "sub",
          fn () =>
             let val a = A.fromList [ "a", "b", "c", "d", "banana" ]
             in
                 check_pairs id [(A.sub (a, 0), "a"),
                                 (A.sub (a, 4), "banana"),
                                 (A.sub (a, 2), "c")]
                 andalso
                 ((A.sub (a, 5); false) handle Subscript => true)
                 andalso
                 ((A.sub (a, ~1); false) handle Subscript => true)
             end
        ),
        ( "update",
          fn () =>
             let val a = A.fromList [ "a", "b", "c", "d", "banana" ]
             in
                 check_lists id
                             (A.toList (A.update (a, 2, "weevil")),
                              [ "a", "b", "weevil", "d", "banana" ])
                 andalso (* check a was not mutated *)
                 check_lists id
                             (A.toList a,
                              [ "a", "b", "c", "d", "banana" ])
                 andalso
                 ((A.update (a, 5, "oops"); false) handle Subscript => true)
                 andalso
                 ((A.update (a, ~1, "oops"); false) handle Subscript => true)
             end
        ),
        ( "tabulate",
          fn () =>
             check_lists Int.toString
                         (A.toList (A.tabulate (4, fn x => x * 2)),
                          [ 0, 2, 4, 6 ])
             andalso
             check_lists Int.toString
                         (A.toList (A.tabulate (0, fn x => x * 2)),
                          [])
        )
    ]
end

structure PersistentArrayTest = PersistentArrayTestFn(struct
                                                       structure A = PersistentArray
                                                       val name = "persistent-array"
                                                       end)

structure PersistentQueueTest :> TESTS = struct

    open TestSupport

    val name = "persistent-queue"

    structure ArrayTestPart = PersistentArrayTestFn(struct
                                                     structure A = PersistentQueue
                                                     val name = name
                                                     end)

    structure Q = PersistentQueue
                                                   
    fun tests () =
        (ArrayTestPart.tests ()) @ [
        ( "prepend",
          fn () => check_lists id (Q.toList (Q.prepend (Q.empty, "hello")),
                                   [ "hello" ])
                   andalso
                   check_lists id (Q.toList (Q.prepend
                                                 (Q.prepend
                                                      (Q.empty, "hello"),
                                                  "world")),
                                   [ "world", "hello" ])
                               
        ),
        ( "popStart",
          fn () =>
             let val a = Q.fromList [ "a", "b", "c", "d" ]
             in
                 check_lists id
                             (Q.toList (#1 (Q.popStart a)), [ "b", "c", "d" ])
                 andalso 
                 check_pairs id
                             [((#2 (Q.popStart a)), "a")]
                 andalso
                 check_lists id
                             (Q.toList (#1 (Q.popStart (Q.fromList [ "a" ]))),
                              [])
                 andalso
                 ((Q.popStart (Q.fromList []); false) handle Size => true)
             end
        ),
        ( "queue-forward",
          fn () =>
             let val q1 = Q.fromList [ "a", "b" ]
                 val q2 = (#1 (Q.popStart (Q.append (q1, "c"))))
                 val q3 = (#1 (Q.popStart (Q.append (q2, "d"))))
                 val q4 = (#1 (Q.popStart q3))
                 val q5 = (#1 (Q.popStart q4))
             in
                 check_lists id (Q.toList q1, [ "a", "b" ])
                 andalso
                 check_lists id (Q.toList q2, [ "b", "c" ])
                 andalso
                 check_lists id (Q.toList q3, [ "c", "d" ])
                 andalso
                 check_pairs id [(Q.sub (q3, 1), "d")]
                 andalso
                 check_lists id (Q.toList q4, [ "d" ])
                 andalso
                 check_lists id (Q.toList q5, [])
                 andalso
                 Q.isEmpty q5
             end
        ),
        ( "queue-reverse",
          fn () =>
             let val q1 = Q.fromList [ "a", "b" ]
                 val q2 = (#1 (Q.popEnd (Q.prepend (q1, "c"))))
                 val q3 = (#1 (Q.popEnd (Q.prepend (q2, "d"))))
                 val q4 = (#1 (Q.popEnd q3))
                 val q5 = (#1 (Q.popEnd q4))
             in
                 check_lists id (Q.toList q1, [ "a", "b" ])
                 andalso
                 check_lists id (Q.toList q2, [ "c", "a" ])
                 andalso
                 check_lists id (Q.toList q3, [ "d", "c" ])
                 andalso
                 check_pairs id [(Q.sub (q3, 1), "c")]
                 andalso
                 check_lists id (Q.toList q4, [ "d" ])
                 andalso
                 check_lists id (Q.toList q5, [])
                 andalso
                 Q.isEmpty q5
             end
        )
        ]
end
                                                     
fun main () =
    let open TestSupport
    in
        app run_test_suite [
            (StringMTrieTest.name, StringMTrieTest.tests ()),
            (StringATrieTest.name, StringATrieTest.tests ()),
            (BitMappedVectorTest.name, BitMappedVectorTest.tests ()),
            (StringBTrieTest.name, StringBTrieTest.tests ()),
            (StringMTrieRangeTest.name, StringMTrieRangeTest.tests ()),
            (StringATrieRangeTest.name, StringATrieRangeTest.tests ()),
            (StringBTrieRangeTest.name, StringBTrieRangeTest.tests ()),
            (HashMapTest.name, HashMapTest.tests ()),
            (PersistentArrayTest.name, PersistentArrayTest.tests ()),
            (PersistentQueueTest.name, PersistentQueueTest.tests ())
        ]
    end

