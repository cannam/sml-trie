
structure Rand = struct (* based on SML/NJ library *)

    val a : Int32.int = 48271
    val m : Int32.int = 2147483647 (* 2^31 - 1 *)

    val q = m div a
    val r = m mod a

    fun random seed = let 
          val hi = seed div q
          val lo = seed mod q
          val test = a * lo - r * hi
          in
            if test > 0 then test else test + m
          end
                     
    fun mkRandom seed = let
          val seed = ref seed
          in
            fn () => (seed := random (!seed); !seed)
          end

    fun range (i : int, j : int) : Int32.int -> int =
          if j < i 
            then raise Fail "j < i"
          else if j = i then fn _ => i
          else let 
            val R = Int32.fromInt (j - i)
            in
              if R = m then fn s => Int32.toInt s
              else fn s => (i + Int32.toInt (s mod (R+1)))
            end
end

val randomGenerator = Rand.mkRandom 1

fun randomString len =
    let val range = Rand.range (0, 61)
    in
        String.implode
            (List.tabulate
                 (len,
                  fn _ =>
                     let val n = range (randomGenerator ())
                     in
                         if n < 26
                         then Char.chr (n + Char.ord #"A")
                         else if n < 52
                         then Char.chr (n - 26 + Char.ord #"a")
                         else Char.chr (n - 52 + Char.ord #"0")
                     end))
    end

datatype verbosity = TERSE | VERBOSE
val verbosity = ref VERBOSE               
        
structure Timing = struct

    fun timed_call f =
        let val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
	in (Time.-(finish, start), result)
	end

    fun benchmark_with_setup (setup, f, name, nruns, count) =
        let val (time, result) =
                List.foldl (fn (_, (t', r')) =>
                               let val s = setup ()
                                   val start = Time.now ()
                                   val result = f s;
                                   val finish = Time.now ()
                                   val time = Time.-(finish, start)
                               in
                                   (Time.+(t', time), SOME result)
                               end)
                           (Time.zeroTime, NONE)
                           (List.tabulate (nruns, fn x => x))
	    val secs = Time.toReal time
        in
            (case !verbosity of
                 VERBOSE =>
                 print (name ^ " | " ^ (Int.toString nruns) ^ " | " ^
                        (Int.toString count) ^ " | " ^
                        (Real.toString (secs / Real.fromInt nruns)) ^ " | " ^
                        (let val rn = (Real.fromInt (nruns * count) / secs)
                         in
                             Int.toString (Real.floor rn)
                             handle Overflow => "lots"
                         end)
                        ^ "\n")
               | TERSE =>
                 print (Int.toString (Real.round ((secs * 1000.0) /
                                                  Real.fromInt nruns))
                        ^ "ms\t");
             case result of
                 SOME r => r
               | NONE => raise Fail "no result!?")
        end

    fun benchmark (f, name, nruns, count) =
        benchmark_with_setup (fn () => (), f, name, nruns, count)
end

fun shuffle vec =
    let val n = Vector.length vec
        val arr = Array.tabulate (n, fn i => Vector.sub (vec, i))
        fun shuffle' 0 = ()
          | shuffle' i =
            let val j = Rand.range (0, i) (randomGenerator ())
                val iv = Array.sub (arr, i)
                val jv = Array.sub (arr, j)
                val _ = Array.update (arr, j, iv)
                val _ = Array.update (arr, i, jv)
            in
                shuffle' (i-1)
            end
        val _ = shuffle' (n - 1)
    in
        Array.vector arr
    end

val numberOfRuns = 3

datatype test_type = TEST_MEMORY | TEST_ALL
                       
signature IMMUTABLE_MAP = sig
    type 'a map
    type key
    val empty : 'a map
    val insert : 'a map * key * 'a -> 'a map
    val remove : 'a map * key -> 'a map
    val find : 'a map * key -> 'a option
    val enumerate : 'a map -> (key * 'a) list
end

signature TEST_IMMUTABLE_ARG = sig

    structure Map : IMMUTABLE_MAP

    val keys : int -> Map.key vector
    val distinctKeys : Map.key vector -> Map.key vector
    val name : string
                        
end
                                   
functor TestImmutableFn (Arg : TEST_IMMUTABLE_ARG) = struct

    structure M = Arg.Map

    val name = Arg.name
                      
    fun nameFor n label =
        name ^ " | " ^ label
                      
    fun testInserts keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "inserts"
        in
            Timing.benchmark
                (fn () => Vector.foldl (fn (k, m) => M.insert (m, k, 1))
                                       (M.empty) keys,
                 name, numberOfRuns, nkeys)
        end
    
    fun testDeletes keys m =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "deletes"
        in
            Timing.benchmark
                (fn () => Vector.foldl (fn (k, m) => M.remove (m, k))
                                       m shuffled,
                 name, numberOfRuns, nkeys)
        end
    
    fun testReads keys m =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "reads"
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 shuffled,
                     name, numberOfRuns, nkeys)
        in
            if n <> nkeys
            then print ("ERROR: Failed to find all " ^ Int.toString nkeys ^
                        " keys (found " ^ Int.toString n ^ ")\n")
            else ()
        end
    
    fun testReadMisses keys m =
        let val nkeys = Vector.length keys
            val nonKeys = Arg.distinctKeys keys
            val name = nameFor nkeys "read misses"
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 nonKeys,
                     name, numberOfRuns, nkeys)
        in
            if n <> 0
            then print ("ERROR: Failed to find expected 0 keys (found " ^
                        Int.toString n ^ ")\n")
            else ()
        end
    
    fun testReadsAfterDeletingHalf keys m =
        let val nkeys = Vector.length keys
            val shuffledToDelete = shuffle keys
            val half = nkeys div 2
            val toDelete = Vector.tabulate
                               (half, fn i => Vector.sub (shuffledToDelete, i))
            val toLookup = shuffle keys
            val name = nameFor nkeys "read half-misses"
            val m = Vector.foldl (fn (k, m) => M.remove (m, k)) m toDelete
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 toLookup,
                     name, numberOfRuns, nkeys)
        in
            if n <> nkeys - half
            then print ("ERROR: Failed to find expected " ^
                        Int.toString (nkeys - half) ^
                        " keys (found " ^ Int.toString n ^ ")\n")
            else ()
        end

    fun testEnumeration keys m =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "enumeration"
            val e = Timing.benchmark
                        (fn () => M.enumerate m, name, numberOfRuns, nkeys)
        in
            if length e <> nkeys
            then print ("ERROR: Failed to enumerate expected " ^
                        Int.toString nkeys ^
                        " items (found " ^ Int.toString (length e) ^ ")\n")
            else ()
        end

    fun testMemory nkeys =
        let fun fill (m, 0) = m
              | fill (m, n) =
                let val key = Vector.sub (Arg.keys 1, 0)
                    val m' = M.insert (m, key, 1)
                in
                    fill (m', n-1)
                end
        in
            ignore (fill (M.empty, nkeys));
            print "- | - | - | - | - | -\n"
        end
            
    fun test testType nkeys =
        case testType of
            TEST_MEMORY => testMemory nkeys
          | TEST_ALL => 
            let val keys = Arg.keys nkeys
                val fullMap = testInserts keys
            in
                testDeletes keys fullMap;
                testReads keys fullMap;
                testReadMisses keys fullMap;
                testReadsAfterDeletingHalf keys fullMap;
                testEnumeration keys fullMap
            end
            
end

structure StringRBMap = RedBlackMapFn
                            (struct
                              type ord_key = string
                              val compare = String.compare
                              end)
                             
fun randomStrings len n =
    Vector.tabulate (n, fn _ => randomString len)

fun distinctStrings strs =
    let val range = Rand.range (0, String.size (Vector.sub (strs, 1)) - 1)
    in
        Vector.map (fn s =>
                       let val replacementIndex = range (randomGenerator ())
                       in
                           String.extract (s, 0, SOME replacementIndex) ^
                           "#" ^
                           String.extract (s, replacementIndex + 1, NONE)
                       end)
                   strs
    end
                                  
structure TestStringHashMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open StringHashMap
                                        type key = hash_key
                                        type 'a map = 'a hash_map
                                    end
                                    val keys = randomStrings 50
                                    val distinctKeys = distinctStrings
                                    val name = "PersistentHashMap/string"
                                    end)
                                  
structure TestStringRBMap = TestImmutableFn
                                (struct
                                  structure Map = struct
                                      open StringRBMap
                                      type key = Key.ord_key
                                      fun new () = empty
                                      val enumerate = listItemsi
                                      val remove' = remove
                                      fun remove (m, k) = #1 (remove' (m, k))
                                  end
                                  val keys = randomStrings 50
                                  val distinctKeys = distinctStrings
                                  val name = "RedBlackMap/string"
                                  end)

structure TestStringMTrieMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open StringMTrieMap
                                        type 'a map = 'a trie
                                    end
                                    val keys = randomStrings 50
                                    val distinctKeys = distinctStrings
                                    val name = "MTrieMap/string"
                                    end)

structure TestStringATrieMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open StringATrieMap
                                        type 'a map = 'a trie
                                    end
                                    val keys = randomStrings 50
                                    val distinctKeys = distinctStrings
                                    val name = "ATrieMap/string"
                                    end)

structure IntRBMap = RedBlackMapFn
                            (struct
                              type ord_key = int
                              val compare = Int.compare
                              end)
                               
fun randomInts n =
    shuffle (Vector.tabulate (n, fn i => i))

fun distinctInts ii =
    let val m = Vector.foldl (fn (i, m) => IntRBMap.insert (m, i, 1))
                             IntRBMap.empty
                             ii
        val n = Vector.length ii
        val range = Rand.range (0, 100000000)
        fun fill jj 0 = jj
          | fill jj remaining = 
            let val candidate = range (randomGenerator ())
            in
                if (IntRBMap.inDomain (m, candidate))
                then fill jj remaining
                else fill (candidate :: jj) (remaining - 1)
            end
    in
        Vector.fromList (fill [] n)
    end

structure IntHashKey = struct
    type hash_key = int
    fun hashVal i = Word32.fromInt i
    val sameKey = op=
end

structure IntHashMap = PersistentHashMapFn(IntHashKey)
                                                 
structure TestIntHashMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open IntHashMap
                                        type key = hash_key
                                        type 'a map = 'a hash_map
                                    end
                                    val keys = randomInts
                                    val distinctKeys = distinctInts
                                    val name = "PersistentHashMap/int"
                                    end)
           
structure TestIntRBMap = TestImmutableFn
                                (struct
                                  structure Map = struct
                                      open IntRBMap
                                      type key = Key.ord_key
                                      fun new () = empty
                                      val enumerate = listItemsi
                                      val remove' = remove
                                      fun remove (m, k) = #1 (remove' (m, k))
                                  end
                                  val keys = randomInts
                                  val distinctKeys = distinctInts
                                  val name = "RedBlackMap/int"
                                  end)

signature MUTABLE_MAP = sig
    type 'a map
    type key
    val new : int -> 'a map (* size hint *)
    val insert : 'a map * key * 'a -> unit
    val remove : 'a map * key -> unit
    val find : 'a map * key -> 'a option
    val enumerate : 'a map -> (key * 'a) list
end

signature TEST_MUTABLE_ARG = sig

    structure Map : MUTABLE_MAP

    val keys : int -> Map.key vector
    val distinctKeys : Map.key vector -> Map.key vector
    val name : string
                        
end
                                   
functor TestMutableFn (Arg : TEST_MUTABLE_ARG) = struct

    structure M = Arg.Map

    val name = Arg.name
                   
    fun nameFor n label =
        name ^ " | " ^ label
                      
    fun testInserts keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "inserts"
        in
            Timing.benchmark_with_setup
                (fn () => M.new (nkeys div 10),
                 fn m => Vector.app (fn k => M.insert (m, k, 1)) keys,
                 name, numberOfRuns, nkeys)
        end
           
    fun testInsertsPresized keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "inserts + prealloc"
        in
            Timing.benchmark_with_setup
                (fn () => M.new nkeys,
                 fn m => Vector.app (fn k => M.insert (m, k, 1)) keys,
                 name, numberOfRuns, nkeys)
        end
            
    fun testDeletes keys =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "deletes"
        in
            Timing.benchmark_with_setup
                (fn () => let val m = M.new nkeys
                              val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
                          in
                              m
                          end,
                 fn m => Vector.app (fn k => M.remove (m, k)) shuffled,
                 name, numberOfRuns, nkeys)
        end
    
    fun testReads keys =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "reads"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 shuffled,
                     name, numberOfRuns, nkeys)
        in
            if n <> nkeys
            then print ("ERROR: Failed to find all " ^ Int.toString nkeys ^
                        " keys (found " ^ Int.toString n ^ ")\n")
            else ()
        end
    
    fun testReadMisses keys =
        let val nkeys = Vector.length keys
            val nonKeys = Arg.distinctKeys keys
            val name = nameFor nkeys "read misses"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 nonKeys,
                     name, numberOfRuns, nkeys)
        in
            if n <> 0
            then print ("ERROR: Failed to find expected 0 keys (found " ^
                        Int.toString n ^ ")\n")
            else ()
        end
    
    fun testReadsAfterDeletingHalf keys =
        let val nkeys = Vector.length keys
            val shuffledToDelete = shuffle keys
            val half = nkeys div 2
            val toDelete = Vector.tabulate
                               (half, fn i => Vector.sub (shuffledToDelete, i))
            val toLookup = shuffle keys
            val name = nameFor nkeys "reads after deleting half"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
            val _ = Vector.app (fn k => M.remove (m, k)) toDelete
            val n = 
                Timing.benchmark
                    (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 toLookup,
                     name, numberOfRuns, nkeys)
        in
            if n <> nkeys - half
            then print ("ERROR: Failed to find expected " ^
                        Int.toString (nkeys - half) ^
                        " keys (found " ^ Int.toString n ^ ")\n")
            else ()
        end

    fun testEnumeration keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "key-value enumeration"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
            val e = Timing.benchmark
                        (fn () => M.enumerate m, name, numberOfRuns, nkeys)
        in
            if length e <> nkeys
            then print ("ERROR: Failed to enumerate expected " ^
                        Int.toString nkeys ^
                        " items (found " ^ Int.toString (length e) ^ ")\n")
            else ()
        end
            
    fun testMemory nkeys =
        let val m = M.new nkeys
            fun fill 0 = ()
              | fill n =
                let val key = Vector.sub (Arg.keys 1, 0)
                    val _ = M.insert (m, key, 1)
                in
                    fill (n-1)
                end
        in
            ignore (fill nkeys);
            print "- | - | - | - | - | -\n"
        end
            
    fun test testType nkeys =
        case testType of
            TEST_MEMORY => testMemory nkeys
          | TEST_ALL => 
            let val keys = Arg.keys nkeys
            in
                testInserts keys;
                testInsertsPresized keys;
                testDeletes keys;
                testReads keys;
                testReadMisses keys;
                testReadsAfterDeletingHalf keys;
                testEnumeration keys
            end
end

structure TestIntHashTable = TestMutableFn
                                 (struct
                                   structure Map = struct
                                       open IntHashTable
                                       type key = Key.hash_key
                                       type 'a map = 'a hash_table
                                       fun new n = mkTable (n, Subscript)
                                       val insert' = insert
                                       fun insert (m, k, v) = insert' m (k, v)
                                       val find' = find
                                       fun find (m, k) = find' m k
                                       val remove' = remove
                                       fun remove (m, k) = ignore (remove' m k)
                                       val enumerate = listItemsi
                                   end
                                   val keys = randomInts
                                   val distinctKeys = distinctInts
                                   val name = "HashTable/int"
                                   end)

structure StringHashTable = HashTableFn(struct
                                         type hash_key = string
                                         fun hashVal s = Word.fromLargeWord
                                                             (Word32.toLargeWord
                                                                  (StringHashKey.hashVal s))
                                         val sameKey = op=
                                         end)
                                 
structure TestStringHashTable = TestMutableFn
                                    (struct
                                      structure Map = struct
                                          open StringHashTable
                                          type key = Key.hash_key
                                          type 'a map = 'a hash_table
                                          fun new n = mkTable (n, Subscript)
                                          val insert' = insert
                                          fun insert (m, k, v) = insert' m (k, v)
                                          val find' = find
                                          fun find (m, k) = find' m k
                                          val remove' = remove
                                          fun remove (m, k) = ignore (remove' m k)
                                          val enumerate = listItemsi
                                      end
                                      val keys = randomStrings 50
                                      val distinctKeys = distinctStrings
                                      val name = "HashTable/string"
                                      end)

signature IMMUTABLE_ARRAY = sig
    type 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val toList : 'a array -> 'a list
    val update : 'a array * int * 'a -> 'a array
    val sub : 'a array * int -> 'a
    val length : 'a array -> int
end

signature TEST_IMMUTABLE_ARRAY_ARG = sig

    structure Array : IMMUTABLE_ARRAY

    val indices : int -> int vector
    val name : string
                   
end
                                
functor TestImmutableArrayFn (Arg : TEST_IMMUTABLE_ARRAY_ARG) = struct

    structure A = Arg.Array

    val name = Arg.name

    fun nameFor n label =
        name ^ " | " ^ label
                      
    fun testTabulate n =
        let val name = nameFor n "tabulation"
            val len = 
                Timing.benchmark
                    (fn () => A.length (A.tabulate (n, fn i => i)),
                     name, numberOfRuns, n)
        in
            if len <> n
            then print ("ERROR: Failed to insert all " ^
                        Int.toString n ^
                        " entries in tabulation (inserted " ^
                        Int.toString len ^ "\n")
            else ()
        end

    fun testMemory n =
        (ignore (testTabulate n);
         print "- | - | - | - | - | -\n")
            
    fun test testType nkeys =
        case testType of
            TEST_MEMORY => testMemory nkeys
          | TEST_ALL => 
            let val indices = Arg.indices nkeys
                val arr = testTabulate nkeys
            in
                ()
            end
    
end

structure TestPersistentArray = TestImmutableArrayFn
                                    (struct
                                      structure Array = PersistentArray
                                      val indices = randomInts
                                      val name = "PersistentArray/int"
                                      end)

structure TestPersistentQueue = TestImmutableArrayFn
                                    (struct
                                      structure Array = PersistentQueue
                                      val indices = randomInts
                                      val name = "PersistentQueue/int"
                                      end)

structure TestVector = TestImmutableArrayFn
                                    (struct
                                      structure Array = struct
                                      open Vector
                                      type 'a array = 'a vector
                                      fun toList v = foldr (op::) [] v
                                      end
                                      val indices = randomInts
                                      val name = "Vector/int"
                                      end)
                                      
val testStubs = [
    (TestIntHashMap.name, TestIntHashMap.test),
    (TestIntRBMap.name, TestIntRBMap.test),
    (TestIntHashTable.name, TestIntHashTable.test),
    (TestStringHashMap.name, TestStringHashMap.test),
    (TestStringRBMap.name, TestStringRBMap.test),
    (TestStringMTrieMap.name, TestStringMTrieMap.test),
    (TestStringATrieMap.name, TestStringATrieMap.test),
    (TestStringHashTable.name, TestStringHashTable.test),
    (TestPersistentArray.name, TestPersistentArray.test),
    (TestPersistentQueue.name, TestPersistentQueue.test),
    (TestVector.name, TestVector.test)
]

val tests =
    map (fn (n, t) => (n, t TEST_ALL)) testStubs @
    map (fn (n, t) => (n ^ "/memory", t TEST_MEMORY)) testStubs
                    
fun usage () =
    let open TextIO
    in
        output (stdErr,
                "\nUsage:\n" ^
                "    hash-performance-test <n>\n" ^
                "    hash-performance-test <n> <test-name>\n" ^
                "\n" ^
                "<n>         - size of test; number of items to insert/delete\n" ^
                "<test-name> - name of single test suite to run (default is to run them all)\n\n" ^
                "Test names ending in /memory print no output, and are intended to be run with\n" ^
                "memory profiling to determine how much space a filled container uses.\n\n" ^
                "Recognised test names are:\n" ^
                (String.concatWith
                     "; " (map (fn (name, _) => "\"" ^ name ^ "\"") tests)) ^
                "\n\n");
        raise Fail "Incorrect arguments specified"
    end

fun printHeader n =
    case !verbosity of
        VERBOSE =>
        print "Container | Test name | No of runs | No of keys | Time per run | Keys per sec\n"
      | TERSE =>
        (print ("\nWith " ^ (Int.toString n) ^ " keys:\n");
         print "Name\tInsert\tDelete\tRead\tRead/miss\tRead/half\tEnumerate")
                
fun runAllTests n =
    (* actually all but the "memory" tests *)
    (printHeader n;
     List.app (fn (tname, t) =>
                  let val parts = String.tokens (fn c => c = #"/") tname
                  in
                      if hd (rev parts) = "memory"
                      then ()
                      else case !verbosity of
                               VERBOSE => (print (tname ^ ":\n"); t n)
                             | TERSE => (print ("\n" ^ tname ^ "\t"); t n)
                  end) tests;
     case !verbosity of
         TERSE => print "\n"
       | _ => ())

fun runAllTestsAllCounts () =
    app runAllTests [ 10000, 100000, 1000000, 3000000, 10000000 ]
        
fun runATest name n =
    let val found = List.foldl (fn ((n, _), found) => if found
                                                      then true
                                                      else n = name) false tests
    in
        if not found
        then usage ()
        else
            (printHeader n;
             List.app (fn (tname, t) => if tname = name
                                        then t n
                                        else ()) tests)
    end
     
fun handleArgs args =
    case args of
        [] => (verbosity := TERSE; runAllTestsAllCounts ())
      | [nstr] =>
        (case Int.fromString nstr of
             NONE => usage ()
           | SOME n => runAllTests n)
      | [nstr, testname] =>
        (case Int.fromString nstr of
             NONE => usage ()
           | SOME n => runATest testname n)
      | _ => usage ()
           
fun main () =
    handleArgs (CommandLine.arguments ())
    handle e =>
           (TextIO.output (TextIO.stdErr, "Exception: " ^ (exnMessage e) ^ "\n");
            OS.Process.exit OS.Process.failure)
        
     
