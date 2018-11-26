
structure Rand = struct (* based on SML/NJ library *)

    val a : Int32.int = 48271
    val m : Int32.int = 2147483647  (* 2^31 - 1 *)
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
               
structure Timing = struct

    fun timed_call f =
        let val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
	in (Time.-(finish, start), result)
	end

    fun time (f, name) =
        let val (time, result) = timed_call f
            val secs = Time.toReal time
        in
            print (name ^ ": " ^ (Real.toString secs) ^ " sec\n");
            result
        end

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

    fun nameFor n label =
        Arg.name ^ ": " ^ Int.toString n ^ " " ^ label
                      
    fun testInserts keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "insertions"
        in
            Timing.time (fn () => Vector.foldl (fn (k, m) => M.insert (m, k, 1))
                                               M.empty keys,
                         name)
        end
    
    fun testDeletes keys m =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "deletes"
        in
            Timing.time (fn () => Vector.foldl (fn (k, m) => M.remove (m, k))
                                               m shuffled,
                         name)
        end
    
    fun testReads keys m =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "reads"
            val n = 
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 shuffled,
                             name)
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
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 nonKeys,
                             name)
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
            val name = nameFor nkeys "reads after deleting half"
            val m = Vector.foldl (fn (k, m) => M.remove (m, k)) m toDelete
            val n = 
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 toLookup,
                             name)
        in
            if n <> nkeys - half
            then print ("ERROR: Failed to find expected " ^
                        Int.toString (nkeys - half) ^
                        " keys (found " ^ Int.toString n ^ ")\n")
            else ()
        end

    fun testEnumeration keys m =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "key-value enumeration"
            val e = Timing.time (fn () => M.enumerate m, name)
        in
            if length e <> nkeys
            then print ("ERROR: Failed to enumerate expected " ^
                        Int.toString nkeys ^
                        " items (found " ^ Int.toString (length e) ^ ")\n")
            else ()
        end
            
    fun testAll nkeys =
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
                                    val name = "string persistent hash map"
                                    end)
                                  
structure TestStringRBMap = TestImmutableFn
                                (struct
                                  structure Map = struct
                                      open StringRBMap
                                      type key = Key.ord_key
                                      val enumerate = listItemsi
                                      val remove' = remove
                                      fun remove (m, k) = #1 (remove' (m, k))
                                  end
                                  val keys = randomStrings 50
                                  val distinctKeys = distinctStrings
                                  val name = "string red-black map"
                                  end)

structure TestStringMTrieMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open StringMTrieMap
                                        type 'a map = 'a trie
                                    end
                                    val keys = randomStrings 50
                                    val distinctKeys = distinctStrings
                                    val name = "string mtrie-map"
                                    end)

structure TestStringATrieMap = TestImmutableFn
                                  (struct
                                    structure Map = struct
                                        open StringATrieMap
                                        type 'a map = 'a trie
                                    end
                                    val keys = randomStrings 50
                                    val distinctKeys = distinctStrings
                                    val name = "string atrie-map"
                                    end)

structure IntRBMap = RedBlackMapFn
                            (struct
                              type ord_key = int
                              val compare = Int.compare
                              end)
                               
fun randomInts n =
    Vector.tabulate (n, fn _ => Int32.toInt (randomGenerator ()))

fun distinctInts ii =
    let val m = Vector.foldl (fn (i, m) => IntRBMap.insert (m, i, 1))
                             IntRBMap.empty
                             ii
        val n = Vector.length ii
        fun fill jj 0 = jj
          | fill jj remaining = 
            let val candidate = Int32.toInt (randomGenerator ())
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
                                    val name = "int persistent hash map"
                                    end)
           
structure TestIntRBMap = TestImmutableFn
                                (struct
                                  structure Map = struct
                                      open IntRBMap
                                      type key = Key.ord_key
                                      val enumerate = listItemsi
                                      val remove' = remove
                                      fun remove (m, k) = #1 (remove' (m, k))
                                  end
                                  val keys = randomInts
                                  val distinctKeys = distinctInts
                                  val name = "int red-black map"
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

    fun nameFor n label =
        Arg.name ^ ": " ^ Int.toString n ^ " " ^ label
                      
    fun testInserts keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "insertions"
            val m = M.new (nkeys div 10)
        in
            Timing.time (fn () => Vector.app (fn k => M.insert (m, k, 1)) keys,
                         name)
        end
           
    fun testInsertsPresized keys =
        let val nkeys = Vector.length keys
            val name = nameFor nkeys "insertions with preallocation"
            val m = M.new nkeys
        in
            Timing.time (fn () => Vector.app (fn k => M.insert (m, k, 1)) keys,
                         name)
        end
            
    fun testDeletes keys =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "deletes"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
        in
            Timing.time (fn () => Vector.app (fn k => M.remove (m, k)) shuffled,
                         name)
        end
    
    fun testReads keys =
        let val nkeys = Vector.length keys
            val shuffled = shuffle keys
            val name = nameFor nkeys "reads"
            val m = M.new nkeys
            val _ = Vector.app (fn k => M.insert (m, k, 1)) keys
            val n = 
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 shuffled,
                             name)
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
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 nonKeys,
                             name)
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
                Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                                       case M.find (m, k) of
                                                           NONE => tot
                                                         | SOME x => tot + x)
                                                   0 toLookup,
                             name)
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
            val e = Timing.time (fn () => M.enumerate m, name)
        in
            if length e <> nkeys
            then print ("ERROR: Failed to enumerate expected " ^
                        Int.toString nkeys ^
                        " items (found " ^ Int.toString (length e) ^ ")\n")
            else ()
        end
            
    fun testAll nkeys =
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
                                   val name = "int mutable hash table"
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
                                      val name = "string mutable hash table"
                                      end)
                                
fun usage () =
    let open TextIO
    in
        output (stdErr,
                "\nUsage:\n" ^
                "    hash-performance-test <n>\n" ^
                "\n" ^
                "where <n> is the number of items to insert/delete\n\n");
        raise Fail "Incorrect arguments specified"
    end

fun runTests n =
    (TestIntHashMap.testAll n;
     TestIntRBMap.testAll n;
     TestIntHashTable.testAll n;
     TestStringHashMap.testAll n;
     TestStringRBMap.testAll n;
(*     TestStringMTrieMap.testAll n;
     TestStringATrieMap.testAll n; *)
     TestStringHashTable.testAll n)
        
fun handleArgs args =
    case args of
        [nstr] => (case Int.fromString nstr of
                       NONE => usage ()
                     | SOME n => runTests n)
      | _ => usage ()
           
fun main () =
    handleArgs (CommandLine.arguments ())
    handle Fail msg =>
           (TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n");
            OS.Process.exit OS.Process.failure)
        
     
