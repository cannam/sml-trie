
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
                     
fun randomStrings len n =
    Vector.tabulate (n, fn _ => randomString len)

structure Timing = struct

    fun timed_call f =
        let val _ = print "about to start\n"
            val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val _ = print "finished\n"
	in (Time.-(finish, start), result)
	end

    fun time (f, name) =
        let val (time, result) = timed_call f
            val secs = Time.toReal time
        in
            print ("Time for " ^ name ^ ": " ^ (Real.toString secs) ^ " sec\n")
        end

end

structure M = StringHashMap

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
                  
fun testInserts nkeys =
    let val keys = randomStrings 50 nkeys
        val _ = print "have keys\n"
        val name = Int.toString nkeys ^ " insertions"
    in
        Timing.time (fn () => Vector.foldl (fn (k, m) => M.insert (m, k, 1))
                                           M.empty keys,
                     name)
    end

fun testDeletes nkeys =
    let val keys = randomStrings 50 nkeys
        val shuffled = shuffle keys
        val name = Int.toString nkeys ^ " deletes"
        val m = Vector.foldl (fn (k, m) => M.insert (m, k, 1)) M.empty keys
    in
        Timing.time (fn () => Vector.foldl (fn (k, m) => M.remove (m, k))
                                           m shuffled,
                     name)
    end

fun testReads nkeys =
    let val keys = randomStrings 50 nkeys
        val shuffled = shuffle keys
        val name = Int.toString nkeys ^ " reads"
        val m = Vector.foldl (fn (k, m) => M.insert (m, k, 1)) M.empty keys
    in
        Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 shuffled,
                     name)
    end

fun testReadMisses nkeys =
    let val keys = randomStrings 50 nkeys
        val nonKeys = randomStrings 50 nkeys
        val name = Int.toString nkeys ^ " read misses"
        val m = Vector.foldl (fn (k, m) => M.insert (m, k, 1)) M.empty keys
    in
        Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 nonKeys,
                     name)
    end

fun testReadsAfterDeletingHalf nkeys =
    let val keys = randomStrings 50 nkeys
        val shuffledToDelete = shuffle keys
        val toDelete = Vector.tabulate
                           (nkeys div 2,
                            fn i => Vector.sub (shuffledToDelete, i))
        val toLookup = shuffle keys
        val name = Int.toString nkeys ^ " reads after deleting half"
        val m = Vector.foldl (fn (k, m) => M.insert (m, k, 1)) M.empty keys
        val m = Vector.foldl (fn (k, m) => M.remove (m, k)) m toDelete
    in
        Timing.time (fn () => Vector.foldl (fn (k, tot) =>
                                               case M.find (m, k) of
                                                   NONE => tot
                                                 | SOME x => tot + x)
                                           0 toLookup,
                     name)
    end

fun main () =
    let val n = 1000000
    in
        testInserts n;
        testDeletes n;
        testReads n;
        testReadMisses n;
        testReadsAfterDeletingHalf n
    end
        
     
