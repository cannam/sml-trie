
functor VectorTrieMapFn (M : TRIE_NODE_MAP)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = M.key where type key = M.key vector = struct

    structure Key = struct
        type element = M.key
        type key = M.key vector * int (* start index *)
        fun isEmpty (v, i) = i >= Vector.length v
        fun head (v, i) = Vector.sub (v, i)
        fun tail (v, i) = (v, i+1)
        fun explode k = if isEmpty k then [] else (head k) :: explode (tail k)
        fun implode ee = (Vector.fromList ee, 0)
        fun equal ((v1, i1), (v2, i2)) =
            let fun equal' (k1, k2) =
                    isEmpty k1 orelse
                    (head k1 = head k2 andalso equal' (tail k1, tail k2))
            in
                Vector.length v1 - i1 = Vector.length v2 - i2 andalso
                equal' ((v1, i1), (v2, i2))
            end
    end
                                                           
    structure T = TrieMapFn
                      (struct
                        structure M = M
                        structure K = Key
                        type element = K.element
                        type key = K.key
                        end)

    fun enkey v = (v, 0)
    fun dekey (v, 0) = v
      | dekey (v, i) = Vector.tabulate (Vector.length v - i,
                                        fn j => Vector.sub (v, j - i))
                             
    type 'a trie = 'a T.trie
    type element = M.key
    type key = M.key vector
    type pattern = element option list
                               
    val empty = T.empty
    val isEmpty = T.isEmpty

    fun insert (t, k, x) = T.insert (t, enkey k, x)
    fun update (t, k, f) = T.update (t, enkey k, f)
    fun remove (t, k) = T.remove (t, enkey k)
    fun contains (t, k) = T.contains (t, enkey k)
    fun find (t, k) = T.find (t, enkey k)
    fun lookup (t, k) = T.lookup (t, enkey k)
                                 
    val foldl = T.foldl

    fun foldli f =
        T.foldli (fn (k, x, acc) => f (dekey k, x, acc))

    fun enumerate t =
        map (fn (k, x) => (dekey k, x)) (T.enumerate t)

    fun prefixOf (t, k) =
        dekey (T.prefixOf (t, enkey k))

    fun prefixMatch (t, k) =
        map (fn (k, x) => (dekey k, x)) (T.prefixMatch (t, enkey k))

    fun foldlPrefixMatch f acc (t, k) =
        T.foldlPrefixMatch f acc (t, enkey k)

    fun foldliPrefixMatch f acc (t, k) =
        T.foldliPrefixMatch (fn (k, x, acc) => f (dekey k, x, acc))
                            acc (t, enkey k)

    fun patternMatch (t, p) =
        map (fn (k, x) => (dekey k, x)) (T.patternMatch (t, p))

    fun foldliPatternMatch f =
        T.foldliPatternMatch (fn (k, x, acc) => f (dekey k, x, acc))
                             
end
