
functor TrieFn (M : PATTERN_MATCH_TRIE_MAP)
	:> PATTERN_MATCH_TRIE
	       where type element = M.element where type entry = M.key = struct

    open M

    type element = M.element
    type entry = M.key
    type pattern = element option list

    type trie = unit M.trie
    type t = trie

    fun keysOf kvl =
        map (fn (k, v) => k) kvl
            
    fun add (t, e) =
        M.insert (t, e, ())

    fun foldl f acc t =
        M.foldli (fn (k, v, acc) => f (k, acc)) acc t
                 
    fun enumerate t =
        keysOf (M.enumerate t)

    fun prefixMatch (t, e) =
        keysOf (M.prefixMatch (t, e))

    fun foldlPrefixMatch f acc (t, e) =
        M.foldliPrefixMatch (fn (k, v, acc) => f (k, acc)) acc (t, e)

    fun patternMatch (t, p) =
        keysOf (M.patternMatch (t, p))

    fun foldlPatternMatch f acc (t, p) =
        M.foldliPatternMatch (fn (k, v, acc) => f (k, acc)) acc (t, p)
                          
end
                                                                        
functor ListMTrieFn (E : MTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    TrieFn(ListMTrieMapFn(E))
                                                                        
functor ListATrieFn (E : ATRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    TrieFn(ListATrieMapFn(E))
