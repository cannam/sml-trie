
functor PatternMatchTrieFn (M : PATTERN_MATCH_TRIE_MAP)
        :> PATTERN_MATCH_TRIE
               where type element = M.element where type entry = M.key where type trie = unit M.trie = struct

    structure T = TrieFn(M)
    open T

    type element = M.element
    type entry = M.key
    type pattern = element option list

    fun keysOf kvl =
        map (fn (k, v) => k) kvl

    fun patternMatch (t, p) =
        keysOf (M.patternMatch (t, p))

    fun foldlPatternMatch f acc (t, p) =
        M.foldliPatternMatch (fn (k, v, acc) => f (k, acc)) acc (t, p)
                             
end
                                                                        
functor ListMTrieFn (E : MTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    PatternMatchTrieFn(ListMTrieMapFn(E))
                                                                        
functor ListATrieFn (E : ATRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    PatternMatchTrieFn(ListATrieMapFn(E))
