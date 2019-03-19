
signature TRIE_MAP_KEYADAPTER_FN_ARG = sig
    type key
    structure T : TRIE_MAP where type key = key
    type external_key
    val enkey : external_key -> key
    val dekey : key -> external_key
end

(* Adapt a trie map into another trie map with a different external
   key type. Useful when implementing a map whose key can be readily
   converted into a type for which an implementation already exists,
   such as a list or vector. You provide conversion functions between
   the existing trie key and the external key you want for your trie
   signature, in both directions, and it provides the wrapped API.
*)
functor TrieMapKeyAdapterFn (A : TRIE_MAP_KEYADAPTER_FN_ARG)
        :> TRIE_MAP
               where type key = A.external_key
               where type 'a trie = 'a A.T.trie = struct

    structure T = A.T

    type 'a trie = 'a T.trie
    type key = A.external_key

    val enkey = A.enkey
    val dekey = A.dekey
                               
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
end

signature PATTERN_MATCH_TRIE_MAP_KEYADAPTER_FN_ARG = sig
    eqtype element
    type key
    structure T : PATTERN_MATCH_TRIE_MAP where type element = element where type key = key
    type external_key
    val enkey : external_key -> key
    val dekey : key -> external_key
end

(* Adapt a pattern-match trie map into another pattern-match trie map
   with a different external key type. Useful when implementing a map
   whose key can be readily converted into a type for which an
   implementation already exists, such as a list or vector. You
   provide conversion functions between the existing trie key and the
   external key you want for your trie signature, in both directions,
   and it provides the wrapped API.
*)
functor PatternMatchTrieMapKeyAdapterFn (A : PATTERN_MATCH_TRIE_MAP_KEYADAPTER_FN_ARG)
        :> PATTERN_MATCH_TRIE_MAP
               where type element = A.T.element
               where type key = A.external_key
               where type 'a trie = 'a A.T.trie = struct

    structure T = TrieMapKeyAdapterFn(A)
                      
    type 'a trie = 'a T.trie
    type key = A.external_key
    type element = A.element
    type pattern = element option list
                           
    open T 

    fun patternMatch (t, p) =
        map (fn (k, x) => (A.dekey k, x)) (A.T.patternMatch (t, p))

    fun foldliPatternMatch f =
        A.T.foldliPatternMatch (fn (k, x, acc) => f (A.dekey k, x, acc))
                             
end
