
signature TRIE_MAP_KEYADAPTER_FN_ARG = sig
    type key
    structure T : TRIE_MAP where type key = key
    type external_key
    val enkey : external_key -> key
    val dekey : key -> external_key
end

(** Adapt a trie map into another trie map with a different external
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
    fun modify (t, k, f) = T.modify (t, enkey k, f)
    fun remove (t, k) = T.remove (t, enkey k)
    fun contains (t, k) = T.contains (t, enkey k)
    fun find (t, k) = T.find (t, enkey k)
    fun lookup (t, k) = T.lookup (t, enkey k)
    fun locate (t, k, order) = Option.map (fn (k, x) => (dekey k, x))
                                          (T.locate (t, enkey k, order))

    val search = T.search
    fun searchi f t = Option.map (fn (k, x) => (dekey k, x))
                                 (T.searchi (fn (k, x) => f (dekey k, x)) t)
                                          
    fun prefixOf (t, k) =
        Option.map dekey (T.prefixOf (t, enkey k))
                                 
    val foldl = T.foldl
    val foldr = T.foldr

    fun foldli f =
        T.foldli (fn (k, x, acc) => f (dekey k, x, acc))

    fun foldri f =
        T.foldri (fn (k, x, acc) => f (dekey k, x, acc))

    fun enumerate t =
        map (fn (k, x) => (dekey k, x)) (T.enumerate t)

    fun foldliPrefix f acc (t, k) =
        T.foldliPrefix (fn (k, x, acc) => f (dekey k, x, acc))
                       acc (t, enkey k)

    fun foldriPrefix f acc (t, k) =
        T.foldriPrefix (fn (k, x, acc) => f (dekey k, x, acc))
                       acc (t, enkey k)

    fun extractPrefix (t, k) =
        T.extractPrefix (t, enkey k)
                       
    fun enumeratePrefix (t, k) =
        map (fn (k, x) => (dekey k, x)) (T.enumeratePrefix (t, enkey k))

    type range = key option * key option

    fun foldlRange f acc (t, (leftConstraint, rightConstraint)) =
        T.foldlRange f acc (t,
                            (Option.map enkey leftConstraint,
                             Option.map enkey rightConstraint))

    fun foldliRange f acc (t, (leftConstraint, rightConstraint)) =
        T.foldliRange (fn (k, x, acc) => f (dekey k, x, acc))
                      acc (t,
                           (Option.map enkey leftConstraint,
                            Option.map enkey rightConstraint))

    fun foldrRange f acc (t, (leftConstraint, rightConstraint)) =
        T.foldrRange f acc (t,
                            (Option.map enkey leftConstraint,
                             Option.map enkey rightConstraint))

    fun foldriRange f acc (t, (leftConstraint, rightConstraint)) =
        T.foldriRange (fn (k, x, acc) => f (dekey k, x, acc))
                      acc (t,
                           (Option.map enkey leftConstraint,
                            Option.map enkey rightConstraint))

    fun extractRange (t, (leftConstraint, rightConstraint)) =
        T.extractRange (t, (Option.map enkey leftConstraint,
                            Option.map enkey rightConstraint))
                      
    fun enumerateRange (t, (leftConstraint, rightConstraint)) =
        map (fn (k, x) => (dekey k, x))
            (T.enumerateRange (t,
                               (Option.map enkey leftConstraint,
                                Option.map enkey rightConstraint)))
end

signature PATTERN_MATCH_TRIE_MAP_KEYADAPTER_FN_ARG = sig
    eqtype element
    type key
    structure T : PATTERN_MATCH_TRIE_MAP where type element = element where type key = key
    type external_key
    val enkey : external_key -> key
    val dekey : key -> external_key
end

(** Adapt a pattern-match trie map into another pattern-match trie map
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

    fun foldliPattern f =
        A.T.foldliPattern (fn (k, x, acc) => f (A.dekey k, x, acc))

    fun foldriPattern f =
        A.T.foldriPattern (fn (k, x, acc) => f (A.dekey k, x, acc))

    fun enumeratePattern (t, p) =
        map (fn (k, x) => (A.dekey k, x)) (A.T.enumeratePattern (t, p))
                             
end
