
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature STRING_TRIE_MAP = sig
    include PATTERN_MATCH_TRIE_MAP
    where type key = string
    where type element = char
end

signature CHAR_LIST_TRIE_MAP = sig
    include PATTERN_MATCH_TRIE_MAP
    where type key = char list
    where type element = char
end
                            
functor StringTrieMapFn (T : CHAR_LIST_TRIE_MAP) :> STRING_TRIE_MAP = struct

    type 'a trie = 'a T.trie
    type element = char
    type pattern = element option list
    type key = string

    val empty = T.empty

    val isEmpty = T.isEmpty

    fun update (trie, s, f) =
        T.update (trie, String.explode s, f)
                      
    fun insert (trie, s, v) =
        T.insert (trie, String.explode s, v)

    fun contains (trie, s) =
        T.contains (trie, String.explode s)
                         
    fun remove (trie, s) =
        T.remove (trie, String.explode s)

    fun find (trie, s) =
        T.find (trie, String.explode s)
                 
    fun foldl f acc trie =
        T.foldl f acc trie

    fun foldli f acc trie =
        T.foldli (fn (k, v, acc) => f (String.implode k, v, acc))
                 acc trie

    fun enumerate trie =
        List.map (fn (k, v) => (String.implode k, v))
                 (T.enumerate trie)

    fun foldlPrefixMatch f acc (trie, s) =
        T.foldlPrefixMatch f acc (trie, String.explode s)

    fun foldliPrefixMatch f acc (trie, s) =
        T.foldliPrefixMatch (fn (k, v, acc) => f (String.implode k, v, acc))
                            acc (trie, String.explode s)
                 
    fun prefixMatch (trie, s) =
        List.map (fn (k, v) => (String.implode k, v))
                 (T.prefixMatch (trie, String.explode s))

    fun prefixOf (trie, s) =
        String.implode (T.prefixOf (trie, String.explode s))

    fun foldliPatternMatch f acc (trie, p) =
        T.foldliPatternMatch (fn (k, v, acc) => f (String.implode k, v, acc))
			     acc (trie, p)
                 
    fun patternMatch (trie, p) =
        List.map (fn (k, v) => (String.implode k, v))
                 (T.patternMatch (trie, p))
                 
end
    
