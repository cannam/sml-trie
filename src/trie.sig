
signature TRIE = sig

    (*!!! should have type trie as well? check mlton coding standards for comparison *)
    type t
    type entry

    (* Empty trie *)
    val empty : t

    (* Add the given entry, returning a new trie. If the entry is
       already present, the returned trie will be unchanged *)
    (*!!! should be insert for consistency with ORD_SET ? what about other names? *)
    val add : t * entry -> t

    (* Test whether the trie contains the given entry *)
    val contains : t * entry -> bool

    (* Return the trie with the given entry removed. If the entry is
       not present, the returned trie will be unchanged *)
    val remove : t * entry -> t

    (* Fold over all the entries in the trie, in sort order *)
    val foldl : (entry * 'a -> 'a) -> 'a -> t -> 'a

    (* Return a list of all entries in the trie, in sort order *)
    val enumerate : t -> entry list

    (* Fold over all the entries in the trie that have the given
       prefix, in sort order. The prefix does not need to be present as
       an entry in the trie *)
    val foldl_prefix_match : (entry * 'a -> 'a) -> 'a -> (t * entry) -> 'a 

    (* Return a list of all entries in the trie that have the given
       entry as a prefix, in sort order. The prefix does not need to
       be present as an entry in the trie *)
    val prefix_match : t * entry -> entry list

    (* Return the longest prefix of the given value that is present as
       an entry in the trie. The given value does not need to be
       present as an entry in the trie; if it is present, it will be
       its own longest prefix, and so it will be returned. If there is
       no prefix of the given entry in the trie, return an empty entry *)
    val prefix_of : t * entry -> entry
    
end

signature PATTERN_MATCH_TRIE = sig

    (* A trie that can do pattern matches as well as prefix
       matches. Say you have a trie containing some (conceptual)
       list/string entries matching ABB, ABC, and BBC. With the plain
       trie you can match e.g. prefix AB to return ABB and ABC. With a
       pattern-match trie you can alternatively provide a query list
       like [NONE, SOME B, NONE] to return all entries having three
       elements with B in the middle, here all three of the listed
       entries.

       This differs from the plain trie not only because of the
       additional pattern match function, but also because the type of
       the individual elements is exposed, whereas TRIE uses an atomic
       entry type. *)
    
    include TRIE

    type element
    type pattern = element option list

    (* Fold over all the entries in the trie that match the given
       pattern, in sort order. Will only return entries with exactly
       the same number of elements as values in the pattern *)
    val foldl_pattern_match : (entry * 'a -> 'a) -> 'a -> (t * pattern) -> 'a

    (* Return all the entries in the trie that match the given
       pattern, in sort order. Will only return entries with exactly
       the same number of elements as values in the pattern *)
    val pattern_match : (t * pattern) -> entry list
	    
end
