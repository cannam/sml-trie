
signature TRIE = sig

    type t
    type entry

    (* Empty trie *)
    val empty : t

    (* Add the given entry, returning a new trie. If the entry is
       already present, the returned trie will be unchanged *)
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

    (* Fold over all the entries in the trie that have the given entry
       as a prefix, in sort order *)
    val foldl_match : (entry * 'a -> 'a) -> 'a -> (t * entry) -> 'a 

    (* Return a list of all entries in the trie that have the given
       entry as a prefix, in sort order *)
    val match : t * entry -> entry list

    (* Return the longest prefix of the given entry that is present as
       an entry in the trie. (If the given entry is itself present, it
       will be its own longest prefix and so it will be returned.) *)
    val prefix_of : t * entry -> entry 
    
end
