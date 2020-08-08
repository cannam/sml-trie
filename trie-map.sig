
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE_MAP = sig

    (*!!! how far should this match ORD_MAP? *)
    
    type 'a trie
    type key

    (** Empty trie *)
    val empty : 'a trie

    (** Test whether a trie is empty *)
    val isEmpty : 'a trie -> bool

    (** Modify a key-value pair in the trie, returning a new trie. The
        function argument should map from the previous value
        associated with the key (or NONE if it was absent before) to
        the new value (or NONE if it is to be removed) *)
    val modify : 'a trie * key * ('a option -> 'a option) -> 'a trie

    (** Insert a key-value pair, returning a new trie. If the key is
        already present, its value will be updated in the new trie *)
    val insert : 'a trie * key * 'a -> 'a trie

    (** Return the trie with the given key removed. If the key is
        not present, the returned trie will be unchanged *)
    val remove : 'a trie * key -> 'a trie
                                          
    (** Test whether the trie contains the given key *)
    val contains : 'a trie * key -> bool

    (** Look for a key and return its corresponding value, or NONE if
        the key is not present in the trie *)
    val find : 'a trie * key -> 'a option

    (** Look for a key and return its corresponding value, raising
        Subscript if the key is not present in the trie *)
    val lookup : 'a trie * key -> 'a

    (** Return the longest prefix of the given key that is present as
        a key in the trie. The given key does not need to be present
        as a key in the trie. If it is present, it will be its own
        longest prefix, and so it will be returned. If there is no
        prefix of the given key in the trie, return an empty key *)
    val prefixOf : 'a trie * key -> key
                                   
    (** Fold over all the values in the trie, in sort order by key *)
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a trie -> 'b

    (** Fold over all the key-value pairs in the trie, in sort order
        by key *)
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a trie -> 'b

    (** Return a list of all key-value pairs in the trie, in sort order
        by key *)
    val enumerate : 'a trie -> (key * 'a) list

    (** Fold over all the key-value pairs in the trie that have the
        given prefix, in sort order by key. The prefix itself does not
        need to be present as a key in the trie *)
    val foldliPrefix : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * key) -> 'b

    (** Return a list of all key-value pairs in the trie that have the
        given key as a prefix, in sort order by key. The prefix itself
        does not need to be present as a key in the trie *)
    val enumeratePrefix : 'a trie * key -> (key * 'a) list

    (* Inclusive range of keys (first, last). If either is NONE, then
       the range is unbounded on that side *)
    type range = key option * key option
                                                      
    (** Fold over all the key-value pairs in the trie that are found
        within the given key range, in sort order by key *)
    val foldliRange : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * range) -> 'b

    (** Return a list of all key-value pairs in the trie that are
        found within the given key range, in sort order by key *)
    val enumerateRange : 'a trie * range -> (key * 'a) list
                                                                              
    (*!!! *)
    val locate : 'a trie * key * order -> (key * 'a) option

                                                     
end

