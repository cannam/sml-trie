
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE_MAP = sig

    type 'a trie
    type key

    (** Empty trie *)
    val empty : 'a trie

    (** Test whether a trie is empty *)
    val isEmpty : 'a trie -> bool

    (** Alter a key-value pair in the trie, returning a new trie. The
        function argument should map from the previous value
        associated with the key, or NONE if it was absent before, to
        the new value, or NONE if it is to be removed. (This is called
        alter rather than modify to avoid confusion with the array
        modify functions, which do something rather different) *)
    val alter : 'a trie * key * ('a option -> 'a option) -> 'a trie

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
                                                                              
    (** Look for the closest key to the given one and return it with
        its corresponding value. If order is EQUAL, then a result is
        returned only if the given key is actually in the trie (like
        find); if order is LESS or GREATER, the given key is still
        returned if it exists, but otherwise the next key comparing
        respectively less or greater than it is returned, if there is
        one *)
    val locate : 'a trie * key * order -> (key * 'a) option
                                                     
    (** Return the longest prefix of the given key that is present as
        a key in the trie. The given key does not need to be present
        as a key in the trie. If it is present, it will be its own
        longest prefix, and so it will be returned. If there is no
        prefix of the given key in the trie, return NONE *)
    val prefixOf : 'a trie * key -> key option

    (** Examine the values in the trie, in sort order by key, and
        return the first one for which the given function returns
        true. This is similar to Vector.find in that it must iterate
        through the trie rather than performing a direct lookup *)
    val search : ('a -> bool) -> 'a trie -> 'a option

    (** Examine the key/value pairs in the trie, in sort order by key,
        and return the first one for which the given function returns
        true. This is similar to Vector.findi in that it must iterate
        through the trie rather than performing a direct lookup *)
    val searchi : (key * 'a -> bool) -> 'a trie -> (key * 'a) option

    (** Map all the values in the trie to new values using the given
        map function, supplied with key and value for each. *)
    val mapi : (key * 'a -> 'b) -> 'a trie -> 'b trie

    (** Map all the values in the trie to new values using the given
        map function, supplied with value only. *)
    val map : ('a -> 'b) -> 'a trie -> 'b trie

    (** Fold over all the key-value pairs in the trie, in sort order
        by key *)
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a trie -> 'b
                                   
    (** Fold over all the values in the trie, in sort order by key *)
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a trie -> 'b

    (** Fold over all the key-value pairs in the trie, in reverse of
        sort order by key *)
    val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a trie -> 'b
                                   
    (** Fold over all the values in the trie, in reverse of sort order
        by key *)
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a trie -> 'b
                                                               
    (** Return a list of all key-value pairs in the trie, in sort order
        by key *)
    val enumerate : 'a trie -> (key * 'a) list

    (** Fold over all the key-value pairs in the trie that have the
        given prefix, in sort order by key. The prefix itself does not
        need to be present as a key in the trie *)
    val foldliPrefix : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * key) -> 'b

    (** Fold over all the key-value pairs in the trie that have the
        given prefix, in reverse of sort order by key. The prefix
        itself does not need to be present as a key in the trie *)
    val foldriPrefix : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * key) -> 'b

    (** Return a trie containing all key-value pairs in the trie that
        have the given key as a prefix, sharing the structure of the
        given trie as far as possible. The prefix itself does not need
        to be present as a key in the trie *)
    val extractPrefix : 'a trie * key -> 'a trie

    (** Return a list of all key-value pairs in the trie that have the
        given key as a prefix, in sort order by key. The prefix itself
        does not need to be present as a key in the trie *)
    val enumeratePrefix : 'a trie * key -> (key * 'a) list

    (** Inclusive range of keys (first, last). If either is NONE, then
        the range is unbounded on that side *)
    type range = key option * key option
                                                      
    (** Fold over all the values in the trie that are found within the
        given key range, in sort order by key *)
    val foldlRange : ('a * 'b -> 'b) -> 'b -> ('a trie * range) -> 'b
                                                      
    (** Fold over all the key-value pairs in the trie that are found
        within the given key range, in sort order by key *)
    val foldliRange : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * range) -> 'b
                                                      
    (** Fold over all the values in the trie that are found within the
        given key range, in reverse of sort order by key *)
    val foldrRange : ('a * 'b -> 'b) -> 'b -> ('a trie * range) -> 'b
                                                      
    (** Fold over all the key-value pairs in the trie that are found
        within the given key range, in reverse of sort order by key *)
    val foldriRange : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * range) -> 'b

    (** Return the keys at either end of the given range. That is,
        return keys k1 and k2, present in the trie, for which the
        range (SOME k1, SOME k2) is equivalent to the given range
        within the given trie. If the given range is empty within the
        given trie, return NONE. This is equivalent to checking the
        first keys of foldli/foldriRange, but typically faster. *)
    val resolveRange : 'a trie * range -> (key * key) option
                                                                              
    (** Return a trie containing all key-value pairs in the trie that
        are found within the given key range, sharing the structure of
        the given trie as far as possible *)
    val extractRange : 'a trie * range -> 'a trie
                                                                              
    (** Return a list of all key-value pairs in the trie that are
        found within the given key range, in sort order by key *)
    val enumerateRange : 'a trie * range -> (key * 'a) list
                                                     
end

