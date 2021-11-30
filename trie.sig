
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE = sig

    type trie
    type entry

    (** Empty trie *)
    val empty : trie

    (** Test whether a trie is empty. Note that this could be slow, as
        it is possible for some implementations to have substantial
        structure but no actual data after items are removed *)
    val isEmpty : trie -> bool

    (** Add the given entry, returning a new trie. If the entry is
        already present, the returned trie will be unchanged *)
    val add : trie * entry -> trie

    (** Test whether the trie contains the given entry *)
    val contains : trie * entry -> bool

    (** Return the entry closest to the given one that exists in the
        trie. If order is EQUAL, then a result is returned only if the
        given entry is actually in the trie; if order is LESS or
        GREATER, the given entry is still returned if it exists, but
        otherwise the next entry comparing respectively less or
        greater than it is returned, if there is one *)
    val locate : trie * entry * order -> entry option

    (** Return the trie with the given entry removed. If the entry is
        not present, the returned trie will be unchanged *)
    val remove : trie * entry -> trie

    (** Return the longest prefix of the given value that is present
        as an entry in the trie. The given value does not need to be
        present as an entry in the trie; if it is present, it will be
        its own longest prefix, and so it will be returned. If there
        is no prefix of the given entry in the trie, return NONE *)
    val prefixOf : trie * entry -> entry option

    (** Fold over all the entries in the trie, in sort order *)
    val foldl : (entry * 'a -> 'a) -> 'a -> trie -> 'a

    (** Fold over all the entries in the trie, in reverse of sort
        order *)
    val foldr : (entry * 'a -> 'a) -> 'a -> trie -> 'a

    (** Return a list of all entries in the trie, in sort order *)
    val enumerate : trie -> entry list

    (** Fold over all the entries in the trie that have the given
        prefix, in sort order. The prefix itself does not need to be
        present as an entry in the trie *)
    val foldlPrefix : (entry * 'a -> 'a) -> 'a -> (trie * entry) -> 'a 

    (** Fold over all the entries in the trie that have the given
        prefix, in reverse of sort order. The prefix itself does not
        need to be present as an entry in the trie *)
    val foldrPrefix : (entry * 'a -> 'a) -> 'a -> (trie * entry) -> 'a 

    (** Return a trie containing all entries in the trie that have the
        given entry as a prefix, sharing the structure of the given
        trie as far as possible. The prefix itself does not need to be
        present as an entry in the trie *)
    val extractPrefix : trie * entry -> trie

    (** Return a list of all entries in the trie that have the given
        entry as a prefix, in sort order. The prefix itself does not
        need to be present as an entry in the trie *)
    val enumeratePrefix : trie * entry -> entry list

    (** Inclusive range of entries (first, last). If either is NONE,
        then the range is unbounded on that side *)
    type range = entry option * entry option
                                                      
    (** Fold over all the entries in the trie that are found within
        the given range, in sort order *)
    val foldlRange : (entry * 'a -> 'a) -> 'a -> (trie * range) -> 'a
                                                      
    (** Fold over all the entries in the trie that are found within
        the given range, in reverse of sort order *)
    val foldrRange : (entry * 'a -> 'a) -> 'a -> (trie * range) -> 'a

    (** Return a trie containing all entries in the trie that are
        found within the given range, sharing the structure of the
        given trie as far as possible *)
    val extractRange : trie * range -> trie

    (** Return a list of all entries in the trie that are found within
        the given range, in sort order *)
    val enumerateRange : trie * range -> entry list
                                                                       
  (*!!! + union / intersection / merge *)
                                                                          
end

