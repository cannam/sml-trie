
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

    (** Insert a key-value pair, returning a new trie. If the key is
        already present, its value will be updated in the new trie *)
    val insert : 'a trie * key * 'a -> 'a trie

    (** Update a key-value pair in the trie, returning a new trie. The
        function argument should map from the previous value
        associated with the key (or NONE if it was absent before) to
        the new value. Thus update (t, k, f) is equivalent to insert
        (t, k, f (find (t, k)) except that it may be quicker. Note
        that this cannot remove anything from the trie *)
(*!!! later
    val update : 'a trie * key * ('a option -> 'a) -> 'a trie
*)

    (** Return the trie with the given key removed. If the key is
        not present, the returned trie will be unchanged *)
    val remove : 'a trie * key -> 'a trie
                                          
    (** Test whether the trie contains the given key *)
    val contains : 'a trie * key -> bool

    (** Look for a key and return its corresponding value, or NONE if
        the key is not present in the trie *)
    val find : 'a trie * key -> 'a option
                                     
    (** Fold over all the values in the trie, in sort order *)
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a trie -> 'b

    (** Fold over all the key-value pairs in the trie, in sort order *)
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a trie -> 'b

    (** Return a list of all key-value pairs in the trie, in sort order *)
    val enumerate : 'a trie -> (key * 'a) list

    (** Return the longest prefix of the given key that is present as
        a key in the trie. The given key does not need to be present
        as a key in the trie. If it is present, it will be its own
        longest prefix, and so it will be returned. If there is no
        prefix of the given key in the trie, return an empty key *)
    val prefixOf : 'a trie * key -> key

    (** Return a list of all entries in the trie that have the given
        key as a prefix, in sort order. The prefix itself does not
        need to be present as a key in the trie *)
    val prefixMatch : 'a trie * key -> (key * 'a) list

    (** Fold over all the values in the trie that have the given
        prefix, in sort order. The prefix itself does not need to be
        present as a key in the trie *)
    val foldlPrefixMatch : ('a * 'b -> 'b) -> 'b -> ('a trie * key) -> 'b

    (** Fold over all the key-value pairs in the trie that have the
        given prefix, in sort order. The prefix itself does not need
        to be present as a key in the trie *)
    val foldliPrefixMatch : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * key) -> 'b
    
end

