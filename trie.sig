
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE = sig

    type trie
    type t = trie
    type entry

    (* Empty trie *)
    val empty : t

    (** Test whether a trie is empty. Note that this could be slow, as
        it is possible for some implementations to have substantial
        structure but no actual data after items are removed *)
    val isEmpty : t -> bool

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

    (* Return the longest prefix of the given value that is present as
       an entry in the trie. The given value does not need to be
       present as an entry in the trie; if it is present, it will be
       its own longest prefix, and so it will be returned. If there is
       no prefix of the given entry in the trie, return an empty entry *)
    val prefixOf : t * entry -> entry

    (* Return a list of all entries in the trie that have the given
       entry as a prefix, in sort order. The prefix itself does not
       need to be present as an entry in the trie *)
    val prefixMatch : t * entry -> entry list

    (* Fold over all the entries in the trie that have the given
       prefix, in sort order. The prefix itself does not need to be
       present as an entry in the trie *)
    val foldlPrefixMatch : (entry * 'a -> 'a) -> 'a -> (t * entry) -> 'a 

  (*!!! + union / intersection / merge *)
                                                                          
end

