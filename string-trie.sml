
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature STRING_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = string
    where type element = char
end

structure StringMTrie :> STRING_TRIE = PatternMatchTrieFn(StringMTrieMap)
structure StringATrie :> STRING_TRIE = PatternMatchTrieFn(StringATrieMap)
structure StringBTrie :> STRING_TRIE = PatternMatchTrieFn(StringBTrieMap)

structure StringTrie = StringMTrie
