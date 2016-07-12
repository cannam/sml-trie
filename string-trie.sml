
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure StringTrie
	  :> PATTERN_MATCH_TRIE
		 where type entry = string where type element = char = struct

    structure CharListTrie = ListEntryTrieFn(struct
				              type t = char
				              val compare = Char.compare
				              end)

    type t = CharListTrie.t
    type trie = t
    type element = char
    type pattern = char option list
    type entry = string

    val empty = CharListTrie.empty

    fun add (trie, s) =
        CharListTrie.add (trie, String.explode s)

    fun contains (trie, s) =
        CharListTrie.contains (trie, String.explode s)
                         
    fun remove (trie, s) =
        CharListTrie.remove (trie, String.explode s)

    fun foldl f acc trie =
        CharListTrie.foldl (fn (e, acc) => f (String.implode e, acc))
                           acc trie

    fun enumerate trie =
        List.map String.implode (CharListTrie.enumerate trie)

    fun foldlPrefixMatch f acc (trie, s) =
        CharListTrie.foldlPrefixMatch (fn (e, acc) => f (String.implode e, acc))
                                 acc (trie, String.explode s)
                 
    fun prefixMatch (trie, s) =
        List.map String.implode (CharListTrie.prefixMatch (trie, String.explode s))

    fun prefixOf (trie, s) =
        String.implode (CharListTrie.prefixOf (trie, String.explode s))

    fun foldlPatternMatch f acc (trie, p) =
        CharListTrie.foldlPatternMatch (fn (e, acc) => f (String.implode e, acc))
					 acc (trie, p)
                 
    fun patternMatch (trie, p) =
        List.map String.implode (CharListTrie.patternMatch (trie, p))
                 
end
