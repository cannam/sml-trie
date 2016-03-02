
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

    fun foldl_prefix_match f acc (trie, s) =
        CharListTrie.foldl_prefix_match (fn (e, acc) => f (String.implode e, acc))
                                 acc (trie, String.explode s)
                 
    fun prefix_match (trie, s) =
        List.map String.implode (CharListTrie.prefix_match (trie, String.explode s))

    fun prefix_of (trie, s) =
        String.implode (CharListTrie.prefix_of (trie, String.explode s))

    fun foldl_pattern_match f acc (trie, p) =
        CharListTrie.foldl_pattern_match (fn (e, acc) => f (String.implode e, acc))
					 acc (trie, p)
                 
    fun pattern_match (trie, p) =
        List.map String.implode (CharListTrie.pattern_match (trie, p))
                 
end
