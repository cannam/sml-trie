
(* For historical compatibility *)

functor ListEntryTrieFn (E : MAP_TRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE
	       where type element = E.t where type entry = E.t list =
    ListMapTrieFn(E)

