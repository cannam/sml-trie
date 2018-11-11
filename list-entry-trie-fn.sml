
(* For historical compatibility *)

functor ListEntryTrieFn (E : MTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE
	       where type element = E.t where type entry = E.t list =
    ListMTrieFn(E)

