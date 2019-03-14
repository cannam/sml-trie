                 
(* Turn a comparable list element type into a trie holding lists of
   that element type. Each level of the trie uses a map to hold
   pointers to its sub-nodes. This can be inefficient for deeper tries
   whose element type has few values, but may be suitable for wide
   flat structures. *)
                             
functor ListMTrieMapFn (E : MTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(MTrieNodeMapFn(E))
                                                                        
functor ListMTrieFn (E : MTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    PatternMatchTrieFn(ListMTrieMapFn(E))
                                                                        
                 
(* Turn a type that can be compactly converted into an integer
   ordering into a trie holding lists of that type. Each level of the
   trie uses a vector to hold pointers to its sub-nodes, indexed by
   integer value of the type. Because this is an immutable structure
   and updating a vector can be slow, this may be inefficient for
   "wide" tries with many sub-nodes per node and a lot of inserts. It
   may be more efficient for deep tries, e.g. of character lists, with
   lots of lookups. *)
                                                      
functor ListATrieMapFn (E : ATRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(ATrieNodeMapFn(E))
                                                                        
functor ListATrieFn (E : ATRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    PatternMatchTrieFn(ListATrieMapFn(E))


(* Turn a type that can be compactly converted into an integer
   ordering within a limited range into a trie holding lists of that
   type with bitmapped vectors at each node. *)
                 
functor ListBTrieMapFn (E : BTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(BTrieNodeMapFn(E))
                                                                        
functor ListBTrieFn (E : BTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t list =
    PatternMatchTrieFn(ListBTrieMapFn(E))
