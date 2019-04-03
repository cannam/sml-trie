                 
(* Turn a comparable vector element type into a trie holding vectors
   of that element type. Each level of the trie uses a map to hold
   pointers to its sub-nodes. This can be inefficient for deeper tries
   whose element type has few values, but may be suitable for wide
   flat structures. *)

functor VectorMTrieMapFn(E : MTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE_MAP
               where type element = E.t where type key = E.t vector =
    VectorTrieMapFn(struct
                     structure M = MTrieNodeMapFn(E)
                     structure V = struct
                      open Vector
                      type elem = E.t
                      type vector = E.t vector
                     end
                     type element = E.t
                     type key = E.t vector
                     end)
                                                                        
functor VectorMTrieFn (E : MTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t vector =
    PatternMatchTrieFn(VectorMTrieMapFn(E))

                 
(* Turn a type that can be compactly converted into an integer
   ordering into a trie holding vectors of that type. Each level of
   the trie uses a vector to hold pointers to its sub-nodes, indexed
   by integer value of the type. Because this is an immutable
   structure and updating a vector can be slow, this may be
   inefficient for "wide" tries with many sub-nodes per node and a lot
   of inserts. It may be more efficient for deep tries, e.g. of
   character vectors, with lots of lookups. *)

functor VectorATrieMapFn(E : ATRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE_MAP
               where type element = E.t where type key = E.t vector =
    VectorTrieMapFn(struct
                     structure M = ATrieNodeMapFn(E)
                     structure V = struct
                      open Vector
                      type elem = E.t
                      type vector = E.t vector
                     end
                     type element = E.t
                     type key = E.t vector
                     end)
                                                                        
functor VectorATrieFn (E : ATRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t vector =
    PatternMatchTrieFn(VectorATrieMapFn(E))


(* Turn a type that can be compactly converted into an integer
   ordering within a limited range into a trie holding vectors of that
   type with bitmapped vectors at each node. *)

functor VectorBTrieMapFn(E : BTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE_MAP
               where type element = E.t where type key = E.t vector =
    VectorTrieMapFn(struct
                     structure M = BTrieNodeMapFn(E)
                     structure V = struct
                      open Vector
                      type elem = E.t
                      type vector = E.t vector
                     end
                     type element = E.t
                     type key = E.t vector
                     end)
                                                                        
functor VectorBTrieFn (E : BTRIE_ELEMENT)
        :> PATTERN_MATCH_TRIE
               where type element = E.t where type entry = E.t vector =
    PatternMatchTrieFn(VectorBTrieMapFn(E))

                      
