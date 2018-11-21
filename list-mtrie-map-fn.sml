
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature MTRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
end

functor MTrieNodeMapFn (E : MTRIE_ELEMENT)
        :> LIST_TRIE_NODE_MAP
               where type key = E.t = struct

    structure M = RedBlackMapFn (struct
                                  type ord_key = E.t
                                  val compare = E.compare
                                  end)

    type key = E.t

    open M
                     
    fun new _ = M.empty
    val update = M.insert
    fun remove (m, k) = #1 (M.remove (m, k))
                                
end

(* Turn a comparable list element type into a trie holding lists of
   that element type. Each level of the trie uses a map to hold
   pointers to its sub-nodes. This can be inefficient for deeper tries
   whose element type has few values, but may be suitable for wide
   flat structures. *)
                             
functor ListMTrieMapFn (E : MTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(MTrieNodeMapFn(E))
                                                                        
