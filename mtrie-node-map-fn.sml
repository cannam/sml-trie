
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature MTRIE_ELEMENT = sig
    eqtype t
    val compare : t * t -> order
end

functor MTrieNodeMapFn (E : MTRIE_ELEMENT)
        :> TRIE_NODE_MAP
               where type key = E.t = struct

    structure M = RedBlackMapFn (struct
                                  type ord_key = E.t
                                  val compare = E.compare
                                  end)

    type key = E.t

    open M
                     
    fun new _ = M.empty
    fun update (m, k, f) = M.insert (m, k, f (M.find (m, k)))
    fun remove (m, k) = #1 (M.remove (m, k))
                                
end

