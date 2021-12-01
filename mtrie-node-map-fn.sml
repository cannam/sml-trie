
(* Copyright 2015-2021 Chris Cannam.
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

    fun remove (m, k) = #1 (M.remove (m, k))

    fun modify (m, k, f) =
        case find (m, k) of
            NONE => (case f NONE of
                         NONE => m
                       | SOME i => insert (m, k, i))
          | SOME e => (case f (SOME e) of
                           NONE => remove (m, k)
                         | SOME i => insert (m, k, i))

    val keyCompare = E.compare
                                
end

