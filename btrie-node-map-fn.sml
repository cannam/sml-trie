
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature BTRIE_ELEMENT = sig
    eqtype t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
end

functor BTrieNodeMapFn (E : BTRIE_ELEMENT)
        :> TRIE_NODE_MAP
               where type key = E.t = struct
                                
    structure V = BitMappedVector
                                        
    type key = E.t
    type 'a map = 'a V.vector

    fun new () = V.new E.maxOrd
    val isEmpty = V.isEmpty
    fun find (v, k) = V.find (v, E.ord k)
    fun foldl f = V.foldl (fn (x, acc) => f (x, acc))
    fun foldli f = V.foldli (fn (i, x, acc) => f (E.invOrd i, x, acc))
    fun update (v, k, f) = V.modify (v, E.ord k, fn x => SOME (f x))
    fun remove (v, k) = V.remove (v, E.ord k)
                                
end
                                          