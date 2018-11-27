
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature BTRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
end

functor BTrieNodeMapFn (E : BTRIE_ELEMENT)
        :> LIST_TRIE_NODE_MAP
               where type key = E.t = struct
                                
    structure V = BitMappedVector
                                        
    type key = E.t
    type 'a map = 'a V.vector

    fun new () = V.new E.maxOrd
    val isEmpty = V.isEmpty
    fun find (v, k) = V.find (v, E.ord k)
    fun foldli f = V.foldli (fn (i, x, acc) => f (E.invOrd i, x, acc))
    fun update (v, k, x) = V.update (v, E.ord k, x)
    fun remove (v, k) = V.remove (v, E.ord k)
                                
end
                                          
functor ListBTrieMapFn (E : BTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(BTrieNodeMapFn(E))

