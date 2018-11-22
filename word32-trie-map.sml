
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure Word32TrieMap
          :> TRIE_MAP
                 where type key = Word32.word = struct

    type key = Word32.word

    (* 32-bit word gets split up into 5-bit chunks, each 0-31, and a
       remaining 2-bit chunk - it is a coincidence that the map range
       spans 32 values and the key is 32 bits *)

    (*!!! though perhaps we should just use 30 bits *)
                   
    structure T = ListTrieMapFn(BTrieNodeMapFn(struct
                                                type t = int
                                                fun ord x = x
                                                fun invOrd x = x
                                                val maxOrd = 32
                                                end))
                               
    type 'a trie = 'a T.trie

    fun explode w =
        let fun explode' (w, n) =
                if n <= 0
                then []
                else Word32.toIntX (Word32.andb (w, 0wx1f)) ::
                     explode' (Word32.>> (w, 0w5), n - 5)
        in
            explode' (w, 32)
        end

    fun implode bb =
        case bb of
            [] => 0w0
          | b::bb => Word32.orb (Word32.<< (implode bb, 0w5), Word32.fromInt b)

    val empty = T.empty

    val isEmpty = T.isEmpty

    fun update (trie, h, f) =
        T.update (trie, explode h, f)
                      
    fun insert (trie, h, v) =
        T.insert (trie, explode h, v)

    fun contains (trie, h) =
        T.contains (trie, explode h)
                         
    fun remove (trie, h) =
        T.remove (trie, explode h)

    fun find (trie, h) =
        T.find (trie, explode h)
                 
    fun foldl f acc trie =
        T.foldl f acc trie

    fun foldli f acc trie =
        T.foldli (fn (k, v, acc) => f (implode k, v, acc))
                 acc trie

    fun enumerate trie =
        List.map (fn (k, v) => (implode k, v))
                 (T.enumerate trie)

    fun foldlPrefixMatch f acc (trie, h) =
        T.foldlPrefixMatch f acc (trie, explode h)

    fun foldliPrefixMatch f acc (trie, h) =
        T.foldliPrefixMatch (fn (k, v, acc) => f (implode k, v, acc))
                            acc (trie, explode h)
                 
    fun prefixMatch (trie, h) =
        List.map (fn (k, v) => (implode k, v))
                 (T.prefixMatch (trie, explode h))

    fun prefixOf (trie, h) =
        implode (T.prefixOf (trie, explode h))
   
end
    
