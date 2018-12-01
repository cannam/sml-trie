
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure Word32NodeMap
          :> LIST_TRIE_NODE_MAP
                 where type key = int = struct

    structure V = BitMappedVector32

    type key = int
    type 'a map = 'a V.vector

    open V
             
    fun new () = V.new 32
                       
end
                      
structure Word32TrieMap
          :> TRIE_MAP
                 where type key = Word32.word = struct

    type key = Word32.word

    (* The 32-bit word key gets split up into 5-bit chunks (and one
       remaining 2-bit chunk). 5 bits represent the range 0-31, thus
       fitting neatly in the 32-bit compression bitmap we have
       available through Word32NodeMap. It is coincidence that this
       happens to be the same as the key size *)

    val bitsPerNode = 5 (* This cannot be > 5, since we are using a
                           32-bit bitmap for 32 slots in our vector *)
    val bitsPerNodeW = Word.fromInt bitsPerNode
    val valuesPerNode = Word.toInt (Word.<< (0w1, bitsPerNodeW))
    val nodeMask = Word32.fromInt (valuesPerNode - 1)
                   
    structure T = ListTrieMapFn(Word32NodeMap)
                               
    type 'a trie = 'a T.trie

    fun explode w =
        let fun explode' (w, n) =
                if n <= 0
                then []
                else Word32.toIntX (Word32.andb (w, nodeMask)) ::
                     explode' (Word32.>> (w, bitsPerNodeW), n - bitsPerNode)
        in
            explode' (w, 32)
        end

    fun implode bb =
        case bb of
            [] => 0w0
          | b::bb => Word32.orb (Word32.<< (implode bb, bitsPerNodeW),
                                 Word32.fromInt b)

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

    fun lookup (trie, h) =
        T.lookup (trie, explode h)
                 
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
    
