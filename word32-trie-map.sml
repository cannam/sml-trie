
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
    fun update (v, k, f) = V.modify (v, k, fn x => SOME (f x))
                       
end

signature WORD32_TRIE_MAP_FN_ARG = sig
    val bitsToUse : int
end
                                            
functor Word32TrieMapFn (Arg : WORD32_TRIE_MAP_FN_ARG)
          :> TRIE_MAP
                 where type key = Word32.word = struct

    type key = Word32.word

    (* The 32-bit word key gets split up into 5-bit chunks (and one
       remaining 2-bit chunk). 5 bits represent the range 0-31, thus
       fitting neatly in the 32-bit compression bitmap we have
       available through Word32NodeMap. It is coincidence that this
       happens to be the same as the key size *)

    val bitsPerNode = 5    (* This cannot be > 5, since we are using a
                              32-bit bitmap for 32 slots in our vector *)
    val bitsToUse = Arg.bitsToUse
    val bitsPerNodeW = Word.fromInt bitsPerNode
    val valuesPerNode = Word.toInt (Word.<< (0w1, bitsPerNodeW))
    val nodeMask = Word32.fromInt (valuesPerNode - 1)
    val nodesPerWord = Int.div (32, bitsPerNode) +
                       (if Int.mod (32, bitsPerNode) = 0 then 0 else 1)

    structure Key = struct
        type element = int
        type key = Word32.word * int  (* length in nodes *)
        fun isEmpty (w, n) = n = 0
        fun head (w, n) = Word32.toIntX (Word32.andb (w, nodeMask))
        fun tail (w, n) = (Word32.>> (w, bitsPerNodeW), n - 1)
        fun explode k = if isEmpty k then []
                        else head k :: explode (tail k)
        fun implode [] = (0w0, 0)
          | implode (x::xs) =
            case implode xs of
                (w, n) => (Word32.orb (Word32.<< (w, bitsPerNodeW),
                                       Word32.fromInt x),
                           n + 1)
        fun equal ((w, n), (w', n')) = w = w' andalso n = n'
    end    
            
    structure T = TrieMapFn
                      (struct
                        structure M = Word32NodeMap
                        structure K = Key
                        type element = K.element
                        type key = K.key
                        end)
                               
    type 'a trie = 'a T.trie

    fun enkey k = (k, nodesPerWord)
    fun dekey (k, n) = k
                             
    val empty = T.empty
    val isEmpty = T.isEmpty

    fun insert (t, k, x) = T.insert (t, enkey k, x)
    fun update (t, k, f) = T.update (t, enkey k, f)
    fun remove (t, k) = T.remove (t, enkey k)
    fun contains (t, k) = T.contains (t, enkey k)
    fun find (t, k) = T.find (t, enkey k)
    fun lookup (t, k) = T.lookup (t, enkey k)

    val foldl = T.foldl

    fun foldli f =
        T.foldli (fn (k, x, acc) => f (dekey k, x, acc))

    fun enumerate t =
        map (fn (k, x) => (dekey k, x)) (T.enumerate t)

    fun prefixOf (t, k) =
        dekey (T.prefixOf (t, enkey k))

    fun prefixMatch (t, k) =
        map (fn (k, x) => (dekey k, x)) (T.prefixMatch (t, enkey k))

    fun foldlPrefixMatch f acc (t, k) =
        T.foldlPrefixMatch f acc (t, enkey k)

    fun foldliPrefixMatch f acc (t, k) =
        T.foldliPrefixMatch (fn (k, x, acc) => f (dekey k, x, acc))
                            acc (t, enkey k)
end
                                                    
structure Word32TrieMap = Word32TrieMapFn(struct val bitsToUse = 32 end)


