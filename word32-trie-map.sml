
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure Word32NodeMap
          :> TRIE_NODE_MAP
                 where type key = int = struct

    structure V = BitMappedVector32

    type key = int
    type 'a map = 'a V.vector

    open V
             
    fun new () = V.new 32
    fun update (v, k, f) = V.modify (v, k, fn x => SOME (f x))
    val keyCompare = Int.compare
                       
end

signature WORD32_TRIE_MAP_FN_ARG = sig
    val bitsToUse : int
end
                                            
structure Word32TrieMap
          :> TRIE_MAP
                 where type key = Word32.word = struct

    type key = Word32.word

    structure Key = struct

        (* The 32-bit word key gets split up into 5-bit chunks (and
           one remaining 2-bit chunk). 5 bits represent the range
           0-31, thus fitting neatly in the 32-bit compression bitmap
           we have available through Word32NodeMap. It is coincidence
           that this happens to be the same as the key size *)

        val bitsPerNode = 5    (* This cannot be > 5, since we are using a
                                  32-bit bitmap for 32 slots in our vector *)

        val bitsPerNodeW = Word.fromInt bitsPerNode
        val valuesPerNode = Word.toInt (Word.<< (0w1, bitsPerNodeW))
        val maskShift = 0w32 - bitsPerNodeW
        val nodesPerWord = Int.quot (32, bitsPerNode) +
                           (case Int.mod (32, bitsPerNode) of
                                0 => 0
                              | _ => 1)
        val bitsInLastNodeW = case Int.mod (32, bitsPerNode) of
                                  0 => bitsPerNodeW
                                | n => Word.fromInt n

        type element = int
        type key = Word32.word
        fun isEmpty w = w = Word32.fromInt 0
        fun head w = Word32.toIntX (Word32.>> (w, maskShift))
        fun tail w = Word32.<< (w, bitsPerNodeW)
        fun explode k = if isEmpty k then []
                        else head k :: explode (tail k)
        fun implode xx =
            let fun implode' (xx, i, acc) =
                    if i + 1 = nodesPerWord
                    then case xx of
                             [] => Word32.<< (acc, bitsInLastNodeW)
                           | x::xs =>
                             Word32.orb (Word32.<< (acc, bitsInLastNodeW),
                                         Word32.>> (Word32.fromInt x,
                                                    bitsPerNodeW -
                                                    bitsInLastNodeW))
                    else case xx of
                             [] =>
                             implode' (xx, i + 1,
                                       Word32.<< (acc, bitsPerNodeW))
                           | x::xs =>
                             implode' (xs, i + 1,
                                       Word32.orb
                                           (Word32.<< (acc, bitsPerNodeW),
                                            Word32.fromInt x))
            in
                implode' (xx, 0, 0w0)
            end
        fun equal (w, w') = w = w'
    end    

    structure T = TrieMapFn
                      (struct
                        structure M = Word32NodeMap
                        structure K = Key
                        type element = K.element
                        type key = K.key
                        end)
                               
    open T
                      
end

