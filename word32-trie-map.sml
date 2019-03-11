
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
                                            
structure Word32TrieMap
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

    val bitsPerNodeW = Word.fromInt bitsPerNode
    val valuesPerNode = Word.toInt (Word.<< (0w1, bitsPerNodeW))
    val nodeMask = Word32.fromInt (valuesPerNode - 1)

    structure Key = struct
        type element = int
        type key = Word32.word
        fun isEmpty w = w = 0w0
        fun head w = Word32.toIntX (Word32.andb (w, nodeMask))
        fun tail w = Word32.>> (w, bitsPerNodeW)
        fun explode k = if isEmpty k then []
                        else head k :: explode (tail k)
        fun implode [] = 0w0
          | implode (x::xs) = Word32.orb (Word32.<< (implode xs, bitsPerNodeW),
                                          Word32.fromInt x)
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


