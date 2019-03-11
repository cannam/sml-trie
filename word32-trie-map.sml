
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
    val nodesPerWord = Int.div (32, bitsPerNode)

    fun extract (w, n) =
        let val b = Word.fromInt (bitsPerNode * n)
            val shifted = Word32.>> (w, b)
            val masked = Word32.andb (shifted, nodeMask)
        in
            Word32.toIntX masked
        end

    structure T = TrieMapFn
                      (struct
                        structure M = Word32NodeMap
                        structure K = struct
                          type element = M.key
                          type key = Word32.word * int
                          fun isEmpty (w, n) = n >= nodesPerWord
                          fun head (w, n) = extract (w, n)
                          fun tail (w, n) = (w, n + 1)
                          fun explode k = if isEmpty k then []
                                          else head k :: explode (tail k)
                          fun implode nn =
                              let fun implode' [] = 0w0
                                    | implode' (n::nn) = 
                                      Word32.orb (Word32.<< (implode' nn,
                                                             bitsPerNodeW),
                                                  Word32.fromInt n)
                              in
                                  (implode' nn, 0)
                              end
                          fun equal ((w, n), (w', n')) =
                              n = n' andalso
                              (isEmpty (w, n) orelse
                               (head (w, n) = head (w', n') andalso
                                equal (tail (w, n), tail (w', n'))))
                        end
                        type element = K.element
                        type key = K.key
                        end)
                               
    type 'a trie = 'a T.trie

    (*!!! this is not right, is it? *)
    fun keyFrom (w, i) = Word32.>> (w, Word.fromInt (i * bitsPerNode))
                             
    val empty = T.empty
    val isEmpty = T.isEmpty

    fun insert (t, k, x) = T.insert (t, (k, 0), x)
    fun update (t, k, f) = T.update (t, (k, 0), f)
    fun remove (t, k) = T.remove (t, (k, 0))
    fun contains (t, k) = T.contains (t, (k, 0))
    fun find (t, k) = T.find (t, (k, 0))
    fun lookup (t, k) = T.lookup (t, (k, 0))

    val foldl = T.foldl

    fun foldli f =
        T.foldli (fn (k, x, acc) => f (keyFrom k, x, acc))

    fun enumerate t =
        map (fn (k, x) => (keyFrom k, x)) (T.enumerate t)

    fun prefixOf (t, k) =
        keyFrom (T.prefixOf (t, (k, 0)))

    fun prefixMatch (t, k) =
        map (fn (k, x) => (keyFrom k, x)) (T.prefixMatch (t, (k, 0)))

    fun foldlPrefixMatch f acc (t, k) =
        T.foldlPrefixMatch f acc (t, (k, 0))

    fun foldliPrefixMatch f acc (t, k) =
        T.foldliPrefixMatch (fn (k, x, acc) => f (keyFrom k, x, acc))
                            acc (t, (k, 0))
end
                                                    
structure Word32TrieMap = Word32TrieMapFn(struct val bitsToUse = 32 end)


