
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure Word32NodeMap
          :> LIST_TRIE_NODE_MAP
                 where type key = int
= struct

    structure V = BitMappedVector32

    type key = int

    datatype 'a map = SPARSE of 'a V.vector
                    | DENSE of (int * 'a option Vector.vector)

    val sparseLimit = 16
                                  
    fun new () = SPARSE (V.new 32)

    fun isEmpty (SPARSE m) = V.isEmpty m
      | isEmpty (DENSE (0, _)) = true
      | isEmpty _ = false

    fun find (SPARSE m, k) = V.find (m, k)
      | find (DENSE (_, m), k) = Vector.sub (m, k)

    fun foldl f acc (SPARSE m) = V.foldl f acc m
      | foldl f acc (DENSE (_, m)) =
        Vector.foldl (fn (SOME x, acc) => f (x, acc)
                       | (_, acc) => acc) acc m

    fun foldli f acc (SPARSE m) = V.foldli f acc m
      | foldli f acc (DENSE (_, m)) =
        Vector.foldli (fn (i, SOME x, acc) => f (i, x, acc)
                        | (_, _, acc) => acc)
                      acc m

    fun makeDense (DENSE d) = DENSE d
      | makeDense (SPARSE m) = 
        DENSE (V.population m, Vector.tabulate (32, fn i => V.find (m, i)))

    fun makeSparse (SPARSE m) = SPARSE m
      | makeSparse (DENSE (_, m)) =
        SPARSE (V.tabulate (32, fn i => Vector.sub (m, i)))
                      
    fun update (SPARSE m, k, v) =
        if V.population m > sparseLimit
        then update (makeDense (SPARSE m), k, v)
        else SPARSE (V.update (m, k, v))
      | update (DENSE (n, m), k, v) =
        case Vector.sub (m, k) of
            NONE => DENSE (n + 1, Vector.update (m, k, SOME v))
          | SOME _ => DENSE (n, Vector.update (m, k, SOME v))

    fun remove (SPARSE m, k) = SPARSE (V.remove (m, k))
      | remove (DENSE (n, m), k) =
        if n <= sparseLimit
        then remove (makeSparse (DENSE (n, m)), k)
        else DENSE (n - 1, Vector.update (m, k, NONE))
                       
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
    
