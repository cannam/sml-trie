
structure PersistentArraySlice :>
          PERSISTENT_ARRAY_SLICE
              where type 'a PersistentArray.array = 'a PersistentArray.array
= struct

    structure PersistentArray = PersistentArray
    structure A = PersistentArrayImpl
    structure T = Word32TrieMap
                      
    datatype 'a slice = S of {
        array : 'a A.array,
        start : int,
        count : int
    }

    fun makeRange (S { array, start, count }) =
        (SOME (Word32.fromInt start),
         if start + count = A.length array
         then NONE
         else (* Trie range is inclusive *)
             SOME (Word32.fromInt (start + count - 1))
        )

    fun length (S { count, ... }) = count

    fun sub (S { array, start, count }, i) =
        if i < 0 orelse i >= count
        then raise Subscript
        else A.sub (array, start + i)

    fun full array = S {
            array = array,
            start = 0,
            count = A.length array
        }
                               
    fun slice (array, start, countOpt) =
        let val len = A.length array
        in
            if start < 0 orelse len < start  (* start = len may be a valid
                                                empty slice *)
            then raise Subscript
            else case countOpt of
                     NONE => S { array = array,
                                 start = start,
                                 count = len - start
                               }
                   | SOME count =>
                     if count < 0 orelse len < start + count
                     then raise Subscript
                     else S { array = array,
                              start = start,
                              count = count
                            }
        end

    fun subslice (S { array, start, count }, subStart, subCountOpt) =
        if subStart < 0 orelse count < subStart
        then raise Subscript
        else case subCountOpt of
                 NONE => S { array = array,
                             start = start + subStart,
                             count = count - subStart
                           }
               | SOME subCount =>
                 if subCount < 0 orelse count < subStart + subCount
                 then raise Subscript
                 else S { array = array,
                          start = start + subStart,
                          count = subCount
                        }

    fun base (S { array, start, count }) = (array, start, count)

    fun isEmpty (S { array, start, count }) = count = 0

    fun array (s as S { array = { size, trie }, start, count }) =
        { size = Word32.fromInt count,
          trie = T.extractRange (trie, makeRange s)
        }
                                                          
    fun foldl f acc (s as S { array = { size, trie }, start, count }) =
        T.foldlRange f acc (trie, makeRange s)
                                                          
    fun foldli f acc (s as S { array = { size, trie }, start, count }) =
        T.foldliRange (fn (w, x, acc) => f (Word32.toInt w, x, acc))
                      acc (trie, makeRange s)
                                                          
    fun foldr f acc (s as S { array = { size, trie }, start, count }) =
        T.foldrRange f acc (trie, makeRange s)
                                                          
    fun foldri f acc (s as S { array = { size, trie }, start, count }) =
        T.foldriRange (fn (w, x, acc) => f (Word32.toInt w, x, acc))
                      acc (trie, makeRange s)
                                            
    fun app f (s as S { array = { size, trie }, start, count }) =
        T.foldliRange (fn (_, x, _) => f x)
                      () (trie, makeRange s)
                                            
    fun appi f (s as S { array = { size, trie }, start, count }) =
        T.foldliRange (fn (w, x, _) => f (Word32.toInt w, x))
                      () (trie, makeRange s)

end
