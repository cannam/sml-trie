
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

    fun array (s as S { array = A.A { size, trie }, start, count }) =
        A.A { size = Word32.fromInt count,
              trie = T.extractRange (trie, makeRange s)
            }
                                                          
    fun foldl f acc (s as S { array = A.A { size, trie }, start, count }) =
        T.foldlRange f acc (trie, makeRange s)
                                                          
    fun foldli f acc (s as S { array = A.A { size, trie }, start, count }) =
        case T.foldlRange (fn (x, (i, acc)) => (i + 1, f (i, x, acc)))
                          (0, acc)
                          (trie, makeRange s) of
            (_, acc) => acc
                                                          
    fun foldr f acc (s as S { array = A.A { size, trie }, start, count }) =
        T.foldrRange f acc (trie, makeRange s)
                                                          
    fun foldri f acc (s as S { array = A.A { size, trie }, start, count }) =
        case T.foldrRange (fn (x, (i, acc)) => (i - 1, f (i, x, acc)))
                          (count - 1, acc)
                          (trie, makeRange s) of
            (_, acc) => acc
                                        
    fun app f s =
        foldl (fn (x, _) => ignore (f x)) () s
                  
    fun appi f s =
        foldli (fn (i, x, _) => ignore (f (i, x))) () s

end
