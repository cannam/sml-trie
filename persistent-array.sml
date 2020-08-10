
structure PersistentArray :> PERSISTENT_ARRAY = struct

    structure T = Word32TrieMap

    type 'a array = {
        size : Word32.word,
        trie : 'a T.trie
    }

    val maxLenW =
        (* Should equal Int.maxInt if Int is no more than 32 bits, or
           the max word32 otherwise *)
        let val wordMax = Word32.- (0w0, 0w1)
        in
            case (Int.precision, Int.maxInt) of
                (NONE, _) => wordMax
              | (_, NONE) => wordMax
              | (SOME p, SOME max) => if p <= 32
                                      then Word32.fromInt max
                                      else wordMax
        end

    val maxLen = Word32.toInt maxLenW

    val empty : 'a array = {
        size = 0w0,
        trie = T.empty
    }
                    
    fun isEmpty ({ size, trie } : 'a array) =
        size = 0w0

    fun length { size, trie } =
        Word32.toInt size
            
    fun append ({ size, trie }, x) =
        let val _ = if size = maxLenW then raise Size else ()
            val t = T.insert (trie, size, x)
        in
            { size = size + 0w1, trie = t }
        end

    fun popEnd { size, trie } =
        case T.find (trie, size - 0w1) of
            NONE => raise Size
          | SOME x =>
            let val t = T.remove (trie, size - 0w1)
            in
                ({ size = size - 0w1, trie = t }, x)
            end

    fun sub (v as { size, trie }, i) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else T.lookup (trie, Word32.fromInt i)

    fun update (v as { size, trie }, i, x) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else let val t = T.insert (trie, Word32.fromInt i, x)
             in
                 { size = size, trie = t }
             end

    fun foldl f acc { size, trie } =
        T.foldl f acc trie

    fun foldli f acc { size, trie } =
        T.foldli (fn (w, x, acc) => f (Word32.toInt w, x, acc)) acc trie

    fun foldr f acc { size, trie } =
        T.foldr f acc trie

    fun foldri f acc { size, trie } =
        T.foldri (fn (w, x, acc) => f (Word32.toInt w, x, acc)) acc trie

    fun mapi f v =
        foldli (fn (i, x, acc) => append (acc, f (i, x))) empty v

    fun map f v =
        foldl (fn (x, acc) => append (acc, f x)) empty v

    fun appi f v =
        foldli (fn (i, x, _) => ignore (f (i, x))) () v
              
    fun app f v =
        foldl (fn (x, _) => ignore (f x)) () v

    fun fromList xx =
        List.foldl (fn (x, acc) => append (acc, x)) empty xx
                    
    fun tabulate (n, f) =
        let fun tabulate' (i, v) =
                if i = n
                then v
                else tabulate' (i + 1, append (v, f i))
        in
            tabulate' (0, empty)
        end

    fun toList v =
        rev (foldl (op::) [] v)
            
end
