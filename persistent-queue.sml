
structure PersistentQueue :> PERSISTENT_QUEUE = struct

    structure T = Word32TrieMap

    type 'a queue = {
        start : Word32.word,
        size : Word32.word,
        trie : 'a T.trie
    }

    val maxLen = PersistentArray.maxLen
    val maxLenW = Word32.fromInt maxLen

    val empty : 'a queue = {
        start = 0w0,
        size = 0w0,
        trie = T.empty
    }
                    
    fun isEmpty ({ start, size, trie } : 'a queue) =
        size = 0w0

    fun length { start, size, trie } =
        Word32.toInt size
                   
    fun prepend ({ start, size, trie }, x) =
        let val _ = if size = maxLenW then raise Size else ()
            val t = T.insert (trie, start - 0w1, x)
        in
            { start = start - 0w1, size = size + 0w1, trie = t }
        end

    fun popStart ({ start, size, trie } : 'a queue) : ('a queue * 'a) =
        case T.find (trie, start) of
            NONE => raise Empty
          | SOME x =>
            let val t = T.remove (trie, start)
            in
                ({ start = start + 0w1, size = size - 0w1, trie = t }, x)
            end

    fun peekStart ({ start, size, trie } : 'a queue) : 'a =
        case T.find (trie, start) of
            NONE => raise Empty
          | SOME x => x
            
    fun append ({ start, size, trie }, x) =
        let val _ = if size = maxLenW then raise Size else ()
            val t = T.insert (trie, start + size, x)
        in
            { start = start, size = size + 0w1, trie = t }
        end

    fun popEnd ({ start, size, trie } : 'a queue) : ('a queue * 'a) =
        case T.find (trie, start + size - 0w1) of
            NONE => raise Empty
          | SOME x =>
            let val t = T.remove (trie, start + size - 0w1)
            in
                ({ start = start, size = size - 0w1, trie = t }, x)
            end

    fun peekEnd ({ start, size, trie } : 'a queue) : 'a =
        case T.find (trie, start + size - 0w1) of
            NONE => raise Empty
          | SOME x => x

    fun sub (v as { start, size, trie }, i) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else T.lookup (trie, start + Word32.fromInt i)

    fun update (v as { start, size, trie }, i, x) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else let val t = T.insert (trie, start + Word32.fromInt i, x)
             in
                 { start = start, size = size, trie = t }
             end

    fun foldli f acc v =
        let fun foldli' i acc =
                if i = length v
                then acc
                else foldli' (i + 1) (f (i, sub (v, i), acc))
        in
            foldli' 0 acc
        end

    fun foldl f =
        foldli (fn (i, x, acc) => f (x, acc))

    fun foldri f acc v =
        let fun foldri' i acc =
                if i = 0
                then acc
                else let val i = i - 1
                     in
                         foldri' i (f (i, sub (v, i), acc))
                     end
        in
            foldri' (length v) acc
        end

    fun foldr f =
        foldri (fn (i, x, acc) => f (x, acc))

    fun mapi f v =
        foldli (fn (i, x, acc) => append (acc, f (i, x))) empty v

    fun map f ({ start, size, trie }) =
        { start = start, size = size, trie = T.map f trie }

    fun appi f v =
        foldli (fn (i, x, _) => ignore (f (i, x))) () v
              
    fun app f v =
        foldl (fn (x, _) => ignore (f x)) () v

    fun fromList [] = empty
      | fromList (x::xs) = prepend (fromList xs, x)
                    
    fun tabulate (n, f) =
        let fun tabulate' (i, v) =
                if i = n
                then v
                else tabulate' (i + 1, append (v, f i))
        in
            tabulate' (0, empty)
        end

    fun toList v =
        foldr (op::) [] v

    fun vector v =
        Vector.fromList (toList v)

    fun queue (n, x) =
        tabulate (n, fn _ => x)
                 
end
                                 
