
structure PersistentArrayImpl = struct

    structure T = Word32TrieMap

    datatype 'a array = A of {
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

    val empty : 'a array = A {
        size = 0w0,
        trie = T.empty
    }
                    
    fun isEmpty (A { size, trie } : 'a array) =
        size = 0w0

    fun length (A { size, trie }) =
        Word32.toInt size
            
    fun append (A { size, trie }, x) =
        let val _ = if size = maxLenW then raise Size else ()
            val t = T.insert (trie, size, x)
        in
            A { size = size + 0w1, trie = t }
        end

    fun popEnd (A { size, trie }) =
        case T.find (trie, size - 0w1) of
            NONE => raise Empty
          | SOME x =>
            let val t = T.remove (trie, size - 0w1)
            in
                (A { size = size - 0w1, trie = t }, x)
            end

    fun peekEnd (A { size, trie }) =
        case T.find (trie, size - 0w1) of
            NONE => raise Empty
          | SOME x => x

    fun sub (v as A { size, trie }, i) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else T.lookup (trie, Word32.fromInt i)

    fun update (v as A { size, trie }, i, x) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else let val t = T.insert (trie, Word32.fromInt i, x)
             in
                 A { size = size, trie = t }
             end

    fun foldl f acc (A { size, trie }) =
        T.foldl f acc trie

    fun foldli f acc (A { size, trie }) =
        (* T.foldli is much slower than T.foldl, because it has to reconstruct
           the keys as it goes. It's quicker for us just to count them *)
        case T.foldl (fn (x, (i, acc)) => (i + 1, f (i, x, acc)))
                     (0, acc) trie of
            (_, acc) => acc

    fun foldr f acc (A { size, trie }) =
        T.foldr f acc trie

    fun foldri f acc (A { size, trie }) =
        (* as foldli *)
        case T.foldr (fn (x, (i, acc)) => (i - 1, f (i, x, acc)))
                     (Word32.toInt size - 1, acc) trie of
            (_, acc) => acc

    fun find f (A { size, trie }) =
        T.search f trie

    fun findi f (A { size, trie }) =
        Option.map (fn (w, x) => (Word32.toInt w, x))
                   (T.searchi (fn (w, x) => f (Word32.toInt w, x)) trie)

    fun exists f (A { size, trie }) =
        Option.isSome (T.search f trie)

    fun all f (A { size, trie }) =
        not (Option.isSome (T.search (fn x => not (f x)) trie))
            
    fun mapi f v =
        foldli (fn (i, x, acc) => append (acc, f (i, x))) empty v

    fun map f (A { size, trie }) =
        A { size = size, trie = T.map f trie }

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
        foldr (op::) [] v

    fun vector v =
        Vector.fromList (toList v)

    fun modifyi f v =
        foldli (fn (i, x, updating) =>
                   case f (i, x) of
                       NONE => updating
                     | SOME x' => update (updating, i, x'))
               v v

    fun modify f v =
        modifyi (fn (i, x) => f x) v

    fun collate f (v1, v2) =
        let val len1 = length v1
            val len2 = length v2
            fun collate' i =
                if i = len1
                then if i = len2
                     then EQUAL
                     else LESS
                else if i = len2
                then GREATER
                else case f (sub (v1, i), sub (v2, i)) of
                         EQUAL => collate' (i+1)
                       | order => order
        in
            collate' 0
        end

    fun array (n, x) =
        tabulate (n, fn _ => x)
                 
end

structure PersistentArray : PERSISTENT_ARRAY = PersistentArrayImpl

