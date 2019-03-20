
signature PERSISTENT_VECTOR = sig

    type 'a vector
            
    val empty : 'a vector
    val isEmpty : 'a vector -> bool
    val length : 'a vector -> int

    val prepend : 'a vector * 'a -> 'a vector
    val popStart : 'a vector -> 'a vector * 'a

    val append : 'a vector * 'a -> 'a vector
    val popEnd : 'a vector -> 'a vector * 'a

    val sub : 'a vector * int -> 'a
    val update : 'a vector * int * 'a -> 'a vector

    val tabulate : int * (int -> 'a) -> 'a vector
                                            
end

structure PersistentVector :> PERSISTENT_VECTOR = struct

    structure T = Word32TrieMap

    type 'a vector = {
        start : Word32.word,
        size : Word32.word,
        trie : 'a T.trie
    }

    val empty = {
        start = 0w0,
        size = 0w0,
        trie = T.empty
    }
                    
    fun isEmpty { start, size, trie } =
        size = 0w0

    fun length { start, size, trie } =
        Word32.toInt (size - start) (*!!! could overflow, int is smaller *)
                   
    fun prepend ({ start, size, trie }, x) =
        let val t = T.insert (trie, start - 0w1, x)
        in
            { start = start - 0w1, size = size + 0w1, trie = t }
        end

    fun popStart { start, size, trie } =
        case T.find (trie, start) of
            NONE => raise Size
          | SOME x =>
            let val t = T.remove (trie, start)
            in
                ({ start = start + 0w1, size = size - 0w1, trie = t }, x)
            end
            
    fun append ({ start, size, trie }, x) =
        let val t = T.insert (trie, start + size, x)
        in
            { start = start, size = size + 0w1, trie = t }
        end

    fun popEnd { start, size, trie } =
        case T.find (trie, start + size - 0w1) of
            NONE => raise Size
          | SOME x =>
            let val t = T.remove (trie, start + size - 0w1)
            in
                ({ start = start, size = size - 0w1, trie = t }, x)
            end

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

    fun tabulate (n, f) =
        let fun tabulate' (i, v) =
                if i = n
                then v
                else tabulate' (i + 1, append (v, f i))
        in
            tabulate' (0, empty)
        end
                 
end
                                 
