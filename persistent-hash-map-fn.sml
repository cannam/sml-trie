
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)
                         
functor PersistentHashMapFn (Key : HASH_KEY)
        :> PERSISTENT_HASH_MAP
               where type hash_key = Key.hash_key = struct

    type hash_key = Key.hash_key
    type hash = Word32.word

    structure T = Word32TrieMap

    type 'a hash_value = hash_key * 'a
    type 'a hash_entry = 'a hash_value list
    type 'a hash_map = 'a hash_entry T.trie

    val empty = T.empty

    val isEmpty = T.isEmpty

    fun addToEntry (h : hash, k : hash_key, v : 'a)
                   (e : 'a hash_entry option) : 'a hash_entry =
        case e of
            NONE => [(k, v)]
          | SOME entry =>
            case List.foldr (fn ((k', v'), (acc, found)) =>
                                if Key.sameKey (k', k)
                                then ((k', v) :: acc, true)
                                else ((k', v') :: acc, found))
                            ([], false)
                            entry of
                (acc, false) => (k, v) :: acc
              | (acc, true) => acc

    fun removeFromEntry (h : hash, k : hash_key)
                        (entry : 'a hash_entry) : 'a hash_entry =
        List.filter (fn (k', _) => not (Key.sameKey (k', k))) entry
                                   
    fun insert (m, k, v) =
        let val h = Key.hashVal k
        in
            T.update (m, h, addToEntry (h, k, v))
        end

    fun remove (m, k) =
        let val h = Key.hashVal k
        in
            case T.find (m, h) of
                NONE => m
              | SOME [v] => T.remove (m, h)
              | SOME entry => T.insert (m, h, removeFromEntry (h, k) entry)
        end

    fun find (m, k) =
        let val h = Key.hashVal k
        in
            case T.find (m, h) of
                NONE => NONE
              | SOME [(k', v)] => if Key.sameKey (k', k)
                                  then SOME v
                                  else NONE
              | SOME entry =>
                case List.find (fn (k', v) => Key.sameKey (k', k)) entry of
                    NONE => NONE
                  | SOME (_, v) => SOME v
        end

    fun lookup (m, k) =
        case find (m, k) of
            NONE => raise Subscript
          | SOME v => v

    fun contains (m, k) =
        case find (m, k) of
            NONE => false
          | _ => true

    fun foldl f acc m =
        T.foldl (fn (entry, acc) =>
                    List.foldl (fn ((k, v), acc) => f (v, acc)) acc entry)
                acc m

    fun foldli f acc m =
        T.foldl (fn (entry, acc) =>
                    List.foldl (fn ((k, v), acc) => f (k, v, acc)) acc entry)
                acc m

    fun enumerate m =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] m)
                
end
