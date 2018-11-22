
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)
                         
functor PersistentHashMap (Key : HASH_KEY)
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
                      
    fun insert (m, k, v) =
        let val h = Key.hashVal k
        in
            T.update (m, h, addToEntry (h, k, v))
        end
                      
end
