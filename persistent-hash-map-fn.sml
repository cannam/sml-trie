
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)
                         
functor PersistentHashMapFn (Key : HASH_KEY)
        :> PERSISTENT_HASH_MAP
               where type hash_key = Key.hash_key = struct

    type hash_key = Key.hash_key
    type hash = Word32.word

    structure T = Word32TrieMap

    type 'a hash_value = hash_key * 'a
    datatype 'a hash_entry = ONE of 'a hash_value
                           | MANY of 'a hash_value list
    type 'a hash_map = 'a hash_entry T.trie

    val empty = T.empty

    val isEmpty = T.isEmpty

    fun addToEntry (h : hash, k : hash_key, v : 'a)
                   (e : 'a hash_entry option) : 'a hash_entry =
        case e of
            NONE => ONE (k, v)
          | SOME (ONE (k', v')) => if Key.sameKey (k', k)
                                   then ONE (k', v)
                                   else MANY [(k, v), (k', v')]
          | SOME (MANY values) => 
            case List.foldr (fn ((k', v'), (acc, found)) =>
                                if Key.sameKey (k', k)
                                then ((k', v) :: acc, true)
                                else ((k', v') :: acc, found))
                            ([], false)
                            values of
                (acc, false) => MANY ((k, v) :: acc)
              | (acc, true) => MANY acc

    fun removeFromValues (h : hash, k : hash_key)
                         (values : 'a hash_value list) : 'a hash_value list =
        List.filter (fn (k', _) => not (Key.sameKey (k', k))) values
                                   
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
              | SOME (ONE (k', _)) => if Key.sameKey (k', k)
                                      then T.remove (m, h)
                                      else m
              | SOME (MANY values) =>
                case removeFromValues (h, k) values of
                    [] => T.remove (m, h)
                  | [value] => T.insert (m, h, ONE value)
                  | values => T.insert (m, h, MANY values)
        end
            
    fun find (m, k) =
        let val h = Key.hashVal k
        in
            case T.find (m, h) of
                NONE => NONE
              | SOME (ONE (k', v)) => if Key.sameKey (k', k)
                                      then SOME v
                                      else NONE
              | SOME (MANY values) =>
                case List.find (fn (k', v) => Key.sameKey (k', k)) values of
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
                    case entry of
                        ONE (k, v) => f (v, acc)
                      | MANY values => 
                        List.foldl (fn ((k, v), acc) =>
                                       f (v, acc)) acc values)
                acc m

    fun foldli f acc m =
        T.foldl (fn (entry, acc) =>
                    case entry of
                        ONE (k, v) => f (k, v, acc)
                      | MANY values => 
                        List.foldl (fn ((k, v), acc) =>
                                       f (k, v, acc)) acc values)
                acc m

    fun enumerate m =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] m)
                
end
