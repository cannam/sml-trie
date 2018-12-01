
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
                           | MANY of 'a hash_value vector
    type 'a hash_map = 'a hash_entry T.trie

    val empty = T.empty

    val isEmpty = T.isEmpty

    fun findKey (values, k) =
        case Vector.findi (fn (i, (k', v')) => Key.sameKey (k', k)) values of
            NONE => NONE
          | SOME (i, _) => SOME i
                      
    fun addToEntry (h : hash, k : hash_key, v : 'a)
                   (e : 'a hash_entry option) : 'a hash_entry =
        case e of
            NONE => ONE (k, v)
          | SOME (ONE (k', v')) => if Key.sameKey (k', k)
                                   then ONE (k', v)
                                   else MANY (Vector.fromList [(k, v), (k', v')])
          | SOME (MANY values) =>
            case findKey (values, k) of
                NONE => MANY (Vector.concat [values, Vector.fromList [(k, v)]])
              | SOME i => MANY (Vector.update (values, i, (k, v)))

    fun removeFromEntry (h : hash, k : hash_key)
                        (e : 'a hash_entry) : 'a hash_entry option =
        case e of
            ONE (k', v') => if Key.sameKey (k', k)
                            then NONE
                            else SOME e
          | MANY values => 
            case findKey (values, k) of
                NONE => SOME (MANY values)
              | SOME i =>
                case Vector.length values of
                    0 => raise Fail "Internal error: zero-length SOME vector"
                  | 1 => NONE
                  | 2 =>
                    SOME (ONE (Vector.sub (values,
                                           if i = 0 then 1 else 0)))
                  | len =>
                    SOME (MANY (Vector.tabulate
                                    (len - 1,
                                     fn j => if j < i
                                             then Vector.sub (values, j)
                                             else Vector.sub (values, j + 1))))
                                   
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
              | SOME entry =>
                case removeFromEntry (h, k) entry of
                    NONE => T.remove (m, h)
                  | SOME entry' => T.insert (m, h, entry')
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
                case findKey (values, k) of
                    NONE => NONE
                  | SOME i => case Vector.sub (values, i) of (k, v) => SOME v
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
                        Vector.foldl (fn ((k, v), acc) =>
                                         f (v, acc)) acc values)
                acc m

    fun foldli f acc m =
        T.foldl (fn (entry, acc) =>
                    case entry of
                        ONE (k, v) => f (k, v, acc)
                      | MANY values => 
                        Vector.foldl (fn ((k, v), acc) =>
                                         f (k, v, acc)) acc values)
                acc m

    fun enumerate m =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] m)
                
end
