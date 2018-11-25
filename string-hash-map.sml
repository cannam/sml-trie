
structure StringHashKey
          :> HASH_KEY
                 where type hash_key = string = struct

    (* Add a character into a hash. From the SML/NJ library. The
       computation is h = 33 * h + 720 + c *)
    fun hashChar (c : char, h : Word32.word) : Word32.word =
        Word32.<<(h, 0w5) + h + 0w720 + (Word32.fromInt (Char.ord c))

    fun hashString s =
        foldl hashChar 0w0 (explode s)

    type hash_key = string
    val hashVal = hashString
    val sameKey = op=
                      
end

structure StringHashMap
          :> PERSISTENT_HASH_MAP
                 where type hash_key = string =
    PersistentHashMapFn(StringHashKey)

