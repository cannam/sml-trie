
(* Copyright 2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature BIT_VECTOR = sig
    type vector
    val new : int -> vector
    val length : vector -> int
    val tabulate : int * (int -> bool) -> vector
    val sub : vector * int -> bool
    val update : vector * int * bool -> vector
    val foldli : (int * bool * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * bool * 'a -> 'a) -> 'a -> vector -> 'a
    val popcount : vector * int -> int
    exception UnsupportedLength
end

structure BitWord32 :> BIT_VECTOR = struct

    local
        fun bitMask i : Word32.word =
            Word32.<< (0w1, Word.andb (Word.fromInt i, 0w31))

        open Word32

        (* 32-bit population-count, Bagwell 2001 *)
        fun pc32 w =
            let open Word32
                val sk5 = 0wx55555555
                val sk3 = 0wx33333333
                val skf0 = 0wxf0f0f0f
                val skff = 0wxff00ff
                val w = w - (andb (>> (w, 0w1), sk5))
                val w = andb (w, sk3) + (andb (>> (w, 0w2), sk3))
                val w = andb (w, skf0) + (andb (>> (w, 0w4), skf0))
                val w = w + >> (w, 0w8)
            in
                andb (w + >> (w, 0w16), 0wx3f)
            end
    in

    type vector = Word32.word
    exception UnsupportedLength

    fun new n = if n = 32
                then Word32.fromInt 0
                else raise UnsupportedLength

    fun length _ = 32
                           
    fun tabulate (n : int, f : int -> bool) : Word32.word =
        if (Int.> (n, 32))
        then raise UnsupportedLength
        else
            let fun tabulate' (i, w) =
                    if i = n
                    then w
                    else
                        let val b = f i
                            val updated = if b
                                          then orb (w, bitMask i)
                                          else w
                        in
                            tabulate' (Int.+ (i, 1), updated)
                        end
            in
                tabulate' (0, 0w0)
            end

    fun sub (w : Word32.word, i : int) : bool =
        andb (w, bitMask i) <> 0w0

    fun update (w : Word32.word, i : int, b : bool) : Word32.word =
        if b
        then orb (w, bitMask i)
        else andb (w, notb (bitMask i))

    fun foldli (f : int * bool * 'a -> 'a)
               (acc : 'a)
               (w : Word32.word) : 'a =
        let fun fold' (0w0, w, i, acc) = acc
              | fold' (bit, w, i, acc) =
                fold' (<< (bit, 0w1), w, Int.+ (i, 1),
                       f (i, andb (w, bit) <> 0w0, acc))
        in
            fold' (0w1, w, 0, acc)
        end

    fun foldri (f : int * bool * 'a -> 'a)
               (acc : 'a)
               (w : Word32.word) : 'a =
        let fun fold' (0w0, w, i, acc) = acc
              | fold' (bit, w, i, acc) =
                fold' (>> (bit, 0w1), w, Int.- (i, 1),
                       f (i, andb (w, bit) <> 0w0, acc))
        in
            fold' (0wx80000000, w, 31, acc)
        end
                  
    (* return number of 1s in the first i bits of the word *)
    fun popcount (w : Word32.word, i : int) : int =
        Word32.toInt
            (pc32 (if Int.<(i, 32)
                   then andb (w, bitMask i - 0w1)
                   else w))

    end
end

structure BitVector :> BIT_VECTOR = struct
    local
        fun wordCount n = ((n + 31) div 32)
        fun wordFor i = (i div 32)
        fun bitInWord (iw, i) = (i - iw * 32)
        fun bitsUsed (iw, n) = let val bn = bitInWord (iw, n)
                               in if bn > 32 then 32 else bn
                               end
                                                      
    in
        type vector = int * BitWord32.vector vector
        exception UnsupportedLength

        fun new n : vector =
            (n, Vector.tabulate (wordCount n, fn _ => BitWord32.new 32))

        fun length ((n, _) : vector) : int =
            n

        fun tabulate (n : int, f : int -> bool) : vector =
            (n, Vector.tabulate
                    (wordCount n,
                     fn iw => BitWord32.tabulate (bitsUsed (iw, n),
                                                  fn ib => f (iw * 32 + ib))))
                
        fun sub ((n, vec) : vector, i : int) : bool =
            if i >= n
            then raise Subscript
            else
                let val iw = wordFor i
                in
                    BitWord32.sub (Vector.sub (vec, iw), bitInWord (iw, i))
                end
                    
        fun update ((n, vec) : vector, i : int, b : bool) : vector =
            if i >= n
            then raise Subscript
            else 
                let val iw = wordFor i
                in
                    (n, Vector.update
                            (vec, iw,
                             BitWord32.update (Vector.sub (vec, iw),
                                               bitInWord (iw, i),
                                               b)))
                end

        fun fold' vectorFold bitwordFold f acc (n, vec) =
            vectorFold (fn (iw, w, acc) =>
                           bitwordFold (fn (ib, b, acc) =>
                                           let val i = iw * 32 + ib
                                           in
                                               if i >= n
                                               then acc
                                               else f (i, b, acc)
                                           end)
                                       acc
                                       w)
                       acc
                       vec
                    
        fun foldli (f : (int * bool * 'a -> 'a))
                   (acc : 'a)
                   (v : vector) : 'a =
            fold' Vector.foldli BitWord32.foldli f acc v
                       
        fun foldri (f : (int * bool * 'a -> 'a))
                   (acc : 'a)
                   (v : vector) : 'a =
            fold' Vector.foldri BitWord32.foldri f acc v
                
        (* population count: return number of 1s in the first i bits
           of the vector *)
        fun popcount ((n, vec) : vector, i : int) : int =
            let val iw = wordFor i
            in
                Vector.foldli
                    (fn (j, w, acc) =>
                        if j > iw
                        then acc
                        else if j < iw
                        then acc + BitWord32.popcount (w, 32)
                        else acc + BitWord32.popcount (w, bitInWord (j, i)))
                    0 vec
            end
    end
end

functor BitMappedVectorFn (V : BIT_VECTOR) = struct

    type 'a vector = V.vector * 'a vector

    fun new n : 'a vector =
        (V.new n, Vector.fromList [])

    fun length ((b, _) : 'a vector) : int =
        V.length b

    fun population ((_, v) : 'a vector) : int =
        Vector.length v
            
    fun isEmpty (vec : 'a vector) : bool =
        population vec = 0
                              
    fun tabulate (n : int, f : int -> 'a option) : 'a vector =
        let val expanded = Vector.tabulate (n, f)
        in
            (V.tabulate
                 (n, fn i => Option.isSome (Vector.sub (expanded, i))),
             Vector.fromList
                 (List.concat
                      (List.tabulate
                           (n, fn i => case Vector.sub (expanded, i) of
                                           NONE => []
                                         | SOME x => [x]))))
        end

    fun contains ((b, _) : 'a vector, i : int) : bool =
        V.sub (b, i)
                       
    fun find ((b, v) : 'a vector, i : int) : 'a option =
        if V.sub (b, i)
        then let val ix = V.popcount (b, i)
             in
                 SOME (Vector.sub (v, ix))
             end
        else NONE

    fun sub (vec : 'a vector, i : int) : 'a =
        case find (vec, i) of
            NONE => raise Subscript
          | SOME x => x

    fun enumerate (vec as (b, v) : 'a vector) : 'a option list =
        V.foldri 
            (fn (i, b, acc) =>
                (if b
                 then SOME (sub (vec, i))
                 else NONE)
                :: acc)
            [] b
            
    fun alter ((b, v) : 'a vector, i : int, f : 'a option -> 'a option) : 'a vector =
        let val pc = V.popcount (b, i)
        in
            if V.sub (b, i)
            then case f (SOME (Vector.sub (v, pc))) of
                     NONE =>
                     (V.update (b, i, false),
                      Vector.tabulate (Vector.length v - 1,
                                       fn j => if j < pc
                                               then Vector.sub (v, j)
                                               else Vector.sub (v, j + 1)))
                   | SOME x =>
                     (b, Vector.update (v, pc, x))
            else case f NONE of
                     NONE =>
                     (b, v)
                   | SOME x =>
                     (V.update (b, i, true),
                      Vector.tabulate (Vector.length v + 1,
                                       fn j => if j < pc
                                               then Vector.sub (v, j)
                                               else if j > pc
                                               then Vector.sub (v, j - 1)
                                               else x))
        end

    fun update (vec, i, x) =
        alter (vec, i, fn _ => SOME x)

    fun remove (vec, i) =
        alter (vec, i, fn _ => NONE)

    fun foldli (f : (int * 'a * 'b -> 'b))
               (acc : 'b) ((b, v) : 'a vector) : 'b =
        case V.foldli
                 (fn (i, bit, (ix, acc)) =>
                     if bit
                     then (ix+1, f (i, Vector.sub (v, ix), acc))
                     else (ix, acc))
                 (0, acc) b of
            (ix, acc) => acc

    fun foldri (f : (int * 'a * 'b -> 'b))
               (acc : 'b) ((b, v) : 'a vector) : 'b =
        case V.foldri
                 (fn (i, bit, (ix, acc)) =>
                     if bit
                     then (ix-1, f (i, Vector.sub (v, ix-1), acc))
                     else (ix, acc))
                 (Vector.length v, acc) b of
            (ix, acc) => acc

    (* foldl/foldr are simpler than foldli/foldri, as they don't need
       to look at the bitmap at all *)
    fun foldl (f : ('a * 'b -> 'b))
              (acc : 'b) ((_, v) : 'a vector) : 'b =
        Vector.foldl f acc v

    fun foldr (f : ('a * 'b -> 'b))
              (acc : 'b) ((_, v) : 'a vector) : 'b =
        Vector.foldr f acc v

end

structure BitMappedVector = BitMappedVectorFn(BitVector)
structure BitMappedVector32 = BitMappedVectorFn(BitWord32)
                                               
