
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature BTRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
end

structure BitMappedVector = struct

    structure BitVector = struct
    local
        fun unitCount n = ((n + 31) div 32)
        fun unitFor i = (i div 32)      
        fun bitMask i : Word32.word =
            (Word32.<< (Word32.fromInt 1, Word.andb(Word.fromInt i, 0w31)))
    in
        type vector = int * Word32.word vector

        fun new n : vector =
            (n, Vector.tabulate (unitCount n, fn _ => 0w0))

        fun length ((n, _) : vector) : int =
            n

        fun tabulate (n : int, f : int -> bool) : vector =
            let fun makeUnit (base, i, unit) =
                    if i = 31 orelse base + i = n
                    then unit
                    else
                        let val b = f (base + i)
                            val updated = if b
                                          then Word32.orb (unit, bitMask i)
                                          else unit
                        in
                            makeUnit (base, i + 1, updated)
                        end
            in
                (n, Vector.tabulate
                        (unitCount n, fn i => makeUnit (i * 32, 0, 0w0)))
            end
                
        fun sub ((n, vec) : vector, i : int) : bool =
            if i >= n then raise Subscript
            else
                let val unit = Vector.sub (vec, unitFor i)
                    val bit = Word32.andb (unit, bitMask i)
                 (*!!!??? : *)
(*                    val _ = print ("bit = " ^ Word32.toString bit ^ "\n") *)
                in
                    bit <> 0w0
                end
                    
        fun update ((n, vec) : vector, i : int, b : bool) : vector =
            if i >= n then raise Subscript
            else 
                let val iy = unitFor i
                    val unit = Vector.sub (vec, iy)
                in
                    (n, Vector.update
                            (vec, iy,
                             if b
                             then Word32.orb (unit, bitMask i)
                             else Word32.andb (unit, Word32.notb (bitMask i))))
                end

        fun foldli (f : (int * bool * 'a -> 'a))
                   (acc : 'a) ((n, vec) : vector) : 'a =
            let fun fold' (bit, ix, unit, acc) =
                  if bit = 0w0 orelse ix = n
                  then acc
                  else fold' (Word32.<< (bit, 0w1), ix + 1, unit,
                              f (ix, Word32.andb (unit, bit) <> 0w0, acc))
            in
                Vector.foldli (fn (i, w, acc) =>
                                  fold' (0w1, i * 32, w, acc))
                              acc vec
            end
                
        (* population count: return number of 1s in the first i bits
           of the vector *)
        fun popcount ((n, vec) : vector, i : int) : int =
            let fun pc32 w =
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
                val iy = unitFor i
                val count = Vector.foldli
                                (fn (i, w, acc) =>
                                    acc + pc32 (if i > iy
                                                then 0w0
                                                else if i < iy
                                                then w
                                                else Word32.andb
                                                         (w, bitMask i - 0w1)))
                                0w0 vec
            in
                Word32.toInt count
            end
    end
    end

    type 'a vector = BitVector.vector * 'a vector

    fun new (n : int) : 'a vector =
        (BitVector.new n, Vector.fromList [])

    fun length ((b, _) : 'a vector) : int =
        BitVector.length b
            
    fun isEmpty ((_, v) : 'a vector) : bool =
        Vector.length v = 0

    fun tabulate (n : int, f : int -> 'a option) : 'a vector =
        let val expanded = Vector.tabulate (n, f)
        in
            (BitVector.tabulate
                 (n, fn i => Option.isSome (Vector.sub (expanded, i))),
             Vector.fromList
                 (List.concat
                      (List.tabulate
                           (n, fn i => case Vector.sub (expanded, i) of
                                           NONE => []
                                         | SOME x => [x]))))
        end

    fun contains ((b, _) : 'a vector, i : int) : bool =
        BitVector.sub (b, i)
                       
    fun find ((b, v) : 'a vector, i : int) : 'a option =
        if BitVector.sub (b, i)
        then let val ix = BitVector.popcount (b, i)
             in
                 SOME (Vector.sub (v, ix))
             end
        else NONE

    fun sub (vec : 'a vector, i : int) : 'a =
        case find (vec, i) of
            NONE => raise Subscript
          | SOME x => x

    fun enumerate (vec as (b, v) : 'a vector) : 'a option list =
        rev
            (BitVector.foldli 
                 (fn (i, b, acc) =>
                     (if b
                      then SOME (sub (vec, i))
                      else NONE)
                         :: acc)
                 [] b)
            
    fun modify ((b, v) : 'a vector, i : int, xo : 'a option) : 'a vector =
        let val pc = BitVector.popcount (b, i)
        in
            if BitVector.sub (b, i)
            then case xo of
                     NONE =>
                     (BitVector.update (b, i, false),
                      Vector.tabulate (Vector.length v - 1,
                                       fn j => if j < pc
                                               then Vector.sub (v, j)
                                               else Vector.sub (v, j + 1)))
                   | SOME x =>
                     (b, Vector.update (v, pc, x))
            else case xo of
                     NONE =>
                     (b, v)
                   | SOME x =>
                     (BitVector.update (b, i, true),
                      Vector.tabulate (Vector.length v + 1,
                                       fn j => if j < pc
                                               then Vector.sub (v, j)
                                               else if j > pc
                                               then Vector.sub (v, j - 1)
                                               else x))
        end

    fun update (vec, i, x) =
        modify (vec, i, SOME x)

    fun erase (vec, i) =
        modify (vec, i, NONE)

    fun foldli (f : (int * 'a * 'b -> 'b))
               (acc : 'b) ((b, v) : 'a vector) : 'b =
        case BitVector.foldli
                 (fn (i, bit, (ix, acc)) =>
                     if bit
                     then (ix+1, f (i, Vector.sub (v, ix), acc))
                     else (ix, acc))
                 (0, acc) b of
            (ix, acc) => acc

    fun foldl (f : ('a * 'b -> 'b))
              (acc : 'b) (vec : 'a vector) : 'b =
        foldli (fn (_, x, acc) => f (x, acc)) acc vec
end

functor ListBTrieMapFn (E : ATRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list = struct

    structure V = BitMappedVector
                                                         
    type element = E.t
    type key = element list
    type pattern = element option list
                               
    datatype 'a node = NO_NODE
                     | NODE of 'a option * 'a node V.vector

    type 'a trie = 'a node
                      
    val empty = NO_NODE

    fun indent level = String.concat (List.tabulate (level, fn _ => "  "))
                    
    fun dump' level NO_NODE = indent level ^ "no-node"
      | dump' level (NODE (item, vec)) =
        indent level ^
        (case item of NONE => "node" | _ => "VALUE") ^
        " [\n" ^ (String.concatWith
                      (",\n" ^ indent level)
                      (map (dump' (level + 1))
                           (rev (V.foldl (op::) [] vec)))) ^
        "\n" ^ indent level ^ "])"

    fun dump (t : 'a trie) = dump' 0 t
                                  
    fun isEmpty NO_NODE = true
      | isEmpty _ = false

    fun insert (NO_NODE, [], v) =
        NODE (SOME v, V.new E.maxOrd)
      | insert (NODE (item, vec), [], v) =
        NODE (SOME v, vec)
      | insert (NO_NODE, x::xs, v) =
        NODE (NONE, V.update (V.new E.maxOrd,
                              E.ord x, insert (NO_NODE, xs, v)))
      | insert (NODE (item, vec), x::xs, v) =
        let val i = E.ord x
        in case V.find (vec, i) of
               NONE =>
               NODE (item, V.update (vec, i, insert (NO_NODE, xs, v)))
             | SOME nsub =>
               NODE (item, V.update (vec, i, insert (nsub, xs, v)))
        end
            
    fun remove (NO_NODE, _) = NO_NODE
      | remove (NODE (item, vec), []) =
        if V.isEmpty vec
        then NO_NODE
        else NODE (NONE, vec)
      | remove (n as NODE (item, vec), x::xs) =
        let val i = E.ord x
        in
            case V.find (vec, i) of
                NONE => n
              | SOME nsub =>
                case remove (nsub, xs) of
                    NODE rn =>
                    NODE (item, V.update (vec, i, NODE rn))
                  | NO_NODE =>
                    case item of
                        NONE => NO_NODE
                      | _ => NODE (item, V.erase (vec, i))
        end

    fun find (NO_NODE, _) = NONE
      | find (NODE (item, _), []) = item
      | find (NODE (item, vec), x::xs) =
        case V.find (vec, E.ord x) of
            NONE => NONE
          | SOME nsub => find (nsub, xs)

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NO_NODE) = acc
      | foldli_helper f (acc, rpfx, NODE (item, vec)) =
        V.foldli (fn (ix, NO_NODE, acc) => acc
                   | (ix, n, acc) =>
                     foldli_helper f (acc, E.invOrd ix :: rpfx, n))
                 (case item of
                      NONE => acc
                    | SOME v => f (rev rpfx, v, acc))
                 vec

    fun foldl f acc NO_NODE = acc
      | foldl f acc n = 
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], n)
                      
    fun foldli f acc NO_NODE = acc
      | foldli f acc n = 
        foldli_helper f (acc, [], n)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (NO_NODE, e) = acc
      | foldliPrefixMatch f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, n, []) = foldli_helper f (acc, rpfx, n)
              | fold' (acc, rpfx, NO_NODE, x::xs) = acc
              | fold' (acc, rpfx, NODE (item, vec), x::xs) =
                case V.find (vec, E.ord x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (NO_NODE, p) = acc
      | foldliPatternMatch f acc (node, p) = 
        let fun fold' (acc, pfx, n, p) =
                case p of
                    [] => 
                    (case n of
                         NO_NODE => acc
                       | NODE (NONE, _) => acc
                       | NODE (SOME v, _) => f (rev pfx, v, acc))
                  | NONE::xs =>
                    (case n of
                         NO_NODE => acc
                       | NODE (_, vec) =>
                         V.foldli (fn (ix, NO_NODE, acc) => acc
                                    | (ix, n, acc) =>
                                      fold' (acc, E.invOrd ix :: pfx, n, xs))
                                  acc vec)
                  | (SOME x)::xs =>
                    case n of
                        NO_NODE => acc
                      | NODE (_, vec) => 
                        case V.find (vec, E.ord x) of
                            NONE => acc
                          | SOME nsub => fold' (acc, x :: pfx, nsub, xs)
        in
            fold' (acc, [], node, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (NO_NODE, e) = []
      | prefixOf (node, e) = 
        let fun prefix' (best, acc, NO_NODE, _) = best
              | prefix' (best, acc, n as NODE (item, vec), x::xs) =
                let val best = case item of
                                   NONE => best
                                 | SOME _ => acc
                in
                    prefix' (best,
                             x :: acc,
                             case V.find (vec, E.ord x) of
                                 NONE => NO_NODE
                               | SOME nsub => nsub,
                             xs)
                end
              | prefix' (best, acc, NODE (SOME _, _), []) = acc
              | prefix' (best, acc, NODE (NONE, _), []) = best
        in
	    rev (prefix' ([], [], node, e))
        end

end

