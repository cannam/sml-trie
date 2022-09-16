
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature ATRIE_ELEMENT = sig
    eqtype t
    val ord : t -> int
    val invOrd : int -> t
end

functor ATrieNodeMapFn (E : ATRIE_ELEMENT)
        :> TRIE_NODE_MAP
               where type key = E.t = struct

    type key = E.t
    datatype 'a map = MAP of { base : int,
                               nonempty : int,
                               vec : 'a option vector
                             }

    fun new () = MAP { base = 0,
                       nonempty = 0,
                       vec = Vector.fromList []
                     }

    fun isEmpty (MAP { nonempty = 0, ... }) = true
      | isEmpty _ = false

    fun find (MAP { base, vec, ... }, k) =
        let val i = E.ord k
        in
            if i < base orelse i >= base + Vector.length vec
            then NONE
            else Vector.sub (vec, i - base)
        end        

    fun map f (MAP { base, nonempty, vec }) =
        MAP { base = base,
              nonempty = nonempty,
              vec = Vector.map (fn a => Option.map f a) vec
            }

    fun mapi f (MAP { base, nonempty, vec }) =
        MAP { base = base,
              nonempty = nonempty,
              vec = Vector.mapi
                        (fn (i, NONE) => NONE
                          | (i, SOME x) => SOME (f (E.invOrd (i + base), x)))
                        vec
            }
            
    fun foldl f acc (MAP { vec, ... }) =
        Vector.foldl (fn (NONE, acc) => acc
                       | (SOME x, acc) => f (x, acc))
                     acc vec

    fun foldli f acc (MAP { base, vec, ... }) =
        Vector.foldli (fn (i, NONE, acc) => acc
                        | (i, SOME x, acc) => f (E.invOrd (i + base), x, acc))
                      acc vec

    fun foldr f acc (MAP { vec, ... }) =
        Vector.foldr (fn (NONE, acc) => acc
                       | (SOME x, acc) => f (x, acc))
                     acc vec

    fun foldri f acc (MAP { base, vec, ... }) =
        Vector.foldri (fn (i, NONE, acc) => acc
                        | (i, SOME x, acc) => f (E.invOrd (i + base), x, acc))
                      acc vec

    fun insert (m as MAP { base, nonempty, vec }, k, x) =
        let val i = E.ord k
        in
            if nonempty = 0
            then MAP { base = i,
                       nonempty = 1,
                       vec = Vector.tabulate (1, fn _ => SOME x)
                     }
            else if i < base
            then MAP { base = i,
                       nonempty = nonempty + 1,
                       vec = Vector.concat [
                           Vector.tabulate (base - i,
                                            fn 0 => SOME x | _ => NONE),
                           vec
                     ]}
            else if i >= base + Vector.length vec
            then MAP { base = base,
                       nonempty = nonempty + 1,
                       vec = Vector.concat [
                           vec,
                           Vector.tabulate (i - base - Vector.length vec,
                                            fn _ => NONE),
                           Vector.fromList [SOME x]
                     ]}
            else let val nonempty' = case Vector.sub (vec, i - base) of
                                         NONE => nonempty + 1
                                       | SOME _ => nonempty
                 in
                     MAP { base = base,
                           nonempty = nonempty',
                           vec = Vector.update (vec, i - base, SOME x)
                         }
                 end
        end
            
    fun remove (m as MAP { base, nonempty, vec }, k) =
        let val i = E.ord k
        in
            if nonempty = 0
            then m
            else if i < base orelse i >= base + Vector.length vec
            then raise Subscript
            else case Vector.sub (vec, i - base) of
                     NONE => m
                   | SOME _ => MAP { base = base,
                                     nonempty = nonempty - 1,
                                     vec = Vector.update (vec, i - base, NONE)
                                   }
        end

    fun alter (m, k, f) =
        case f (find (m, k)) of
            NONE => remove (m, k)
          | SOME i => insert (m, k, i)

    fun keyCompare (k1, k2) = Int.compare (E.ord k1, E.ord k2)
            
end

