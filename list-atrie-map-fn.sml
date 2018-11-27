
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature ATRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
end

functor ATrieNodeMapFn (E : ATRIE_ELEMENT)
        :> LIST_TRIE_NODE_MAP
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

    fun foldl f acc (MAP { vec, ... }) =
        Vector.foldl (fn (NONE, acc) => acc
                       | (SOME x, acc) => f (x, acc))
                     acc vec

    fun foldli f acc (MAP { base, vec, ... }) =
        Vector.foldli (fn (i, NONE, acc) => acc
                        | (i, SOME x, acc) => f (E.invOrd (i + base), x, acc))
                      acc vec

    fun update (m as MAP { base, nonempty, vec }, k, x) =
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
            
end

(* Turn a type that can be compactly converted into an integer
   ordering into a trie holding lists of that type. Each level of the
   trie uses a vector to hold pointers to its sub-nodes, indexed by
   integer value of the type. Because this is an immutable structure
   and updating a vector can be slow, this may be inefficient for
   "wide" tries with many sub-nodes per node and a lot of inserts. It
   may be more efficient for deep tries, e.g. of character lists, with
   lots of lookups. *)
                                                      
functor ListATrieMapFn (E : ATRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list =
    ListTrieMapFn(ATrieNodeMapFn(E))
