
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature ATRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
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
	       where type element = E.t where type key = E.t list = struct

    type element = E.t
    type key = element list
    type pattern = element option list

    datatype 'a node = LEAF of 'a option
                     | NODE of {
                         item : 'a option,
                         base : int,
                         nonempty : int,
                         vec : 'a node vector
                     }

    type 'a trie = 'a node

    val empty = LEAF NONE

    fun isEmpty (LEAF NONE) = true
      | isEmpty (LEAF _) = false
      | isEmpty (NODE { nonempty, ... }) = nonempty = 0
                    
    fun updateNodeVec (LEAF item, i, v) =
        if isEmpty v
        then LEAF item
        else updateNodeVec (NODE { item = item,
                                   base = 0,
                                   nonempty = 0,
                                   vec = Vector.fromList []
                                 }, i, v)
      | updateNodeVec (NODE { item, base, nonempty, vec }, i, v) : 'a node =
        if i < base
        then updateNodeVec (
                NODE { item = item,
                       base = i,
                       nonempty = nonempty,
                       vec = Vector.concat [
                           Vector.tabulate (base - i, fn _ => empty),
                           vec
                     ]}, i, v)
        else let val fin = base + Vector.length vec
             in
                 if i >= fin
                 then updateNodeVec (
                         NODE { item = item,
                                base = base,
                                nonempty = nonempty,
                                vec = Vector.concat [
                                    vec,
                                    Vector.tabulate (i - fin + 1,
                                                     fn _ => empty)
                              ]}, i, v)
                 else let val nonempty =
                              case (isEmpty (Vector.sub (vec, i - base)),
                                    isEmpty v) of
                                  (true, true) => nonempty
                                | (true, false) => nonempty + 1
                                | (false, true) => nonempty - 1
                                | (false, false) => nonempty
                      in
                          case nonempty of
                              0 => empty
                            | _ => NODE {
                                      item = item,
                                      base = base,
                                      nonempty = nonempty,
                                      vec = Vector.update (vec, i - base, v)
                                  }
                      end
             end

    fun insert (LEAF _, [], v) = LEAF (VALUE v)
      | insert (NODE (_, m), [], v) = NODE (VALUE v, m)
      | insert (n, x::xs, v) =
        let val ix = E.ord x
        in
            case n of
                LEAF i => 
                NODE (i, Vector.tabulate (E.maxOrd,
                                          fn n => if n = ix
                                                  then insert (empty, xs, v)
                                                  else empty))
              | NODE (i, nn) => 
                NODE (i, Vector.update (nn, ix,
                                        insert (Vector.sub (nn, ix), xs, v)))
        end

    fun remove (LEAF _, []) = LEAF NO_VALUE
      | remove (NODE (_, nn), []) = NODE (NO_VALUE, nn)
      | remove (LEAF i, x::xs) = LEAF i
      | remove (NODE (i, nn), x::xs) =
        let val ix = E.ord x
        in
            NODE (i, Vector.update (nn, ix, remove (Vector.sub (nn, ix), xs)))
        end

    fun find (LEAF (VALUE v), []) = SOME v
      | find (NODE (VALUE v, _), []) = SOME v
      | find (NODE (_, nn), x::xs) =
        let val ix = E.ord x
        in
            find (Vector.sub (nn, ix), xs)
        end
      | find _ = NONE

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NODE (i, nn)) =
        Vector.foldli (fn (ix, LEAF (VALUE v), acc) =>
                          f (rev (E.invOrd ix :: rpfx), v, acc)
                        | (_, LEAF NO_VALUE, acc) => acc
                        | (ix, n, acc) =>
                          foldli_helper f (acc, E.invOrd ix :: rpfx, n))
                      (case i of
                           VALUE v => f (rev rpfx, v, acc)
                         | NO_VALUE => acc)
                      nn
      | foldli_helper f (acc, rpfx, LEAF (VALUE v)) = f (rev rpfx, v, acc)
      | foldli_helper f (acc, rpfx, LEAF NO_VALUE) = acc

    fun foldl f acc trie =
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], trie)
                      
    fun foldli f acc trie =
        foldli_helper f (acc, [], trie)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, NODE (_, nn), x::xs) =
                fold' (acc, x :: rpfx, Vector.sub (nn, E.ord x), xs)
              | fold' (acc, rpfx, trie, []) = foldli_helper f (acc, rpfx, trie)
              | fold' (acc, rpfx, LEAF _, _) = acc
        in
            fold' (acc, [], trie, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (trie, p) =
        let fun fold' (acc, pfx, NODE (_, nn), (SOME x)::xs) =
                fold' (acc, x :: pfx, Vector.sub (nn, E.ord x), xs)
              | fold' (acc, pfx, LEAF (VALUE v), []) = f (rev pfx, v, acc)
	      | fold' (acc, pfx, NODE (VALUE v, _), []) = f (rev pfx, v, acc)
	      | fold' (acc, pfx, _, []) = acc
              | fold' (acc, pfx, LEAF _, _) = acc
              | fold' (acc, pfx, NODE (_, nn), NONE::xs) =
                Vector.foldli (fn (ix, n, acc) =>
                                  fold' (acc, E.invOrd ix :: pfx, n, xs))
                              acc
                              nn
        in
            fold' (acc, [], trie, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (trie, e) =
        let fun prefix' (best, acc, NODE (i, nn), x::xs) =
                let val best =
                        case i of
                            VALUE _ => acc
                          | NO_VALUE => best
                in
                    prefix' (best, x :: acc, Vector.sub (nn, E.ord x), xs)
                end
              | prefix' (best, acc, LEAF (VALUE _), _) = acc
              | prefix' (best, acc, NODE (VALUE _, _), []) = acc
              | prefix' (best, acc, LEAF (NO_VALUE), _) = best
              | prefix' (best, acc, NODE (NO_VALUE, _), []) = best
        in
	    rev (prefix' ([], [], trie, e))
        end

end

