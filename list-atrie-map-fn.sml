
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

    datatype 'a node = NODE of {
                         item : 'a option,
                         base : int,
                         nonempty : int,
                         vec : 'a node option vector
                     }

    type 'a trie = 'a node option
                      
    val empty = NONE
                    
    fun isEmpty NONE = true
      | isEmpty (SOME (NODE { nonempty = 0, ... })) = true
      | isEmpty _ = false

    fun findInNodeVec (NODE { item, base, nonempty, vec }, i) =
        if i < base orelse i >= base + Vector.length vec
        then NONE
        else Vector.sub (vec, i - base)

    fun removeFromNodeVec (n as NODE { item, base, nonempty, vec }, i) =
        case findInNodeVec (n, i) of
            NONE => n
          | SOME _ => NODE { item = item,
                             base = base,
                             nonempty = nonempty - 1,
                             vec = Vector.update (vec, i - base, NONE)
                           }
                        
    fun updateNodeVec (n as NODE { item, base, nonempty, vec }, i, v) =
        if nonempty = 0
        then (case v of
                  NONE => n
                | v => NODE { item = item,
                              base = i,
                              nonempty = 1,
                              vec = Vector.tabulate (1, fn _ => v)
                            })
        else if i < base
        then updateNodeVec (
                NODE { item = item,
                       base = i,
                       nonempty = nonempty,
                       vec = Vector.concat [
                           Vector.tabulate (base - i, fn _ => NONE),
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
                                    Vector.tabulate (i - fin + 1, fn _ => NONE)
                              ]}, i, v)
                 else let val nonempty =
                              case (Vector.sub (vec, i - base),
                                    isEmpty v) of
                                  (SOME _, true) => nonempty
                                | (SOME _, false) => nonempty + 1
                                | (NONE, true) => nonempty - 1
                                | (NONE, false) => nonempty
                      in
                          case nonempty of
                              0 => NODE {
                                      item = item,
                                      base = base,
                                      nonempty = nonempty,
                                      vec = vec
                                  }
                            | _ => NODE { item = item,
                                          base = base,
                                          nonempty = nonempty,
                                          vec = Vector.update (vec, i - base, v)
                                        }
                      end
             end

    fun insert (NONE, xx, v) =
        insert (SOME (NODE { item = NONE,
                             base = 0,
                             nonempty = 0,
                             vec = Vector.fromList []
                     }), xx, v)
      | insert (SOME (NODE { item, base, nonempty, vec }), [], v) =
        SOME (NODE { item = SOME v,
                     base = base,
                     nonempty = nonempty,
                     vec = vec
                   })
      | insert (SOME n, x::xs, v) =
        let val i = E.ord x
        in
            case findInNodeVec (n, i) of
                NONE => SOME (updateNodeVec (n, i, insert (empty, xs, v)))
              | nsub => SOME (updateNodeVec (n, i, insert (nsub, xs, v)))
        end

    fun remove (NONE, _) = NONE
      | remove (SOME (NODE { item, base, nonempty, vec }), []) =
        SOME (NODE { item = NONE,
                     base = base,
                     nonempty = nonempty,
                     vec = vec
                   })
      | remove (SOME n, x::xs) =
        let val i = E.ord x
        in
            case findInNodeVec (n, i) of
                NONE => SOME n
              | nsub => SOME (updateNodeVec (n, i, remove (nsub, xs)))
        end

    fun find (NONE, _) = NONE
      | find (SOME (NODE { item, ... }), []) = item
      | find (SOME n, x::xs) =
        case findInNodeVec (n, E.ord x) of
            NONE => NONE
          | nsub => find (nsub, xs)

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NODE { item, base, vec, ... }) =
        Vector.foldli
            (fn (ix, NONE, acc) => acc
              | (ix, SOME n, acc) =>
                foldli_helper f (acc, E.invOrd (base + ix) :: rpfx, n))
            (case item of
                 NONE => acc
               | SOME v => f (rev rpfx, v, acc))
            vec

    fun foldl f acc NONE = acc
      | foldl f acc (SOME node) = 
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], node)
                      
    fun foldli f acc NONE = acc
      | foldli f acc (SOME node) = 
        foldli_helper f (acc, [], node)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (NONE, e) = acc
      | foldliPrefixMatch f acc (SOME node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, n, []) = foldli_helper f (acc, rpfx, n)
              | fold' (acc, rpfx, NODE { base, vec, ... }, x::xs) =
                case Vector.sub (vec, E.ord x - base) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (NONE, p) = acc
      | foldliPatternMatch f acc (SOME node, p) = 
        let fun fold' (acc, pfx, n, p) =
                case p of
                    [] => 
                    (case n of
                         NODE { item = SOME v, ... } => f (rev pfx, v, acc)
                       | _ => acc)
                  | NONE::xs =>
                    (case n of
                         NODE { base, vec, ... } =>
                         Vector.foldli (fn (ix, NONE, acc) => acc
                                         | (ix, SOME n, acc) =>
                                           fold' (acc,
                                                  E.invOrd (base + ix) :: pfx,
                                                  n, xs))
                                       acc vec)
                  | (SOME x)::xs =>
                    (case n of
                         NODE { vec, ... } =>
                         case findInNodeVec (n, E.ord x) of
                             NONE => acc
                           | SOME nsub => fold' (acc, x :: pfx, nsub, xs))
        in
            fold' (acc, [], node, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (NONE, e) = []
      | prefixOf (SOME node, e) = 
        let fun prefix' (best, acc, n as NODE { item, base, vec, ... }, x::xs) =
                let val best =
                        case item of
                            NONE => best
                          | SOME _ => acc
                in
                    case findInNodeVec (n, E.ord x) of
                        NONE => best
                      | SOME nsub => prefix' (best, x :: acc, nsub, xs)
                end
              | prefix' (best, acc, NODE { item = SOME _, ... }, []) = acc
              | prefix' (best, acc, NODE { item = NONE, ... }, []) = best
        in
	    rev (prefix' ([], [], node, e))
        end

end

