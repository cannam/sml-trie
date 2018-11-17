
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

    datatype 'a node = NO_NODE
                     | NODE of {
                         item : 'a option,
                         base : int,
                         nonempty : int,
                         vec : 'a node vector
                     }

    type 'a trie = 'a node
                      
    val empty = NO_NODE

    fun indent level = String.concat (List.tabulate (level, fn _ => "  "))
                    
    fun dump' level NO_NODE = indent level ^ "no-node"
      | dump' level (NODE { item, nonempty, base, vec }) =
        indent level ^
        (case item of NONE => "node" | _ => "VALUE") ^
        " (nonempty = " ^ Int.toString nonempty ^
        ", base = " ^ Int.toString base ^
        ", len = " ^ Int.toString (Vector.length vec) ^
        ": [\n" ^ (String.concatWith (",\n" ^ indent level)
                                     (map (dump' (level + 1))
                                          (Vector.foldr (op::) [] vec))) ^
        "\n" ^ indent level ^ "])"

    fun dump (t : 'a trie) = dump' 0 t
                                  
    fun isEmpty NO_NODE = true
      | isEmpty _ = false

    fun findInNode (NO_NODE, i) = NO_NODE
      | findInNode (NODE { item, base, nonempty, vec }, i) =
        if i < base orelse i >= base + Vector.length vec
        then NO_NODE
        else Vector.sub (vec, i - base)

    fun updateNode (NO_NODE, i, NO_NODE) = NO_NODE
      | updateNode (NO_NODE, i, v) =
        updateNode (NODE { item = NONE,
                              base = 0,
                              nonempty = 0,
                              vec = Vector.fromList []
                            }, i, v)
      | updateNode (n as NODE { item, base, nonempty, vec }, i, v) =
        if nonempty = 0
        then case v of
                  NO_NODE => n
                | v => NODE { item = item,
                              base = i,
                              nonempty = 1,
                              vec = Vector.tabulate (1, fn _ => v)
                            }
        else if i < base
        then updateNode (
                NODE { item = item,
                       base = i,
                       nonempty = nonempty,
                       vec = Vector.concat [
                           Vector.tabulate (base - i, fn _ => NO_NODE),
                           vec
                     ]}, i, v)
        else if i >= base + Vector.length vec
        then updateNode (
                NODE { item = item,
                       base = base,
                       nonempty = nonempty,
                       vec = Vector.concat [
                           vec,
                           Vector.tabulate (i - base - Vector.length vec + 1,
                                            fn _ => NO_NODE)
                     ]}, i, v)
        else let val nonempty' =
                     case (Vector.sub (vec, i - base), v) of
                         (NODE _, NODE _) => nonempty
                       | (NODE _, NO_NODE) => nonempty - 1
                       | (NO_NODE, NODE _) => nonempty + 1
                       | (NO_NODE, NO_NODE) => nonempty
             in
                 case nonempty' of
                     0 => (case item of
                               NONE => NO_NODE
                             | _ => NODE { item = item,
                                           base = base,
                                           nonempty = 0,
                                           vec = vec
                                         }
                          )
                   | _ => NODE { item = item,
                                 base = base,
                                 nonempty = nonempty',
                                 vec = Vector.update (vec, i - base, v)
                               }
             end

    fun insert (NO_NODE, [], v) =
        NODE { item = SOME v,
               base = 0,
               nonempty = 0,
               vec = Vector.fromList []
             }
      | insert (NODE { item, base, nonempty, vec }, [], v) =
        NODE { item = SOME v,
               base = base,
               nonempty = nonempty,
               vec = vec
             }
      | insert (NO_NODE, x::xs, v) =
        NODE { item = NONE,
               base = E.ord x,
               nonempty = 1,
               vec = Vector.fromList [ insert (NO_NODE, xs, v) ]
             }
      | insert (n, x::xs, v) =
        let val i = E.ord x
        in updateNode (n, i, insert (findInNode (n, i), xs, v))
        end
            
    fun remove (NO_NODE, _) = NO_NODE
      | remove (NODE { item, base, nonempty = 0, vec }, []) = NO_NODE
      | remove (NODE { item, base, nonempty, vec }, []) =
        NODE {
            item = NONE,
            base = base,
            nonempty = nonempty,
            vec = vec
        }
      | remove (n as NODE { item, base, nonempty, vec }, x::xs) =
        case findInNode (n, E.ord x) of
            NO_NODE => n
          | nsub => updateNode (n, E.ord x, remove (nsub, xs))
                                     
    fun find (NO_NODE, _) = NONE
      | find (NODE { item, ... }, []) = item
      | find (n, x::xs) =
        case findInNode (n, E.ord x) of
            NO_NODE => NONE
          | nsub => find (nsub, xs)

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NO_NODE) = acc
      | foldli_helper f (acc, rpfx, NODE { item, base, vec, ... }) =
        Vector.foldli
            (fn (ix, NO_NODE, acc) => acc
              | (ix, n, acc) =>
                foldli_helper f (acc, E.invOrd (base + ix) :: rpfx, n))
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
              | fold' (acc, rpfx, NODE { base, vec, ... }, x::xs) =
                (*!!! this looks wrong - can't E.ord x be out of range? Test *)
                case Vector.sub (vec, E.ord x - base) of
                    NO_NODE => acc
                  | nsub => fold' (acc, x :: rpfx, nsub, xs)
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
                       | NODE { item = NONE, ... } => acc
                       | NODE { item = SOME v, ... } => f (rev pfx, v, acc))
                  | NONE::xs =>
                    (case n of
                         NO_NODE => acc
                       | NODE { base, vec, ... } =>
                         Vector.foldli (fn (ix, NO_NODE, acc) => acc
                                         | (ix, n, acc) =>
                                           fold' (acc,
                                                  E.invOrd (base + ix) :: pfx,
                                                  n, xs))
                                       acc vec)
                  | (SOME x)::xs =>
                    case findInNode (n, E.ord x) of
                        NO_NODE => acc
                      | nsub => fold' (acc, x :: pfx, nsub, xs)
        in
            fold' (acc, [], node, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (NO_NODE, e) = []
      | prefixOf (node, e) = 
        let fun prefix' (best, acc, NO_NODE, _) = best
              | prefix' (best, acc, n as NODE { item, base, vec, ... }, x::xs) =
                let val best = case item of
                                   NONE => best
                                 | SOME _ => acc
                in
                    prefix' (best, x :: acc, findInNode (n, E.ord x), xs)
                end
              | prefix' (best, acc, NODE { item = SOME _, ... }, []) = acc
              | prefix' (best, acc, NODE { item = NONE, ... }, []) = best
        in
	    rev (prefix' ([], [], node, e))
        end

end

