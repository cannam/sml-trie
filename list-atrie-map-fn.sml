
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature ATRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
    val toString : t -> string
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

    fun printStructure trie =
        let fun space depth =
                case depth of
                    0 => ()
                  | _ => (print "  "; space (depth - 1))
            fun print' depth node =
                (space depth;
                 case node of
                     LEAF NONE => print "#\n"
                   | LEAF (SOME v) => print "VALUE\n"
                   | NODE { item, base, nonempty, vec } =>
                     (case item of
                          NONE => print "# "
                        | SOME v => print "VALUE ";
                      print ("(base " ^ (Int.toString base) ^
                             ", nonempty " ^ (Int.toString nonempty) ^ ") [\n");
                      Vector.appi (fn (i, nsub) =>
                                      (space depth;
                                       print (E.toString (E.invOrd (base + i)) ^ ":\n");
                                       print' (depth + 1) nsub)) vec;
                      space depth;
                      print "]\n"))
        in
            print' 0 trie
        end
                      
    val empty = LEAF NONE

    fun isEmpty (LEAF NONE) = true
      | isEmpty (LEAF _) = false
      | isEmpty (NODE { nonempty, ... }) = nonempty = 0

    fun findInNodeVec (LEAF _, _) = NONE
      | findInNodeVec (NODE { item, base, nonempty, vec }, i) =
        if i < base
        then NONE
        else if i >= base + Vector.length vec
        then NONE
        else let val nsub = Vector.sub (vec, i - base)
             in
                 if isEmpty nsub
                 then NONE
                 else SOME nsub
             end
                 
    fun updateNodeVec (LEAF item, i, v) =
        if isEmpty v
        then LEAF item
        else updateNodeVec (NODE { item = item,
                                   base = 0,
                                   nonempty = 0,
                                   vec = Vector.fromList []
                                 }, i, v)
      | updateNodeVec (n as NODE { item, base, nonempty, vec }, i, v) =
        if nonempty = 0 andalso isEmpty v
        then n
        else if nonempty = 0
        then NODE { item = item,
                    base = i,
                    nonempty = 1,
                    vec = Vector.tabulate (1, fn _ => v)
                  }
        else if i < base
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
                            | _ => NODE { item = item,
                                          base = base,
                                          nonempty = nonempty,
                                          vec = Vector.update (vec, i - base, v)
                                        }
                      end
             end

    fun insert (LEAF _, [], v) =
        LEAF (SOME v)
      | insert (NODE { item, base, nonempty, vec }, [], v) =
        NODE { item = SOME v,
               base = base,
               nonempty = nonempty,
               vec = vec
             }
      | insert (n, x::xs, v) =
        let val i = E.ord x
        in
            case findInNodeVec (n, i) of
                NONE => updateNodeVec (n, i, insert (empty, xs, v))
              | SOME nsub => updateNodeVec (n, i, insert (nsub, xs, v))
        end

    fun remove (LEAF _, []) = LEAF NONE
      | remove (NODE { item, base, nonempty, vec }, []) =
        NODE { item = NONE,
               base = base,
               nonempty = nonempty,
               vec = vec
             }
      | remove (LEAF i, x::xs) = LEAF i
      | remove (n, x::xs) =
        let val i = E.ord x
        in
            case findInNodeVec (n, i) of
                NONE => n
              | SOME nsub => updateNodeVec (n, i, remove (nsub, xs))
        end

    fun find (LEAF item, []) = item
      | find (NODE { item, ... }, []) = item
      | find (n, x::xs) =
        case findInNodeVec (n, E.ord x) of
            NONE => NONE
          | SOME nsub => find (nsub, xs)

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NODE { item, base, vec, ... }) =
        Vector.foldli
            (fn (ix, LEAF (SOME v), acc) =>
                f (rev (E.invOrd (base + ix) :: rpfx), v, acc)
              | (_, LEAF NONE, acc) => acc
              | (ix, n, acc) =>
                foldli_helper f (acc, E.invOrd (base + ix) :: rpfx, n))
            (case item of
                 NONE => acc
               | SOME v => f (rev rpfx, v, acc))
            vec
      | foldli_helper f (acc, rpfx, LEAF (SOME v)) = f (rev rpfx, v, acc)
      | foldli_helper f (acc, rpfx, LEAF NONE) = acc

    fun foldl f acc trie =
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], trie)
                      
    fun foldli f acc trie =
        foldli_helper f (acc, [], trie)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, NODE { base, vec, ... }, x::xs) =
                fold' (acc, x :: rpfx, Vector.sub (vec, E.ord x - base), xs)
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
        let fun fold' (acc, pfx, n, p) =
                case p of
                    [] => 
                    (case n of
                         LEAF (SOME v) => f (rev pfx, v, acc)
                       | NODE { item = SOME v, ... } => f (rev pfx, v, acc)
                       | _ => acc)
                  | NONE::xs =>
                    (case n of
                         LEAF _ => acc
                       | NODE { base, vec, ... } =>
                         Vector.foldli (fn (ix, n, acc) =>
                                           fold' (acc,
                                                  E.invOrd (base + ix) :: pfx,
                                                  n, xs))
                                       acc vec)
                  | (SOME x)::xs =>
                    (case n of
                         LEAF _ => acc
                       | NODE { base, vec, ... } =>
                         case findInNodeVec (n, E.ord x) of
                             NONE => acc
                           | SOME nsub => fold' (acc, x :: pfx, nsub, xs))
        in
            fold' (acc, [], trie, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (trie, e) =
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
              | prefix' (best, acc, LEAF (SOME _), _) = acc
              | prefix' (best, acc, NODE { item = SOME _, ... }, []) = acc
              | prefix' (best, acc, LEAF NONE, _) = best
              | prefix' (best, acc, NODE { item = NONE, ... }, []) = best
        in
	    rev (prefix' ([], [], trie, e))
        end

end

