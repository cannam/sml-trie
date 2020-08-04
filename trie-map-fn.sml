
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

(** Signature for the map used within a trie node to store branching
    information.
 *)
signature TRIE_NODE_MAP = sig
    eqtype key
    type 'a map
    val new : unit -> 'a map
    val isEmpty : 'a map -> bool
    val find : 'a map * key -> 'a option
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val modify : 'a map * key * ('a option -> 'a option) -> 'a map
    val remove : 'a map * key -> 'a map
    val keyCompare : key * key -> order
end

signature TRIE_KEY = sig
    eqtype element
    type key
    val isEmpty : key -> bool
    val head : key -> element
    val tail : key -> key
    val explode : key -> element list
    val implode : element list -> key
    val equal : key * key -> bool
end

signature TRIE_MAP_FN_ARG = sig
    eqtype element
    type key
    structure M : TRIE_NODE_MAP where type key = element
    structure K : TRIE_KEY where type element = element where type key = key
end

functor TrieMapFn (A : TRIE_MAP_FN_ARG)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = A.K.element where type key = A.K.key = struct

    structure M = A.M
    structure K = A.K
                      
    type element = K.element
    type key = K.key
    type pattern = element option list

    datatype 'a node = LEAF of 'a
                     | TWIG of key * 'a (* key nonempty, else it's a leaf *)
                     | BRANCH of 'a option * 'a node M.map

    datatype 'a trie = EMPTY
                     | POPULATED of 'a node
        
    val empty = EMPTY

    fun isEmpty EMPTY = true
      | isEmpty _ = false

    fun newBranch opt = BRANCH (opt, M.new ())

    fun isEmptyBranch (BRANCH (NONE, m)) = M.isEmpty m
      | isEmptyBranch _ = false

    fun isCanonical n =
        case n of
            LEAF _ => true
          | TWIG (k, _) =>
            if K.isEmpty k
            then (print "Not canonical: twig with empty key\n"; false)
            else true
          | BRANCH (NONE, m) =>
            if M.isEmpty m
            then
                (print "Not canonical: empty branch\n"; false)
            else true
          | BRANCH (_, m) => 
            if M.foldl (fn (mi, acc) => acc orelse isEmptyBranch mi)
                       false m
            then
                (print "Not canonical: branch with empty sub-branch\n"; false)
            else if M.foldl (fn (mi, acc) => acc orelse not (isCanonical mi))
                            false m
            then 
                (print "Not canonical: branch with non-canonical sub-branch\n"; false)
            else true

    fun modify' (n, xx, f : 'a option -> 'a option) =
        if K.isEmpty xx
        then
            case n of
                LEAF existing =>
                (case f (SOME existing) of
                     NONE => newBranch NONE
                   | SOME replacement => LEAF replacement)
              | TWIG (kk, unrelated) =>
                (case f NONE of
                     NONE => TWIG (kk, unrelated)
                   | SOME new =>
                     (* switch to inserting the existing item back into
                        a branch built on the new one *)
                     modify' (newBranch (SOME new), kk, fn _ => SOME unrelated))
              | BRANCH (iopt, m) => BRANCH (f iopt, m)
        else (* xx is nonempty, so we are not at our leaf yet *)
            case n of
                LEAF unrelated =>
                (case f NONE of
                     NONE => LEAF unrelated
                   | SOME new =>
                     modify' (newBranch (SOME unrelated), xx, fn _ => SOME new))
              | TWIG (kk, existing) =>
                (if K.equal (kk, xx)
                 then case f (SOME existing) of
                          NONE => newBranch NONE
                        | SOME replacement => TWIG (kk, replacement)
                 else case f NONE of
                          NONE => TWIG (kk, existing)
                        | SOME new => 
                          if K.head kk = K.head xx (* e.g. XDEF next to XABC *)
                          then let val nsub = modify' (newBranch NONE,
                                                       K.tail xx,
                                                       fn _ => SOME new)
                                   (* reinsert existing into new: *)
                                   val nsub = modify' (nsub,
                                                       K.tail kk,
                                                       fn _ => SOME existing)
                               in
                                   BRANCH (NONE,
                                           M.modify (M.new (), K.head xx,
                                                     fn _ => SOME nsub))
                               end
                          else (* e.g. CDEF next to GHIJ, both known nonempty *)
                              modify' (modify' (newBranch NONE, kk,
                                                fn _ => SOME existing),
                                       xx, fn _ => SOME new))
              | BRANCH (iopt, m) =>
                BRANCH (iopt,
                        M.modify
                            (m, K.head xx,
                             fn NONE =>
                                (case f NONE of
                                     NONE => NONE
                                   | SOME new =>
                                     SOME (let val xs = K.tail xx
                                           in
                                               if K.isEmpty xs
                                               then LEAF new
                                               else TWIG (xs, new)
                                           end))
                             | SOME nsub =>
                               let val nsub' = modify' (nsub, K.tail xx, f)
                               in
                                   if isEmptyBranch nsub'
                                   then NONE
                                   else SOME nsub'
                               end))
                               
    fun modify (n, xx, f) =
        let val n' = modify' (case n of
                                  EMPTY => newBranch NONE
                                | POPULATED n => n,
                              xx, f)
        in
            if isEmptyBranch n'
            then EMPTY
            else
(*                let val _ = if not (isCanonical n')
                            then print "NOT CANONICAL\n"
                            else ()
                in *)
                    POPULATED n'
(*                end *)
        end

    fun insert (nopt, xx, v) =
        modify (nopt, xx, fn _ => SOME v)
                        
    fun remove (nopt, xx) =
        modify (nopt, xx, fn _ => NONE)

    fun isPrefixOf ([], yy) = true
      | isPrefixOf (xx, []) = false
      | isPrefixOf (x::xs, y::ys) = x = y andalso isPrefixOf (xs, ys)

    fun find' (n, xx) =
        if K.isEmpty xx
        then 
            case n of
                LEAF item => SOME item
              | TWIG (kk, item) => NONE  (* kk should always be nonempty *)
              | BRANCH (iopt, m) => iopt
        else
            case n of
                LEAF _ => NONE
              | TWIG (kk, item) => (if K.equal (kk, xx)
                                    then SOME item
                                    else NONE)
              | BRANCH (_, m) =>
                case M.find (m, K.head xx) of
                    NONE => NONE
                  | SOME nsub => find' (nsub, K.tail xx)

    fun findi (EMPTY, _) = NONE
      | findi (POPULATED n, xx) =
        case find' (n, xx) of
            NONE => NONE
          | SOME v => SOME (xx, v)

    fun find (EMPTY, _) = NONE
      | find (POPULATED n, xx) =
        find' (n, xx)

    fun locateEqual (n, xx, _) =
        case find' (n, xx) of
            NONE => NONE
          | SOME v => SOME (NONE, v)

    fun firstIn (n, rpfx) =
        case n of
            LEAF item => SOME (rev rpfx, item)
          | TWIG (kk, item) => SOME (rev rpfx @ K.explode kk, item)
          | BRANCH (SOME item, _) => SOME (rev rpfx, item)
          | BRANCH (NONE, m) =>
            (*!!! inefficient without custom help from M *)
            M.foldli (fn (_, _, SOME result) => SOME result
                      | (k, n', NONE) => firstIn (n', k :: rpfx))
                     NONE m

    fun locateGreater (n, xx, rpfx) = (*!!! todo: locateLess *)
        if K.isEmpty xx
        then
            case n of
                LEAF item => SOME (NONE, item)
              | TWIG (kk, item) =>
                SOME (SOME (K.implode (rev rpfx @ K.explode kk)), item)
              | BRANCH (SOME item, m) => SOME (NONE, item)
              | n => case firstIn (n, rpfx) of
                         NONE => NONE
                       | SOME (pfx, item) =>
                         SOME (SOME (K.implode pfx), item)
        else
            case n of
                LEAF item => NONE
              | TWIG (kk, item) =>
                (if isPrefixOf (K.explode xx, K.explode kk)
                 then SOME (SOME (K.implode (rev rpfx @ K.explode kk)), item)
                 else NONE)
              | BRANCH (_, m) =>
                let val (x, xs) = (K.head xx, K.tail xx)
                in M.foldli (fn (_, _, SOME result) => SOME result
                              | (k, n', NONE) =>
                                case M.keyCompare (k, x) of
                                    LESS => NONE
                                  | GREATER =>
                                    locateGreater(n', K.implode [], k::rpfx)
                                  | EQUAL =>
                                    locateGreater(n', xs, k::rpfx))
                            NONE
                            m
                end
                    
    fun locate (EMPTY, xx, ord) = NONE
      | locate (POPULATED n, xx, ord) =
        case (case ord of GREATER => locateGreater
                        | LESS => raise Fail "not yet implemented!"
                        | EQUAL => locateEqual) (n, xx, []) of
            NONE => NONE
          | SOME (NONE, item) => SOME (xx, item)
          | SOME (SOME kk, item) => SOME (kk, item)
                               
    fun lookup (t, k) =
        case find (t, k) of
            NONE => raise Subscript
          | SOME v => v
                              
    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
             
    fun foldl f acc t =
        let fun fold' (n, acc) =
                case n of
                    LEAF item => f (item, acc)
                  | TWIG (kk, item) => f (item, acc)
                  | BRANCH (iopt, m) =>
                    M.foldl fold' (case iopt of
                                       NONE => acc
                                     | SOME item => f (item, acc))
                            m
        in
            case t of
                EMPTY => acc
              | POPULATED n => fold' (n, acc)
        end

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldliNode f (rpfx, n, acc) =
        let fun f' (pfx, item, acc) = f (K.implode pfx, item, acc)
        in
            case n of
                LEAF item => f' (rev rpfx, item, acc)
              | TWIG (kk, item) => f' ((rev rpfx) @ (K.explode kk), item, acc)
              | BRANCH (iopt, m) =>
                M.foldli (fn (x, n, acc) => foldliNode f (x :: rpfx, n, acc))
                         (case iopt of
                              NONE => acc
                            | SOME item => f' (rev rpfx, item, acc))
                         m
        end

    fun foldliNodeRange f (rpfx, n, leftConstraintK, rightConstraintK, acc) =
        let val leftConstraint = Option.map K.explode leftConstraintK
            val rightConstraint = Option.map K.explode rightConstraintK

            val _ = case leftConstraintK of
                        NONE => print "no left constraint\n"
                      | SOME _ => print "have left constraint\n"

            val _ = case rightConstraintK of
                        NONE => print "no right constraint\n"
                      | SOME _ => print "have right constraint\n"
                                             
            fun f' (pfx, item, acc) =
                f (K.implode pfx, item, acc)

            fun hd' [] = NONE (*!!! no? *)
              | hd' (x::_) = SOME x

            (* When foldliNodeRange is entered, leftConstraint and
               rightConstraint may be NONE (no constraint), SOME []
               (constraint at start of this node), or SOME other
               (constraint on sub-node). For leftConstraint there is
               no distinction between NONE and SOME [], so we can just
               pass the optional list or [] if there was none. For
               rightConstraint, there is a distinction, but it is that
               we don't want to call the node traversal function at
               all if the constraint is SOME []. So we can similarly
               just pass the optional list, passing [] if there was
               none but not calling at all if there was one and it was
               empty. So we don't need an option in these functions -
               an empty list represents "no constraint" rather than a
               constraint at the start of the node.

               We use the names leftConstraint/rightConstraint to
               refer to the option types and lc/rc to refer to these
               unwrapped versions. *)
                                  
            fun leaf (item, lc, _) =
                case lc of
                    [] => (print "leaf -> accept\n"; f' (rev rpfx, item, acc))
                  | _ => (print "leaf -> reject\n"; acc)

            fun twig (kk, item, lc, rc) =
                let val kk' = K.explode kk
                in
                    if (isPrefixOf (lc, kk') andalso
                        (*!!! no, it should be if rc is null or sorts > kk' *)
                        (null rc orelse not (isPrefixOf (rc, kk'))))
                    then (print "twig -> accept\n";  f' ((rev rpfx) @ kk', item, acc))
                    else (print "twig -> reject\n"; acc)
                end

            fun branch (iopt, m, [], []) =
                foldliNode f (rpfx, n, acc)
              | branch (iopt, m, lc, rc) =
                M.foldli
                    (fn (x, n, acc) =>
                        let val accept =
                                (null lc orelse M.keyCompare (x, hd lc) <> LESS)
                                andalso
                                (null rc orelse M.keyCompare (x, hd rc) = LESS)
                        in
                            if not accept then
                                (print "branch -> map entry -> reject\n";
                                 acc)
                            else
                                (print "branch -> map entry -> accept and recurse\n";
                                foldliNodeRange
                                    f
                                    (x :: rpfx,
                                     n,
                                     (*!!! to refactor once working *)
                                     case lc of
                                         [] => NONE
                                       | (c::cc) =>
                                         if c = x
                                         then SOME (K.implode cc)
                                         else NONE,
                                     case rc of
                                         [] => NONE
                                       | (c::cc) =>
                                         if c = x
                                         then SOME (K.implode cc)
                                         else NONE,
                                    acc))
                        end)
                    (case iopt of
                         NONE => acc
                       | SOME item =>
                         case lc of
                             [] => f' (rev rpfx, item, acc)
                           | _ => acc)
                    m
        in
            case rightConstraint of
                SOME [] => acc
              | _ =>
                let val lc = Option.getOpt (leftConstraint, [])
                    val rc = Option.getOpt (rightConstraint, [])
                in
                    case n of
                        LEAF item => leaf (item, lc, rc)
                      | TWIG (kk, item) => twig (kk, item, lc, rc)
                      | BRANCH (iopt, m) => branch (iopt, m, lc, rc)
                end
        end

    fun foldliRange f acc (t, leftConstraint, rightConstraint) =
        case t of
            EMPTY => (print "empty root\n"; acc)
          | POPULATED n => (print "non-empty root\n"; 
            foldliNodeRange f ([], n, leftConstraint, rightConstraint, acc))
            
    fun foldli f acc t =
        case t of
            EMPTY => acc
          | POPULATED n => foldliNode f ([], n, acc)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)
            
    fun foldliPrefixMatch' f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (rpfx, n, xx, acc) =
                if K.isEmpty xx
                then foldliNode f (rpfx, n, acc)
                else
                    case n of
                        LEAF item => acc
                      | TWIG (kk, item) =>
                        (if isPrefixOf (K.explode xx, K.explode kk)
                         then foldliNode f (rpfx, n, acc)
                         else acc)
                      | BRANCH (_, m) => 
                        case M.find (m, K.head xx) of
                            NONE => acc
                          | SOME nsub =>
                            fold' ((K.head xx) :: rpfx, nsub, (K.tail xx), acc)
        in
            fold' ([], node, e, acc)
        end

    fun foldliPrefixMatch f acc (trie, e) =
        case trie of
            EMPTY => acc
          | POPULATED n => foldliPrefixMatch' f acc (n, e)
            
    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch' f acc (node, p) =
        let fun f' (pfx, item, acc) = f (K.implode pfx, item, acc)
            fun fold' (rpfx, n, xx, acc) =
                case (n, xx) of
                    (LEAF item, []) => f' (rev rpfx, item, acc)
                  | (TWIG (kk, item), []) => f' (rev rpfx, item, acc)
                  | (BRANCH (NONE, _), []) => acc
                  | (BRANCH (SOME item, _), []) => f' (rev rpfx, item, acc)
                  | (LEAF _, xx) => acc
                  | (TWIG (kk, item), xx) =>
                    if ListPair.allEq (fn (k, NONE) => true
                                        | (k, SOME x) => k = x)
                                      (K.explode kk, xx)
                    then f' (rev rpfx @ K.explode kk, item, acc)
                    else acc
                  | (BRANCH (_, m), NONE::xs) =>
                    M.foldli (fn (x, n, acc) =>
                                 fold' (x :: rpfx, n, xs, acc))
                             acc m
                  | (BRANCH (_, m), (SOME x)::xs) =>
                    case M.find (m, x) of
                        NONE => acc
                      | SOME nsub => fold' (x :: rpfx, nsub, xs, acc)
        in
            fold' ([], node, p, acc)
        end

    fun foldliPatternMatch f acc (trie, p) =
        case trie of
            EMPTY => acc
          | POPULATED node => foldliPatternMatch' f acc (node, p)
            
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))
                                          
    fun prefixOf (trie, e) =
        let fun prefix' (n, xx, best, acc) =
                if K.isEmpty xx
                then case n of
                         LEAF item => acc
                       | TWIG (kk, item) => acc
                       | BRANCH (NONE, m) => best
                       | BRANCH (SOME item, m) => acc
                else case n of
                         LEAF item => acc
                       | TWIG (kk, item) =>
                         if K.equal (kk, xx) orelse
                            isPrefixOf (K.explode kk, K.explode xx)
                         then rev (K.explode kk) @ acc
                         else best
                       | BRANCH (iopt, m) =>
                         let val (x, xs) = (K.head xx, K.tail xx)
                             val best = case iopt of NONE => best
                                                   | SOME _ => acc
                         in
                             case M.find (m, x) of
                                 NONE => best
                               | SOME nsub => prefix' (nsub, xs, best, x::acc)
                         end
        in
            K.implode
                (case trie of
                     EMPTY => []
                   | POPULATED node => rev (prefix' (node, e, [], [])))
        end

end

