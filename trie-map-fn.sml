
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
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
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
    type range = key option * key option

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

    fun compareKeys (kk, kk') =
        case (kk, kk') of
            ([], []) => EQUAL
          | ([],  _) => LESS
          | (_,  []) => GREATER
          | (k::ks, k'::ks') => case M.keyCompare (k, k') of
                                    EQUAL => compareKeys (ks, ks')
                                  | other => other

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

    (* We use the name rpfx throughout this file to refer to a
       reversed prefix of a key built up using cons as we descend
       through the trie. It can be converted back into a key with
       (K.implode o rev). *)

    fun locate' (folder, order) =
        let fun boundaryItem (n, rpfx) =
                case n of
                    LEAF item => SOME (rpfx, item)
                  | TWIG (kk, item) => SOME (rev (K.explode kk) @ rpfx, item)
                  | BRANCH (SOME item, m) =>
                    (* In a canonical structure (which we are supposed
                       to be) a branch always has at least one
                       subnode, so we only have to consider the branch
                       item if we are looking for the first item in
                       the branch (in which case it's it) - otherwise
                       we go straight to the subnodes *)
                    if order = LESS
                    then boundaryItem (BRANCH (NONE, m), rpfx)
                    else SOME (rpfx, item)
                  | BRANCH (NONE, m) =>
                    folder (fn (_, _, SOME result) => SOME result
                             | (k, n', NONE) => boundaryItem (n', k::rpfx))
                           NONE m

            fun locate'' (n, xx, rpfx) =
                if K.isEmpty xx
                then if order = GREATER
                     then boundaryItem (n, rpfx)
                     else case n of
                              LEAF item => SOME (rpfx, item)
                            | BRANCH (SOME item, _) => SOME (rpfx, item)
                            | _ => NONE
                else
                    case n of
                        LEAF item =>
                        if order = LESS
                        then SOME (rpfx, item)
                        else NONE
                      | TWIG (kk, item) =>
                        if compareKeys (K.explode xx, K.explode kk) <> order
                        then boundaryItem (n, rpfx)
                        else NONE
                      | BRANCH (iopt, m) =>
                        let val (x, xs) = (K.head xx, K.tail xx)
                        in case folder (fn (_, _, SOME result) => SOME result
                                         | (k, n', NONE) =>
                                           case M.keyCompare (k, x) of
                                               EQUAL => locate'' (n', xs, k::rpfx)
                                             | other =>
                                               if other = order
                                               then boundaryItem (n', k::rpfx)
                                               else NONE)
                                       NONE
                                       m
                            of SOME result => SOME result
                             | NONE =>
                               if order = GREATER
                               then NONE
                               else Option.map (fn item => (rpfx, item)) iopt
                        end
        in
            locate''
        end

    fun locate (EMPTY, xx, ord) = NONE
      | locate (POPULATED n, xx, ord) =
        let val conv = Option.map (fn (kk, item) => (K.implode (rev kk), item))
        in
            case ord of
                LESS => conv (locate' (M.foldri, LESS) (n, xx, []))
              | EQUAL => findi (POPULATED n, xx)
              | GREATER => conv (locate' (M.foldli, GREATER) (n, xx, []))
        end
                               
    fun lookup (t, k) =
        case find (t, k) of
            NONE => raise Subscript
          | SOME v => v
                              
    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false

    fun searchNode f =
        let fun search' n =
                case n of
                    LEAF item => if f item
                                 then SOME item
                                 else NONE
                  | TWIG (kk, item) => if f item
                                       then SOME item
                                       else NONE
                  | BRANCH (iopt, m) =>
                    if Option.isSome iopt andalso f (Option.valOf iopt)
                    then iopt
                    else M.foldl (fn (n', SOME r) => SOME r
                                   | (n', NONE) => search' n')
                                 NONE
                                 m
        in
            search'
        end

    fun search (f : 'a -> bool) (t : 'a trie) : 'a option =
        case t of
            EMPTY => NONE
          | POPULATED n => searchNode f n
            
    fun searchiNode f =
        let fun searchi' (rpfx, n) =
                case n of
                    LEAF item =>
                    let val k = K.implode (rev rpfx)
                    in
                        if f (k, item)
                        then SOME (k, item)
                        else NONE
                    end
                  | TWIG (kk, item) =>
                    let val k = K.implode (rev rpfx @ K.explode kk)
                    in
                        if f (k, item)
                        then SOME (k, item)
                        else NONE
                    end
                  | BRANCH (iopt, m) =>
                    let val k = K.implode (rev rpfx)
                    in
                        if Option.isSome iopt andalso f (k, Option.valOf iopt)
                        then SOME (k, Option.valOf iopt)
                        else M.foldli (fn (x, n', SOME r) => SOME r
                                        | (x, n', NONE) =>
                                          searchi' (x::rpfx, n'))
                                      NONE
                                      m
                    end
        in
            searchi'
        end

    fun searchi (f : key * 'a -> bool) (t : 'a trie) : (key * 'a) option =
        case t of
            EMPTY => NONE
          | POPULATED n => searchiNode f ([], n)
            
    fun foldlNode f =
        let fun fold' (n, acc) =
                case n of
                    LEAF item => f (item, acc)
                  | TWIG (kk, item) => f (item, acc)
                  | BRANCH (iopt, m) =>
                    let val acc = case iopt of
                                      NONE => acc
                                    | SOME item => f (item, acc)
                    in M.foldl fold' acc m
                    end
        in
            fold'
        end

    fun foldrNode f =
        let fun fold' (n, acc) =
                case n of
                    LEAF item => f (item, acc)
                  | TWIG (kk, item) => f (item, acc)
                  | BRANCH (iopt, m) =>
                    let val acc = M.foldr fold' acc m
                    in case iopt of
                           NONE => acc
                         | SOME item => f (item, acc)
                    end
        in
            fold'
        end

    fun foldl (f : 'a * 'b -> 'b) (acc : 'b) (t : 'a trie) : 'b =
        case t of
            EMPTY => acc
          | POPULATED n => foldlNode f (n, acc)
                        
    fun foldr (f : 'a * 'b -> 'b) (acc : 'b) (t : 'a trie) : 'b =
        case t of
            EMPTY => acc
          | POPULATED n => foldrNode f (n, acc)

    fun foldliNode f =
        let fun f' (rpfx, item, acc) = f (K.implode (rev rpfx), item, acc)
            fun foldli' (rpfx, n, acc) =
                case n of
                    LEAF item =>
                    f' (rpfx, item, acc)
                  | TWIG (kk, item) =>
                    f' ((rev (K.explode kk)) @ rpfx, item, acc)
                  | BRANCH (iopt, m) =>
                    M.foldli (fn (x, n, acc) => foldli' (x::rpfx, n, acc))
                             (case iopt of
                                  NONE => acc
                                | SOME item => f' (rpfx, item, acc))
                             m
        in
            foldli'
        end
       
    fun foldriNode f =
        let fun f' (rpfx, item, acc) = f (K.implode (rev rpfx), item, acc)
            fun foldri' (rpfx, n, acc) =
                case n of
                    LEAF item =>
                    f' (rpfx, item, acc)
                  | TWIG (kk, item) =>
                    f' ((rev (K.explode kk)) @ rpfx, item, acc)
                  | BRANCH (iopt, m) =>
                    let val acc = M.foldri (fn (x, n, acc) =>
                                               foldri' (x::rpfx, n, acc)) acc m
                    in
                        case iopt of
                            NONE => acc
                          | SOME item => f' (rpfx, item, acc)
                    end
        in
            foldri'
        end
           
    fun foldli f acc t =
        case t of
            EMPTY => acc
          | POPULATED n => foldliNode f ([], n, acc)
            
    fun foldri f acc t =
        case t of
            EMPTY => acc
          | POPULATED n => foldriNode f ([], n, acc)

    fun enumerate trie =
        foldri (fn (k, v, acc) => (k, v) :: acc) [] trie
            
    fun foldiPrefixNode nodeFolder f =
        let fun foldi' (rpfx, xx, n, acc) =
                if K.isEmpty xx
                then nodeFolder f (rpfx, n, acc)
                else
                    case n of
                        LEAF item => acc
                      | TWIG (kk, item) =>
                        (if isPrefixOf (K.explode xx, K.explode kk)
                         then nodeFolder f (rpfx, n, acc)
                         else acc)
                      | BRANCH (_, m) => 
                        case M.find (m, K.head xx) of
                            NONE => acc
                          | SOME nsub =>
                            foldi' ((K.head xx) :: rpfx, (K.tail xx), nsub, acc)
        in
            foldi'
        end

    fun foldliPrefix f acc (trie, e) =
        case trie of
            EMPTY => acc
          | POPULATED n => foldiPrefixNode foldliNode f ([], e, n, acc)

    fun foldriPrefix f acc (trie, e) =
        case trie of
            EMPTY => acc
          | POPULATED n => foldiPrefixNode foldriNode f ([], e, n, acc)
            
    fun enumeratePrefix (trie, e) =
        foldriPrefix (fn (k, v, acc) => (k, v) :: acc) [] (trie, e)

    fun extractPrefixNode (xx, n) =
        if K.isEmpty xx
        then SOME n
        else
            case n of
                LEAF item => NONE
              | TWIG (kk, item) =>
                (if isPrefixOf (K.explode xx, K.explode kk)
                 then SOME n
                 else NONE)
              | BRANCH (_, m) =>
                case M.find (m, K.head xx) of
                    NONE => NONE
                  | SOME nsub =>
                    case extractPrefixNode (K.tail xx, nsub) of
                        NONE => NONE
                      | SOME nsub' => 
                        SOME (BRANCH (NONE, M.modify (M.new (), K.head xx,
                                                      fn _ => SOME nsub')))

    fun extractPrefix (trie, e) =
        case trie of
            EMPTY => EMPTY
          | POPULATED n =>
            case extractPrefixNode (e, n) of
                NONE => EMPTY
              | SOME n => POPULATED n

    fun foldiNodeRange right f (rpfx, n, leftConstraintK, rightConstraintK, acc) =
        let fun f' (pfx, item, acc) =
                f (K.implode pfx, item, acc)

            (* When foldiNodeRange is entered, leftConstraint and
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
                                  
            fun leaf (item, [], _) = f' (rev rpfx, item, acc)
              | leaf _ = acc

            fun acceptTwig (kk, lc, rc) =
                 (null lc orelse compareKeys (kk, lc) <> LESS)
                 andalso
                 (null rc orelse compareKeys (kk, rc) <> GREATER)
                             
            fun twig (kk, item, lc, rc) =
                let val kk' = K.explode kk
                in
                    if acceptTwig (kk', lc, rc)
                    then f' ((rev rpfx) @ kk', item, acc)
                    else acc
                end

            fun subConstraint (x, []) = NONE
              | subConstraint (x, c::cs) = if c = x
                                           then SOME (K.implode cs)
                                           else NONE

            fun acceptMapElement (x, lc, rc) =
                (null lc orelse M.keyCompare (x, hd lc) <> LESS)
                andalso
                (null rc orelse M.keyCompare (x, hd rc) <> GREATER)

            fun branchl (iopt, m, [], []) =
                foldliNode f (rpfx, n, acc)
              | branchl (iopt, m, lc, rc) =
                M.foldli
                    (fn (x, n, acc) =>
                        if not (acceptMapElement (x, lc, rc)) then acc
                        else foldiNodeRange false
                                            f (x :: rpfx, n,
                                               subConstraint (x, lc),
                                               subConstraint (x, rc),
                                               acc))
                    (case iopt of
                         NONE => acc
                       | SOME item => case lc of
                                          [] => f' (rev rpfx, item, acc)
                                        | _ => acc)
                    m
                                                    
            fun branchr (iopt, m, [], []) =
                foldriNode f (rpfx, n, acc)
              | branchr (iopt, m, lc, rc) =
                let val acc =
                        M.foldri
                            (fn (x, n, acc) =>
                                if not (acceptMapElement (x, lc, rc)) then acc
                                else foldiNodeRange true
                                                    f (x :: rpfx, n,
                                                       subConstraint (x, lc),
                                                       subConstraint (x, rc),
                                                       acc))
                            acc m
                in
                    case iopt of
                        NONE => acc
                      | SOME item => case lc of
                                         [] => f' (rev rpfx, item, acc)
                                       | _ => acc
                end

            val branch = if right then branchr else branchl
                    
            val leftConstraint = Option.map K.explode leftConstraintK
            val rightConstraint = Option.map K.explode rightConstraintK

            val lc = Option.getOpt (leftConstraint, [])
            val rc = Option.getOpt (rightConstraint, [])
        in
            case rightConstraint of
                SOME [] =>
                (* if we have a leaf or branch-with-item, we should
                   accept the item - since our ranges are inclusive -
                   but we shouldn't recurse or follow a twig *)
                (if null lc
                 then case n of
                          LEAF item => leaf (item, lc, NONE)
                        | TWIG _ => acc
                        | BRANCH (NONE, _) => acc
                        | BRANCH (SOME item, _) => f' (rev rpfx, item, acc)
                 else acc)
              | _ =>
                (case n of
                     LEAF item => leaf (item, lc, rc)
                   | TWIG (kk, item) => twig (kk, item, lc, rc)
                   | BRANCH (iopt, m) => branch (iopt, m, lc, rc))
        end

    fun foldliRange f acc (t, (leftConstraint, rightConstraint)) =
        case t of
            EMPTY => acc
          | POPULATED n =>
            foldiNodeRange false
                           f ([], n, leftConstraint, rightConstraint, acc)

    fun foldriRange f acc (t, (leftConstraint, rightConstraint)) =
        case t of
            EMPTY => acc
          | POPULATED n =>
            foldiNodeRange true
                           f ([], n, leftConstraint, rightConstraint, acc)

    fun enumerateRange (trie, range) =
        foldriRange (fn (k, v, acc) => (k, v) :: acc) [] (trie, range)

    fun foldiPattern' mapFolder f acc (node, p) =
        let fun f' (pfx, item, acc) = f (K.implode pfx, item, acc)
            fun fold' (rpfx, n, xx, acc) =
                case (n, xx) of
                    (LEAF item, []) => f' (rev rpfx, item, acc)
                  | (TWIG (kk, item), []) => acc
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
                    mapFolder (fn (x, n, acc) =>
                                 fold' (x :: rpfx, n, xs, acc))
                             acc m
                  | (BRANCH (_, m), (SOME x)::xs) =>
                    case M.find (m, x) of
                        NONE => acc
                      | SOME nsub => fold' (x :: rpfx, nsub, xs, acc)
        in
            fold' ([], node, p, acc)
        end

    fun foldliPattern f acc (trie, p) =
        case trie of
            EMPTY => acc
          | POPULATED node => foldiPattern' M.foldli f acc (node, p)

    fun foldriPattern f acc (trie, p) =
        case trie of
            EMPTY => acc
          | POPULATED node => foldiPattern' M.foldri f acc (node, p)
            
    fun enumeratePattern (trie, p) =
        foldriPattern (fn (k, v, acc) => (k, v) :: acc) [] (trie, p)
                                          
    fun prefixOf (trie, e) =
        let fun prefix' (n, xx, best, acc) =
                if K.isEmpty xx
                then case n of
                         LEAF item => SOME acc
                       | TWIG (kk, item) => best
                       | BRANCH (NONE, m) => best
                       | BRANCH (SOME item, m) => SOME acc
                else case n of
                         LEAF item => SOME acc
                       | TWIG (kk, item) =>
                         if K.equal (kk, xx) orelse
                            isPrefixOf (K.explode kk, K.explode xx)
                         then SOME (rev (K.explode kk) @ acc)
                         else best
                       | BRANCH (iopt, m) =>
                         let val (x, xs) = (K.head xx, K.tail xx)
                             val best = case iopt of NONE => best
                                                   | SOME _ => SOME acc
                         in
                             case M.find (m, x) of
                                 NONE => best
                               | SOME nsub => prefix' (nsub, xs, best, x::acc)
                         end
        in
            Option.map (K.implode o rev)
                (case trie of
                     EMPTY => NONE
                   | POPULATED node => prefix' (node, e, NONE, []))
        end

end

