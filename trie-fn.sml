
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

functor TrieFn (M : TRIE_MAP)
	:> TRIE
	       where type entry = M.key where type trie = unit M.trie = struct

    open M

    type entry = M.key

    type trie = unit M.trie
    type t = trie

    fun keysOf kvl =
        map (fn (k, v) => k) kvl
            
    fun add (t, e) =
        M.insert (t, e, ())

    fun foldl f acc t =
        M.foldli (fn (k, v, acc) => f (k, acc)) acc t
                 
    fun enumerate t =
        keysOf (M.enumerate t)

    fun prefixMatch (t, e) =
        keysOf (M.prefixMatch (t, e))

    fun foldlPrefixMatch f acc (t, e) =
        M.foldliPrefixMatch (fn (k, v, acc) => f (k, acc)) acc (t, e)

    (*!!! *)
    fun foldlRange f acc (t, leftConstraint, rightConstraint) =
        M.foldliRange (fn (k, v, acc) => f (k, acc)) acc
                      (t, leftConstraint, rightConstraint)
                      
end
