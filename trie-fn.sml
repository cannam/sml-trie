
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

functor TrieFn (M : TRIE_MAP)
	:> TRIE
	       where type entry = M.key where type trie = unit M.trie = struct

    open M

    type entry = M.key

    type trie = unit M.trie
    type t = trie

    type range = entry option * entry option

    fun keysOf kvl =
        map (fn (k, v) => k) kvl
            
    fun add (t, e) =
        M.insert (t, e, ())

    fun foldl f acc t =
        M.foldli (fn (k, v, acc) => f (k, acc)) acc t
                 
    fun enumerate t =
        keysOf (M.enumerate t)

    fun foldlPrefix f acc (t, e) =
        M.foldliPrefix (fn (k, v, acc) => f (k, acc)) acc (t, e)

    fun enumeratePrefix (t, e) =
        keysOf (M.enumeratePrefix (t, e))

    fun foldlRange f acc (t, range) =
        M.foldliRange (fn (k, v, acc) => f (k, acc)) acc (t, range)

    fun enumerateRange (t, range) =
        keysOf (M.enumerateRange (t, range))
                      
end
