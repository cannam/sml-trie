
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

functor ListTrieMapFn (M : TRIE_NODE_MAP)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = M.key where type key = M.key list = struct

    structure Key = struct
        type element = M.key
        type key = M.key list
        fun isEmpty [] = true | isEmpty _ = false
        val head = List.hd
        val tail = List.tl
        fun explode x = x
        fun implode x = x
        fun equal (x, y) = x = y
    end
                      
    structure T = TrieMapFn(struct
                              structure M = M
                              structure K = Key
                              type element = K.element
                              type key = K.key
                            end)

    open T
end
