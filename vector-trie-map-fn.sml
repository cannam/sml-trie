
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature VECTOR_TRIE_MAP_FN_ARG = sig
    eqtype element
    type key
    structure M : TRIE_NODE_MAP where type key = element
    structure V : MONO_VECTOR where type elem = element where type vector = key
end

functor VectorTrieMapFn (A : VECTOR_TRIE_MAP_FN_ARG)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = A.element where type key = A.key = struct

    structure M = A.M
    structure V = A.V
                      
    structure Key = struct
        type element = M.key
        type key = V.vector * int (* start index *)
        fun isEmpty (v, i) = i >= V.length v
        fun head (v, i) = V.sub (v, i)
        fun tail (v, i) = (v, i+1)
        fun explode k = if isEmpty k then [] else (head k) :: explode (tail k)
        fun implode ee = (V.fromList ee, 0)
        fun equal ((v1, i1), (v2, i2)) =
            let fun equal' (k1, k2) =
                    isEmpty k1 orelse
                    (head k1 = head k2 andalso equal' (tail k1, tail k2))
            in
                V.length v1 - i1 = V.length v2 - i2 andalso
                equal' ((v1, i1), (v2, i2))
            end
    end
                                                           
    structure T = PatternMatchTrieMapKeyAdapterFn
                      (struct
                        type element = Key.element
                        type key = Key.key
                        type external_key = A.key
                        structure T = 
                            TrieMapFn(struct
                                       structure M = M
                                       structure K = Key
                                       type element = K.element
                                       type key = K.key
                                       end)
                        fun enkey v = (v, 0)
                        fun dekey (v, 0) = v
                          | dekey (v, i) = V.tabulate
                                               (V.length v - i,
                                                fn j => V.sub (v, j - i))
                        end)

    open T
end
