
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature STRING_TRIE_MAP = sig
    include PATTERN_MATCH_TRIE_MAP
    where type key = string
    where type element = char
end

structure StringMTrieMap :> STRING_TRIE_MAP
    = VectorTrieMapFn
          (struct
            structure M = MTrieNodeMapFn(struct
			                  type t = char
			                  val compare = Char.compare
			                  end)
            structure V = CharVector
            type element = char
            type key = V.vector
            end)

structure StringATrieMap :> STRING_TRIE_MAP
    = VectorTrieMapFn
          (struct
            structure M = ATrieNodeMapFn(struct
			                  type t = char
                                          val ord = Char.ord
                                          val invOrd = Char.chr
			                  end)
            structure V = CharVector
            type element = char
            type key = V.vector
            end)

structure StringBTrieMap :> STRING_TRIE_MAP
    = VectorTrieMapFn
          (struct
            structure M = BTrieNodeMapFn(struct
				          type t = char
                                          val ord = Char.ord
                                          val invOrd = Char.chr
                                          val maxOrd = Char.maxOrd
                                          end)
            structure V = CharVector
            type element = char
            type key = V.vector
            end)

structure StringTrieMap = StringMTrieMap

