
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature STRING_TRIE_MAP = sig
    include PATTERN_MATCH_TRIE_MAP
    where type key = string
    where type element = char
end

signature STRING_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = string
    where type element = char
end

                                
(* A trie-map using strings as keys. Each level of the trie uses a
   red-black tree to hold pointers to its sub-nodes. This is
   probably the best trie to use for strings until you have a good
   reason to think otherwise. *)

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

structure StringMTrie :> STRING_TRIE = PatternMatchTrieFn(StringMTrieMap)


(* A trie-map using strings as keys. Each level of the trie uses a
   vector to hold pointers to its sub-nodes, indexed by integer value
   of the character at that level. This might be effective for deep
   tries of strings of ASCII characters with a small vocabulary of
   characters. *)
                                     
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

structure StringATrie :> STRING_TRIE = PatternMatchTrieFn(StringATrieMap)


(* A trie-map using strings as keys. Each level of the trie uses a
   bitmapped vector of characters to hold pointers to its sub-nodes. *)
                                                         
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

structure StringBTrie :> STRING_TRIE = PatternMatchTrieFn(StringBTrieMap)

                                                         
structure StringTrieMap = StringMTrieMap
structure StringTrie = StringMTrie
