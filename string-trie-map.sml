
structure StringMTrieMap = StringTrieMapFn
                               (ListMTrieMapFn(struct
				                type t = char
				                val compare = Char.compare
				                end))

structure StringATrieMap = StringTrieMapFn
                               (ListATrieMapFn(struct
				                type t = char
                                                val ord = Char.ord
                                                val invOrd = Char.chr
                                                val maxOrd = Char.maxOrd
                                                end))

structure StringBTrieMap = StringTrieMapFn
                               (ListBTrieMapFn(struct
				                type t = char
                                                val ord = Char.ord
                                                val invOrd = Char.chr
                                                val maxOrd = Char.maxOrd
                                                end))

