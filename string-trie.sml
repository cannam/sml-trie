
signature STRING_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = string
    where type element = char
end

structure StringMTrie :> STRING_TRIE = TrieFn(StringMTrieMap)
structure StringATrie :> STRING_TRIE = TrieFn(StringATrieMap)

structure StringTrie = StringATrie
