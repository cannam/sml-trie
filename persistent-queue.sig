
signature PERSISTENT_QUEUE = sig

    include PERSISTENT_ARRAY

    type 'a queue = 'a array

    val prepend : 'a array * 'a -> 'a array
    val popStart : 'a array -> 'a array * 'a
                                            
end
