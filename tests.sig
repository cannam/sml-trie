
signature TESTS = sig

    type test = string * (unit -> bool)

    val name : string
    val tests : unit -> test list

end
                      
