println("Input expression to evaluate or \"quit\" to leave!")

def ieval
    print("\nlysee> ")
    set input = readln().trim()
    if input == "quit" then
        return
    else
        println(eval(input))
        @loop
    end
end

if @catch ieval then
    println(@emsg)
end

println("bye-bye!")

