def move(From To)
    println(format("move %(From) to %(To)"))
end

def hanoi(n From middle To)
    if n == 1 then
        move(From To)
    else
        hanoi(n - 1 From To middle)
        move(From To)
        hanoi(n - 1 middle From To)
    end
end

hanoi(5 "A" "B" "C")
