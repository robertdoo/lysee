def pop_min(list)
    set v = list[0]
    set x = 0
    set n = length(list)
    if n > 1 then
        while 1..n - 1:i if list[i] < v then
            set v = list[i]
            set x = i
        end
    end
    delete(list x)
    v
end

def sort(list)
    set n = length(list)
    set b = copy(list)
    clear(list)
    while n:x do list << pop_min(b) end
    list
end

@in_main and
println(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))

