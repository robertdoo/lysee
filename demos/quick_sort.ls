def do_sort(list L R)
    set I = L
    set J = R
    set P = (L + R) >> 1
    repeat
        while list[I] < list[P] do set I += 1 end
        while list[J] > list[P] do set J -= 1 end
        if I <= J then
            exchange(list I J)
            if P == I then
                set P = J
            elif P == J then
                set P = I
            end
            set I += 1
            set J -= 1
        end
    until I > J;
    if L < J then do_sort(list L J) end
    set L = I
    I < R and @loop
end

def sort(list)
    set n = length(list)
    if n > 1 then
        do_sort(list 0 n - 1)
    end
    list
end

@in_main and
println(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))

