load('math')

def sort(list)
    set gap = length(list)
    while gap > 1 do
        set gap = round(gap / 2)
        while gap..length(list) - 1:i do
            set j = i
            while j > 0 do
                if list[j] <= list[j - gap] then
                    exchange(list j  j - gap)
                end
                set j -= gap
            end
        end
    end
    list
end

@in_main and
println(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))

