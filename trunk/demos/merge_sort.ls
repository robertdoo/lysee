load('math')

def merge(l r)
    set list = {}
    while (length(l) > 0) and (length(r) > 0) do
        if l[0] < r[0] then
            list << l[0]
            delete(l 0)
        else
            list << r[0]
            delete(r 0)
        end
    end
    list <<< l <<< r
end

def sort(list)
    set n = length(list)
    if n < 2 then return list end
    set m = round(n / 2)
    set l = sort(copy(list 0 m))
    set r = sort(copy(list m - n))
    clear(list)
    list <<< merge(l r)
end

@in_main and
println(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))

