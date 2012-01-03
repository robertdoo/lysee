def sort(list)
    set c = map (reduce list 0 max) + 1 0
    while list:x do
        set c[x] = 1
    end
    clear(list);
    while length(c):x if c[x] then
        list << x
    end
    list
end

@in_main and
print(sort({14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42}))
