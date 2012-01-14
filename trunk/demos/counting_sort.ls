def sort(list)
    set c = @map (@folder list max) + 1 !0
    for x in list do
        set c[x] = 1
    end
    clear(list);
    for x in length(c) if c[x] do
        list << x
    end
    list
end

@in_main and
print(sort({14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42}))
