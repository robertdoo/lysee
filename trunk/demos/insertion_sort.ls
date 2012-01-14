def sort(list)
    set n = length(list)
    if n > 1 then
        for i in n do
            set v = list[i]
            for j in i - 1..-1 do
                (j < 0) or (list[j] <= v) and break;
                set list[j + 1] = list[j]
            end
            set list[j + 1] = v
        end
    end
    list
end

@in_main and
print(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))
