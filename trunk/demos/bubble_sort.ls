def sort(list)
    set n = length(list)
    while n - 1:i do
      while i + 1..n - 1:j if list[i] > list[j] then
        exchange(list i j)
      end
    end
    list
end

@in_main and
print(sort({2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4}))

