def fib(n)
    n < 2 ? n : fib(n - 2) + fib(n - 1)
end

@in_main and println(fib(20))
