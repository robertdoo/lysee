def fib:int |n:int|
  return n if n < 2;
  return fib(n - 2) + fib(n - 1);
end

= fib(20);
