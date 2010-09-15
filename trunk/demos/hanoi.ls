def move |from, to|
  println(@"move %(from) to %(to)");
end

def hanoi |n:int, from, middle, to|
  if n == 1 then
    move(from, to)
  else
    hanoi(n - 1, from, to, middle);
    move(from, to);
    hanoi(n - 1, middle, from, to)
  end
end

hanoi(5, "A", "B", "C");
