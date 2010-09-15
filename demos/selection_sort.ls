def pop_min |list|
  v = list[0];
  x = 0;
  n = list.length;
  if n > 1 then:
    for i in 1..n - 1 do:
      if list[i] < v then
        v = list[i];
        x = i;
      end
  list.delete(x);
  return v;
end

def sort |list|
  n = list.length;
  if n > 1 then
    b = list.copy(0, n);
    list.clear();
    for n do: list << pop_min(b);
  end
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
