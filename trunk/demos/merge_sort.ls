import math;

def merge |l, r|
  list = l.copy(0, 0);
  while l.length > 0 and r.length > 0 do:
    if l[0] < r[0] then
      list << l[0];
      l.delete(0);
    else
      list << r[0];
      r.delete(0);
    end
  for v in l do: list << v;
  for v in r do: list << v;
  return list
end

def sort |list|
  n = list.length;
  return list if n < 2;
  m = round(n / 2);
  l = sort(list.copy(0, m));
  r = sort(list.copy(m, n - m));
  list.clear();
  return list +< merge(l, r);
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
