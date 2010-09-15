import math;

def heapify |list, index, size|
  l = (2 * index) + 1;
  r = l + 1;
  x = (l < size) and (list[l] > list[index]) ? l: index;
  x = r if (r < size) and (list[r] > list[x]);
  if x != index then
    list.exchange(index, x);
    heapify(list, x, size)
  end
end

def build_heap |list|
  i = round(list.length / 2) - 1;
  while i >= 0 do
    heapify(list, i, list.length);
    -- i;
  end
end

def sort |list|
  n = list.length;
  if n > 1 then
    build_heap(list);
    while n > 0 do
      list.exchange(0, n - 1);
      -- n;
      heapify(list, 0, n)
    end
  end
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
