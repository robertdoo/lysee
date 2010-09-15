def sort |list|
  n = list.length;
  for i in 0..n - 2 do:
    for j in i + 1..n - 1 do:
      list.exchange(i, j) if list[i] > list[j];
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
