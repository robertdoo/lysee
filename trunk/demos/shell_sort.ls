def sort |list|
  gap:int = list.length;
  while gap > 1 do
    gap /= 2;
    for i in gap..list.length - 1 do
      j = i;
      while j > 0 do
        list.exchange(j, j - gap) if list[j] <= list[j-gap];
        j -= gap;
      end
    end
  end
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
