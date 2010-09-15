def sort |list|
  n = list.length;
  if n > 1 then:
    for i, v in n, list do
      j = i - 1;
      while j >= 0 do
        break if list[j] <= v;
        list[j + 1] = list[j];
        j -= 1;
      end
      list[j + 1] = v;
    end
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
