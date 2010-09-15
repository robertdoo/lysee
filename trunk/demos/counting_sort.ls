def sort |list|
  if list.length > 1 then
    min:int = list.min;
    max:int = list.max;
    counter = map(max - min + 1, || return 0 end);
    for v in list do:
      ++ counter[v as int];
    list.clear();
    for v in counter.length if counter[v] do: list << v;
  end
  return list;
end

= sort([14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42]), eol;
