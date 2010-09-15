def do_sort |list, L:int, R:int|
  repeat
    I:int = L;
    J:int = R;
    P:int = (L + R) >> 1;
    repeat
      while list[I] < list[P] do: ++I;
      while list[J] > list[P] do: --J;
      if I <= J then
        list.exchange(I, J);
        if P == I then P = J else
        if P == J then P = I end end
        ++I;
        --J;
      end;
    until I > J;
    do_sort(list, L, J) if L < J;
    L = I;
  until I >= R;
end;

def sort |list|
  do_sort(list, 0, list.length - 1);
  return list;
end

= sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]), eol;
= sort(__envs__);
