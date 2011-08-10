{def do_sort list L R ->
    {repeat
        {I = L}
        {J = R}
        {P = (L + R) >> 1}
        {repeat
            {while list[I] < list[P] do {I += 1}}
            {while list[J] > list[P] do {J -= 1}}
            {if I <= J then
                {list.exchange I, J}
                {if P == I then {P = J} else
                 if P == J then {P = I}}
                {I += 1}
                {J -= 1}}
         until I > J}
        {if L < J then {do_sort list, L, J}}
        {L = I}
     until I >= R}}

{def sort list ->
    {do_sort list, 0, (length list) - 1}
    list}

{if __in_main__ then
    {println sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4])}}

