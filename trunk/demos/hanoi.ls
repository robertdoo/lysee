{fun move From To ->
    {println @"move %(From) to %(To)"}}

{fun hanoi n From middle To ->
    {if n == 1 then
        {move From To}
        else
            {hanoi n - 1 From To middle}
            {move From To}
            {hanoi n - 1 middle From To}}}

{hanoi 5 "A" "B" "C"}
