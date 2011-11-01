{fun pop_min list ->
    {v = {list 0}}
    {x = 0}
    {n = {length list}}
    {if n > 1 then
        {while 1..n - 1: i do
            {if {list i} < v then
                {v = {list i}}
                {x = i}}}}
    {list.delete x}
    v}

{fun sort list ->
    {n = {length list}}
    {if n > 1 then
        {b = {list.copy 0 n}}
        {list.clear}
        {while n: x do
            (list << {pop_min b})}}
    list}

{if __in_main__ then
    {println {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}

