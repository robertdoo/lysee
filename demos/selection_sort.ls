{fun pop_min list ->
    {v = {get list 0}}
    {x = 0}
    {n = {length list}}
    {if n > 1 then
        {for i in 1..n - 1 do
            {if {get list i} < v then
                {v = {get list i}}
                {x = i}}}}
    {list.delete x}
    v}

{fun sort list ->
    {n = {length list}}
    {if n > 1 then
        {b = {list.copy 0 n}}
        {list.clear}
        {for x in n do
            (list << {pop_min b})}}
    list}

{if __in_main__ then
    {println {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}

