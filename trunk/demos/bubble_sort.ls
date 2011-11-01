{fun sort list ->
    {n = {length list}}
    {while 0..n - 2: i do
        {while i + 1..n - 1: j do
            {if {list i} > {list j} then
                {list.exchange i j}}}}
    list}

{if __in_main__ then
    {print {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}

