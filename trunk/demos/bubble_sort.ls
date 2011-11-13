{def sort list:
    {n = {length list}}
    {0 => i:
        {i + 1 => j:
            {list.exchange i j if {list i} > {list j}}
            {loop j + 1 if j < n - 1}}
        {loop i + 1 if i < n - 2}}
    list}

{if __in_main__ then
    {print {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}

