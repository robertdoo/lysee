{load 'math'}

{fun sort list ->
    {gap = {length list}}
    {while gap > 1 do
        {gap = {round gap / 2}}
        {while gap..{length list} - 1: i do
            {j = i}
            {while j > 0 do
                {if {list j} <= {list (j - gap)} then
                    {list.exchange j  (j - gap)}}
                {j -= gap}}}}
    list}

{if __in_main__ then
    {println {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}
