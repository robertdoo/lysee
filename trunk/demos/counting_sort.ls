{fun getv list comp ->
    {v = {get list 0}}
    {for i in 1..{length list} - 1 do
        {if {comp {get list i} v} then
            {v = {get list i}}}}
    v}

{fun sort list ->
    {if {length list} > 1 then
        {counter = {varlist {getv list (>)} + 1}}
        {for v in list do
            {set counter v  1}}
        {list.clear}
        {for v in {length counter} if {get counter v} do
            (list << v)}}
    list}

{if __in_main__ then
    {print {sort [14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42]}}}
