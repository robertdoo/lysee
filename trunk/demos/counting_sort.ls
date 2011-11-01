{fun getv list comp ->
    {v = {get list 0}}
    {while 1..{length list} - 1: i do
        {if {comp {list i} v} then
            {v = {list i}}}}
    v}

{fun sort list ->
    {if {length list} > 1 then
        {counter = {varlist {getv list (>)} + 1}}
        {while list: v do
            {counter v 1}}
        {list.clear}
        {while {length counter}: v do
            { if {counter v} then
                (list << v)}}}
    list}

{if __in_main__ then
    {print {sort [14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42]}}}
