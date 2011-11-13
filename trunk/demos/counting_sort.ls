{def getv list comp:
    {l = {length list}}
    {v = {get list 0}}
    {1 => i:
        {v = {list i} if {comp {list i} v}}
        {loop i + 1 if i < l - 1}}
    v}

{def sort list:
    {if {length list} > 1 then
        {counter = {varlist {getv list (>)} + 1}}
        {while list: v do
            {counter v 1}}
        {list.clear}
        {while {length counter}: v do
            { if {counter v} then
                (list << v)}}}
    list}

{if __in_main__
    {print {sort [14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42]}}}
