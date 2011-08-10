{def getv list comp ->
    {v = list[0]}
    {for i in 1..(length list) - 1 do
        {if comp list[i], v then
            {v = list[i]}}}
    v}

{def sort list ->
    {if (length list) > 1 then
        {counter = varlist((getv list, >) + 1)}
        {for v in list do
            {counter[v] = 1}}
        {list.clear()}
        {for v in length counter if counter[v] do
            {list << v}}}
    list}

{if __in_main__ then
    {print sort([14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42])}}

