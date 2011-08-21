{fun sort list ->
    {n = {length list}}
    {if n > 1 then
        {for i in n do
            {v = {get list i}}
            {j = i - 1}
            {while j >= 0 do
                {break if {get list j} <= v}
                {set list j + 1 {get list j}}
                {j -= 1}}
            {set list j + 1 v}}}
  list}

{if __in_main__ then
    {print {sort [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]}}}

