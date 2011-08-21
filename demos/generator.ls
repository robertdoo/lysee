{fun generator_func seed ->
    (fun {$seed ++})}

{if __in_main__ then
    {generator = {generator_func 10}}
    {for x in 10 do
        {println {generator}}}}
