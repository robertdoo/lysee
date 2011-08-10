{def generator_func seed ->
    {-> {$seed +=1}}}

{if __in_main__ then
    {generator = generator_func 10}
    {for x in 10 do
        {println generator()}}}
