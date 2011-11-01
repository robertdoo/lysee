{fun generator_func seed ->
    {->{$seed ++}}}

{if __in_main__ then
    {generator = {generator_func 10}}
    {while 10: x do
        {println {generator}}}}
