def inc_generator(seed)
    {@ def set seed += 1 end}
end

if @in_main then
    set inc = inc_generator(10)
    yield each 10 def
        println(inc())
    end
end

