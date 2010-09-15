def generator_func |seed:int|  
  return |?|
    ++ my.seed;
    return my.seed;
  end; 
end
  
generator = generator_func(10);
for x in 10 do 
  = generator() + " ";
end
