def generator_func |seed:int|  
  return |this|
    ++ this.seed; 
    return this.seed; 
  end; 
end
  
generator = generator_func(10);
for x in 10 do 
  = generator() + " ";
end