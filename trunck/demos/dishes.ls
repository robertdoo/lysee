const TA:int = 1;
const TB:int = 2;
const TC:int = 3;

def name:string |index:int|
  return "A" if (index == TA);
  return "B" if (index == TB);
  return "C";
end

def rest:int |src:int, dst:int|
  return 6 - src - dst; # TA+TB+TC
end

def move |count:int, src:int, dst:int|
  if count == 1 then
    = "move dish", count, "from", name(src), "to", name(dst), eol;
    return;
  end  
  mid = rest(src, dst);
  cnt = count - 1;
  move(cnt, src, mid);
  = "move dish", count, "from", name(src), "to", name(dst), eol;  
  move(cnt, mid, dst);
end

# move 5 dishes from place A to place C, requires:
# 1. one time one dish
# 2. small dishes on big dishes
# 3. three places: A, B, C
 
move(5, TA, TC);
