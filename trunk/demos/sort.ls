import sh;

l = map(15, |x| return random(100) end);
println("Sort " + l + " with:");
println();

e = strlist();
s = searcher(__file__.filePath() + "*_sort.ls");
while not s.eof do
  e << s.fullName();
  println("    " + e.length + ". " + s.name.replace("_sort.ls", " sort"));
  s.next();
end
println();

= ">>> ";
x = ((readln().trim() or "1") as int) - 1;
println(x + 1);
println("Sort by " + e[x] + eol + load(e[x]).sort(l));




