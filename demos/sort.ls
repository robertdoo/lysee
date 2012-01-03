load('sh')

set l = {14 55 0 3 86 20 27 67 31 16 37 42}
println("Sort " + l + " with:")

set e = {}
set s = searcher(@file.filePath() + "*_sort.ls")
while not(s.eof()) do
    e << s.fullName()
    println("    " + length(e) + ". " + s.name().replace("_sort.ls" " sort"))
    s.next()
end

print(">>> ")
set x = ((readln().trim() or "1") as int) - 1
if x in length(e) then
    println("Sort by " + e[x])
    println("===> " + load(e[x]).sort(l))
end




