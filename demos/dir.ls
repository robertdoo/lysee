load('sh')
print("List files of directory: ")
set dir = incPD((readln().trim() or ".").fullFileName())
set sr = searcher(dir + "*.*")
while not(sr.eof()) do
    println(sr.fullName())
    sr.next()
end

