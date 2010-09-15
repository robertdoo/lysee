import sh;

= "List files of directory: ";
dir = readln().trim();

if not dir then dir = ${knpath}; 
else
  if not isdir(dir) then
    throw(@"""Directory "%(dir)" not exists!""");
  end
  dir = incPD(dir.fullFileName());
end

sr = searcher(dir + "*.*");
while not sr.eof do
  = sr.fullName(), eol;
  sr.next();
end
