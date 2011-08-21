{import sh}
{print "List files of directory: "}
{dir = {{readln}.trim} or "."}
{dir = {incPD {dir.fullFileName}}}
{sr = {searcher dir + "*.*"}}
{while not {sr.eof} do
    {println {sr.fullName}}
    {sr.next}}

