{import sh}

{l = {map 10 ={random 100}}}
{println "Sort " + l + " with:"}
{e = []}
{s = {searcher {__file__.filePath} + "*_sort.ls"}}
{while not {s.eof} do
    (e << {s.fullName})
    {println "    " + {length e} + ". " + {{s.name}.replace "_sort.ls" " sort"}}
    {s.next}}
{print ">>> "}
{x = (({{readln}.trim} or "1") as int) - 1}
{if x in {length e} then
    {println "Sort by " + {get e x}}
    {print "=> " + {{get {load {get e x}} "sort"} l}}}




