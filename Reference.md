## 1 Introduction ##

Lysee is a small, fast, reliable and cross platform script language. Its interpreter is a standalone program. Lysee kernel is used to be embedded into Pascal programs as a critical mission controler and script the host. Lysee script can be embedded into HTML to develop active WEB sites.

"LSE" is abbreviation of "Lysee Script Engine".

**Features**

  * Written in pure Pascal language.
  * Support module and functional programings.
  * Run in platforms where [Free Pascal](http://www.freepascal.org/), [Lazarus](http://www.lazarus.freepascal.org/) or Delphi exist.


---

## 2 Install and configure ##


---

### 2.1 Installation ###

Download the latest installer or source code:

| **Platform** | **How to** | **Default Position** |
|:-------------|:-----------|:---------------------|
| Windows      | Run the installer | c:\lysee             |
| Linux/Unix   | make; sudo make install | /usr/local/lib/lysee |


---

### 2.2 File Structure ###

| **Name** | **Description** |
|:---------|:----------------|
| modules  | stores lysee modules |
| LICENSE  | license file    |
| COPYING  | copyright file  |
| lysee.so lysee.dll | lysee kernel    |
| lysee lysee.exe lysee\_fpc.exe | interpreter     |
| lysee\_pad lysee\_pad.exe | syntax highlighter editor |
| lysee.config | configuration file |


---

### 2.3 Configuration ###

Add installation path to system search PATH:

| **Platform** | **How to** |
|:-------------|:-----------|
| Windows      | attch installation path to system environment variable PATH |

Edit file lysee.config to configure lysee:

| **Variable** | **Description** | **Default Value** |
|:-------------|:----------------|:------------------|
| lse\_tmpath  | temporary directory | ${LYSEE}/temp     |
| lse\_search  | module search path | ${LYSEE}/modules  |

The following constants can be used in lysee.config:

| **Constant** | **Description** | **Value** |
|:-------------|:----------------|:----------|
| ${confile}   | Configuration file name | Full file name of file lysee.config |
| ${kernel}    | Kernel file name | Full file name of kernel |
| ${kndir}     | Kernel directory  | Full file name of kernel directory |
| ${knpath}    | Kernel path     | Full file name of kernel path |
| ${home}      | Home path of user | Full file name of current user home path |
| ${program}   | current program | ParamStr(0) |
| ${tmpath}    | temporary path  | Set by lse\_tmpath in lysee.config |
| ${search}    | module search path | Set by lse\_search in lysee.config |
| ${keywords}  | keyword list    | main,int,string,for,while,... |
| ${mimefile}  | MIME file       | ${knpath}/mime.config |


---

### 2.4 Test ###

Open a console and type "lysee" and give a ENTER, you will see something like below.

```
lysee 3.0.3 - Copyright (c) 2003-2011 Li Yun Jie

   :: /C=CANCEL /F=FILE /Q=QUIT /R=RESTART ::

>>>
```


---

### 2.5 Hello World ###

Let's print "Hello World":

```
lysee 3.0.3 - Copyright (c) 2003-2011 Li Yun Jie

   :: /C=CANCEL /F=FILE /Q=QUIT /R=RESTART ::

>>> print("Hello World")
Hello World 
>>>
```

Where, ">>>" prompts you enter lysee command or script.


---

### 2.6 Execute Script File ###

Save "print("hello world")" to file hw.ls, and run it with lysee:

```
c:\lysee>lysee hw.ls
Hello World 
c:\lysee>
```

".ls" is file extension of Lysee script file.


---

### 2.7 Command Line Arguments ###

Run " lysee --help" to print Lysee usages:

```
c:\lysee>lysee --help
Lysee 3.0.3 - interactive LYSEE script interpreter

Usage: lysee [OPTION]... [FILE [ARGS]...]

Option:
  -v, --version           display the version of lysee and exit.
  -h, --help              print this help and exit.
  -s, --search=PATH       set module search path.
  -S, --script=SCRIPT     execute script.
  -w, --wait=SECONDS      wait for some seconds before going on.
  -t, --times=COUNT       execute specified times.
  -p, --pause             pause after execute script file.

File:
  .ls                     execute this file directly.
  .lsp, .html, .htm, .*   execute as a LSP file (Lysee Script Page).

Args:
  *                       arguments for file execution.

c:\lysee>
```

**Options:**

| **Option** | **Attachments** |	**Description** |
|:-----------|:----------------|:----------------|
| -v/--version | None            | Print version informations and quit |
| -h/--help  | None            | Print usages and quit |
| -s/--search | Path list seperated with ';' | Add search path |
| -w/--wait  | Seconds         | Wait for seconds before running |
| -t/--times | Times           | Repeat running times |
| -p/--pause | None            | Pause after execute a file |

**File:**

| **Extension** | **File Type** | **Description** |
|:--------------|:--------------|:----------------|
| .ls           | Lysee script file or  module | Pure lysee statement |
| .lsp          |  LSP（Lysee Script Page） | Active HTML web page |
| |	Deal as .lsp file | produce WEB page |

**Args:**

Can be accessed with string list @args.

**Command Lines:**

| **Command Line** | **Description** |
|:-----------------|:----------------|
| lysee backup.ls "skip=.tmp,....,.ppu" | backup lysee project |
| lysee -w 3600 -t 9999 spider.ls | run web spider once an hour |
| lysee logout.lsp user=mack | user mack logout|


---

## 3 Language Reference ##


---

### 3.1 Tokens ###

These are lysee keywords, identities, numbers and strings here.

**3.1.1 Charactors**

Lysee script is is made of these charactors:

| **Type** | **Range** | **Usage** |
|:---------|:----------|:----------|
| letter   | a..z A..Z | identities, keywords |
| digit    | 0..9      | numbers, identities |
| char     | + - **/ \ = < > [ ] ( ) : ^ @ { } $ # ...**| operators, delimiters, comments |
| char pair | // // == != >= <= | comments, operators |
| space    | SPACE, TAB, \n, \r, ... | delimiters |

**3.1.2 Comments**

Line comments and block comments:

| **Type** | **Begin Tag** | **End Tag** | **Sample** |
|:---------|:--------------|:------------|:-----------|
| Line Comment 1 | #             | #13, #10, EOF | # a shell comment |
| Line Comment 2 | //            | #13, #10, EOF | // a line comment |
| Block Comment | /| **/**| /**This is a block comment**/ |

**3.1.3 Identity**

Identity consists of alpha, '_' and digit charactors, and the first charactor must not be digit._

| **Status** | **Identity** | **Reason** |
|:-----------|:-------------|:-----------|
| Yes        | JAMES\_is\_007 |            |
| No         | 007\_is\_JAMES | Starts with digit |

Identity is case sensitive, abc、Abc、ABc and ABC are different.

**3.1.4 Keywords**

```
>>> print(@keywords)
@catch,@each,@filter,@folder,@loop,@map,@reduce,@throw,@yield,
and,as,break,continue,def,do,downto,elif,else,end,float,for,if,
in,int,is,like,main,object,or,repeat,return,set,string,sys,then,
to,until,variant,void,while
>>>
```

**3.1.5 Number**

Number has 2 types, int and float:

| **Type** | **Base 10** | **Base 16** |
|:---------|:------------|:------------|
| int      | 2008        | 0x07D8      |
| float    | 2008.4      | N/A         |

**3.1.6 String**

Lysee char is single byte ANSI char(0..255): 'a'..'z', '0'..'9', ...

Escaped charactors are listed below:

| **Char** | **Base 16 Escape** | **ASCII** | **Description** |
|:---------|:-------------------|:----------|:----------------|
| \0       | \x00               | 0         | end of string   |
| \a       | \x07               | 7 | BELL  |
| \b       | \x08               | 8         | BACKSPACE       |
| \e       | \x1B               | 27        | ESCAPE          |
| \f       | \x0C               | 12        | FORMFEED        |
| \n       | \x0A               | 10        | NEWLINE         |
| \r       | \x0D               | 13        | CARRIGE RETURN  |
| \t       | \x09               | 9         | TAB             |
| \v       | \x0B               | 11        | 	VERTICAL TAB   |
| \\       | \x5C               | 92        | '\'             |
| \'       | \x27               | 39        | single quote mark |
| \"       | \x22               | 34        | double quote mark |

string is sequence of char, ended with '\0'. string has 2 formats:

| **Format** | **Begin Tag** | **End Tag** | **Example** | **Escape** | **Description** |
|:-----------|:--------------|:------------|:------------|:-----------|:----------------|
| 1          | "             | "           | "c:\\lysee\\lysee.exe" | Yes        | Escaped with '\' |
| 2          | '             | '           | 'c:\lysee\lysee.exe' | No         | No escape       |

Lysee string can span over two or more lines.


---

### 3.2 Types ###

**3.2.1 Simple Types**

There are 3 buildin simple types in kernel:

| **Type** | **Description** |
|:---------|:----------------|
| string   | '\0' ended ANSI string |
| int      | integer(64bits) |
| float    | double(64bits)  |

**3.2.2 Other types**

| **Type** | **Description** |
|:---------|:----------------|
| void     | has only one value: @nil |
| variant  | cantains any value of any type |
| type     | type of type    |
| module   | kernel module or other loaded modules |
| function | function        |
| stream   | file stream, memory stream, socket stream, ... |
| varlist  | variant list    |
| hashed   | map, dictionary |
| vargen   | variant generator |
| time     | time            |

**3.2.3 Variant**

Variant can be any data type, depends on the time you check:

```
>>> set v = "string"
string
>>> set v = 18
18
>>> set v = {,} # empty varlist
{,}
>>> add(v "hello world") # add a line
0
>>> add(v 10)
1
>>> print(v)
{"hello world",10}
>>>
```

**3.2.4 Vargen**

vargen means variant generator, used to enum sub items of varlist, string, stream or ranges:

vargen for varlist:

```
>>> set g = {0 1 2 3 4} as vargen
>>> for x in g do print(x) end
01234
>>> for x in {0 1 2 3 4} do print(x) end
01234
>>>
```

vargen for string:

```
>>> set g = "hello" as vargen
>>> for x in g do print(x.upper()) end
HELLO
>>> for x in "hello" do print(x.upper()) end
HELLO
>>>
```

vargen for integer:

```
>>>set g = 5 as vargen
>>> for x in g do print(x) end
01234
>>> for x in 5 do print(x) end
01234
>>> for x in 0..4 do print(x) end
01234
>>> 
>>> for x = 0 to 4 do print(x) end
01234
>>> for x = 4 downto 0 do print(x) end
43210
>>> 
```

vargen for stream:

```
>>> set m = memory()
>>> m.write("hello")
5
>>> m.seek(0)
>>> for x in m do println('\x' + hex(x) + ": " + chr(x)) end
\x68: h
\x65: e
\x6C: l
\x6C: l
\x6F: o
>>>
```


---

### 3.3 Variables ###

Variable is a named value in stack or memory.

**3.3.1 Local Variables**

Define local variable by assign value to it. Default variable type is variant if not specified.
```
>>> set v1 = 10
10
>>> set v2 = 4
4
>>> set min_value = v1 > v2 ? v2 : v1
4
>>> min_value
4
>>>
```

**3.3.2 Formal Parameters**

Parameters of a function.


---

### 3.4 Constants ###

**3.4.1 Simple Constants**

Contant values of builtin simple types:

| **Type** | **Sample values** |
|:---------|:------------------|
| string   | "I like coding"   |
| int      |  -843, 0, 1, 2008 |
| float    | 3.1415926, 0.618  |

**3.4.2 @Constants**
```
>>> @?
@?       :  @XXXX list
@        :  current context
@@       :  global values
@nil     :  nil value
@eol     :  line break
@X       :  ths no.X param: X = 1..LSE_MAX_PARAMS
@prmc    :  actual param count
@prms    :  actual param list
@confile :  config file
@kernel  :  kernel file
@knpath  :  kernel file path
@kndir   :  kernel file directory
@home    :  home path of current user
@program :  program file
@search  :  search path
@tmpath  :  temp path
@keywords:  keyword list
@now     :  current time
@args    :  argument list
@envs    :  environment value list
@dir     :  current directory
@modules :  loaded modules
@libs    :  system level modules
@file    :  current file
@func    :  current function
@line    :  current line number
@main    :  main function
@maxint  :  max integer value
@minint  :  min integer value
@module  :  current module
@pd      :  path delimiter char
@pascal  :  pascal language informations
@inmain  :  check in main function
@errno   :  error number
@ename   :  error name
@emsg    :  error message
@erow    :  error row
@ecol    :  error col
@efile   :  error file
@emodule :  error module
@etext   :  formated error text
>>>
```


---

### 3.5 Functions ###

Lysee function includes normal and lambda functions.

**3.5.1 Normal function**

**Syntax: def** `FuncName(ParamList)` .... **end**

```
>>> def repeatStr(S times)
  >   S * times
  > end
>>> repeatStr("Aa" 4)
AaAaAaAa
>>>
```

**3.5.2 Lambda functions**

Lambda function is used as a simple value in expression.

| **Syntax** | **Description** |
|:-----------|:----------------|
| **def**`(ParamList)` ... **end** | unnamed function |
| **!EXPRESSION** | equal of: def`(_)` **EXPRESSION** end |

```
>>> @map 10 def(v) v + 1 end
{1 2 3 4 5 6 7 8 9 10}
>>> @map 10 !_ + 1
{1 2 3 4 5 6 7 8 9 10}
>>>
```

One ! means one parametre:

```
>>> reduce 0 "hello" !!_ + ord(__) // _:first param, __:second param
532
>>>
```


---

### 3.6 Expressions ###

Expression is any legal combination of symbols that represents a value:

| **Usages** | **Example** |
|:-----------|:------------|
| Assign Value | set magic = sys::random`()` % 100 |
| Condition  | if handle == 0 then @throw "Failed open file" end |
| Constants  | "OK" 1 3.14 |
| Enviroment | ${PATH}     |
| Call Function | sys::genid`()` |

Expression in Lysee is calculated from left to right.

**3.6.1 Operators**

Operators in Lysee has different priority levels. They are list below from hight to low:

| **Level** | **Operators** |
|:----------|:--------------|
| 1         | `*  /  %`     |
| 2         | +  -          |
| 3         | ^  &  `|`  <<  >> |
| 4         | is  as        |
| 5         | ==  !=  <  <=  >  >=  in like |
| 6         | and  or       |

**3.6.2 Concatenate strings**

Use operator + to concatenate strings:

```
>>> "hello" + "world"
helloworld
>>> "TIME: " + @now
TIME: 20120121121814046
>>>
```

**3.6.3 Times Concatenate**

Use operator `*` to cocatenate string by specified times. In this case left value must be a string and right value must be a integer, or else nil is returned:

```
>>> "Aa" * 4;
AaAaAaAa
>>> = "Cc" * 1.42;

>>>
```

**3.6.4 Shift Left Insert**

When calculate OBJECT << VALUE, Lysee will try to insert/add VALUE into/to OBJECT, and expression result is the object:

```
>>> set a = {,}
{,}
>>> a << 10
{10}
>>> a << 100
{10 100}
>>>
```

The OBJECT should be stream,varlist or user defined containers.

**3.6.5 varlist object**

**Syntax:** {V1 V2 ...} or {V1, V2, ...}

```
>>> set a = {1 2 3}
{1 2 3}
>>> set a = {} << 1 << 2 << 3;
>>>
{1 2 3}
>>> a = varlist() << 1 << 2 << 3;
{1 2 3}
>>> a;
{1 2 3}
>>>
```

{} or {,}  means empty varlist object.

**3.6.6 hashed object**

**Syntax:** {Key1:V1 Key2:V2 ...} or {Key1:V1, Key2:V2, ...}

```
>>> set a = {"name": "libudi"
  >          "sex" : "male"
  >          "age" : 40}
{"name":"libudi" "sex":"male" "age":40}
>>> for s in props(a) do
  >   println(s + " " + a[s])
  > end
name libudi
sex male
age 40
>>> set a = {:}
>>> a.name = "libudi"
>>> a.sex = "male"
>>> a.age = 40
>>> for s in props(a) do
  >   println(s + " " + a[s])
  > end
name libudi
sex male
age 40
>>>
```

{:} means empty hashed object.

**3.6.7 yield and @yield**

| **Syntax** | **Description** |
|:-----------|:----------------|
| **yield** FUNC PRM1 PRM2 ... | execute FUNC with current context |
| **yield do** (PRM!=V! ...) **end** | execute anonimous function with current context |

```
>>> set sum_range = 100
100
>>> def sum(i)
  >   set n = 0
  >   while i < sum_range do
  >     set n += i
  >     set i += 1
  >   end
  >   n
  > end
>>> sum(0)
[RuntimeError]: (module=main file=C:\lysee\lysee.exe row=3 col=13 errno=1300) oject sum_range not found
>>> yield sum 0
4950
>>> {@ sum 0}() # same as yield sum 0
4950
>>> yield do (i = 0)
  >   set n = 0
  >   while i < sum_range do
  >     set n += i
  >     set i += 1
  >   end
  >   n
  > end
4950
>>>
```

**@yield** works same as **yield**.

**3.6.8 map and @map**

| **Syntax:** | **Description** |
|:------------|:----------------|
| **map** VARGEN FUNC | map VARGEN items to varlist |
| **@map** VARGEN FUNC | process **map** with current context |

```
>>> map 10 !_ + 1
{1 2 3 4 5 6 7 8 9 10}
>>> map 10 !_ + 100
{100 101 102 103 104 105 106 107 108 109}
>>> map "hello" !_
{"h" "e" "l" "l" "o"}
>>> map "hello"
{"h" "e" "l" "l" "o"}
>>>
```

map works as the following function:

```
>>> def map(VARGEN FUNC)
  >     set list = {}
  >     for x in VARGEN do
  >         list << FUNC(x)
  >     end
  >     list
  > end
>>> map(10 !_ + 1)
{1 2 3 4 5 6 7 8 9 10}
>>>
```

**3.6.9 reduce and @reduce**

| **Syntax:** | **Description** |
|:------------|:----------------|
| **reduce** INIT\_VALUE VARGEN FUNC | reduce VARGEN items to a value |
| **@reduce** INIT\_VALUE VARGEN FUNC | process **reduce** with current context |

```
>>> reduce 1000 10 def(v x)
  >     println(x + " " + v)
  >     v + x
  > end
0 1000
1 1000
2 1001
3 1003
4 1006
5 1010
6 1015
7 1021
8 1028
9 1036
1045
>>>
```

@reduce works as the following function:

```
>>> def reduce(INIT_VALUE VARGEN FUNC)
  >     for x in VARGEN do
  >         set INIT_VALUE = FUNC(INIT_VALUE x)
  >     end
  >     INIT_VALUE
  > end

>>> reduce(1000 10 +)
1045
>>>
```

**3.6.10 folder and @folder**

| **Syntax:** | **Description** |
|:------------|:----------------|
| **folder** VARGEN FUNC | folder VARGEN items to a value from left |
| **@folder** VARGEN FUNC | process **folder** with current context |

```
>>> folder 10 def(v x)
  >     println(x + " " + v)
  >     x + v
  > end
1 0
2 1
3 3
4 6
5 10
6 15
7 21
8 28
9 36
45
>>>
```

@folder works as the following function:

```
>>> def folder(VARGEN FUNC) # folder left
  >     set v = VARGEN as vargen
  >     reduce v.generate() v FUNC
  > end

>>> folder(10 +)
45
>>>
```

**3.6.11 each and @each**

| **Syntax:** | **Description** |
|:------------|:----------------|
| **each** VARGEN FUNC | process each VARGEN item |
| **@each** VARGEN FUNC | process **each** with current context |

```
>>> each 10 println
0
1
2
3
4
5
6
7
8
9
>>>
```

@each works as the following function:

```
>>> def each(VARGEN FUNC)
  >     for x in VARGEN do
  >         FUNC(x)
  >     end
  > end
>>> each(10 println)
0
1
2
3
4
5
6
7
8
9
>>>
```

**3.6.12 filer and @filter**

| **Syntax:** | **Description** |
|:------------|:----------------|
| **filter** VARGEN FUNC | filter VARGEN items to a varlist |
| **@filter** VARGEN FUNC | **filter** with current context |

```
>>> filter 10 !_ % 3 == 0
{0 3 6 9}
>>>
```

@filter works as the following function:

```
>>> def filter(VARGEN FUNC)
  >     set list = {}
  >     for x in VARGEN do
  >         if FUNC(x) then
  >             list << x
  >         end
  >     end
  >     list
  > end

>>> filter(10 !_ % 3 == 0)
{0 3 6 9}
>>>
```

**3.6.13 throw, catch and @catch**

| **Syntax** | **Description** |
|:-----------|:----------------|
| **throw** ERROR\_MESSAGE ERROR\_ID | throw error     |
| **throw** ERROR\_MESSAGE | throw runtime error |
| **throw**  | throw again last error |
| **catch** FUNC PRM1  ... | catch exception in FUNC |
| **@catch** FUNC PRM1 ... | protect FUNC with current context |
| **catch do** (PRM1=V1 ...) end | catch exception in anonimous function |
| **@catch do** (PRM1=V1 ...) end | protect anonimous function with current context |

```
>>> def check(ok msg)
  >     ok or throw msg
  > end

>>> catch check 1 "something wrong"
0
>>> catch check 0 "something wrong"
1
>>> if catch check 0 "something wrong" then throw end
[mainError]: (module=main file=C:\lysee\lysee.exe row=2 col=11 errno=1300) check() - something wrong
>>>
```

**3.6.14 @continue**

| **Syntax** | **Description** |
|:-----------|:----------------|
| @continue PRM1 PRM2 ... | set parametres and jump to head of current function |

```
>>> def sumi(n r)
  >    if n < 100 then
  >        set r += n
  >        @continue n + 1  # increase n and jump to head of sumi
  >    end
  > end
>>> sumi(0 100000)
104950
>>>
```

**3.6.15 @break**

Used to break **each**, **map**, **reduce**, **folder** and **filter**.

```
>>> each 10 def (x)
  >     println(x)
  >     if x == 4 then @break end
  > end
0
1
2
3
4
>>>
```

**3.6.16 Call a list**

| **Syntax** | **Description** |
|:-----------|:----------------|
| {FUNC PRM1 PRM2 ...}() | execute FUNC    |
| {CONTEXT FUNC PRM1 PRM2 ...}() | execute FUNC with CONTEXT |

```
>>> def max3(v1 v2 v3)
  >     max(max(v1 v2) v3)
  > end
>>> max3(2 10 5)
10
>>> {max3 2}(10 5)
10
>>> set n = 100
100
>>> def max4(v1 v2 v3)
  >     max(max3(v1 v2 v3) n)
  > end
>>> {@ max4 2}(10 5)
100
>>> set n = 0
0
>>> {{"n":200} max4 2}(10 5)
200
>>>
```


---

### 3.7 Statements ###

Statement is the smallest standalone element of lysee script. Lysee program is formed by a sequence of one or more statements. Usually, Lysee statement ends with ';'.

**3.7.1 Assign value**

```
>>> set hs = {:}
{:}
>>> set hs.time = @now
20120122165259942
>>> set hs["name"] = nameof(hashed)
hashed
>>> hs
{"time":20120122165259942 "name":"hashed"}
>>> set list = {0 1 2 3}
{0 1 2 3}
>>> set list[1] = "lysee"
lysee
>>> list
{0 "lysee" 2 3}
>>>
```

**3.7.2 Expression**

```
>>> @now.encodeGMT()
Sun, 22 Jan 2012 16:56:54 GMT
>>> 10
10
>>>
```

**3.7.3 if clause**

**Syntax:** **if** CONDITION **then** ... `[`**elif** CONDITION **then** ... **else** ...`]` **end**

```
>>> for x in 10 do
  >     if x % 3 == 0 then
  >         println(x) 
  >     end
  > end
0
3
6
9
>>>
```

**3.7.4 while clause**

**Syntax:** **while** CONDITION **do** ... **end**

```
>>> set g = 10 as vargen
>>> while g.send("x") do
  >    if x == 6 then break else println(x) end
  > end
0
1
2
3
4
5
>>>
```

**3.7.5 repeat until**

**Syntax:** **repeat** ... **until** CONDITION

```
>>> set n = 0
>>> repeat
  >    set n += random(10)
  >    println(n)
  > until n > 20
4
4
8
8
12
12
14
15
16
22
>>>
```

**3.7.6 for clause**

**Syntax:**
  * **for** VARB **in** VARGEN `[`**if** CONDITION`]` **do** ... **end**
  * **for** VARB = LOW\_VALUE **to** HIGH\_VALUE `[`**if** CONDITION`]` **do** ... **end**
  * **for** VARB = HIGH\_VALUE **downto** LOW\_VALUE `[`**if** CONDITION`]` **do** ... **end**

```
>>> for x in 10 do print(x) end
0123456789
>>> for x in 0..9 do print(x) end
0123456789
>>> for x = 0 to 9 if x > 4 do print(x) end
56789
>>> for x = 9 downto 0 if x < 7 do print(x) end
6543210
>>>
```

**break** and **continue**  can be used in while, repeat and for clauses.