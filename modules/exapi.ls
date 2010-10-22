class TExportAPI ||

  def writeln |text:string|
    my.stream.writeln(text);
  end

  def getDesc |desc:string|
    return desc.trim() or "&nbsp;";
  end

  def getProt |func:function|
    return func.prototype.copy(4, 1024);
  end

  def getTotal ||
    return format("TOTAL: %d modules | %d functions | %d classes | %d methods",
      [my.moduleCount, my.funcCount, my.classCount, my.methodCount])
  end

  def beginPage ||
    my.writeln("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"""");
    my.writeln(""""http://www.w3.org/TR/html4/loose.dtd">""");
    my.writeln("""<html>""");
    my.writeln("""<head>""");
    my.writeln("""<meta http-equiv="Content-Type" content="text/html; charset=utf-8">""");
    my.writeln("""<title>Class Reference Of Lysee """ + sys.version + """</title>""");
    my.writeln("""<style type="text/css">""");
    my.writeln("""<!--""");
    my.writeln(""".white {""");
    my.writeln("""	color: #FFFFFF;""");
    my.writeln("""	font-family: "Courier New", Courier, mono;""");
    my.writeln("""	font-size: 10pt;""");
    my.writeln("""}""");
    my.writeln(""".black {""");
    my.writeln("""	color: #000000F;""");
    my.writeln("""	font-family: "Courier New", Courier, mono;""");
    my.writeln("""	font-size: 10pt;""");
    my.writeln("""}""");
    my.writeln("""-->""");
    my.writeln("""</style>""");
    my.writeln("""</head>""");
    my.writeln("""<body>""");
    my.writeln("""<h2 align="center">Class Reference Of Lysee """ + sys.version + """</h2>""");
    my.writeln("""<table border="0" align="center" cellpadding="4" cellspacing="1" bgcolor="#CCCCCC">""");
    my.writeln("""<tr bgcolor="#999999" class="white">""");
    my.writeln("""<th>ITEM</th>""");
    my.writeln("""<th>NAME</th>""");
    my.writeln("""<th>DESCRIPTION</th>""");
    my.writeln("""</tr>""");
    my.writeln("""<!--BEGIN API LIST-->""");
  end

  def writeClass |clss:type|
    my.writeln("""<tr>""");
    my.writeln("""<td align="center" bgcolor="#FFFFFF" class="black" nowrap>class</td>""");
    my.writeln("""<td bgcolor="#0000FF" class="white">""" + clss.name + """</td>""");
    my.writeln("""<td bgcolor="#0000FF" class="white">""" +  my.getDesc(clss.description) + """</td>""");
    my.writeln("""</tr>""");

    list = clss.methods;
    my.methodCount += list.length;

    for func in list if func.name == clss.name do
      my.writeln("""<tr class="black">""");
      my.writeln("""<td bgcolor="#FFFFFF"><div align="center" nowrap>constructor</div></td>""");
      my.writeln("""<td bgcolor="#97FFC0">&nbsp;&nbsp;""" + my.getProt(func) + """</td>""");
      my.writeln("""<td bgcolor="#97FFC0">""" + my.getDesc(func.description) + """</td>""");
      my.writeln("""</tr>""");
      break;
    end

    for func in list if func.name != clss.name do
      my.writeln("""<tr class="black">""");
      my.writeln("""<td bgcolor="#FFFFFF"><div align="center" nowrap>method</div></td>""");
      my.writeln("""<td bgcolor="#C6C6FF">&nbsp;&nbsp;""" + my.getProt(func) + """</td>""");
      my.writeln("""<td bgcolor="#C6C6FF">""" + my.getDesc(func.description) + """</td>""");
      my.writeln("""</tr>""");
    end
  end

  def writeModule |modu:module|
    my.writeln("""<tr bgcolor="#FF0000" class="white">""");
    my.writeln("""<td nowrap>module</td>""");
    my.writeln("""<td>""" + modu.name + """</td>""");
    my.writeln("""<td>""" + my.getDesc(modu.description) + """</td>""");
    my.writeln("""</tr>""");

    list = modu.funcs;
    my.funcCount += list.length;
    for func in list do
      my.writeln("""<tr class="black">""");
      my.writeln("""<td align="center" bgcolor="#FFFFFF" nowrap>function</td>""");
      my.writeln("""<td bgcolor="#FFC6C6">&nbsp;&nbsp;""" + my.getProt(func) + """</td>""");
      my.writeln("""<td bgcolor="#FFC6C6">""" + my.getDesc(func.description) + """</td>""");
      my.writeln("""</tr>""");
    end

    list = modu.classes;
    my.classCount += list.length - 1;
    for clss in list if clss.name != modu.name do
      my.writeClass(clss);
    end

    ++ my.moduleCount;
  end

  def endPage ||
    my.writeln("""<tr>""");
    my.writeln("""<td colspan=3 bgcolor="#000000" class="white" align="right">""");
    my.writeln(my.getTotal());
    my.writeln("""</td>""");
    my.writeln("""</tr>""");
    my.writeln("""<!--END API LIST-->""");
    my.writeln("""</table>""");
    my.writeln("""</body>""");
    my.writeln("""</html>""");
  end

  def exportToFile |FileName:string|
    my.moduleCount = 0;
    my.funcCount = 0;
    my.classCount = 0;
    my.methodCount = 0;
    my.stream = openfs(FileName, "c");
    my.beginPage();
    each(libs(), my.writeModule);
    my.endPage();
    my.stream = nil;
  end
end

def exportAPI |FileName:string|
  TExportAPI().exportToFile(FileName);
end

