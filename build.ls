path:string = __file__.filePath();

fpc_exe:string = """fpc.exe""";
fpc_opt:string = """-MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.""";
fpc_map:varlist = [
  {"lpr": """lysee_fpc.lpr""",
   "out": """lysee_fpc.exe""",
   "fup": ""
  },
  {"lpr": """lysee_kernel.lpr""",
   "out": """lysee_kernel.dll""",
   "fup": ""
  },
  {"lpr": """lysee_pad_fpc.lpr""",
   "out": """lysee_pad_fpc.exe""",
   "fup": """..\..\..\..\Developments\lazarus\components\synedit\units\i386-win32\
             ..\..\..\..\Developments\lazarus\lcl\units\i386-win32\
             ..\..\..\..\Developments\lazarus\lcl\units\i386-win32\win32\
             ..\..\..\..\Developments\lazarus\packager\units\i386-win32\"""
  },
  {"lpr": """modules\lysee_ib.lpr""",
   "out": """modules\lysee_ib.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_odbc.lpr""",
   "out": """modules\lysee_odbc.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_postgres.lpr""",
   "out": """modules\lysee_postgres.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_sh.lpr""",
   "out": """modules\lysee_sh.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_zipper.lpr""",
   "out": """modules\lysee_zipper.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_inifs.lpr""",
   "out": """modules\lysee_inifs.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_strutils.lpr""",
   "out": """modules\lysee_strutils.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_syncobj.lpr""",
   "out": """modules\lysee_syncobj.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_ymd.lpr""",
   "out": """modules\lysee_ymd.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_ym.lpr""",
   "out": """modules\lysee_ym.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_hz.lpr""",
   "out": """modules\lysee_hz.dll""",
   "fup": """modules"""
  },
  {"lpr": """modules\lysee_math.lpr""",
   "out": """modules\lysee_math.dll""",
   "fup": """modules"""
  }
];

cmds:strlist = strlist();

cmds.add("@echo off");
cmds.add(path.copy(0, 2));
cmds.add("cd " + path);
cmds.add("DEL /S /F /Q *.dcu *.exe *.dll *.o *.ppu *.tmp *.~* *.zip *.rar *.local *.identcache *.compiled *.bak");
cmds.add("");

// FPC

for hs:hashed in fpc_map do
  fup:string = hs.read("fup", "").trim(); 
  fup = "" if fup == ".";
  if fup then
    ll:strlist = strlist(fup.trim());
    fup = "";
    for pp:string in ll if pp.trim() do
      fup = fup + " -Fu" + pp.trim();
    end
  end
    
  if hs.lpr == "lysee_pad_fpc.lpr" then
    fup = "-WG -dSYN_LAZARUS -dLCL -dLCLwin32 " + fup;
  end
    
  src:string = hs.lpr;
  dst:string = hs.out;  

  cmds.add(@"echo ***** %(src) => %(dst)");
  cmds.add(@"del %(dst)");
  cmds.add(@"%(fpc_exe) %(fpc_opt) %(fup) -o%(dst) %(src)");
  cmds.add("");
end

// DELPHI

cmds.add("""dcc32.exe -B -M lysee.dpr""");
cmds.add("""dcc32.exe -B -M -CC lysee_exe.dpr""");
cmds.add("");

cmds.add("cd modules");
cmds.add("");

cmds.add("""dcc32.exe -B -M lysee_adodbv.dpr""");
cmds.add("");

cmds.add("cd ..");
cmds.add("");

cmds.add("@echo on");
cmds.saveToFile("build.bat");
