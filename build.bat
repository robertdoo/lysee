@echo off
D:
cd D:\budi\lysee\lysee\
DEL /S /F /Q *.dcu *.exe *.dll *.o *.ppu *.tmp *.~* *.zip *.rar *.local *.identcache *.compiled *.bak
echo ***** lysee_fpc.lpr => lysee_fpc.exe
del lysee_fpc.exe
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fu. -olysee_fpc.exe lysee_fpc.lpr
echo ***** lysee_kernel.lpr => lysee_kernel.dll
del lysee_kernel.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fu. -olysee_kernel.dll lysee_kernel.lpr
echo ***** lysee_pad_fpc.lpr => lysee_pad_fpc.exe
del lysee_pad_fpc.exe
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu. -WG -dSYN_LAZARUS -dLCL -dLCLwin32  -Fu..\..\..\..\Developments\lazarus\components\synedit\units\i386-win32\ -Fu..\..\..\..\Developments\lazarus\lcl\units\i386-win32\ -Fu..\..\..\..\Developments\lazarus\lcl\units\i386-win32\win32\ -Fu..\..\..\..\Developments\lazarus\packager\units\i386-win32\ -olysee_pad_fpc.exe lysee_pad_fpc.lpr
echo ***** modules\lysee_odbc.lpr => modules\lysee_odbc.dll
del modules\lysee_odbc.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_odbc.dll modules\lysee_odbc.lpr
echo ***** modules\lysee_sh.lpr => modules\lysee_sh.dll
del modules\lysee_sh.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_sh.dll modules\lysee_sh.lpr
echo ***** modules\lysee_zipper.lpr => modules\lysee_zipper.dll
del modules\lysee_zipper.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_zipper.dll modules\lysee_zipper.lpr
echo ***** modules\lysee_inifs.lpr => modules\lysee_inifs.dll
del modules\lysee_inifs.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_inifs.dll modules\lysee_inifs.lpr
echo ***** modules\lysee_strutils.lpr => modules\lysee_strutils.dll
del modules\lysee_strutils.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_strutils.dll modules\lysee_strutils.lpr
echo ***** modules\lysee_syncobj.lpr => modules\lysee_syncobj.dll
del modules\lysee_syncobj.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_syncobj.dll modules\lysee_syncobj.lpr
echo ***** modules\lysee_ymd.lpr => modules\lysee_ymd.dll
del modules\lysee_ymd.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_ymd.dll modules\lysee_ymd.lpr
echo ***** modules\lysee_ym.lpr => modules\lysee_ym.dll
del modules\lysee_ym.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_ym.dll modules\lysee_ym.lpr
echo ***** modules\lysee_hz.lpr => modules\lysee_hz.dll
del modules\lysee_hz.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_hz.dll modules\lysee_hz.lpr
echo ***** modules\lysee_math.lpr => modules\lysee_math.dll
del modules\lysee_math.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_math.dll modules\lysee_math.lpr
echo ***** modules\lysee_md5.lpr => modules\lysee_md5.dll
del modules\lysee_md5.dll
fpc.exe -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewn -Fu.  -Fumodules -omodules\lysee_md5.dll modules\lysee_md5.lpr
dcc32.exe -B -M lysee.dpr
dcc32.exe -B -M -CC lysee_exe.dpr
cd ..
@echo on
