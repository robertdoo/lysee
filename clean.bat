@echo off

DEL /S /F /Q *.dcu *.exe *.dll *.o *.ppu *.tmp *.~* *~ *.zip *.rar *.local *.identcache *.compiled *.bak *.a *.db *.lo

rmdir __history 
rmdir temp
rmdir backup
rmdir modules\backup
rmdir modules\__history
rmdir lib\i386-win32
rmdir lib
