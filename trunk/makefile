LYSEE_HOME    = /usr/local/lib/lysee
LYSEE_CONFILE = $(LYSEE_HOME)/lysee.config
LYSEE_MODULES = $(LYSEE_HOME)/modules
LYSEE_DEMOS   = $(LYSEE_HOME)/demos
LYSEE_MANUAL  = $(LYSEE_HOME)/manual
LYSEE_SYNE    = /usr/lib/lazarus/0.9.28.2

OPTS = -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewnh -Fu.

all: lysee lysee_pad lysee.so ib.so odbc.so postgres.so sh.so zipper.so \
     strutils.so inifs.so syncobj.so math.so

lysee:

	fpc $(OPTS) -olysee lysee_fpc.lpr

lysee_pad:

	fpc $(OPTS) -WG -Fu${LYSEE_SYNE}/components/synedit/units/i386-linux/ \
	                -Fu${LYSEE_SYNE}/ideintf/units/i386-linux/ \
	                -Fu${LYSEE_SYNE}/lcl/units/i386-linux/ \
	                -Fu${LYSEE_SYNE}/lcl/units/i386-linux/gtk2/ \
	                -Fu${LYSEE_SYNE}/packager/units/i386-linux/ \
	                -dSYN_LAZARUS -dLCL -dLCLgtk2 \
	                -olysee_pad lysee_pad_fpc.lpr

lysee.so:

	fpc $(OPTS) -olysee.so lysee_kernel.lpr

ib.so:

	fpc $(OPTS) -omodules/ib.so modules/lysee_ib.lpr

odbc.so:

	fpc $(OPTS) -omodules/odbc.so modules/lysee_odbc.lpr

postgres.so:

	fpc $(OPTS) -omodules/postgres.so modules/lysee_postgres.lpr

sh.so:

	fpc $(OPTS) -omodules/sh.so modules/lysee_sh.lpr

zipper.so:

	fpc $(OPTS) -omodules/zipper.so modules/lysee_zipper.lpr

strutils.so:

	fpc $(OPTS) -Fumodules -omodules/strutils.so modules/lysee_strutils.lpr

inifs.so:

	fpc $(OPTS) -Fumodules -omodules/inifs.so modules/lysee_inifs.lpr

syncobj.so:

	fpc $(OPTS) -Fumodules -omodules/syncobj.so modules/lysee_syncobj.lpr

math.so:

	fpc $(OPTS) -Fumodules -omodules/math.so modules/lysee_math.lpr

install:

	mkdir -p $(LYSEE_MODULES)
	mkdir -p $(LYSEE_DEMOS)
	mkdir -p $(LYSEE_MANUAL)
	cp -f LICENSE $(LYSEE_HOME)
	cp -f LICENSE.txt $(LYSEE_HOME)
	cp -f lysee $(LYSEE_HOME)
	cp -f lysee_pad $(LYSEE_HOME)
	cp -f lysee.so $(LYSEE_HOME)
	cp -f modules/*.so $(LYSEE_MODULES)
	cp -f modules/*.ls $(LYSEE_MODULES)
	cp -f -r demos/* $(LYSEE_DEMOS)
	cp -f -r ../homepage/sc/manual/* $(LYSEE_MANUAL)
	ln -s -f ${LYSEE_HOME}/lysee /usr/bin/lysee
	ln -s -f ${LYSEE_HOME}/lysee_pad /usr/bin/lysee_pad

uninstall:

	rm /usr/bin/lysee
	rm /usr/bin/lysee_pad
	rm -f -r ${LYSEE_HOME}

clean:

	rm -f -r *.o *.ppu *.a *.so *~ *.compiled \
	         lysee lysee_fpc lysee_pad lysee_pad_fpc\
	         temp backup __history *~ modules/*~ \
	         modules/*.o modules/*.ppu modules/*.a modules/*.so \
	         modules/backup modules/__history modules/*.compiled \
	         units/*.o units/*.ppu units/*.a units/backup units/__history

