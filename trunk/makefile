LYSEE_HOME    = /usr/local/lib/lysee
LYSEE_CONFILE = $(LYSEE_HOME)/lysee.config
LYSEE_MODULES = $(LYSEE_HOME)/modules
LYSEE_DEMOS   = $(LYSEE_HOME)/demos
LYSEE_SYNE    = /usr/lib/lazarus/0.9.30

OPTS = -MObjFPC -Sgi -CX -Cr -O2 -OoREGVAR -Xs -XX -vewnh -Fu.

all: lysee lysee_cgi lysee_pad lysee.so sqldb.so sh.so zipper.so hz.so \
     ym.so ymd.so md5.so strutils.so inifs.so syncobj.so math.so

lysee:

	fpc $(OPTS) -olysee lysee_fpc.lpr

lysee_cgi:

	fpc $(OPTS) -olysee_cgi lysee_cgi_fpc.lpr

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

sqldb.so:

	fpc $(OPTS) -omodules/sqldb.so modules/lysee_sqldb.lpr

sh.so:

	fpc $(OPTS) -omodules/sh.so modules/lysee_sh.lpr

ym.so:

	fpc $(OPTS) -omodules/ym.so modules/lysee_ym.lpr

ymd.so:

	fpc $(OPTS) -omodules/ymd.so modules/lysee_ymd.lpr

md5.so:

	fpc $(OPTS) -omodules/md5.so modules/lysee_md5.lpr

zipper.so:

	fpc $(OPTS) -omodules/zipper.so modules/lysee_zipper.lpr

hz.so:

	fpc $(OPTS) -omodules/hz.so modules/lysee_hz.lpr

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
	cp -f LICENSE $(LYSEE_HOME)
	cp -f LICENSE.txt $(LYSEE_HOME)
	cp -f lysee $(LYSEE_HOME)
	cp -f lysee_cgi $(LYSEE_HOME)
	cp -f lysee_pad $(LYSEE_HOME)
	cp -f lysee.so $(LYSEE_HOME)
	cp -f modules/sqldb.so $(LYSEE_MODULES)/sqldb.lo
	cp -f modules/sh.so $(LYSEE_MODULES)/sh.lo
	cp -f modules/zipper.so $(LYSEE_MODULES)/zipper.lo
	cp -f modules/hz.so $(LYSEE_MODULES)/hz.lo
	cp -f modules/ym.so $(LYSEE_MODULES)/ym.lo
	cp -f modules/ymd.so $(LYSEE_MODULES)/ymd.lo
	cp -f modules/md5.so $(LYSEE_MODULES)/md5.lo
	cp -f modules/strutils.so $(LYSEE_MODULES)/strutils.lo
	cp -f modules/inifs.so $(LYSEE_MODULES)/inifs.lo
	cp -f modules/syncobj.so $(LYSEE_MODULES)/syncobj.lo
	cp -f modules/math.so $(LYSEE_MODULES)/math.lo
	cp -f modules/*.ls $(LYSEE_MODULES)
	cp -f -r demos/* $(LYSEE_DEMOS)
	chmod a+r $(LYSEE_HOME)/LICENSE*
	ln -s -f ${LYSEE_HOME}/lysee /usr/bin/lysee
	ln -s -f ${LYSEE_HOME}/lysee_cgi /usr/bin/lysee_cgi
	ln -s -f ${LYSEE_HOME}/lysee_pad /usr/bin/lysee_pad

uninstall:

	rm /usr/bin/lysee
	rm /usr/bin/lysee_cgi
	rm /usr/bin/lysee_pad
	rm -f -r ${LYSEE_HOME}

clean:

	rm -f -r *.o *.ppu *.a *.so *~ *.compiled \
	         lysee lysee_cgi lysee_fpc lysee_pad lysee_pad_fpc\
	         temp backup __history *~ modules/*~ \
	         modules/*.o modules/*.ppu modules/*.a modules/*.so \
	         modules/backup modules/__history modules/*.compiled \
	         units/*.o units/*.ppu units/*.a units/backup units/__history

