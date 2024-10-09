#CFLAG = -g  -m32 -std=gnu99 -Wall -Wno-unused -Wno-misleading-indentation
#CFLAG = -m32 -O4 -march=native -fomit-frame-pointer -fno-caller-saves -std=gnu99
#CFLAG = -m32 -O2  -march=native -fomit-frame-pointer -std=gnu99
#CFLAG = -O4 -m32 -march=native -fomit-frame-pointer -std=gnu99
#CFLAG = -O4 -m64 -march=core2 -fomit-frame-pointer -std=gnu99
#CFFLAG = -O3 -march=native -fomit-frame-pointer -fno-caller-saves -std=gnu99
#CFLAG =  -mtune=generic -fomit-frame-pointer -std=gnu99
#CFLAG =  -O4 -m32 -march=atom -fomit-frame-pointer -std=gnu99
#CFLAG = -O2  -ffloat-store
#CFLAG = -O -pg -std=gnu99
#CFLAG = -O4 -march=native -fomit-frame-pointer -fprofile-arcs -ftest-coverage -std=gnu99
ifeq ($(ARM),1)
CFLAG = -O3 -march=native -mfloat-abi=hard -fomit-frame-pointer -std=gnu99
else ifeq ($(HOST),LINUX32)
CFLAG = -O3 -m32 -std=gnu99 -Wall -Wno-maybe-uninitialized -Wno-stringop-truncation -Wno-unused -Wno-misleading-indentation
else
CFLAG = -g -O -march=native -std=gnu99 -Wall -Wno-maybe-uninitialized -Wno-stringop-truncation -Wno-unused -Wno-misleading-indentation
endif
#CFLAG = -O4 -march=native -fomit-frame-pointer -fprofile-use -std=gnu99
LIBS = -lm
GCCHOST != uname -snr
PROC != uname -mp
CC= gcc
ifeq ($(HOST),MACOS)
CC= /usr/local/Cellar/gcc/9.2.0_1/bin/gcc-9
OPTS = -DGRAF=$(GRAF) -DFAST=$(FAST) -DDEBUG=$(DEBUG) -DINSTCON=$(INSTCON) \
	-DSOUND=$(SOUND) -DSTATS=$(STATS) -DSOCKET=$(SOCKET) \
	-DALPS_VERSION=$(VERSION)  -DHOST=$(HOST) \
	-DGCCFLAGS="$(CFLAG)" -DGCCHOST="$(HOST)" -DGCCPROC="$(PROC)" \
	-I /Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include -I /opt/local/include 
else
OPTS = -DGRAF=$(GRAF) -DFAST=$(FAST) -DDEBUG=$(DEBUG) -DINSTCON=$(INSTCON) \
	-DSOUND=$(SOUND) -DSTATS=$(STATS)  -DSOCKET=$(SOCKET) \
	-DALPS_VERSION=$(VERSION)  -DHOST=$(HOST)  \
	-DGCCFLAGS="$(CFLAG)" -DGCCHOST="$(GCCHOST)" -DGCCPROC="$(PROC)"
endif
INCLUDE = alps.h sound.h inst.h graf.h rascal.h
SRC     = alps.c format.c rascal.c term.c sound.c inst.c graf.c
OBJECTS = alps.o format.o rascal.o term.o sound.o inst.o graf.o
PROGRAMS = alps
SHELL   = /bin/bash
VERSION = $(shell head -1 alps.c | cut -d '-' -f 2)
alpsf: CFLAG = $(CFFLAG)
alps : FAST = 0
alpsf: FAST = 1
CFLAGS  = $(CFLAG) $(OPTS)
GRAF    = 0
DEBUG   = 0
INSTCON = 0
SOUND   = 0
STATS   = 0
SOCKET  = 0
HOST    = LINUX64

ifeq ($(GRAF),1)
ifeq ($(ARM),1)
GRAFLIBS = -L/usr/X11R6/lib -lEZ -lXext -LX11 -lICE -lX11 -lpng
else
GRAFLIBS = -L/usr/X11R6/lib -lEZ -lXext -LX11 -lSM -lICE -lX11 \
	-ljpeg -lpng -ltiff -lz
endif
endif

ifeq ($(INSTCON),1)
INSTLIB = -lgpib
endif

ifeq ($(STATS),1)
LIBS += -lrt
endif

all: $(PROGRAMS) TAGS

strip:
	strip $(PROGRAMS)

clean:
	$(RM) $(PROGRAMS) $(OBJECTS) core *.gcda *.gcno alps alpsf

TAGS:	$(SRC) $(INCLUDE)
	etags -I $(SRC) $(INCLUDE)

$(OBJECTS): $(INCLUDE) Makefile

alps:	$(OBJECTS) Makefile
	$(CC) $(CFLAGS) -o alps $(OBJECTS) $(GRAFLIBS) $(INSTLIB) $(LIBS)
	ln -f alps ../alps

alpsf:	$(SRC) $(INCLUDE) Makefile
	$(CC) $(CFLAG) -o alpsf $(SRC) $(OPTS) $(GRAFLIBS) $(INSTLIB) $(LIBS)
	ln -f alpsf ../alpsf

