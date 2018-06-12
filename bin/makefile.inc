
INSTALLROOT=/home/WORK/ecens/bin
DATAROOT=/home/WORK/ecens/rawdata
CC=gcc

CFLAGS= -DLITTLE -DSWAPBYTE  -O2 
DBCFLAGS= -DLITTLE -DSWAPBYTE -g -DDEBUG=1

FC       = pgf90

FREE = -Mfree
FIXED = -Mnofree

# Define pre-processor command line options for .F files.

CPP = pgf90
CPPFLAGS = $(INC)  -Dx86_64 -DF90 -DFORTRANUNDERSCORE -DNODYNAMIC -DMULTI_EXEC

# Define include paths.
NETCDF=/usr/local/netcdf-4.0
JASPER=/usr/local/jasper
INC = -I$(NETCDF)/include -I$(JASPER)/include 

AR  = ar
RANLIB   = ranlib
PERL = /usr/bin/perl
LIBPATHFLAG=-L

# Grib1 and Grib2 decoder libs and flags
DEGRIBLIBS=-ljasper -lpng -lz 
DEGRIBFLAGS=-DUSE_JPEG2000 -DUSE_PNG
CDEGRIBFLAGS=-DLINUX -DSWAPBYTE -DFORTRANUNDERSCORE

OTHERLIBS = $(LIBPATHFLAG)$(NETCDF)/lib -lnetcdf $(LIBPATHFLAG)$(JASPER)/lib -lnetcdf -lgcc
