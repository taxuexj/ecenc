#
INSTALLROOT=/home/WORK/ecens
DATAROOT=/home/WORK/ecens/rawdata
FC       = pgf90
# Define include paths.
NETCDF=/usr/local/netcdf-4.0
JASPER=/usr/local/jasper
INC = -I$(NETCDF)/include -I$(JASPER)/include
PERL = /usr/bin/perl
LIBPATHFLAG=-L
#
OTHERLIBS = $(LIBPATHFLAG)$(NETCDF)/lib -lnetcdf
.SUFFIXES: .o .c .f .a .f90
#EXE = enthin.exe
#SRC= enthin.f
all: enthin.exe ecensc.exe ecensp.exe enh.exe jh.exe kindex.exe
enthin.exe: enthin.f 
	$(FC) enthin.f $(OTHERLIBS) -o $@
ecensc.exe: ecensc.f
	$(FC) ecensc.f $(OTHERLIBS) -o $@
ecensp.exe: ecensp.f
	$(FC) ecensp.f $(OTHERLIBS) -o $@
enh.exe: enh.f
	$(FC) enh.f $(OTHERLIBS) -o $@
jh.exe: jh.f
	$(FC) jh.f $(OTHERLIBS) -o $@
kindex.exe: kindex.f
	$(FC) kindex.f $(OTHERLIBS) -o $@

#exe: $(EXE)
#$(EXE): $(SRC) 
#	$(FC) $(SRC) $(OTHERLIBS) -o $@
	rm -f *.o
debug: $(SRC)
	$(RM) $(EXE) ;
	$(MAKE) LAPSLIBS='$(DBLAPSLIBS)' \
	CFLAGS='$(DBCFLAGS)' FFLAGS='$(DBFLAGS)'

install: $(EXE)
	$(INSTALL) cp *.exe  $(INSTALLROOT)/bin/$(EXE)
clean:
	rm -f *.exe  *.o *~ *#
distclean: clean
