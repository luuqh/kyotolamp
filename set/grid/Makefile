#!/bin/sh
SDIR		= .
XDIR		= .
EXEC		= a.out 

# Fortran compiler
F90		= frt

OBJS = grids.f90
FFLAGS= -Am
 
LIB = -lnetcdf 
#NCDIR = /sw/lib
NCDIR = /thin/local/NETCDF/netcdf-3.6.1
LIBDIR = -L$(NCDIR)/lib
#TECDIR = -L$/home3/t/t51851/tidemodel/version1/output/
TECLIB = #/home3/t/t51851/tidemodel/version1/output/tecio64.a

INCDIR = -I$(NCDIR)/include

# Object name 
all : $(XDIR)/$(EXEC)

FSRC   = $(OBJS:.o=.f90)
LIST = ${addsuffix .lst,$(FSRC)}
ERROR = ${addsuffix .err,$(FSRC)}
$(XDIR)/$(EXEC) :	$(OBJS)
	$(F90) $(FFLAGS) $(OBJS)  $(LIBDIR) $(INCDIR) -o $@ $(LIBDIR) $(LIB) $(TECLIB)

readnc.o	: readnc.f90

# Fortran compilation
.SUFFIXES	:
.SUFFIXES	: .o .F90
.F90.o:
	$(F90) -c $(INCDIR) -Qo$<.lst -Z$<.err -o $@ $<
clean:
	rm -f $(OBJS)
cleanall:
	rm -f $(OBJS) $(LIST) $(ERROR) *.mod

