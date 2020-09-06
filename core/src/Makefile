#!/bin/sh
#
#  $Id: Makefile 2010/02/02 luu $
#
SDIR		= .
XDIR		= ..
EXEC		= a.out

F90		= mpifrt

FWD =	set_grids.o \
	set_param.o \
	set_particles.o \
	set_off.o \
	lib_index.o \
	lib_map.o \
	lib_random.o \
	lib_string.o \
	load_flow.o \
	load_grids.o \
	load_particles.o \
	load_namelist.o \
	save_particles.o \
	set_reallocate.o \
	p_drift.o \
	p_boundary.o \
	p_packing.o \
	p_interpol.o \
	p_transport.o 

MODULES = commons.o
MODEL = main.o 
OBJS = $(MODULES) $(MODEL) $(FWD)

# Object name
FSRC   = $(OBJS:.o=.F)
all : $(XDIR)/$(EXEC)

FSRC   = $(OBJS:.o=.F90)
LIST = ${addsuffix .lst,$(FSRC)}
ERROR = ${addsuffix .err,$(FSRC)}
$(XDIR)/$(EXEC) :	$(OBJS)
	$(F90) $(OBJS) -o $@

main.o			: commons.o
load_flow.o 		: commons.o
load_grids.o 		: commons.o
load_particles.o 	: commons.o
load_namelist.o 	: commons.o
save_particles.o	: commons.o
p_boundary.o 		: commons.o
p_packing.o 		: commons.o
p_transport.o 	: commons.o
p_interpol.o		: commons.o
p_drift.o		: commons.o
set_param.o		: commons.o
set_grids.o 		: commons.o
set_off.o 		: commons.o
set_particles.o	: commons.o
set_reallocate.o	: commons.o
lib_index.o		: 
lib_map.o 		: 
lib_random.o 		: 
lib_string.o 		: 
lib_index.o		: 
commons.o		: 

# Fortran compilation
.SUFFIXES	:
.SUFFIXES	: .o .F90
.F90.o:
	$(F90) -c -Fixed -Qo$<.lst -Z$<.err -o $@ $<

clean:
	rm -f $(OBJS)
cleanall:
	rm -f $(OBJS) $(LIST) $(ERROR) *.mod

