#
#  Makefile for CHART/COUPE F90 version. This file was manually generated. Take care of not shuffling it !
#  You must manually do the link between make.macro and the corresponding macro.xxxx for your machine 
#  configuration.

##----------------------------------------------------------------------
## CHART/COUPE 7.0 , MEOM 2010
## $Id$
## Copyright (c) 2010, J.-M. Molines.
## Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
##----------------------------------------------------------------------
include make.macro

CHARTSRC= calcul.f90         cdf.f90       chart.F90  checkfiles.F90 modexternal.f90 \
  moddrawpalette.f90 isocontours.f90    modcolor.f90      modcom.f90    modeflim.f90 \
      modmapncar.f90    modparam.f90 modreadargs.f90 modvaltable.f90     overlay.f90 readbimg.f90 \
         readpal.f90    tracecol.f90        util.f90   val_table.f90     vectors.f90  \
          cpmpxy.f90       vvmxy.f90

CHARTOBJ= calcul.o           cdf.o        chart.o  checkfiles.o  modexternal.o \
  moddrawpalette.o   isocontours.o     modcolor.o      modcom.o     modeflim.o \
      modmapncar.o      modparam.o  modreadargs.o modvaltable.o      overlay.o   readbimg.o \
         readpal.o      tracecol.o         util.o   val_table.o      vectors.o \
         nclinfo.o    modoptflag.o ncar_routine.o

COUPESRC =
COUPEOBJ= calcul.o           cdf.o        coupe.o  checkfiles.o  modexternal.o modcoupe.o \
  moddrawpalette.o   isocontours.o     modcolor.o      modcom.o     modeflim.o \
      modmapncar.o      modparam.o  modreadargs.o modvaltable.o      overlay.o readbimg.o \
         readpal.o         sigma.o     tracecol.o        util.o    val_table.o  vectors.o \
         nclinfo.o    modoptflag.o ncar_routine.o

all: chart coupe  vecrot_opa vecrot

chart : $(CHARTOBJ)
	$(LD) $(CHARTOBJ) -o chart $(FFLAGS)

coupe : $(COUPEOBJ)
	$(LD) $(COUPEOBJ) -o coupe $(FFLAGS)

clean : 
	\rm -f *.o *.mod *~

cleanall :  clean
	\rm -f chart coupe

# dependence below

modparam.o : modparam.f90
	$(FC) $(FFLAGS) -c $*.f90 

modcolor.o : modcolor.f90
	$(FC) $(FFLAGS) -c $*.f90 

modmapncar.o : modmapncar.f90
	$(FC) $(FFLAGS) -c $*.f90 

modvaltable.o : modvaltable.f90
	$(FC) $(FFLAGS) -c $*.f90 

modoptflag.o : modoptflag.f90
	$(FC) $(FFLAGS) -c $*.f90 

modcom.o : modcom.f90 modparam.o modoptflag.o
	$(FC) $(FFLAGS) -c $*.f90 

modeflim.o : modeflim.f90 modcom.o modcolor.o util.o val_table.o
	$(FC) $(FFLAGS) -c $*.f90 

modcoupe.o : modcoupe.f90 modcom.o modmapncar.o modvaltable.o readbimg.o modcolor.o modexternal.o \
             checkfiles.o isocontours.o tracecol.o
	$(FC) $(FFLAGS) -c $*.f90 

modreadargs.o : modreadargs.f90  modcom.o modcolor.o val_table.o util.o calcul.o readbimg.o
	$(FC) $(FFLAGS) -c $*.f90 

ncar_routine.o : ncar_routine.f90  modcom.o  vectors.o
	$(FC) $(FFLAGS) -c $*.f90 

calcul.o : calcul.f90 modcom.o 
	$(FC) $(FFLAGS) -c $*.f90 

cdf.o : cdf.f90 modcom.o
	$(FC) $(FFLAGS) -c $*.f90 

modexternal.o : modexternal.f90 modcom.o modcolor.o
	$(FC) $(FFLAGS) -c $*.f90 

cpmpxy.o : cpmpxy.f90 modcom.o 
	$(FC) $(FFLAGS) -c $*.f90 

moddrawpalette.o : moddrawpalette.f90 modcom.o
	$(FC) $(FFLAGS) -c $*.f90 

isocontours.o : isocontours.f90 modcom.o modcolor.o modmapncar.o util.o val_table.o nclinfo.o
	$(FC) $(FFLAGS) -c $*.f90 

nclinfo.o : nclinfo.f90 
	$(FC) $(FFLAGS) -c $*.f90 

overlay.o : overlay.f90 modparam.o  modcom.o
	$(FC) $(FFLAGS) -c $*.f90 

readbimg.o : readbimg.f90 modcom.o  util.o cdf.o
	$(FC) $(FFLAGS) -c $*.f90 

readpal.o : readpal.f90 modcom.o modcolor.o
	$(FC) $(FFLAGS) -c $*.f90 

sigma.o : sigma.f90 modcom.o
	$(FC) $(FFLAGS) -c $*.f90 

tracecol.o : tracecol.f90 modcom.o modmapncar.o modvaltable.o modcolor.o util.o modexternal.o isocontours.o  \
            val_table.o moddrawpalette.o
	$(FC) $(FFLAGS) -c $*.f90 

util.o : util.f90 modcom.o
	$(FC) $(FFLAGS) -c $*.f90 

val_table.o : val_table.f90 modcom.o modvaltable.o util.o
	$(FC) $(FFLAGS) -c $*.f90 

vectors.o : vectors.f90 modcom.o modmapncar.o modcolor.o modeflim.o  cdf.o tracecol.o readbimg.o
	$(FC) $(FFLAGS) -c $*.f90 

vvmuxy.o : vvmuxy.f90 modcom.o vectors.o
	$(FC) $(FFLAGS) -c $*.f90 

checkfiles.o:  checkfiles.F90 modcom.o util.o  readbimg.o
	$(FC) $(FFLAGS) -c $*.F90 

# -----------------------------------------------
chart.o : chart.F90  modcom.o modcolor.o util.o modeflim.o  calcul.o \
          modreadargs.o cdf.o checkfiles.o  readpal.o overlay.o tracecol.o vectors.o isocontours.o val_table.o
	$(FC) $(FFLAGS) -c $*.F90 

coupe.o : coupe.F90  modcom.o modcolor.o modeflim.o modcoupe.o sigma.o util.o \
          modreadargs.o checkfiles.o  readpal.o overlay.o tracecol.o isocontours.o val_table.o
	$(FC) $(FFLAGS) -c $*.F90 

vecrot_opa : vecrot_opa.f90
	$(FC) $@.f90  -o $@ $(FFLAGS)

vecrot : vecrot.f90
	$(FC) $@.f90  -o $@ $(FFLAGS)

