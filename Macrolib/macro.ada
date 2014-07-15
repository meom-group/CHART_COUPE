#----------------------------------------------------------------------
# CHART/COUPE 7.0 , MEOM 2010
# $Id$
# Copyright (c) 2010, J.-M. Molines.
# Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
#----------------------------------------------------------------------
FC = ifort
LD = ncargf90
NCDFDIR =
NCDF =
NCL =

# To activate debuging uncomment the following line
DEBUGFLAG = -O -CB -g -traceback -fpe0 -ftrapuv
#DEBUGFLAG =  -O3

#FFLAGS = $(DEBUGFLAG) -static -assume byterecl -convert big_endian $(NCDF) $(NCL)
FFLAGS = $(DEBUGFLAG)  -assume byterecl -convert big_endian $(NCDF) $(NCL)

INSTALL=$(WORKDIR)/bin

