# Makefile pour les utilitaire bimg
# actuellement chart et coupe
#
#    ######   ##  ##    ##   ######
#    ##   ##  ##  ###  ###  ##    ##
#    ######   ##  ########  ##
#    ##   ##  ##  ## ## ##  ##  #####
#    ##   ##  ##  ##    ##  ##    ##
#    ######   ##  ##    ##   ######
#
#FFLAGS=  -byteswapio  -Mextend -O3 -fast 
FFLAGS=  -byteswapio  -Mextend  -O3 -fast 
#FFLAGS=  -byteswapio -Mextend  -O -g -C
#FFLAGS=  -byteswapio -Mextend  -O  -g -C
FC = pgf90

CHARTOBJ = \
chart.o  color.o readpal.o readbimg.o util.o calcul.o\
deflim.o readargs.o tracecol.o isocontours.o val_table.o \
drawpalette.o checkfiles.o vectors.o lnblnk.o flush.o overlay.o sigma.o \
cdf.o

CHARTSRC = \
chart.f  color.f readpal.f readbimg.f util.f calcul.f\
deflim.f readargs.f tracecol.f isocontours.f val_table.f\
drawpalette.f checkfiles.F vectors.f lnblnk.f flush.f overlay.f sigma.f \
cdf.f

COUPEOBJ = \
coupe.o  color.o readpal.o readbimg.o util.o calcul.o\
deflim.o readargs.o tracecol.o isocontours.o val_table.o\
drawpalette.o checkfiles.o lnblnk.o flush.o overlay.o sigma.o \
cdf.o

COUPESRC = \
coupe.f  color.f readpal.f readbimg.f util.f calcul.f\
deflim.f readargs.f tracecol.f isocontours.f val_table.f\
drawpalette.f checkfiles.F lnblnk.f flush.f overlay.f sigma.f \
cdf.f


DEPEND = parameter.h common.h
COMPILE = pgf90 $(FFLAGS)   -c $*.f 

BINDIR = /usr/local/bin

all: chart coupe

chart: ${CHARTOBJ}
	ncargf90   $(FFLAGS) ${CHARTOBJ} -o $@  -lnetcdf

coupe: ${COUPEOBJ}
	ncargf90    $(FFLAGS) ${COUPEOBJ} -o $@  -lnetcdf

clean:
	-/bin/rm -f *.o core *% *.trace *~

install:	chart	coupe
	mv -f chart $(BINDIR)/chart
	mv -f coupe $(BINDIR)/coupe

bipro1: chart coupe
	 rcp -p coupe root@meolbipro1:/usr/local/bin/coupetest

remote_install: chart coupe
	rcp -p chart root@meolbipro1:/usr/local/bin/chart
	rcp -p coupe root@meolbipro1:/usr/local/bin/coupe
	rcp -p chart root@meollnx1:/usr/local/bin/chart
	rcp -p coupe root@meollnx1:/usr/local/bin/coupe
	rcp -p chart root@meolk7:/usr/local/bin/chart
	rcp -p coupe root@meolk7:/usr/local/bin/coupe
#	rcp -p chart root@meolipx:/usr/local/bin/chart
#	rcp -p coupe root@meolipx:/usr/local/bin/coupe
	rcp -p chart root@meollnx2:/usr/local/bin/chart
	rcp -p coupe root@meollnx2:/usr/local/bin/coupe
	rcp -p chart root@meollnx5:/usr/local/bin/chart
	rcp -p coupe root@meollnx5:/usr/local/bin/coupe
#	rcp -p chart root@meolatl:/usr/local/bin/chart
#	rcp -p coupe root@meolatl:/usr/local/bin/coupe
	rcp -p chart meolpacif:/usr/local/bin/chart
	rcp -p coupe meolpacif:/usr/local/bin/coupe

remote8.1: chart coupe
	rcp -p chart meollab:/usr/local/bin/chart
	rcp -p coupe meollab:/usr/local/bin/coupe
	rcp -p chart meoldrak:/usr/local/bin/chart
	rcp -p coupe meoldrak:/usr/local/bin/coupe
	rcp -p chart meolfram:/usr/local/bin/chart
	rcp -p coupe meolfram:/usr/local/bin/coupe
#	rcp -p chart meollnx3:/usr/local/bin/chart
#	rcp -p coupe meollnx3:/usr/local/bin/coupe
	rcp -p chart meolmed:/usr/local/bin/chart
	rcp -p coupe meolmed:/usr/local/bin/coupe

remote_k200: chart coupe
	rcp -p chart root@meolk200:/usr/local/bin/chart5
	rcp -p chart root@meolk200:/usr/local/bin/coupe5

remote_755: chart coupe
	rcp -p chart root@meol755:/usr/local/bin/chart5
	rcp -p chart root@meol755:/usr/local/bin/coupe5

meollnx1: chart coupe
	rcp -p chart root@meollnx1:/usr/local/bin/chart
	rcp -p coupe root@meollnx1:/usr/local/bin/coupe
                                                                                                     
install4:	chart	coupe
	mv -f chart $(BINDIR)/chart4
	mv -f coupe $(BINDIR)/coupe4

demo:	chart	coupe
	mv -f chart $(DEMODIR)/dchart
	mv -f coupe $(DEMODIR)/dcoupe

chart.total:
	cat $(CHARTSRC) > chart.total

coupe.total:
	cat $(COUPESRC) > coupe.total


common.h : parameter.h chart_version.h

vecrot_opa: vecrot_opa.f
	pgf90 -O -I/usr/local/netcdf-3.6.1/pgi/include -L /usr/local/netcdf-3.6.1/pgi/lib -lnetcdf -Mfree -Mextend -byteswapio vecrot_opa.f -o vecrot_opa 

tar :
	(cd ../ ; tar cf chart-coupe-6.0.tar CHART6.0/*.[Ffh] CHART6.0/Makefile* )




# DO NOT DELETE
