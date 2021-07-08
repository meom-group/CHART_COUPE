FC=ifort

all: readcmap.exe readcmap2.exe readcmap3.exe

readcmap.exe: readcmap.f90
	$(FC) readcmap.f90 -o readcmap


readcmap2.exe: readcmap2.f90
	$(FC) readcmap2.f90 -o readcmap2.exe

readcmap3.exe: readcmap3.f90
	$(FC) readcmap3.f90 -o readcmap3.exe

py_hexapal.exe: py_hexapal.f90
	$(FC) py_hexapal.f90 -o py_hexapal.exe


clean:
	\rm *~ *.exe
