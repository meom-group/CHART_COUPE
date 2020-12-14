FC=ifort

all: readcmap readcmap2 readcmap3

readcmap: readcmap.f90
	$(FC) readcmap.f90 -o readcmap


readcmap2: readcmap2.f90
	$(FC) readcmap2.f90 -o readcmap2

readcmap3: readcmap3.f90
	$(FC) readcmap3.f90 -o readcmap3

py_hexapal: py_hexapal.f90
	$(FC) py_hexapal.f90 -o py_hexapal


clean:
	\rm *~
