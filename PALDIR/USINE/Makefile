all: readcmap readcmap2 readcmap3

readcmap: readcmap.f90
	pgf90 readcmap.f90 -o readcmap


readcmap2: readcmap2.f90
	pgf90 readcmap2.f90 -o readcmap2

readcmap3: readcmap3.f90
	pgf90 readcmap3.f90 -o readcmap3


clean:
	\rm *~
