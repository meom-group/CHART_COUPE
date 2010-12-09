 PROGRAM readcmap
  IMPLICIT NONE
  INTEGER,PARAMETER :: ncolor=256
  INTEGER :: narg, numin=10, iargc, ji
  INTEGER, DIMENSION(3,ncolor) :: idta
  REAL(KIND=4), DIMENSION(ncolor) :: red, green, blue
  CHARACTER(LEN=80) :: cfile
  
  
  narg=iargc()
  IF ( narg /= 1 ) THEN
     PRINT *, 'Usage: readcmap ''colormap.txt file'' '
     STOP
  ENDIF
  
  CALL getarg(1,cfile)
  OPEN(numin,file=cfile)

  READ(numin,*) idta
  red=idta(1,:)/255.
  green=idta(2,:)/255.
  blue=idta(3,:)/255.

  DO ji=20,ncolor
   WRITE(6,9000) ji, red(ji), green(ji), blue(ji)
  ENDDO

9000 FORMAT(i3.3, 2x, 3f8.4 )

  CLOSE(numin)

  END PROGRAM readcmap
