 PROGRAM readcmap2
  IMPLICIT NONE
  INTEGER,PARAMETER :: ncolor=256, npal=ncolor-20
  INTEGER :: narg, numin=10, iargc, ji, j0
  INTEGER, DIMENSION(3,ncolor) :: idta
  REAL(KIND=4) :: rj, alpha
  REAL(KIND=4), DIMENSION(npal) :: red, green, blue
  CHARACTER(LEN=80) :: cfile
  
  
  narg=iargc()
  IF ( narg /= 1 ) THEN
     PRINT *, 'Usage: readcmap2 ''colormap.txt file'' '
     STOP
  ENDIF
  
  CALL getarg(1,cfile)
  OPEN(numin,file=cfile)

  READ(numin,*) idta
  CLOSE(numin)

! Now I have ncol(256) R,G,B value and I want to reduce them on npal (236)
!  keeping the first and last values

 red(1)=idta(1,1)/255.   ; red(npal)=idta(1,ncolor)/255.
 green(1)=idta(2,1)/255. ; green(npal)=idta(2,ncolor)/255.
 blue(1)=idta(3,1)/255.  ; blue(npal)=idta(3,ncolor)/255.

 DO ji=2,npal-1
   rj= ( (ji-1)*float(ncolor)/(npal-1.) )
   j0=int(rj)
   alpha=rj-j0
   red(ji)   = ( alpha*idta(1,j0+1) +(1.-alpha)*idta(1,j0)) /255.
   green(ji) = ( alpha*idta(2,j0+1) +(1.-alpha)*idta(2,j0)) /255.
   blue(ji)  = ( alpha*idta(3,j0+1) +(1.-alpha)*idta(3,j0)) /255.
 END DO


  

  DO ji=1,npal
   WRITE(6,9000) ji+19, red(ji), green(ji), blue(ji)
  ENDDO

9000 FORMAT(i3.3, 2x, 3f8.4 )

  CLOSE(numin)

  END PROGRAM readcmap2
