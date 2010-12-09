PROGRAM showpal
 IMPLICIT NONE
! Define error file, Fortran unit number, and workstation type,
! and workstation ID.
!
 INTEGER, PARAMETER :: IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1
 INTEGER :: ISZDM
 INTEGER :: ji, jn, ic
 INTEGER :: narg, iargc, numpal=10, nn, i20, ilast
 CHARACTER(LEN=80) :: cpal, cdum
 INTEGER, DIMENSION(:), ALLOCATABLE :: iind
 REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: red, green, blue

 INTEGER :: IHOV,  NBOX,  IFTP,  NLBS, LBAB
 INTEGER, ALLOCATABLE, DIMENSION (:) :: LFIN
 REAL(KIND=4) :: XLEB, XREB, YBEB, YTEB,  WSFB, HSFB
 CHARACTER(LEN=10), ALLOCATABLE, DIMENSION (:) :: LLBS


 narg=iargc()
 IF ( narg /= 1 ) THEN
    PRINT *, 'USAGE : showpal palfile'
    STOP 1
 ENDIF

 CALL getarg(1,cpal)
! open gks 
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
 CALL readpal (cpal )

 DO YBEB=0, 0.9, 0.05
! plot color bar
 IHOV= 0                                  ! horizontal label bar
 XLEB=0 ; XREB=0.7 ; YTEB=YBEB+0.05
! YBEB=0.1; YTEB=0.15 ! Position of label bar
 NBOX = -1* ( ilast-19 +5 )               ! number of color box <0 for no lines
 WSFB=1 ; HSFB= 0.9                     ! fraction of box plotted
 LFIN=(/(ji,ji=0,  ilast-19 +4)/)
 IFTP = 1                                 ! solid fill
 LLBS = ' '
 NLBS = ilast-19+5
 LBAB = 0                                 ! 0 : nolabel  1 2 3 
 CALL LBLBAR (IHOV, XLEB, XREB, YBEB, YTEB, NBOX, WSFB, HSFB, LFIN, IFTP, LLBS, NLBS, LBAB)
 CALL PLCHHQ(0.7,YBEB+0.02,trim(cpal),-1.,0.,-1.)
 END DO

 CALL GDAWK (IWKID)
 CALL GCLWK (IWKID)
 CALL GCLKS

 CONTAINS
 
 SUBROUTINE readpal (cdpal)

 CHARACTER(LEN=*) , INTENT(in) :: cdpal

 OPEN(numpal,FILE=cdpal)
 ! How many lines in pal ?
 nn=0
 DO 
 READ(numpal,*,END=100)
 nn=nn+1
 END DO
100 PRINT *, ' Nbre de lignes dans ', TRIM(cdpal),' = ',nn
 ALLOCATE (iind(nn), red(nn), green(nn), blue(nn) )
 ALLOCATE (LLBS(nn), LFIN(nn) )
 REWIND(numpal)
 jn=0
 iind = 0 ; red = 0. ; green = 0. ; blue=0.
 DO ji=1,nn
    READ(numpal,'(a)') cdum
    IF ( cdum(1:1)  /=  '#' .AND.  cdum  /= ' ' ) THEN
      jn=jn+1 ; READ(cdum,*) ic, red(ic),green(ic), blue(ic) ; iind(ic)=ic
      IF ( ic == 20 ) i20=ic
    ENDIF
 END DO
   ilast=ic
 PRINT *, ' Nbre de lignes de couleurs effectives = ', jn

  DO ic=1,nn
!   PRINT *, iind(ic) ,  red(ic),green(ic), blue(ic)
  END DO
      
! Load first 5 colors 
 DO ji=1,5 
        print *, iind(ji),red(ji), green(ji), blue(ji)
        CALL GSCR (IWKID,iind(ji)-1,red(ji), green(ji), blue(ji) )
 END DO
        CALL GSCR (IWKID,1, 0.,0.,0.)
        CALL GSCR (IWKID,0, 1.,1.,1.)
!
! Load other colors starting from 20
 DO  ji=1, ilast -19
          CALL GSCR (IWKID, ji+4, red(i20-1+ji), green(i20-1+ji), blue(i20-1+ji) )
 END DO

  close(numpal)



   END SUBROUTINE readpal 

END PROGRAM showpal
