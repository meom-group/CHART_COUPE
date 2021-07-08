PROGRAM py_hexapal
  !!======================================================================
  !!                     ***  PROGRAM  py_hexapal  ***
  !!=====================================================================
  !!  ** Purpose : transform a chart palette file into an hexdecimal 
  !!              palette suitable for python applications
  !!
  !!  ** Method  : read decimal rgb, transform to integer [0-255] and
  !!               write results in hexadecimal on std output
  !!
  !! History : 1.0  : 09/2012  : J.M Molines  : Original code
  !!----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER ::  jc
  INTEGER :: ncol, narg, iargc, icol, ios, nskip, nncol
  INTEGER :: nlign, nbloc, nleft, idx
  INTEGER :: numpal=10
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rgb
  REAL(KIND=4)                              :: r, g, b

  CHARACTER(LEN=132) :: cf_pal
  CHARACTER(LEN=132) :: cline
  CHARACTER(LEN=132) :: cmap
  !!---------------------------------------------------------------------
  narg=iargc()
  IF ( narg /= 1 ) THEN
    PRINT *, 'USAGE : py_hexapal palette_file'
    STOP
  ENDIF

  CALL getarg(1, cf_pal)
  OPEN(numpal,file=cf_pal)
  idx=INDEX('.',cf_pal)
  IF ( idx /= 0 ) THEN
     cmap=cf_pal(1:idx-1)
  ELSE
     cmap=cf_pal
  ENDIF
  

  icol=-1
  ios=0

  ! Palette file can have blank line inserted as well as comment line starting with #
  ! First skip the head of the file up to color #20 
  nskip = 0
  DO WHILE ( icol < 20 )
   nskip=nskip+1
   READ(numpal,'(a)' ) cline
   cline=ADJUSTL(cline)
   IF ( cline(1:1) == '#' .OR.  cline(1:1) == '' )  THEN
    ! skip line
   ELSE
    READ(cline,*) icol, r,g,b
   ENDIF
  END DO
  nskip=nskip -1

  ! now count the number of colors
  ncol=1
  ios=0
  DO WHILE ( ios == 0 ) 
    READ(numpal, '(a)', iostat=ios) cline
    cline=ADJUSTL(cline)
    IF ( cline(1:1) == '#' .OR.  cline(1:1) == '' )  THEN
    ! skip line
   ELSE
    READ(cline,*), icol, r,g,b
    ncol=ncol+1
   ENDIF
  ENDDO
  ncol = ncol - 1
  PRINT *, 'NCOL=', ncol

  ALLOCATE ( rgb(3,ncol) )
  REWIND(numpal)
  ! skip header
  DO jc=1,nskip
    READ(numpal,*)
  ENDDO

  nncol=0
  ios=0
   DO WHILE ( nncol < ncol )
    READ(numpal, '(a)', iostat=ios) cline
    cline=ADJUSTL(cline)
    IF ( cline(1:1) == '#' .OR.  cline(1:1) == '' )  THEN
    ! skip line
   ELSE
    nncol=nncol+1
    READ(cline,*), icol, rgb(1,nncol),  rgb(2,nncol),  rgb(3,nncol)
    PRINT *, nncol, icol, rgb(1,nncol),  rgb(2,nncol),  rgb(3,nncol)
   ENDIF
  ENDDO

  ! output in pythom format
  nlign=ncol/10
  nbloc=nlign*10
  nleft=ncol-nbloc
  print *, nlign, nbloc,nleft
  PRINT '("self.",a,"=[")',TRIM(cmap)
!  DO jc=1,ncol-1
!   PRINT '(10("''#",3z2.2,"'',"))', nint(rgb(:,1:nbloc)*255)
   PRINT '(10("''#",3z2.2,"'', "))', nint(rgb(:,1:ncol)*255)
!  ENDDO
! PRINT '("''#",3z2.2,"'',")', nint(rgb(:,nbloc+1:ncol-1)*255)
! PRINT '("''#",3z2.2,"'']")', nint(rgb(:,ncol)*255)
  print '("]")' 


END PROGRAM py_hexapal
