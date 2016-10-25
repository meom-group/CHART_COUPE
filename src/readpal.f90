MODULE readpal
  !!======================================================================
  !!                     ***  MODULE  readpal  ***
  !!  Contains routines used to create color palette, from file or default
  !!=====================================================================
  !! History : 1.0   ! 06/1993   ! E. Brown     : Original code
  !!           5.0   ! 01/2002   ! J.M. Molines : rewriting
  !!           7.0   ! 11/2010   ! J.M. Molines : F90 and Doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! ReadPaletteFile  : read chart palette file (ASCII)
  !! DefaultPalette   : set a default palette
  !! JetPal           : inspired from matlab, can adjust the number of colors
  !!----------------------------------------------------------------------
  USE modcom
  USE modcolor

  IMPLICIT NONE

  PRIVATE

  PUBLIC  :: ReadPaletteFile  ! called from chart, coupe
  PUBLIC  :: DefaultPalette   ! called from chart, coupe

  PRIVATE :: JetPal           ! called from this module

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS


  SUBROUTINE ReadPaletteFile(cd_fpal, kncol, kreverse )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ReadPaletteFile  ***
    !!
    !! ** Purpose :  Read a Palette chart/coupe file 
    !!
    !! References : chart manual 
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),  INTENT(in) :: cd_fpal
    INTEGER(KIND=4),  INTENT(out) :: kncol
    INTEGER(KIND=2),   INTENT(in) :: kreverse

    CHARACTER(LEN=256)    :: cline
    INTEGER(KIND=4)       :: idcolor
    INTEGER(KIND=4)       :: ji, jj, jk
    INTEGER(KIND=4)       :: icol1, icol2, ierr
    INTEGER(KIND=4)       :: istatus, ilen
    INTEGER(KIND=4)       :: isteps
    INTEGER(KIND=4)       :: icol0,  icolstp
    INTEGER(KIND=4)       :: ilu = 50
    REAL(KIND=4)          :: zwk1(3),zwk2(3), zfg(3)
    REAL(KIND=4)          :: zincr(3), zr,zg,zb
    !!----------------------------------------------------------------------
    OPEN (ilu,file=cd_fpal ,iostat=istatus, status='old') 

    CALL  flush(6)
    idcolor = -1
    zfg(:)  = 0.0

    !  Look for idcolor 1
    DO WHILE ( idcolor <  1 )
       READ(ilu,'(a)',END=6) cline
       IF (cline(1:1) /=  '#' )  THEN
          ilen = LEN_TRIM(cline)
          IF ( ilen /=  0 )   THEN
             READ (cline,*) idcolor,(zwk1(jk),jk=1,3)
          ENDIF
       ENDIF
    END DO
    zfg = zwk1
6   CONTINUE

    REWIND(ilu)

    !  initialize all colors to zfg
    DO ji = 0,  COLOR_NRES
       CALL gscr(1,ji, zfg(1), zfg(2), zfg(3))
    END DO

    ! ... read and affect colors to the idcolor in the palette
    ! ... set and update the marks_index table
    idcolor     = -1
    marks_count = 0
    kncol       = 0
    !
    DO WHILE ( idcolor < NBOXMAX )
       READ(ilu,'(a)',END=7) cline
       IF (cline(1:1) /=  '#' )  THEN
          ilen = LEN_TRIM(cline)
          IF ( ilen /=  0 )   THEN
             READ (cline,*) idcolor,(zwk1(jk),jk=1,3)
             CALL gscr(1,idcolor, zwk1(1), zwk1(2), zwk1(3))
             IF (idcolor >=  COLOR_NRES ) THEN
                kncol       = kncol + 1
                marks_count = marks_count + 1
                marks_index(marks_count) = idcolor
             END IF
          ENDIF
       ENDIF
    END DO
7   CONTINUE
    CLOSE(ilu)

    ! ... fill colors between idcolor by interpolatted color
    DO ji = 1, marks_count -1
       icol1 = marks_index(ji)
       icol2 = marks_index(ji+1)
       isteps = icol2-icol1
       IF ( isteps /=  1 ) THEN
          CALL gqcr(1,icol1,0,ierr,zwk1(1), zwk1(2), zwk1(3))
          CALL gqcr(1,icol2,0,ierr,zwk2(1), zwk2(2), zwk2(3))
          zincr(:) = ( zwk1(:) - zwk2(:) ) / isteps
          DO jj=icol1+1, icol2-1
             zwk2(:)=zwk1(:) + (jj - icol1)* zincr(:)
             kncol = kncol +1
             CALL gscr(1,jj,zwk2(1),zwk2(2),zwk2(3) )
          END DO
       END IF
    END DO
     
    ! ... if -rev option swap foreground (1) and background (0) color
    IF ( kreverse == 1 ) THEN
       CALL gqcr(1,1,0,ierr,zwk1(1), zwk1(2), zwk1(3))
       CALL gqcr(1,0,0,ierr,zwk2(1), zwk2(2), zwk2(3))
       !
       CALL gscr(1,0,zwk1(1), zwk1(2), zwk1(3))
       CALL gscr(1,1,zwk2(1), zwk2(2), zwk2(3))
    END IF

    marks_index(marks_count+1) = kncol + COLOR_NRES

    IF (opt_palout == 1) THEN
       OPEN (ilu,file='used_palette')
       WRITE(ilu,'(a)')'# automatically generated palette file, -crlout option'
       WRITE(ilu,'(a,i4,a)')'# the ',COLOR_NRES,' first colors are reserved'
       DO ji=0,COLOR_NRES-1
          CALL gqcr(1,ji,0,ierr,zr,zg,zb)
          WRITE(ilu,'(i3,2x,3f10.4)')ji,zr,zg,zb
       ENDDO
       WRITE(ilu,'(a)')'# Following colors are for data '

       DO ji=COLOR_NRES, COLOR_NRES+kncol-1
          CALL gqcr(1,ji,0,ierr,zr,zg,zb)
          WRITE(ilu,'(i3,2x,3f10.4)')ji,zr,zg,zb
       END DO
       CLOSE(ilu)
    ENDIF

  END SUBROUTINE ReadPaletteFile


  SUBROUTINE DefaultPalette (kncol, kreverse, kpal)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DefaultPalette  ***
    !!
    !! ** Purpose :  Set a default palette. Various palette are available 
    !!               according to kpal
    !!
    !! ** Method  :  kpal = 0 : color defaults like jetpal14
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(out) :: kncol
    INTEGER(KIND=2),  INTENT(in) :: kreverse
    INTEGER(KIND=2),  INTENT(in) :: kpal

    INTEGER(KIND=4)        :: ji, jj
    INTEGER(KIND=4)        :: ierr
    INTEGER(KIND=4)        :: icol0, icol1, icolstp
    INTEGER(KIND=4)        :: ilu = 99
    REAL(KIND=4)           :: zrgbw(3,NBOXMAX)
    REAL(KIND=4)           :: zrgbv(3,14)
    REAL(KIND=4)           :: zrgbg(3,11)
    REAL(KIND=4)           :: zrgbsh(3,2)
    REAL(KIND=4)           :: zrgbvec(3,2)
    REAL(KIND=4)           :: zr, zg, zb
    !!----------------------------------------------------------------------
    ! Default color palette : jetpal14 (kpal=0)
    DATA zrgbv / &
    &  0.    ,    0.    ,    0.9   ,&
    &  0.    ,    0.4500,    1.0000,&
    &  0.    ,    0.6000,    1.0000,&
    &  0.    ,    0.7500,    1.0000,&
    &  0.    ,    0.9000,    1.0000,&
    &  0.2500,    1.0000,    1.0000,&
    &  0.5000,    1.0000,    0.7500,&
    &  0.7500,    1.0000,    0.5000,&
    &  0.9000,    1.0000,    0.4500,&
    &  1.0000,    1.0000,         0,&
    &  1.0000,    0.7500,         0,&
    &  1.0000,    0.5000,         0,&
    &  1.0000,    0.2500,         0,&
    &  0.9000,         0,         0/
    !
    ! Color palette used with -greyscale (kpal=2)
    DATA zrgbg /&
    & 0.142857 , 0.142857 , 0.142857,&
    & 0.222222 , 0.222222 , 0.222222,&
    & 0.301587 , 0.301587 , 0.301587,&
    & 0.380952 , 0.380952 , 0.380952,&
    & 0.460317 , 0.460317 , 0.460317,&
    & 0.539683 , 0.539683 , 0.539683,&
    & 0.619048 , 0.619048 , 0.619048,&
    & 0.698413 , 0.698413 , 0.698413,&
    & 0.777778 , 0.777778 , 0.777778,&
    & 0.857143 , 0.857143 , 0.857143,&
    & 0.936508 , 0.936508 , 0.936508/

    !
    ! Color palette used with -shade (kpal=3)
    DATA zrgbsh /&
    &  0.9 , 0.9, 0.9,&
    &  1.0 , 1.0, 1.0/
    !
    ! Color palette for option -vecshade (kpal=4)
    ! It is white everywhere, only the ocean color is used (4)
    DATA zrgbvec /&
    & 1., 1., 1.,&
    & 1., 1., 1. /

    IF (kreverse == 0) THEN
       CALL gscr(1,COLOR_BACKGROUND,1.,1.,1.) ! background 
       CALL gscr(1,COLOR_FOREGROUND,0.,0.,0.) ! foreground
    ELSE 
       CALL gscr(1,COLOR_BACKGROUND,0.,0.,0.) ! background 
       CALL gscr(1,COLOR_FOREGROUND,1.,1.,1.) ! foreground
    ENDIF

    !     continents: index 2
    IF (kpal < 2) THEN
       CALL gscr(1,COLOR_CONTINENT,0.5,0.5,0.5)
    ELSE
       CALL gscr(1,COLOR_CONTINENT,0.502,0.502,0.502)
    ENDIF

    !     spval: index 3
    IF (kpal /=  3 ) THEN
       CALL gscr(1,COLOR_SPVAL,0.9,0.9,0.9)
    ELSE
       CALL gscr(1,COLOR_SPVAL,1.,1.,1.)
    ENDIF

    !     ocean: index 4
    IF (kpal /= 4) THEN
       CALL gscr(1,COLOR_OCEAN,1.,1.,1.)
    ELSE
       CALL gscr(1,COLOR_OCEAN,0.9,0.9,0.9)
    ENDIF

    !     isocontours 1: index 5
    CALL gscr(1,COLOR_ISOCONTOUR,0.,0.,0.)

    !     vectors : index 6
    CALL gscr(1,COLOR_VECTOR,0.,0.,0.)

    !     continents perimeter: index 7
    CALL gscr(1,COLOR_CONTINENT_PERIM,0.,0.,0.)

    !     free colors:  8 to COLOR_NRES - 1
    DO ji=8,COLOR_NRES-1
       CALL gscr (1,ji,0.,0.,0.)
    ENDDO

    PRINT *,' Used internal palette  # :',kpal
    IF (opt_prev == 0) THEN
       icol0   = 0
       icolstp = 1
    ELSE
       icol0   = kncol + 1
       icolstp = -1
    ENDIF
    !
    SELECT CASE ( kpal )
     CASE ( 0 )
        kncol = 14
        IF (opt_prev == 1) icol0 = kncol + 1
        marks_count = 14
        DO ji=1,kncol
           DO jj=1,3
              zrgbw(jj,ji) = zrgbv(jj,icol0+icolstp*ji)
           ENDDO
        ENDDO
     CASE ( 2 )
        kncol=11
        IF (opt_prev == 1) icol0=kncol+1
        marks_count = 11
        DO ji=1,kncol
           DO jj=1,3
              zrgbw(jj,ji) = zrgbg(jj,icol0+icolstp*ji)
           ENDDO
        ENDDO
     CASE ( 3 ) 
        kncol =2
        IF (opt_prev == 1) icol0=kncol+1
        marks_count = 2
        DO ji=1,kncol
           DO jj=1,3
              zrgbw(jj,ji) = zrgbsh(jj,icol0+icolstp*ji)
           ENDDO
        ENDDO
     CASE ( 4 )
        kncol =2
        IF (opt_prev == 1) icol0=kncol+1
        marks_count = 2
        DO ji=1,kncol
           DO jj=1,3 
              zrgbw(jj,ji) = zrgbvec(jj,icol0+icolstp*ji)
           ENDDO
        ENDDO
     CASE ( 5 )
        CALL JetPal(zrgbw, ncol_jetpal)
        kncol=ncol_jetpal
        marks_count = ncol_jetpal
     CASE ( 6 )
        CALL JetPal (zrgbw, ncol_jetpal)
        kncol=ncol_jetpal
        marks_count = ncol_jetpal
        DO ji =2, ncol_jetpal,2
           zrgbw(:,ji)=0.
        ENDDO
     CASE DEFAULT
       PRINT *,'ERROR : Default palette # ', kpal,' unknown !'
       STOP
    END SELECT

    DO ji=1,kncol
       CALL gscr(1,ji+COLOR_NRES-1, zrgbw(1,ji),  zrgbw(2,ji), zrgbw(3,ji))
       marks_index(ji) = ji+COLOR_NRES-1
    ENDDO

    marks_index(marks_count+1) = kncol+COLOR_NRES

    IF (opt_palout == 1) THEN
       OPEN (ilu,file='used_palette')
       WRITE(ilu,'(a)')'# automatically generated palette file, -crlout option'
       WRITE(ilu,'(a,i4,a)')'# the ',COLOR_NRES,' first colors are reserved'
       DO ji=0,COLOR_NRES-1
          CALL gqcr(1,ji,0,ierr,zr,zg,zb)
          WRITE(ilu,'(i3,2x,3f10.4)')ji,zr,zg,zb
       ENDDO
       WRITE(ilu,'(a)')'# Following colors are for data '

       DO ji=1,kncol
          WRITE(ilu,'(i3,2x,3f10.4)')ji-1+COLOR_NRES,zrgbw(1,ji),  zrgbw(2,ji), zrgbw(3,ji)
       ENDDO
       CLOSE(ilu)
    ENDIF

  END SUBROUTINE DefaultPalette


  SUBROUTINE JetPal(prgb ,kncol)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE JetPal  ***
    !!
    !! ** Purpose :   Define a palette similar to matlab jetpal pallette
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: prgb  ! (3,NBOXMAX)
    INTEGER(KIND=4),               INTENT(in) :: kncol

    INTEGER(KIND=4)                  :: ji
    INTEGER(KIND=4)                  :: ix, ix2
    REAL(KIND=4), DIMENSION(NBOXMAX) :: zx, zy, ze
    !!----------------------------------------------------------------------
    IF ( kncol > NBOXMAX ) THEN
       PRINT *,'ERROR in JetPal : Too many colors.  Possible maxi is :',NBOXMAX
       STOP
    ENDIF

    ix=NINT(kncol/4.)

    ix2=ix/2
    ! initialisation of working arrays
    zx(1:ix ) = (/ (float(ji      )/ix, ji=1,ix ) /)
    zy(1:ix2) = (/ (float(ji+ix2-1)/ix, ji=1,ix2) /)
    ze(1:ix ) = (/ (  1.              , ji=1,ix ) /)
     
    ! Filling Red values
    prgb(1 , 1         :ix2+ix   ) = 0.
    prgb(1 , ix2+ix+1  :ix2+2*ix ) = zx (1  :ix               )
    prgb(1 , ix2+2*ix+1:ix2+3*ix ) = ze (1  :ix               )
    prgb(1 , ix2+3*ix+1:kncol    ) = zy (ix2:kncol-ix2-3*ix:-1)

    ! Filling Green values
    prgb(2 , 1         :ix2      ) = 0.
    prgb(2 , ix2+1     :ix2+ix   ) = zx (1  : ix2             )
    prgb(2 , ix2+ix+1  :ix2+2*ix ) = ze (1  : ix              )
    prgb(2 , ix2+2*ix+1:ix2+3*ix ) = zx (ix : 1            :-1)
    prgb(2 , ix2+3*ix+1:kncol    ) = 0.

    ! Filling Blue values
    prgb(3 , 1         :ix2      ) = zy (1  : ix2             )
    prgb(3 , ix2+1     :ix2+ix   ) = ze (1  : ix              )
    prgb(3 , ix2+ix+1  :ix2+2*ix ) = zx (ix : 1            :-1)
    prgb(3 , ix2+2*ix+1:kncol    ) = 0.

  END SUBROUTINE JetPal

END MODULE readpal
