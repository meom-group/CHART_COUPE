MODULE modeflim
  !!======================================================================
  !!                     ***  MODULE  modeflim  ***
  !! Compute or read limits for the color bar.
  !!=====================================================================
  !! History : 1.0   !  06-1993 E. Brown     : Original code
  !!         : 7.0   !  11-2010 J.M. Molines : f90 + Dr. norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : see public below
  !!----------------------------------------------------------------------
  USE modcom             
  USE modcolor
  USE util
  USE val_table

  IMPLICIT NONE

  PRIVATE

  PUBLIC  :: WriteLimits   ! used in chart coupe and vectors
  PUBLIC  :: ClrGetMark    ! used in chart coupe
  PUBLIC  :: ClrGetLimits  ! used in chart coupe
  PUBLIC  :: VecGetLimits  ! used in vectors

  PRIVATE :: DefLim        !  limit computation (private)

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE DefLim(pdata_in, bdimg, plimit, knbox, kmethod, pxmin, pxmax)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DefLim  ***
    !!
    !! ** Purpose :  Define limit for the color palette
    !!
    !! ** Method  :  3 methods are available, depending on the kmethod arg.
    !!               set with -clrmet option ( default = 2)
    !!               - method 1 : linear distribution beetween min and max
    !!               - method 2 : Choose limit for maximum contrast
    !!               - method 3 : limit read in an ascii files
    !!                 If  -clrmask option is used, pdata_in is set to spval
    !!               when original values outbound the limits.
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pdata_in ! input data field.
    TYPE( bimgfile ),             INTENT(in)    :: bdimg    ! 
    REAL(KIND=4), DIMENSION(:),   INTENT(out)   :: plimit   ! limit (knbox+1) in
    !                                                       ! data units.
    INTEGER(KIND=4),               INTENT(in)   :: knbox    ! number of colors
    INTEGER(KIND=4),              INTENT(inout) :: kmethod  ! method to use
    !                                                       ! 1, 2 or 3
    REAL(KIND=4),                  INTENT(in)   :: pxmin, pxmax ! palette extrema
    ! local
    INTEGER(KIND=4),PARAMETER   :: jpdiscr = 2000   ! size of the histogram for 
    !                                               ! for clrmet 2
    INTEGER(KIND=4)             :: inx, iny         ! integer wrk variables

    INTEGER(KIND=4)             :: ji,jj,jnb
    REAL(KIND=4)                :: zdxbox, zdv, zdnva

    ! histogram building
    INTEGER(KIND=4)             :: inv(jpdiscr+1)   ! inventory of points in a box
    INTEGER(KIND=4)             :: inva(jpdiscr+1)  ! cumulated histogram
    REAL(KIND=4)                :: zv(jpdiscr+1)
    INTEGER(KIND=4)             :: ihb              ! index of histogram box
    INTEGER(KIND=4)             :: ihb0, ihb1, ihb2

    REAL(KIND=4)                :: zspv
    INTEGER(KIND=4)             :: inumlim
    INTEGER(KIND=4)             :: ilu = 88
    INTEGER i,j
    !!----------------------------------------------------------------------

    inx  = bdimg%nxdata
    iny  = bdimg%nydata
    zspv = bdimg%spval

    PRINT *,'  VMAX= ', pxmax
    PRINT *,'  VMIN= ', pxmin

    IF (pxmin == pxmax) THEN
       PRINT *,'  Constant values,  -clrmet 1 imposed'
       kmethod=1
    ENDIF

    SELECT CASE ( kmethod )
       !---------------------------------------------------------------------
    CASE ( 1 )                           ! Method 1 : Linear interpolation
       !---------------------------------------------------------------------
       CALL flush(6)
       zdxbox=(pxmax-pxmin)/knbox

       DO ji=1,knbox+1
          plimit(ji)=pxmin+(ji-1)*zdxbox
       ENDDO

       IF ( opt_msk  == 1 ) THEN   ! set outbound values to spval
          WHERE ( pdata_in(1:inx,1:iny) > plimit(knbox+1) .OR. &
               &    pdata_in(1:inx,1:iny) < plimit(1) ) pdata_in(1:inx,1:iny) = zspv
       ENDIF

       !---------------------------------------------------------------------
    CASE ( 2 )                          ! Method 2 : Equalize
       !---------------------------------------------------------------------
       CALL flush(6)
       zdv=(pxmax-pxmin)/jpdiscr

       inv(:)  = 0
       inva(:) = 0

       DO ji=1,jpdiscr+1
          zv(ji)=(ji-1)*zdv+pxmin   ! set the jpdiscr limits
       ENDDO

       IF ( opt_msk  == 1 ) THEN   ! set outbound values to spval
          WHERE ( pdata_in(1:inx,1:iny) > pxmax  .OR. &
               &    pdata_in(1:inx,1:iny) < pxmin ) pdata_in(1:inx,1:iny) = zspv
       ENDIF

       DO jj=1,iny
          DO ji=1,inx
             IF ((pdata_in(ji,jj) /= zspv).AND. &
                  &             (pdata_in(ji,jj) <= pxmax).AND. &
                  &             (pdata_in(ji,jj) > pxmin)) THEN
                ihb=INT((pdata_in(ji,jj)-pxmin)/zdv)+1
                ihb=MIN(ihb,jpdiscr)
                inv(ihb)=inv(ihb)+1
             ENDIF
          ENDDO
       ENDDO

       !     cumulating histogram
       DO i=2,jpdiscr+1
          inva(i) = inva(i-1)+inv(i-1)
       ENDDO

       ! We want to set the limits so that each color box has the same
       ! number of pixels: (zdnva)
       zdnva=FLOAT(inva(jpdiscr+1))/knbox
       ihb1=0 ;  ihb0=1   ! initialization

       ! enforce extrema of the palette
       plimit(1)=pxmin  ;   plimit(knbox+1)=pxmax

       DO jnb=1,knbox-1
          ihb1=ihb1+zdnva

          ihb=ihb0
          DO WHILE ( inva(ihb) < ihb1 )
             ihb=ihb+1
          ENDDO
          ihb0=ihb-1 

          ! linear interpolation to retrieve data limit

          ihb2=ihb0+1

          ! eventually skip empty histogram boxes 
          DO WHILE ( inva(ihb2) == inva(ihb0) .AND. ihb2 < jpdiscr + 1 ) 
             ihb2=ihb2+1
          END DO

          IF ( ihb2 == jpdiscr + 2 ) STOP 'in DefLim ' ! likely to never happen

          plimit(jnb+1) = zv(ihb0)+(ihb1-inva(ihb0))*(zv(ihb2)-zv(ihb0))/ &
               &                     (inva(ihb2)-inva(ihb0))
       ENDDO

       !---------------------------------------------------------------------
    CASE ( 3 )                          ! Method 3 : Forced limits
       !---------------------------------------------------------------------
       CALL flush(6)
       inumlim = 0

       OPEN(ilu,file=cf_clrlim,form='formatted')
       DO ji=1,knbox+1
          READ(ilu, *, END=22) plimit(ji)
       ENDDO
22     CONTINUE
       CLOSE(ilu)

       inumlim=ji-1

       IF ( opt_msk  == 1 ) THEN   ! set outbound values to spval
          WHERE ( pdata_in(1:inx,1:iny) > plimit(knbox+1) .OR. &
               &    pdata_in(1:inx,1:iny) < plimit(1) ) pdata_in(1:inx,1:iny) = zspv
       ENDIF

       IF (inumlim < knbox+1) THEN
          PRINT *,' ERROR in DefLim : less limits in file than  colors in the palette.'
          PRINT *,'     Give a palette with ',inumlim -1,' colors'
          STOP
          !            knbox = i-1
       ENDIF

       !---------------------------------------------------------------------
    CASE DEFAULT                        ! not a valid method 
       !---------------------------------------------------------------------
       PRINT *,'Error in DefLim : this method (',kmethod,' ) is unknown'
       STOP 
    END SELECT

  END SUBROUTINE DefLim


  SUBROUTINE WriteLimits (cdfilename, plimit, klim)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE WriteLimits  ***
    !!
    !! ** Purpose :  Write found limits to an ASCII file, than can be read
    !!              as input with -clrlim option (met=3) 
    !!
    !! ** Method  :  This action is taken when -clrlout file is used
    !!
    !! References :  
    !!----------------------------------------------------------------------

    CHARACTER(LEN=*),           INTENT(in) :: cdfilename ! output file name
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: plimit     ! limit array
    INTEGER(KIND=4),            INTENT(in) :: klim       ! number of limit to write

    INTEGER(KIND=4) :: ji
    INTEGER(KIND=4) :: ilu = 88 ! logical unit
    !!----------------------------------------------------------------------

    OPEN(ilu,file=cdfilename,form='formatted')
    DO ji=1,klim
       WRITE(ilu,'(e15.5)') plimit(ji)
    ENDDO
    CLOSE(ilu)

  END SUBROUTINE WriteLimits


  SUBROUTINE ClrGetMark()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ClrGetMark  ***
    !!
    !! ** Purpose : Read vclrmark in ascii file. Option -clrmark
    !!
    !! ** Method  : All variables are in modcom module
    !!
    !! References :  
    !!----------------------------------------------------------------------
    INTEGER(KIND=4) :: ilu = 88  ! logical unit
    !!----------------------------------------------------------------------
    OPEN(ilu ,file=cf_clrmark,form='formatted')
    nmark = 1
    DO 
       READ(ilu ,*,END=22) vclrmark(nmark)
       nmark = nmark+1
       IF ( nmark >  NCLRMARK ) THEN
          PRINT *, 'Error in ClrGetMark : Too many marks in file ', TRIM(cf_clrmark)
          PRINT *, '  Increase NCLRMARK. (Actual value is )', NCLRMARK
          STOP 
       ENDIF
    END DO
22  CONTINUE
    CLOSE(ilu )
    nmark = nmark -1
    IF (nmark < 2 ) THEN
       PRINT *, 'Error in ClrGetMark : At least 2 values are required in ', TRIM(cf_clrmark)
       STOP 
    ENDIF

  END SUBROUTINE ClrGetMark


  SUBROUTINE ClrGetLimits (pclrdata, bdimgclr, plimit, kncol)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ClrGetLimits  ***
    !!
    !! ** Purpose : Main routine to compute color limits
    !!
    !! ** Method  : set parameters and call DefLim
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pclrdata ! input color data
    TYPE( bimgfile ),                INTENT(in) :: bdimgclr ! data structure
    REAL(KIND=4), DIMENSION(:),     INTENT(out) :: plimit   ! limits size NBOXMAX +1
    INTEGER(KIND=4),                 INTENT(in) :: kncol    ! number of colors

    REAL(KIND=4)  :: zvmax, zvmin
    !!----------------------------------------------------------------------

    CALL FindMinMax (bdimgclr, pclrdata, zvmin, zvmax)

    IF (opt_min == 0) cl_min = zvmin         
    IF (opt_max == 0) cl_max = zvmax

    IF (opt_clrmark  ==  1 ) THEN
       cl_min = vclrmark(1)
       cl_max = vclrmark(nmark)
    ENDIF

    CALL SetClrMin (zvmin)
    CALL SetClrMax (zvmax)

    CALL DefLim ( pclrdata, bdimgclr, plimit, kncol, cl_met, cl_min, cl_max)

  END SUBROUTINE ClrGetLimits


  SUBROUTINE VecGetLimits (pvecdata, bdimgvec, plimit, kncol)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE VecGetLimits  ***
    !!
    !! ** Purpose : Compute the color limits for vectors
    !!
    !! ** Method  : Set parameters and call DefLim
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pvecdata ! vector data
    TYPE( bimgfile ),                INTENT(in) :: bdimgvec
    REAL(KIND=4), DIMENSION(:),     INTENT(out) :: plimit   ! limits (NBOXMAX +1)
    INTEGER(KIND=4),                 INTENT(in) :: kncol

    REAL(KIND=4)                :: zvmax, zvmin
    !!----------------------------------------------------------------------

    CALL FindMinMax (bdimgvec, pvecdata, zvmin, zvmax)

    IF (opt_min == 0) cl_min = zvmin         
    IF (opt_max == 0) cl_max = zvmax

    IF (opt_clrmark  ==  1 ) THEN
       cl_min = vclrmark(1)
       cl_max = vclrmark(nmark)
    ENDIF

    zvmin=cl_min
    zvmax=cl_max

    IF (rvv_vlc  ==  0 ) rvv_vlc = zvmin
    IF (rvv_vhc  ==  0 ) rvv_vhc = zvmax

    CALL SetVecMin (zvmin)
    CALL SetVecMax (zvmax)

    CALL DefLim (pvecdata, bdimgvec, plimit, kncol, nvv_met, rvv_vlc, rvv_vhc)
  END SUBROUTINE VecGetLimits

END MODULE modeflim
