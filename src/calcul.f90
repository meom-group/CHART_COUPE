MODULE calcul
  !!======================================================================
  !!                     ***  MODULE  calcul   ***
  !! This module hold all subroutines dedicated to calculation required
  !! besides the plotting functions.
  !!=====================================================================
  !! History : 1.0  ! 06/1993   ! E. Brown     : original code
  !!           4.0  ! 06/1996   ! J.M. Molines : add heading and distance
  !!           7.0  ! 11/2010   ! J.M. Molines : F90 and Doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : see below
  !!----------------------------------------------------------------------
  USE modcom

  IMPLICIT NONE

  PRIVATE

  PUBLIC ::  CalculateCutHeading  ! 
  PUBLIC ::  CalculateCutDistance ! 
  PUBLIC ::  interp               ! interpolation on a finer grid
  
  PRIVATE :: dist                 ! compute orthodromic distance between 2 points
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE interp(pmap_out, pmap_in, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE interp  ***
    !!
    !! ** Purpose : Interpolate a coarse grid on a finer grid
    !!
    !! ** Method  : Bilinear interpolation is used. The input data has the
    !!              size of the data on file 
    !!              
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: pmap_out ! interpolated grid
    REAL(KIND=4), DIMENSION(:,:), INTENT(in ) :: pmap_in  ! coarse data in
    TYPE( bimgfile ),           INTENT(inout) :: bdimg    ! data structure

    INTEGER(KIND=4) ::  jil, jjl                ! dummy loop index
    INTEGER(KIND=4) ::  inx, iny, ioldnx, ioldny
    INTEGER(KIND=4) ::  ii, ij
    REAL(KIND=4)    ::  zcof, zspv
    REAL(KIND=4)    ::  zxii, zyij, zx, zy
    REAL(KIND=4)    ::  zxfrac, zx1frac, zyfrac, zy1frac
    REAL(KIND=4)    ::  za, zb, zc, zd
    !!----------------------------------------------------------------------

    inx  = bdimg%nxdata
    iny  = bdimg%nydata
    zspv = bdimg%spval

    IF ( lo_debug ) THEN
       PRINT *,'#INTERP  BEGIN  nxdta  nydta  ', inx, iny
       PRINT *,'#INTERP         nxfile nyfile ', bdimg%nxfile, bdimg%nyfile
    ENDIF

    CALL flush(6)
    ! 
    zcof = MIN ((float(NXX-1)/float(inx-1)), (float(NYY-1)/float(iny-1)))

    ioldnx = inx
    ioldny = iny

    inx = INT((float(inx-1)*zcof)) + 1
    iny = INT((float(iny-1)*zcof)) + 1

    DO jil  = 1, inx
       zx   = 1. + float(jil-1)/zcof
       ii   = INT(zx)
       zxii = float(ii)

       DO jjl  = 1, iny
          zy   = 1. + float(jjl-1)/zcof
          ij   = INT(zy)
          zyij = float (ij)
          ! interpolation is valid only if used pmap_in are valid, and inside the data domain
          IF ( ( (pmap_in(ii  ,ij  ) == zspv)                      )       .OR. &
             & ( (pmap_in(ii+1,ij  ) == zspv) .AND. (ii /= ioldnx) )       .OR. &
             & ( (pmap_in(ii  ,ij+1) == zspv) .AND. (ij /= ioldny) )       .OR. &
             & ( (pmap_in(ii+1,ij+1) == zspv) .AND. (ii /= ioldnx).AND. (ij /= ioldny) ) ) THEN
                 pmap_out(jil,jjl) = zspv
          ELSE
             zxfrac  = zx - zxii
             zx1frac = 1. - zxfrac                 
             zyfrac  = zy - zyij
             zy1frac = 1. - zyfrac

             za = zxfrac  * zyfrac  
             zb = zx1frac * zyfrac  
             zc = zx1frac * zy1frac
             zd = zxfrac  * zy1frac

             pmap_out(jil,jjl) = za*pmap_in(ii+1,ij+1) + &
             &                   zb*pmap_in(ii  ,ij+1) + &
             &                   zc*pmap_in(ii  ,ij  ) + &
             &                   zd*pmap_in(ii+1,ij  )

          ENDIF
       ENDDO
    ENDDO

    bdimg%nxdata = inx
    bdimg%nydata = iny

    IF (lo_debug) PRINT *,'#INTERP END: nxdta nydta', inx, iny

  END SUBROUTINE interp


  SUBROUTINE CalculateCutHeading( pmap_coord )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CalculateCutHeading  ***
    !!
    !! ** Purpose :  Compute the geographical direction of a transect
    !!
    !! ** Method  :  Elementary Trigonometry 
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),DIMENSION(:), INTENT(in) :: pmap_coord  ! transect limit (4 real)

    REAL(KIND=4)  ::  zxa, zxb, zya, zyb, zpi     ! real working space
    REAL(KIND=4)  ::  zzz     ! real working space
    !!----------------------------------------------------------------------

    zpi=ACOS(-1.)   ! something like 3.14159...

    zxa=pmap_coord(1)*zpi/180.
    zxb=pmap_coord(2)*zpi/180.
    zzz=TAN((45-pmap_coord(3)/2.)*zpi/180.)
    IF ( zzz > 0. ) THEN 
      zya=-alog(zzz)
    ELSE
      zya=0.
    ENDIF

    zzz=TAN((45-pmap_coord(4)/2.)*zpi/180.)
    IF ( zzz > 0. ) THEN 
      zyb=-alog(zzz)
    ELSE
      zyb=0.
    ENDIF

    angled=ATAN2((zxb-zxa),(zyb-zya))
    angled=angled*180./zpi

    IF (angled  < 0) angled=angled+360.

    PRINT '(a,f5.1)',' Transect orientation: N',angled

  END SUBROUTINE CalculateCutHeading

 
  SUBROUTINE CalculateCutDistance(pmap_coord, kpoints)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CalculateCutDistance  ***
    !!
    !! ** Purpose :  Compute the length of a transect leg
    !!
    !! ** Method  :  Use distance along orthodromy. This routine is used
    !!             when graduations in km is required. The distance in the
    !!             the sum od the distances between interpolated points
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: pmap_coord ! transect position (4 real)
    INTEGER(KIND=4),            INTENT(in) :: kpoints    ! number of points

    INTEGER(KIND=4) :: ji
    REAL(KIND=8)    :: zdx0, zdx1, zdy0, zdy1, zddx, zddy
    !!----------------------------------------------------------------------
    cut_dist=0.

    zddx=DBLE((pmap_coord(2)-pmap_coord(1)))/( kpoints-1)
    zddy=DBLE((pmap_coord(4)-pmap_coord(3)))/( kpoints-1)
    IF ( lo_debug ) PRINT *,'#CCD Compute distance  ...'

    zdx0=pmap_coord(1)
    zdy0=pmap_coord(3)

    DO ji=2, kpoints
       zdx1=pmap_coord(1) + (ji-1) * zddx
       zdy1=pmap_coord(3) + (ji-1) * zddy

       cut_dist=cut_dist + dist(zdx0,zdx1,zdy0,zdy1)

       zdx0=zdx1
       zdy0=zdy1
    ENDDO

    PRINT '(a,f7.1,a)',' Transect is ',cut_dist,' km long.'
  END SUBROUTINE CalculateCutDistance


  REAL(KIND=8) FUNCTION dist(plona, plonb, plata, platb)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION dist  ***
    !!
    !! ** Purpose : Compute othodromic distance between 2 points
    !!            used in CalculateCutDistance.
    !!
    !! ** Method  :  Othodromic formula, double precision needed.  
    !!             Distance is given in km 
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=8), INTENT(in) :: plata, plona  ! position of point A
    REAL(KIND=8), INTENT(in) :: platb, plonb  ! position of point B

    REAL(KIND=8)  ::  zlatar, zlatbr, zlonar, zlonbr ! in radians
    REAL(KIND=8)  ::  zpi                            ! 3.14
    REAL(KIND=8)  ::  zpds        
    REAL(KIND=8)  ::  zux, zuy, zuz, zvx, zvy, zvz, zconv
    REAL(KIND=8)  ::  zearth_radius
    !!----------------------------------------------------------------------
    zpi           = dacos(-1.d0)
    zconv         = zpi/180.
    zearth_radius = (6378.137+6356.7523)/2.0 ! Earth radius in km

    zlatar=plata*zconv 
    zlatbr=platb*zconv

    zlonar=plona*zconv
    zlonbr=plonb*zconv

    IF (zlonar /= zlonbr) THEN
       !
       zux=dcos(zlonar)*dcos(zlatar)
       zuy=dsin(zlonar)*dcos(zlatar)
       zuz=dsin(zlatar)
       !
       zvx=dcos(zlonbr)*dcos(zlatbr)
       zvy=dsin(zlonbr)*dcos(zlatbr)
       zvz=dsin(zlatbr)
       zpds=zux*zvx+zuy*zvy+zuz*zvz
       IF (zpds > 1.d0) THEN
          dist=0.
       ELSE
          dist=zearth_radius*dacos(zpds)
       ENDIF
    ELSE
       dist=zearth_radius*(zlatbr-zlatar)
    ENDIF

  END FUNCTION dist

END MODULE calcul
