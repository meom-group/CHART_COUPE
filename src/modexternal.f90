MODULE modexternal
  !!======================================================================
  !!                     ***  MODULE  modexternal  ***
  !!  external functions that are used as argument to arscam /ardrln
  !!  and cpcldm which deals with area map masking.
  !!=====================================================================
  !! History : <ver  !  date     name      action 
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !!  colcontinents : used for coloring the continents
  !!  colocean      : used for coloring ocean values
  !!  colcontours   : used for coloring the contours 
  !!  colbathy      : used for coloring the bathy in sections
  !!  drwbathy      : used to draw the bathy limit
  !!  drawcl        : used to draw the contour lines on the area map
  !!----------------------------------------------------------------------
  USE modcom
  USE modcolor

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: colcontinents !  used is tracecol as argument of arscam
  PUBLIC :: colocean      !  used in tracecol as argument of arscam
  PUBLIC :: colcontours   !  used in tracecol as argument of arscam
  PUBLIC :: colbathy      !  used in coupe as argument of arscam
  PUBLIC :: drwbathy      !  used in coupe as argument of ardrln
  PUBLIC :: drawcl        !  used in isocontour as argument of cpcldm

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE colcontinents (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE colcontinents  ***
    !!
    !! ** Purpose : COLor CONTinents : Draw the continent with a 
    !!          dedicated color 
    !!
    !! ** Method  : This routine must follow the specif for ARSCAM  
    !!
    !! References : Manual pages for ARSCAM 
    !!----------------------------------------------------------------------
    REAL(KIND=4),    DIMENSION(kncs), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4), DIMENSION(knai), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                  INTENT(in) :: kncs, knai

    INTEGER(KIND=4) :: ji, iai1, iairef
    !!----------------------------------------------------------------------
    IF (opt_high == 0 ) THEN  ! use high resolution data base
       iairef = 2
    ELSE
       iairef = 1032
    ENDIF

    iai1=-1
    DO ji=1,knai
       IF (kiag(ji) == 1) THEN 
          iai1=kiai(ji)
       ENDIF
    END DO
    ! iai1= 2 = ocean 
    ! iai1= index area du pays, si positive voir DOC NCAR V2.00 p 271-286

    IF (iai1 > 0) THEN
       IF (iai1 /= iairef) THEN
          CALL gsfaci(COLOR_CONTINENT)
          CALL gfa(kncs-1,pxcs,pycs)
       ENDIF
    ENDIF

  END SUBROUTINE colcontinents


  SUBROUTINE colocean (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE colocean  ***
    !!
    !! ** Purpose :  Draw the ocean with a different color from background 
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    DIMENSION(kncs), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4), DIMENSION(knai), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                  INTENT(in) :: kncs, knai

    INTEGER(KIND=4) :: ji, iai1, iairef
    !!----------------------------------------------------------------------
    IF (opt_high == 0 ) THEN
       iairef = 2
    ELSE
       iairef = 1032
    ENDIF

    iai1=-1
    DO  ji=1,knai
       IF (kiag(ji) == 1) THEN 
          iai1=kiai(ji)
       ENDIF
    END DO

    ! iai1= 2 = ocean 

    IF (iai1 == iairef) THEN
       CALL gsfaci(COLOR_OCEAN)
       CALL gfa(kncs-1, pxcs, pycs)
    ENDIF
  END SUBROUTINE colocean


  SUBROUTINE colbathy (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE colbathy  ***
    !!
    !! ** Purpose : Used in a similar way to colcontinents, the role played
    !!          by the continent being played here by the bottom topography
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    DIMENSION(kncs), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4), DIMENSION(knai), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                  INTENT(in) :: kncs, knai

    INTEGER(KIND=4) ::  ji, iai1
    !!----------------------------------------------------------------------

    iai1=-1
    DO  ji=1,knai
       IF (kiag(ji) == 15) THEN
          iai1=kiai(ji)
       ENDIF
    END DO

    IF (iai1  > 0) THEN
       IF (iai1 == 3) THEN
          CALL gsfaci(COLOR_CONTINENT)
          CALL gsplci(COLOR_CONTINENT_PERIM)
          CALL gfa(kncs-1, pxcs, pycs)
          CALL gsplci(1)    ! set polyline color index
       ENDIF
    ENDIF
  END SUBROUTINE colbathy


  SUBROUTINE colcontours (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE colcontours  ***
    !!
    !! ** Purpose :  used to draw colored contours 
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    DIMENSION(kncs+1), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4), DIMENSION(knai  ), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                    INTENT(in) :: kncs, knai

    INTEGER(KIND=4)  :: ji, iai1, iaiperim
    !!----------------------------------------------------------------------

    iai1=-1
    DO  ji=1,knai
       IF (kiag(ji) == 3) THEN
          iai1=kiai(ji)
       ELSEIF (kiag(ji) == 15) THEN
          iaiperim = kiai(ji)
       ENDIF
    ENDDO


    IF ( iai1 > 0 .AND. iaiperim /= 31 ) THEN
       CALL gsfaci(iai1)
       CALL gfa(kncs-1, pxcs, pycs)
    ENDIF

  END SUBROUTINE colcontours


  SUBROUTINE drwbathy (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE drwbathy  ***
    !!
    !! ** Purpose : Draw the limit of the bathymetry with a special color  
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),  DIMENSION(kncs+1), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4), DIMENSION(knai), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                  INTENT(in) :: kncs, knai

    INTEGER(KIND=4) ::  ji, iai1
    !!----------------------------------------------------------------------

    iai1=-1
    DO  ji=1,knai
       IF (kiag(ji) == 15) THEN
          CALL gsplci(COLOR_CONTINENT_PERIM)
          CALL curved (pxcs, pycs, kncs)
          CALL gsplci(1)    ! set polyline color index
       ENDIF
    ENDDO

  END SUBROUTINE drwbathy


  SUBROUTINE drawcl (pxcs, pycs, kncs, kiai, kiag, knai)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE drawcl  ***
    !!
    !! ** Purpose :  Draw contour lines masked my the area map 
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    DIMENSION(kncs+1), INTENT(in) :: pxcs ,pycs
    INTEGER(KIND=4),   DIMENSION(knai), INTENT(in) :: kiai ,kiag
    INTEGER(KIND=4),                    INTENT(in) :: kncs, knai

    REAL(KIND=4)     :: zmap_coef_x, zmap_coef_y, zmap_coef_z
    REAL(KIND=4)     :: zxc0, zyc0, zzc0
    REAL(KIND=4)     :: zlat, zlon, zdep
    REAL(KIND=4)     :: zrl, zrr, zrb, zrt, zur, zul, zut, zub
    REAL(KIND=4)     :: zclv

    INTEGER(KIND=4)  :: ji, il, idr
    INTEGER(KIND=4)  :: ilu=95      ! logical unit for cntsave
    !!----------------------------------------------------------------------
    idr = 1
    DO ji=1,knai
       IF (kiai(ji) < 0 ) idr = 0
    ENDDO

    IF (opt_dash == 1) THEN   ! use dash line for negative contour_value (zclv)
       CALL CPGETR('CLV - contour level',zclv )
       IF (zclv < 0.) THEN
          CALL dashdb(52428)
       ELSE
          CALL dashdb(65535)
       ENDIF
    ENDIF

    IF (idr /= 0) CALL curved (pxcs, pycs, kncs)

    IF (opt_cntsav == 1) THEN  
       ! save the contour line in a file for offline posterior use

       IF ( opt_chart == 1 ) THEN  ! different behaviour in chart and coupe
          !  pxcs, pycs are in plotter unit. We want them in user unit :
          CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, il)
          zmap_coef_x=(rmap_marg(2)-rmap_marg(1))/(zrr - zrl)
          zmap_coef_y=(rmap_marg(4)-rmap_marg(3))/(zrt - zrb)

          zxc0=rmap_marg(1) - zmap_coef_x * zrl
          zyc0=rmap_marg(3) - zmap_coef_y * zrb

          OPEN(ilu, file=cf_cntsav)
          WRITE(ilu,'(a)')'# Longitude, Latitude'
          WRITE(ilu,*) 9999., 9999.  ! to mark the end of a segment
          DO ji=1,kncs
             zlon=pxcs(ji)*zmap_coef_x + zxc0
             zlat=pycs(ji)*zmap_coef_y + zyc0
             WRITE(ilu,*) zlon, zlat 
          ENDDO

       ELSE IF (opt_coupe == 1 ) THEN
          !  pxcs, pycs are in plotter unit. We want them in user unit :
          CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, il)
          zmap_coef_x=(rmap_marg(2)-rmap_marg(1))/(zrr - zrl)
          zmap_coef_y=(rmap_marg(4)-rmap_marg(3))/(zrr - zrl)
          zmap_coef_z=(prof_min    - prof_max   )/(zrt - zrb)

          zxc0=rmap_marg(1) - zmap_coef_x * zrl
          zyc0=rmap_marg(3) - zmap_coef_y * zrl
          zzc0=prof_max     - zmap_coef_y * zrb

          OPEN(ilu, file=cf_cntsav)
          WRITE(ilu,'(a)')'# Along_section, depth'
          WRITE(ilu,*) 9999., 9999.  ! to mark the end of a segment
          DO ji=1,kncs
             zlon=pxcs(ji)*zmap_coef_x + zxc0
             zlat=pxcs(ji)*zmap_coef_y + zyc0
             zdep=pycs(ji)*zmap_coef_z + zzc0
             WRITE(ilu,*) zlon, zlat, zdep
          ENDDO
       ENDIF
    ENDIF

  END SUBROUTINE drawcl


END MODULE modexternal
