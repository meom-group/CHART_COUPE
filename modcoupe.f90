MODULE modcoupe
  !!======================================================================
  !!                     ***  MODULE  modcoupe  ***
  !! All routines need to perform a section across the data. Formely thos
  !! routines where within the coupe program.
  !!=====================================================================
  !! History : 7.O  ! 12/2010  ! J.M. Molines : migration from coupe
  !!                                          : F90 + doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!      CoupeInitNCAR
  !!      AddGrid
  !!      CalculateCutPlane
  !!      ColorCoupe
  !!      CreateBathy
  !!      NearestPoint
  !!----------------------------------------------------------------------
  USE modcom 
  USE modmapncar
  USE modvaltable
  USE readbimg
  USE modcolor
  USE modexternal
  USE checkfiles
  USE isocontours, ONLY : ColorContour
  USE tracecol,    ONLY : AddPalette

  IMPLICIT NONE

  PRIVATE

  INTEGER(KIND=4) :: ii0(3,NXX), jj0(3,NXX)              ! local module variable
  REAL(KIND=4)    :: alfan(NXX), alfa1(NXX), alfa2(NXX)  ! local module variable

  PUBLIC  :: CoupeInitNCAR
  PUBLIC  :: AddGrid
  PUBLIC  :: CalculateCutPlane
  PUBLIC  :: ColorCoupe
  PUBLIC  :: CreateBathy

  PRIVATE :: NearestPoint

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE CoupeInitNCAR(bdimgclr, bdimgcnt)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CoupeInitNCAR  ***
    !!
    !! ** Purpose :   Initialize NCAR for section plots
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimgclr
    TYPE( bimgfile ), INTENT(inout) :: bdimgcnt

    INTEGER(KIND=4)                :: ji
    INTEGER(KIND=4)                :: ip, iimin, ijmin, iquadran
    INTEGER(KIND=4)                :: iasf(13)
    REAL(KIND=4)                   :: zdx, zdy
    REAL(KIND=4)                   :: zxn, zyn, zxx, zyy, zx(3), zy(3)
    REAL(KIND=4)                   :: zaire2
    REAL(KIND=4), DIMENSION(5)     :: zxperim = (/0.0, 1.0, 1.0, 0.0, 0.0/)  !
    REAL(KIND=4), DIMENSION(5)     :: zyperim = (/0.0, 0.0, 1.0, 1.0, 0.0/)  !
    !!----------------------------------------------------------------------
    iasf(:) = 1

    CALL gsclip(1)
    CALL gsasf (iasf)

    !  text color and text quality
    CALL pcsetc ('FC','|'     )   ! set control character to |
    CALL pcsetc ('FN', c_font )   ! font
    CALL pcseti ('CC(0)',1    )   ! character color index
    CALL gaseti ('LTY',1      )   ! use plcchq to draw labels

    CALL gsplci (1)               ! set polyline color index
    CALL gsfais (1)               ! set polyline fill interior style

    CALL arinam (niama, jp_iama)
    CALL aredam (niama, zxperim, zyperim, 5, 15, 30, 31 )            
    !
    ! Prepare data in case of irregular grid
    IF (bdimgclr%ngrid  ==  3 .OR. bdimgcnt%ngrid  ==  3 ) THEN

       !  section starting point
       iimin = 0
       ijmin = 0

       CALL NearestPoint(rmap_coord(1), rmap_coord(3), iimin, ijmin, iquadran)
       zdx = (rmap_coord(2) - rmap_coord(1) ) / (NXX-1)
       zdy = (rmap_coord(4) - rmap_coord(3) ) / (NXX-1)  ! NXX yes yes yes  !!!

       DO ji=1,NXX 
          zxx = rmap_coord(1) + (ji -1)*zdx
          zyy = rmap_coord(3) + (ji -1)*zdy

          CALL NearestPoint(zxx, zyy, iimin, ijmin, iquadran)
          zxn = xygr(iimin,ijmin,1)
          zyn = xygr(iimin,ijmin,2)

          SELECT CASE ( iquadran  )
           CASE ( 1 )
             zx(1)     = xygr(iimin+1, ijmin,1)
             zy(1)     = xygr(iimin+1, ijmin,2)
             ii0(1,ji) = iimin + 1
             jj0(1,ji) = ijmin

             zx(2)     = xygr(iimin, ijmin+1,1)
             zy(2)     = xygr(iimin, ijmin+1,2)
             ii0(2,ji) = iimin
             jj0(2,ji) = ijmin + 1

           CASE ( 2 )
             zx(1)     = xygr(iimin, ijmin+1,1)
             zy(1)     = xygr(iimin, ijmin+1,2)
             ii0(1,ji) = iimin
             jj0(1,ji) = ijmin + 1

             zx(2)     = xygr(iimin-1, ijmin+1,1)
             zy(2)     = xygr(iimin-1, ijmin+1,2)
             ii0(2,ji) = iimin - 1
             jj0(2,ji) = ijmin + 1

           CASE ( 3 )
             zx(1)     = xygr(iimin-1, ijmin+1,1)
             zy(1)     = xygr(iimin-1, ijmin+1,2)
             ii0(1,ji) = iimin - 1
             jj0(1,ji) = ijmin + 1

             zx(2)     = xygr(iimin-1, ijmin,1)
             zy(2)     = xygr(iimin-1, ijmin,2)
             ii0(2,ji) = iimin - 1
             jj0(2,ji) = ijmin

           CASE ( 4 )
             zx(1)     = xygr(iimin-1, ijmin,1)
             zy(1)     = xygr(iimin-1, ijmin,2)
             ii0(1,ji) = iimin - 1
             jj0(1,ji) = ijmin

             zx(2)     = xygr(iimin, ijmin-1,1)
             zy(2)     = xygr(iimin, ijmin-1,2)
             ii0(2,ji) = iimin
             jj0(2,ji) = ijmin - 1

           CASE ( 5 )
             zx(1)     = xygr(iimin, ijmin-1,1)
             zy(1)     = xygr(iimin, ijmin-1,2)
             ii0(1,ji) = iimin 
             jj0(1,ji) = ijmin - 1

             zx(2)     = xygr(iimin+1, ijmin-1,1)
             zy(2)     = xygr(iimin+1, ijmin-1,2)
             ii0(2,ji) = iimin + 1
             jj0(2,ji) = ijmin - 1

           CASE ( 6 )
             zx(1)     = xygr(iimin+1, ijmin-1,1)
             zy(1)     = xygr(iimin+1, ijmin-1,2)
             ii0(1,ji) = iimin + 1
             jj0(1,ji) = ijmin - 1

             zx(2)     = xygr(iimin+1, ijmin,1)
             zy(2)     = xygr(iimin+1, ijmin,2)
             ii0(2,ji) = iimin + 1
             jj0(2,ji) = ijmin 
          END SELECT

          ii0(3,ji)=iimin
          jj0(3,ji)=ijmin

          zaire2 = zxn*(zy(1)-zy(2)) + zx(1)*(zy(2)-zyn) + zx(2)*(zyn-zy(1))

          alfa1(ji) = ( zxx*(zy(2) - zyn  ) + zx(2)*(zyn   - zyy ) + zxn  * (zyy   - zy(2)) ) /zaire2
          alfa2(ji) = ( zxx*(zyn   - zy(1)) + zx(1)*(zyy   - zyn ) + zxn  * (zy(1) - zyy  ) ) /zaire2
          alfan(ji) = ( zxx*(zy(1) - zy(2)) + zx(1)*(zy(2) - zyy ) + zx(2)* (zyy   - zy(1)) ) /zaire2

          IF (iquadran == 7) THEN
             PRINT *,' Iquadran = 7 for i=',ji
             alfan(ji)=1.
             alfa1(ji)=0.
             alfa2(ji)=0.
             ii0(1,ji)= ii0(3,ji)
             jj0(1,ji)= jj0(3,ji)
             ii0(2,ji)= ii0(3,ji)
             jj0(2,ji)= jj0(3,ji)
          ENDIF
       ENDDO  
    ENDIF ! gridxy

  END SUBROUTINE CoupeInitNCAR


  SUBROUTINE CalculateCutPlane (pcut_plane, bdimginfo, bdimgzlevel, kt, kdim, kopt_scale, pscale, kopt_mean, pmean0, kopt_abs)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CalculateCutPlane  ***
    !!
    !! ** Purpose :  Return the data on the section in pcut_plane
    !!
    !! ** Method  :  Read every level, and interpolate to obtain NXX grid points
    !!               Then perform a vertical interpolation to obtain NYY points
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: pcut_plane      !   (NXX,NYY)
    TYPE( bimgfile ),           INTENT(inout) :: bdimginfo
    TYPE( bimgfile ),           INTENT(inout) :: bdimgzlevel
    INTEGER(KIND=4),               INTENT(in) :: kt
    INTEGER(KIND=4),               INTENT(in) :: kdim
    INTEGER(KIND=2),               INTENT(in) :: kopt_scale
    REAL(KIND=4),                  INTENT(in) :: pscale
    INTEGER(KIND=2),               INTENT(in) :: kopt_mean
    REAL(KIND=4),                  INTENT(in) :: pmean0
    INTEGER(KIND=2),               INTENT(in) :: kopt_abs

    INTEGER(KIND=4)                  :: ifrst = 0
    INTEGER(KIND=4)                  :: ji, jj, jk
    INTEGER(KIND=4)                  :: ip, ilev, inz
    INTEGER(KIND=4)                  :: isrc, jsrc
    REAL(KIND=4), DIMENSION(NXX, NA) :: zx_interp
    REAL(KIND=4), DIMENSION(NXX,NYY) :: zwork 
    REAL(KIND=4), DIMENSION(NA)      :: zdep
    REAL(KIND=4)                     :: zprof, zdz 
    REAL(KIND=4)                     :: zdx, zdy
    REAL(KIND=4)                     :: zxinc, zyinc
    REAL(KIND=4)                     :: zfracx, zfracy, zfracz
    REAL(KIND=4)                     :: zspv
    LOGICAL                          :: ll_goodp
    !!----------------------------------------------------------------------
    inz = bdimginfo%nzfile

    IF (bdimginfo%lspval0) THEN
       zspv = rp_defspval
    ELSE
       zspv = bdimginfo%spval
    ENDIF

    zx_interp (:,1:inz) = zspv
    pcut_plane(:,:    ) = zspv

    !  compute points on each levels
    ilev = 0
    DO jk = 1, bdimginfo%nzfile
       IF (nlevel(jk) == 1) THEN
          ilev = ilev + 1

          !  depth of the levels
          zdep(ilev) = -bdimginfo%depth(jk)
          CALL BimgReadData (zwork, bdimginfo, rmap_coord, kt, jk, kdim, kopt_scale, pscale, &
             &                                                           kopt_mean, pmean0, kopt_abs )
          IF (bdimginfo%lspval0) bdimginfo%spval=rp_defspval

          IF (bdimginfo%ngrid  /=  3 ) THEN 
             zdx = float(bdimginfo%nxdata-1) / float(NXX)
             zdy = float(bdimginfo%nydata-1) / float(NXX) ! oui, oui, NXX
             zxinc = 0.0
             zyinc = 0.0

             DO ji = 1, NXX
                zfracx = zxinc - AINT(zxinc)
                zfracy = zyinc - AINT(zyinc)

                isrc = 1 + INT(zxinc)
                jsrc = 1 + INT(zyinc)

                IF (zfracx == 0) THEN
                   IF ((zwork(isrc,jsrc) == zspv) .OR. (zwork(isrc,jsrc+1) == zspv)) THEN
                      zx_interp(ji,ilev) = zspv
                   ELSE
                      zx_interp(ji,ilev) = zwork (isrc,jsrc+1) *       zfracy + &
                        &                  zwork (isrc,jsrc  ) * (1. - zfracy)
                   ENDIF
                ELSE IF (zfracy == 0) THEN
                   IF ((zwork(isrc,jsrc) == zspv) .OR. (zwork(isrc+1,jsrc) == zspv)) THEN
                      zx_interp(ji,ilev) = zspv
                   ELSE
                      zx_interp(ji,ilev) = zwork (isrc+1,jsrc) *       zfracx + &
                        &                  zwork (isrc  ,jsrc) * (1. - zfracx) 
                   ENDIF
                ELSE
                   IF (     ( zwork(isrc  ,jsrc  ) == zspv).OR. &
                        &   ( zwork(isrc+1,jsrc  ) == zspv).OR. &
                        &   ( zwork(isrc  ,jsrc+1) == zspv).OR. &
                        &   ( zwork(isrc+1,jsrc+1) == zspv)    ) THEN
                      zx_interp(ji,ilev) = zspv
                   ELSE
                      zx_interp(ji,ilev) =  zwork (isrc+1,jsrc+1) *     zfracx  *     zfracy  + &
                        &                   zwork (isrc  ,jsrc+1) * (1.-zfracx) *     zfracy  + &
                        &                   zwork (isrc+1,jsrc  ) *     zfracx  * (1.-zfracy) + &
                        &                   zwork (isrc  ,jsrc  ) * (1.-zfracx) * (1.-zfracy)
                   ENDIF
                ENDIF

                zxinc = zxinc + zdx
                zyinc = zyinc + zdy
             ENDDO

          ELSE

             ! In case of irregular grid (gridxy), interpolation coefficients are 
             ! computed in CoupeInitNCAR.
             ! take care to special values near the boundaries -- to be improved --
             DO ji=1,NXX
                ll_goodp=.TRUE.
                ll_goodp=ll_goodp .AND. (zwork(ii0(3,ji),jj0(3,ji))  /=  bdimginfo%spval)
                ll_goodp=ll_goodp .AND. (zwork(ii0(1,ji),jj0(1,ji))  /=  bdimginfo%spval)
                ll_goodp=ll_goodp .AND. (zwork(ii0(2,ji),jj0(2,ji))  /=  bdimginfo%spval)

                IF (ll_goodp) THEN
                   zx_interp(ji,ilev) = ( alfan(ji)*zwork(ii0(3,ji),jj0(3,ji)) + &
                        &                 alfa1(ji)*zwork(ii0(1,ji),jj0(1,ji)) + &
                        &                 alfa2(ji)*zwork(ii0(2,ji),jj0(2,ji)) ) 
                ELSE
                   zx_interp(ji,ilev) = bdimginfo%spval
                ENDIF
             ENDDO
          ENDIF  ! gridxy
       ENDIF   ! zdep
       !        If find a zdep with all data = spval, then skip remaining zdep
       IF ( SUM(zx_interp(:,ilev)) == NXX*bdimginfo%spval ) EXIT
    ENDDO

    ! vertical interpolation
    IF (opt_sigma == 0) THEN
       zdz   = (prof_max - prof_min) / float(NYY)
       zprof = prof_min 
    ELSE
       zdz   = (zdep(ilev) - zdep(1)) / float(NYY)
       zprof = zdep(1) 
    ENDIF

    ip = 1
    DO jk = NYY, 1,-1
       IF ((zprof <= zdep(1)).OR.(zprof > zdep(ilev))) THEN
          pcut_plane(:,jk) = zspv
       ELSE 
          ip = 1
          DO WHILE ((zprof <= zdep(ip)).OR.(zprof > zdep(ip+1)).AND. (ip <= ilev))
             ip = ip+1
          ENDDO

          IF (ip <= ilev) THEN
             zfracz = (zprof - zdep(ip)) / (zdep(ip+1) - zdep(ip))
          ENDIF

          DO ji = 1,NXX
             IF (     (zx_interp(ji,ip  ) == zspv ) .OR. &
                  &   (zx_interp(ji,ip+1) == zspv ) .OR. &
                  &   (ip == ilev))                 THEN
                pcut_plane (ji,jk) = zspv
             ELSE 
                pcut_plane (ji,jk) = (1.0-zfracz) * zx_interp(ji,ip  ) &
                  &                     + zfracz  * zx_interp(ji,ip+1)
             ENDIF
          ENDDO
       ENDIF
       zprof = zprof + zdz
    ENDDO

    bdimginfo%nxdata = NXX
    bdimginfo%nydata = NYY

    IF (opt_sigma == 1.AND.ifrst == 0) THEN
       !     compute zlevel on each layer
       ALLOCATE ( rys (NXX+1,NYY+1), rzs (NXX,NA ) )
       ifrst=1
       zspv=bdimgzlevel%spval
       ilev = 0
       PRINT *,'ZLEVEL...'

       DO jk = 1, bdimgzlevel%nzfile
          IF (nlevel(jk) == 1) THEN
             ilev = ilev + 1

             ! depth
             zdep(ilev) = -bdimgzlevel%depth(jk)

             bdimgzlevel%lclr = .FALSE. 
             bdimgzlevel%lcnt = .FALSE.
             CALL BimgReadData (zwork, bdimgzlevel, rmap_coord, 1, jk, 1)

             IF (bdimgzlevel%ngrid  /=  3 ) THEN
                zdx   = float(bdimgzlevel%nxdata-1) / float(NXX)
                zdy   = float(bdimgzlevel%nydata-1) / float(NXX) ! oui, oui, NXX
                zxinc = 0.0
                zyinc = 0.0

                DO ji = 1, NXX
                   zfracx = zxinc - AINT(zxinc)
                   zfracy = zyinc - AINT(zyinc)

                   isrc = 1 + INT(zxinc)
                   jsrc = 1 + INT(zyinc)
                   IF (zfracx == 0) THEN
                      IF (    (zwork(isrc,jsrc  ) == zspv)  .OR. &
                          &   (zwork(isrc,jsrc+1) == zspv)) THEN
                         rzs(ji,ilev) = zspv
                      ELSE
                         rzs(ji,ilev) = zwork (isrc,jsrc+1) *     zfracy + &
                          &             zwork (isrc,jsrc  ) * (1.-zfracy)

                      ENDIF
                   ELSEIF (zfracy == 0) THEN
                      IF ((zwork(isrc,jsrc) == zspv) .OR. (zwork(isrc+1,jsrc) == zspv)) THEN
                         rzs(ji,ilev) = zspv
                      ELSE
                         rzs(ji,ilev) = zwork (isrc+1,jsrc) *     zfracx  + &
                          &             zwork (isrc,jsrc  ) * (1.-zfracx) 
                      ENDIF
                   ELSE
                      IF (    (zwork(isrc  ,jsrc  ) == zspv).OR. &
                          &   (zwork(isrc+1,jsrc  ) == zspv).OR. &
                          &   (zwork(isrc  ,jsrc+1) == zspv).OR. &
                          &   (zwork(isrc+1,jsrc+1) == zspv))  THEN
                         rzs(ji,ilev) = zspv
                      ELSE
                         rzs(ji,ilev) = zwork (isrc+1,jsrc+1) *     zfracx  *     zfracy  + &
                          &             zwork (isrc  ,jsrc+1) * (1.-zfracx) *     zfracy  + &
                          &             zwork (isrc+1,jsrc  ) *     zfracx  * (1.-zfracy) + &
                          &             zwork (isrc  ,jsrc  ) * (1.-zfracx) * (1.-zfracy)
                      ENDIF
                   ENDIF

                   zxinc = zxinc + zdx
                   zyinc = zyinc + zdy
                ENDDO
             ELSE
                ! In case of irregular grid (gridxy), interpolation coefficients are 
                ! computed in CoupeInitNCAR
                ! take care to special values near the boundaries -- to be improved --
                DO ji=1,NXX
                   ll_goodp=.TRUE.
                   ll_goodp=ll_goodp .AND. (zwork(ii0(3,ji),jj0(3,ji))  /=  bdimginfo%spval)
                   ll_goodp=ll_goodp .AND. (zwork(ii0(1,ji),jj0(1,ji))  /=  bdimginfo%spval)
                   ll_goodp=ll_goodp .AND. (zwork(ii0(2,ji),jj0(2,ji))  /=  bdimginfo%spval)

                   IF (ll_goodp) THEN
                      rzs(ji,ilev) = alfan(ji)*zwork(ii0(3,ji),jj0(3,ji)) + &
                           &         alfa1(ji)*zwork(ii0(1,ji),jj0(1,ji)) + &
                           &         alfa2(ji)*zwork(ii0(2,ji),jj0(2,ji)) 
                   ELSE
                      rzs(ji,ilev) = zspv
                   ENDIF
                ENDDO
             ENDIF  ! gridxy
          ENDIF
       ENDDO

       ! vertical interpolation
       zdz   = (zdep(ilev) - zdep(1)) / float(NYY)
       zprof = zdep(1) 

       ip = 1
       DO jk = NYY, 1,-1
          IF ( (zprof <= zdep(1)) .OR. (zprof > zdep(ilev)) ) THEN
             rys(:,jk) = zspv
          ELSE 
             ip = 1
             DO WHILE (( (zprof <= zdep(ip)) .OR. (zprof > zdep(ip+1))) .AND. (ip <= ilev))
                ip = ip+1
             ENDDO

             IF (ip <= ilev) THEN
                zfracz = (zprof - zdep(ip)) / (zdep(ip+1) - zdep(ip))
             ENDIF

             DO ji = 1,NXX
                IF (    (rzs(ji,ip  ) == zspv) .OR. &
                     &  (rzs(ji,ip+1) == zspv) .OR. &
                     &  (ip == ilev)) THEN
                   rys (ji,jk) = zspv
                ELSE 
                   rys (ji,jk) = (1.0-zfracz) * rzs(ji,ip) + zfracz * rzs (ji,ip+1)
                ENDIF
             ENDDO
          ENDIF
          rys(NXX+1,jk)=rys(NXX,jk)
          zprof = zprof + zdz
       ENDDO
    ENDIF

  END SUBROUTINE CalculateCutPlane


  SUBROUTINE CreateBathy ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CreateBathy  ***
    !!
    !! ** Purpose :  prepare and draw bathy data for section
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ) :: bl_bathyinfo

    INTEGER(KIND=4)  :: ji
    INTEGER(KIND=4)  :: isrc, jsrc
    INTEGER(KIND=4)  :: iytick
    INTEGER(KIND=4)  :: ilog
    INTEGER(KIND=4)  :: inx, iny

    REAL(KIND=4)                              :: zrl, zrr, zrb, zrt, zul, zur, zub, zut
    REAL(KIND=4)                              :: zdx, zdy, zxinc, zyinc, zfracx, zfracy
    REAL(KIND=4)                              :: zspv
    REAL(KIND=4)                              :: zr1, zr2
    REAL(KIND=4), DIMENSION(NXX+3)            :: zbat_x, zbat_y   ! bathy as a poly line
    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zbathy !  (NXX,NYY)
    LOGICAL                                   :: ll_goodp, ll_exist
    !!----------------------------------------------------------------------
    PRINT *, 'Reading bathy file for coupe'
    CALL BimgAlloc ( bl_bathyinfo )

    INQUIRE ( FILE=cf_bathy, EXIST=ll_exist)
    IF ( .NOT. ll_exist ) THEN ! find another way ...
       CALL getenv('BIMG_BATHY', cf_bathy)
       INQUIRE ( FILE=cf_bathy, EXIST=ll_exist)
    ENDIF

    IF ( ll_exist) THEN
       ALLOCATE ( zbathy (NXX, NYY ) )
       bl_bathyinfo%cfname    = cf_bathy
       bl_bathyinfo%cmodifier = 'none'
       bl_bathyinfo%cvarname  = 'Bathymetry'
       bl_bathyinfo%num       = BimgOpenFile (bl_bathyinfo)
       CALL BimgReadHeader (bl_bathyinfo)

       inx  = bl_bathyinfo%nxdata
       iny  = bl_bathyinfo%nydata

       ! grid option
       IF ( opt_batgrid /= 0 ) CALL ReadGrid (bl_bathyinfo, cf_batgrid)

       bl_bathyinfo%lclr = .FALSE. 
       bl_bathyinfo%lcnt = .FALSE.

       CALL BimgReadData (zbathy,bl_bathyinfo,rmap_coord,1,1,1)

       zspv = bl_bathyinfo%spval
       ! force bathymetry to be negative
       WHERE ( zbathy(1:inx,1:iny) /= zspv ) zbathy(1:inx,1:iny) = -ABS( zbathy(1:inx,1:iny) )

       SELECT CASE ( bl_bathyinfo%ngrid )
        CASE ( 3 )
          !  Irregular grid case, alfan alfa .. already computes in CoupeInitNCAR
          DO ji=1,NXX
             ll_goodp=.TRUE.
             ll_goodp=ll_goodp .AND. (zbathy(ii0(3,ji),jj0(3,ji))  /=  zspv )
             ll_goodp=ll_goodp .AND. (zbathy(ii0(1,ji),jj0(1,ji))  /=  zspv )
             ll_goodp=ll_goodp .AND. (zbathy(ii0(2,ji),jj0(2,ji))  /=  zspv )
             zbat_x(ji) = ji
             IF (ll_goodp) THEN
                zbat_y(ji) = -(alfan(ji)*zbathy(ii0(3,ji),jj0(3,ji)) + &
                     &         alfa1(ji)*zbathy(ii0(1,ji),jj0(1,ji)) + &
                     &         alfa2(ji)*zbathy(ii0(2,ji),jj0(2,ji)) )
             ELSE
                zbat_y(ji) = zspv
             ENDIF
          ENDDO
        CASE DEFAULT
          zdx = float(bl_bathyinfo%nxdata-1) / float(NXX)
          zdy = float(bl_bathyinfo%nydata-1) / float(NXX) ! oui, oui, NXX

          zxinc = 0.0
          zyinc = 0.0

          DO ji = 1, NXX
             zfracx = zxinc - INT(zxinc)
             zfracy = zyinc - INT(zyinc)

             isrc = 1 + INT(zxinc)
             jsrc = 1 + INT(zyinc)

             zbat_x(ji) = ji

             !  take the bathy value only when neighbours are not spval
             !  When on the boundary, do take care of i+1 or j+1
             IF  ( zfracx == 0 ) THEN 
                IF (   (zbathy(isrc,jsrc  ) == zspv) .OR.  &
                   & ( (zbathy(isrc,jsrc+1) == zspv) .AND. &
                   &   (jsrc /= NYY) )             ) THEN
                   zbat_y(ji) = zspv
                ELSE
                   zbat_y(ji) = -(zbathy (isrc,jsrc+1) *     zfracy  + &
                   &              zbathy (isrc,jsrc  ) * (1.-zfracy))
                ENDIF

             ELSE IF (zfracy == 0) THEN
                IF (   (zbathy(isrc  ,jsrc) == zspv) .OR.  &
                   & ( (zbathy(isrc+1,jsrc) == zspv) .AND. &
                   &   (isrc /= NXX) )             ) THEN
                   zbat_y(ji) = zspv
                ELSE
                   zbat_y(ji) = -(zbathy (isrc+1,jsrc) *     zfracx  + &
                   &              zbathy (isrc  ,jsrc) * (1.-zfracx))
                ENDIF
             ELSE
                IF (   (zbathy(isrc  ,jsrc  ) == zspv) .OR.  &
                   & ( (zbathy(isrc+1,jsrc  ) == zspv) .AND. &
                   &   (isrc /= NXX) )                .OR.  &
                   & ( (zbathy(isrc  ,jsrc+1) == zspv) .AND. &
                   &   (jsrc /= NYY) )                .OR.  &
                   & ( (zbathy(isrc+1,jsrc+1) == zspv) .AND. &
                   &   (jsrc /= NYY)                  .AND. &
                   &   (isrc /= NXX) )               ) THEN
                   zbat_y(ji) = zspv
                ELSE
                   zbat_y(ji) = -(zbathy (isrc+1,jsrc+1) *     zfracx  *     zfracy  + &
                   &              zbathy (isrc  ,jsrc+1) * (1.-zfracx) *     zfracy  + &
                   &              zbathy (isrc+1,jsrc  ) *     zfracx  * (1.-zfracy) + &
                   &              zbathy (isrc  ,jsrc  ) * (1.-zfracx) * (1.-zfracy))
                ENDIF
             ENDIF
             zxinc = zxinc + zdx
             zyinc = zyinc + zdy
          ENDDO
       END SELECT

       DO ji = 1,NXX
          IF      (zbat_y(ji) >= prof_max ) THEN
             zbat_y(ji) = 1
          ELSE IF (zbat_y(ji) <= prof_min ) THEN
             zbat_y(ji) = NYY
          ELSE 
             zbat_y(ji) = 1 + NYY - INT ( ((zbat_y(ji) - prof_min) / (prof_max-prof_min)) * float(NYY) )
          ENDIF
       ENDDO

       ! close polyline
       zbat_x(NXX+1) = zbat_x(NXX)
       zbat_y(NXX+1) = 0

       zbat_x(NXX+2) = zbat_x(1)
       zbat_y(NXX+2) = 0

       zbat_x(NXX+3) = zbat_x(1)
       zbat_y(NXX+3) = zbat_y(1)

       DEALLOCATE ( zbathy ) 
    ENDIF  ! ll_exist  !JMM : the code below will likely not work if no file found ...

    IF (opt_xybat == 0) THEN
       x1bat = x1pos
       x2bat = x2pos
       y1bat = y1pos
       y2bat = y2pos
    ENDIF

    CALL getset (zrl, zrr, zrb, zrt, zul, zur, zub, zut, ilog)

    IF(opt_marg == 0) THEN
       CALL set(x1bat, x2bat, y1bat, y2bat, 1., float(NXX), 1., float(NYY) ,1)
    ELSE
       IF     (rmap_coord(3) == rmap_coord(4)) THEN
          !  zonal section
          zr2 = 1 + (rmap_marg(2) - rmap_coord(1)) / (rmap_coord(2) - rmap_coord(1))*(NXX-1)
          zr1 = 1 + (rmap_marg(1) - rmap_coord(1)) / (rmap_coord(2) - rmap_coord(1))*(NXX-1)
       ELSEIF (rmap_coord(1) == rmap_coord(2)) THEN
          !  meridional section
          zr2 = 1 + (rmap_marg(4) - rmap_coord(3)) / (rmap_coord(4) - rmap_coord(3))*(NXX-1)
          zr1 = 1 + (rmap_marg(3) - rmap_coord(3)) / (rmap_coord(4) - rmap_coord(3))*(NXX-1)
       ELSE
          PRINT *,' ERROR: -marg option incompatible with oblique sections' 
          STOP
       ENDIF
       CALL set(x1bat, x2bat, y1bat, y2bat, zr1, zr2,1., float(NYY),1)
    ENDIF

    CALL  aredam (niama, zbat_x, zbat_y, NXX+3, 15, 5, 3)

    IF (opt_mapfill == 1) THEN
       CALL arscam(niama, xcra, ycra, jp_cra, niaia, nigia, jp_agid, colbathy)    
    ENDIF

    CALL ardrln (niama, zbat_x, zbat_y, NXX+3, xcra, ycra, jp_cra,niaia, nigia, jp_agid, drwbathy)

    CALL set (zrl, zrr, zrb, zrt, zul, zur, zub, zut, ilog)
    CALL BimgDeAlloc ( bl_bathyinfo )

  END SUBROUTINE CreateBathy


  SUBROUTINE ColorCoupe (pfld, bdimg, kncol, plimit)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ColorCoupe  ***
    !!
    !! ** Purpose :   Draw the section in colors
    !!
    !! ** Method  :  If option -print (clriso) used, work with isocontours 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(in) :: pfld    ! NXX x NYY
    TYPE( bimgfile ),             INTENT(in) :: bdimg
    INTEGER(KIND=4),              INTENT(in) :: kncol
    REAL(KIND=4), DIMENSION(:),   INTENT(in) :: plimit  ! NBOXMAX+1

    INTEGER(KIND=4) ::  ji,jj
    INTEGER(KIND=4) ::  icol, ioffset
    INTEGER(KIND=4) ::  inx, iny
    INTEGER(KIND=4) ::  imap_flag
    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE :: iclria

    REAL(KIND=4), DIMENSION(4)                   :: zcoords
    REAL(KIND=4)                                 :: zspv
    REAL(KIND=4)                                 :: zptr
    REAL(KIND=4)                                 :: zr1, zr2

    !!----------------------------------------------------------------------
    IF (opt_color == 0) RETURN


    inx  = bdimg%nxdata
    iny  = bdimg%nydata
    zspv = bdimg%spval


    IF (opt_sigma == 1) THEN
       imap_flag=5
    ELSE
       imap_flag=0
    ENDIF

    IF(opt_marg == 0) THEN
       CALL set(x1pos,x2pos,y1pos,y2pos,1.,float(inx),1.,float(iny),1)
    ELSE
       IF     (rmap_coord(3) == rmap_coord(4)) THEN
          !  zonal section
          zr2 = 1 + (rmap_marg(2) - rmap_coord(1)) / (rmap_coord(2) - rmap_coord(1))*(inx-1)
          zr1 = 1 + (rmap_marg(1) - rmap_coord(1)) / (rmap_coord(2) - rmap_coord(1))*(inx-1)
       ELSEIF (rmap_coord(1) == rmap_coord(2)) THEN
          !  meridional section
          zr2 = 1 + (rmap_marg(4) - rmap_coord(3)) / (rmap_coord(4) - rmap_coord(3))*(inx-1)
          zr1 = 1 + (rmap_marg(3) - rmap_coord(3)) / (rmap_coord(4) - rmap_coord(3))*(inx-1)
       ELSE
          PRINT *,' ERROR: -marg option incompatible with oblique sections' 
          STOP
       ENDIF
       CALL set(x1pos, x2pos, y1pos, y2pos, zr1, zr2, 1., float(iny), 1)
    ENDIF

    SELECT CASE ( opt_print )
    !-------------------------------------------------------------------------------
    CASE ( 0 )   ! use a color cell array
    !-------------------------------------------------------------------------------
       ALLOCATE ( iclria(inx,iny) )
       CALL  flush(6)

       DO ji=1,inx
          DO jj=1,iny
             IF      (pfld(ji,jj) == zspv          ) THEN 
                iclria(ji,jj) = COLOR_SPVAL
             ELSE IF (pfld(ji,jj) <= plimit(1)     ) THEN
                iclria(ji,jj) = COLOR_NRES+1
             ELSE IF (pfld(ji,jj) >= plimit(kncol) ) THEN
                iclria(ji,jj) = kncol + COLOR_NRES - 1
             ELSE
                ioffset = 0
                zptr = float(kncol)

                DO WHILE (zptr /= 1.0)
                   zptr = zptr / 2.0
                   IF (zptr <= 1.0) zptr = 1.0

                   icol = NINT(zptr)+ioffset

                   IF (pfld(ji,jj) >= plimit(icol)) THEN
                      IF (pfld(ji,jj) <= plimit(icol+1)) THEN
                         zptr = 1.0
                      ENDIF
                      ioffset = icol
                   ENDIF
                ENDDO
                iclria(ji,jj) = ioffset + COLOR_NRES - 1
             ENDIF
          ENDDO
       ENDDO

       CALL gca(1., 1., float(inx), float(iny), inx, iny, 1, 1, inx, iny, iclria)
       DEALLOCATE (iclria )
    !-------------------------------------------------------------------------------
    CASE ( 1 )   ! use a color isocontours
    !-------------------------------------------------------------------------------
       zcoords(1) = 1.
       zcoords(2) = float(NXX)
       zcoords(3) = 1.
       zcoords(4) = float(NYY)

       CALL ColorContour (pfld, bdimg, plimit, zcoords, imap_flag )
       CALL arscam(niama, xcra, ycra, jp_cra, niaia, nigia, jp_agid, colcontours)  
    END SELECT

    IF (opt_palbar == 1)  CALL AddPalette (plimit, kncol)

  END SUBROUTINE ColorCoupe


  SUBROUTINE AddGrid ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE AddGrid  ***
    !!
    !! ** Purpose :   Add a grid on the section plot
    !!
    !! ** Method  : Individual choices can be made for ygrid, ygrid and zgrid
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4)   :: ixtick, iytick
    INTEGER(KIND=4)   :: ilog
    INTEGER(KIND=4)   :: ixlb, iylb, izlb
    REAL(KIND=4)      :: zrl, zrr, zrb, zrt, zul, zur, zub, zut
    REAL(KIND=4)      :: zr1, zr2, zr2bis
    REAL(KIND=4)      :: zcutdist_adj
    CHARACTER(LEN=20) :: cl_label
    !!----------------------------------------------------------------------
    CALL pcsetr ('SA', scal_cslab)      ! change character size
    CALL getset (zrl, zrr, zrb, zrt, zul, zur, zub, zut, ilog)
    CALL set (zrl, zrr, zrb, zrt, zrl, zrr, -prof_max, -prof_min,1)
    ixlb = opt_labx
    iylb = opt_laby
    izlb = opt_labz

    IF (opt_xstep  == 0) xstep  = ABS( rmap_marg(1) - rmap_marg(2))/4.0
    IF (opt_ystep  == 0) ystep  = ABS( rmap_marg(3) - rmap_marg(4))/4.0
    IF (opt_zstep  == 0) zstep  = ABS( prof_min     - prof_max    )/4.0
    IF (opt_kmstep == 0) kmstep = cut_dist/4.

    CALL labmod ('(i10)', int_table(ICLR_ZAXIS)%format,0, 0, 1, 1, 0, 5,0) ! see man labmod !

    IF (opt_grad == 1) THEN
       iytick = NINT (ABS(prof_min - prof_max )/zstep)
    ELSE 
       iytick = 0
    ENDIF

    IF (opt_zgrid == 0) THEN
       CALL gridal (0, 0, iytick, nzsub, 0, izlb, 5, 0., 0.)
    ELSE
       CALL gasetr ('WMJ - major tick line width ', zlw )
       CALL gridal (0, 0, iytick, nzsub, 0, izlb, 0, 0., 0.)
       CALL gasetr ('WMJ - major tick line width ', 1.0 )
    ENDIF

    IF (opt_xgrid == 1) THEN
       CALL gasetr ('WMJ - major tick line width ', xlw )
       ixtick = NINT (ABS(rmap_marg(1) - rmap_marg(2))/xstep)
       CALL gridal (ixtick, nxsub, 0, 0, 0, 0, 0, 0., 0.)
       CALL gasetr ('WMJ - major tick line width ', 1.0 )
    ENDIF

    IF (opt_ygrid == 1) THEN
       CALL gasetr ('WMJ - major tick line width ', ylw )
       ixtick = NINT (ABS(rmap_marg(3) - rmap_marg(4))/ystep)
       CALL gridal (ixtick, nysub, 0, 0, 0, 0, 0, 0., 0.)
       CALL gasetr ('WMJ - major tick line width ', 1.0 )
    ENDIF


    ! Longitude and latitude
    IF (opt_nolon == 0) THEN
       CALL labmod (int_table(ICLR_XAXIS)%format, '(i1)', 0, 0, 1, 1, 0, 2, 0)

       CALL set (0., 1., 0., 1., 0., 1., 0., 1., 1)
       WRITE(cl_label,'(a)') TRIM(cxaxist)
       CALL plchhq (x1pos-0.02, ylon, TRIM(cl_label), -1., 0., 1.)

       IF (rmap_marg(1) /= rmap_marg(2)) THEN
          ixtick = NINT (ABS(rmap_marg(1) - rmap_marg(2))/xstep)

          CALL set (zrl, zrr, 0., 1., rmap_marg(1), rmap_marg(2), 0., 1., 1)
          CALL halfax (ixtick, nxsub, 0, 0, 0, ylon, ixlb, -1)
       ELSE 
          WRITE(cl_label,'(f6.2)') rmap_marg(1)
          CALL plchhq (x1pos, ylon, cl_label, -1., 0., -1.)
       ENDIF
    ENDIF

    IF (opt_nolat == 0) THEN
       CALL labmod (int_table(ICLR_YAXIS)%format,'(i1)', 0, 0, 1, 1, 0, 2, 0)

       CALL set (0., 1., 0., 1., 0., 1., 0., 1., 1)
       WRITE(cl_label,'(a)') TRIM(cyaxist)
       CALL plchhq (x1pos-0.02, ylat, TRIM(cl_label), -1., 0., 1.)

       IF (rmap_marg(3) /= rmap_marg(4)) THEN
          ixtick = NINT (ABS(rmap_marg(3) - rmap_marg(4))/ystep)

          CALL set (zrl, zrr, 0., 1., rmap_marg(3), rmap_marg(4), 0., 1., 1)
          CALL halfax (ixtick, nysub, 0, 0, 0, ylat, iylb, -1)
       ELSE 
          WRITE(cl_label,'(f6.2)') rmap_marg(3)
          CALL plchhq (x1pos, ylat, cl_label, -1., 0., -1.)
       ENDIF
    ENDIF

    ! -km option
    IF (opt_km == 1) THEN
       CALL set (0., 1., 0., 1., 0., 1., 0., 1., 1)
       WRITE(cl_label,'(a)') TRIM(ckmaxist)
       CALL plchhq(x1pos-0.02, ykm, TRIM(cl_label), -1., 0., 1.)

       zcutdist_adj = (INT(cut_dist/kmstep) +1 )* kmstep

       IF (opt_marg == 1) THEN
          IF (rmap_marg(2) /= rmap_marg(1)) THEN
             ! zonal section
             zr1 = zrl + (zrr - zrl)*(rmap_coord(1) - rmap_marg(1)) / (rmap_marg(2) - rmap_marg(1))
             zr2 = zrl + (zrr - zrl)*(rmap_coord(2) - rmap_marg(1)) / (rmap_marg(2) - rmap_marg(1))
          ELSEIF (rmap_marg(4) /= rmap_marg(3)) THEN
             ! meridional section
             zr1 = zrl + (zrr - zrl)*(rmap_coord(3) - rmap_marg(3)) / (rmap_marg(4) - rmap_marg(3))
             zr2 = zrl + (zrr - zrl)*(rmap_coord(4) - rmap_marg(3)) / (rmap_marg(4) - rmap_marg(3))
          ENDIF
       ELSE ! opt_marg == 0
          zr1=zrl
          zr2=zrr
       ENDIF
       zr2bis = (zr2 - zr1) / cut_dist*zcutdist_adj + zr1
       IF (zr2bis >= 1) THEN
          PRINT *,' zr2bis too large  ...', zr2bis
          zr2bis = zr2
          zcutdist_adj = cut_dist ! tant pis !
       ENDIF

       CALL set (zr1, zr2bis, 0., 1., 0., zcutdist_adj, 0., 1., 1)

       ixtick = NINT(zcutdist_adj/kmstep)
       CALL halfax (ixtick, nkmsub, 0, 0, 0, ykm, ixlb, -1)
    ENDIF  ! km

    CALL pcsetr ('SA', 0.88888888888) ! reset character size
    CALL set (zrl, zrr, zrb, zrt, zul, zur, zub, zut, ilog)

  END SUBROUTINE AddGrid

  SUBROUTINE NearestPoint(plon, plat, kimin, kjmin, kquadran)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE NearestPoint  ***
    !!
    !! ** Purpose :  Find the position on the (i,j) grid of the nearest point
    !!               located at plon, plat 
    !!
    !! ** Method  :  Use and efficient algorithm for searching the data array. 
    !!               P.A. Darbon and A. de Miranda acknowledged for this algo.
    !!----------------------------------------------------------------------
    REAL(KIND=4),    INTENT(in)     :: plon, plat
    INTEGER(KIND=4), INTENT(inout ) :: kimin, kjmin
    INTEGER(KIND=4), INTENT(out )   :: kquadran

    INTEGER(KIND=4)                           :: ji, jj
    INTEGER(KIND=4)                           :: ierr
    INTEGER(KIND=4)                           :: ii0, ii1, ij0, ij1
    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zdis
    INTEGER(KIND=4)                           :: ilocmi(2)
    INTEGER(KIND=4), PARAMETER                :: jp_rad = 10

    REAL(KIND=4), DIMENSION(0:6)              :: zalpha, zx, zy
    REAL(KIND=4)                              :: zdx, zdy, zdalph
    REAL(KIND=4)                              :: zpi
    LOGICAL                                   :: ll_bwest, ll_best, ll_bsud, ll_bnord
    !!----------------------------------------------------------------------
    zpi=ACOS(-1.)
    kquadran = 0
    !
    ! look for points in the neigbourhood of the previous one, in an radius of jp_rad
    IF (kimin > 0 .AND. kjmin  >  0 ) THEN
       ii0 = kimin - jp_rad
       ii1 = kimin + jp_rad
       ij0 = kjmin - jp_rad
       ij1 = kjmin + jp_rad

       IF (ii0 <= 0  ) ii0 = 1
       IF (ii1 > nigr) ii1 = nigr
       IF (ij0 <= 0  ) ij0 = 1
       IF (ij1 > njgr) ij1 = njgr
    ELSE   ! used at the first call : take the whole domain
       ii0 = 1
       ii1 = nigr 
       ij0 = 1
       ij1 = njgr
    ENDIF

    IF (ALLOCATED(zdis) .AND. SIZE(zdis,1) == ii1 -ii0 +1  .AND. SIZE(zdis,2) == ij1 - ij0 +1 ) THEN
       !         no need to reallocate zdis
    ELSE
       IF (ALLOCATED(zdis))  DEALLOCATE(zdis)
       ALLOCATE(zdis(ii1-ii0+1,ij1-ij0+1) )
    ENDIF
    !  this is a rough proxy for a pseudo distance, but work in this case
    zdis(:,:)=( plat - xygr(ii0:ii1,ij0:ij1,2) )*( plat - xygr(ii0:ii1,ij0:ij1,2) ) +  &
         &    ( plon - xygr(ii0:ii1,ij0:ij1,1) )*( plon - xygr(ii0:ii1,ij0:ij1,1) )

    ilocmi = MINLOC(zdis)
    kimin  = ilocmi(1) + ii0 - 1
    kjmin  = ilocmi(2) + ij0 - 1

    ll_bwest = .FALSE.
    ll_best  = .FALSE.
    ll_bsud  = .FALSE.
    ll_bnord = .FALSE.

    IF (kimin == 1   ) ll_bwest = .TRUE.
    IF (kimin == nigr) ll_best  = .TRUE.
    IF (kjmin == 1   ) ll_bsud  = .TRUE.
    IF (kjmin == njgr) ll_bnord = .TRUE.

    zx(0)=xygr(kimin  ,kjmin  ,1)
    zy(0)=xygr(kimin  ,kjmin  ,2)

    zx(1)=xygr(kimin+1,kjmin  ,1)
    zy(1)=xygr(kimin+1,kjmin  ,2)

    zx(2)=xygr(kimin  ,kjmin+1,1)
    zy(2)=xygr(kimin  ,kjmin+1,2)

    zx(3)=xygr(kimin-1,kjmin+1,1)
    zy(3)=xygr(kimin-1,kjmin+1,2)

    zx(4)=xygr(kimin-1,kjmin  ,1)
    zy(4)=xygr(kimin-1,kjmin  ,2)

    zx(5)=xygr(kimin  ,kjmin-1,1)
    zy(5)=xygr(kimin  ,kjmin-1,2)

    zx(6)=xygr(kimin+1,kjmin-1,1)
    zy(6)=xygr(kimin+1,kjmin-1,2)

    !                       (3)         (2)
    !                          \         |           
    !                            \       |           
    !                              \     |   x     
    !                       (4) ------- (0)_________ (1)
    !                                    \
    !                                     \
    !                                      \          (6)
    !                                      (5)
    !
    ! determine the points which limitthe cell where the current point is located
    ! 6 quadran are defined in order to always have the same triangle.
    ! Compute the angle
    zdy = (plat - zy(0) )
    zdx = (plon - zx(0) )
    IF (zdx  ==  0. .AND. zdy  ==  0.) THEN  ! strike, good shot !!!
       zalpha(0) = 0.
       kquadran  = 7
    ELSE
       zalpha(0) = 180./zpi * ATAN2(zdy, zdx)
    ENDIF

    IF (zalpha(0) <= 0) zalpha(0) = 360. + zalpha(0)

    DO ji=1,6
       zdy        = (zy(ji) - zy(0) )
       zdx        = (zx(ji) - zx(0) )
       zalpha(ji) = 180./zpi* ATAN2(zdy,zdx)
       IF (zalpha(ji) <= 0) zalpha(ji) = 360. + zalpha(ji)
    ENDDO

    zdalph = zalpha(1)
    DO ji=0,6
       zalpha(ji) = zalpha(ji) - zdalph
       IF (zalpha(ji) <= 0) zalpha(ji) = 360. + zalpha(ji)
    ENDDO

    zdalph=zalpha(0)
    IF (kquadran  /=  7 ) THEN
       IF      (zdalph  >=  zalpha(6) ) THEN ! 6eme quadran. 
          kquadran=6
       ELSE IF (zdalph  >=  zalpha(5) ) THEN ! 5eme quadran
          kquadran=5
       ELSE IF (zdalph  >=  zalpha(4) ) THEN ! 4eme quadran
          kquadran=4
       ELSE IF (zdalph  >=  zalpha(3) ) THEN ! 3eme quadran
          kquadran=3
       ELSE IF (zdalph  >=  zalpha(2) ) THEN ! 2eme quadran
          kquadran=2
       ELSE IF (zdalph  >=  0.        ) THEN ! 1eme quadran
          kquadran=1
       ELSE
          PRINT *,'   ',zdalph, (zalpha(ji),ji=1,6)
          STOP ' pb '
       ENDIF
    ENDIF

    ierr=0
    IF (ll_bwest .AND. (kquadran == 2 .OR. kquadran == 3) ) THEN
       PRINT *, 'point west of the domain '
       ierr=ierr+1
    ENDIF
    IF (ll_best .AND. (kquadran == 1 .OR. kquadran == 4) ) THEN
       PRINT *, 'point east of the domain '
       PRINT *, ll_best, kquadran
       ierr=ierr+1
    ENDIF
    IF (ll_bnord .AND. (kquadran == 1 .OR. kquadran == 2) ) THEN
       PRINT *, 'point north of the domain '
       ierr=ierr+1
    ENDIF
    IF (ll_bsud .AND. (kquadran == 3 .OR. kquadran == 4) ) THEN
       PRINT *, 'point south of the domain '
       ierr=ierr+1
    ENDIF

    IF (ierr /=  0 ) THEN
       PRINT *, ierr, ' errors found in  NearestPoint ( modcoupe module) '
       STOP 
    ENDIF

  END SUBROUTINE NearestPoint

END MODULE modcoupe
