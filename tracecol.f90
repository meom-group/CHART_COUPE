MODULE tracecol
  !!======================================================================
  !!                     ***  MODULE  tracecol  ***
  !! All routines dedicated to the color mapping 
  !!=====================================================================
  !! History : 1.0   ! 06/1993   !  E. Brown      : original code
  !!           7.0   ! 12/2010   ! J.M. Molines   : F90 and Doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !!     InitMap
  !!     DrawMeridians 
  !!     AddText
  !!     AddPalette
  !!     ColorMap
  !!     DrawContinents 
  !!     DrawOceans
  !!     mark
  !!     CreateCoordsFile
  !!     TraceMeridiens
  !!     TraceMeridian
  !!     SetPlotWindow
  !!----------------------------------------------------------------------
  USE modcom
  USE modmapncar
  USE modvaltable
  USE modexternal
  USE modcolor

  USE val_table
  USE util
  USE isocontours
  USE moddrawpalette

  IMPLICIT NONE

  PRIVATE

  REAL(KIND=4), DIMENSION(5)     :: xperim = (/0.0, 1.0, 1.0, 0.0, 0.0/)  !
  REAL(KIND=4), DIMENSION(5)     :: yperim = (/0.0, 0.0, 1.0, 1.0, 0.0/)  !

  PUBLIC :: InitMap
  PUBLIC :: DrawMeridians 
  PUBLIC :: AddText
  PUBLIC :: AddPalette
  PUBLIC :: ColorMap
  PUBLIC :: DrawContinents 
  PUBLIC :: DrawOceans
  PUBLIC :: mark
  PUBLIC :: SetPlotWindow

  PRIVATE :: CreateCoordsFile
  PRIVATE :: TraceMeridiens 
  PRIVATE :: TraceMeridian

  INTERFACE                  ! cfux and cfuy are NCAR functions used in ColorMap
     FUNCTION cfux (cdx )
       REAL(KIND=4) :: cdx
     END FUNCTION cfux

     FUNCTION cfuy (cdy )
       REAL(KIND=4) :: cdy
     END FUNCTION cfuy

     CHARACTER(LEN=24) FUNCTION fdate ()
     END FUNCTION fdate
  
     FUNCTION mapaci (kid )
       INTEGER(KIND=4) :: kid 
     END FUNCTION mapaci

     FUNCTION mpisci (kid )
       INTEGER(KIND=4) :: kid 
     END FUNCTION mpisci

  END INTERFACE

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------


CONTAINS


  SUBROUTINE InitMap ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE InitMap  ***
    !!
    !! ** Purpose :  Initialize map parameters 
    !!
    !! ** Method  :   
    !!
    !! References :  NCAR man pages and web site
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), DIMENSION(13) :: iasf  
    REAL(KIND=4), DIMENSION(2)     :: zcorner1, zcorner2, zcorner3, zcorner4
    REAL(KIND=4)                   :: zang
    REAL(KIND=4)                   :: zpi
    !!----------------------------------------------------------------------
    CALL gsclip(0)
    iasf(:)    = 1

    CALL gsasf (iasf)  ! Set  aspect  source  flags: 

    ! color and text quality for all the map
    CALL pcsetc ('FC'   , '|'   )   ! set control character to |
    CALL pcsetc ('FN'   , c_font)   ! font
    CALL pcseti ('CC(0)',1      )   ! character color index
    CALL gaseti ('LTY'  ,1      )   ! use plcchq to draw labels

    CALL gsplci(1)                  ! set polyline color index
    CALL gsfais(1)                  ! set polyline fill interior style

    CALL arinam(niama, jp_iama)     ! initialize the area map

    SELECT CASE ( opt_noproj ) 
       !-----------------------------------------------
    CASE ( 0 )   ! projection is to be used
       !-----------------------------------------------
       IF (opt_rlat == 0) THEN
          rlat_map = (rmap_marg(4)-rmap_marg(3))/2. + rmap_marg(3)
       ENDIF

       IF (opt_rlon == 0) THEN
          rlon_map = (rmap_marg(2)-rmap_marg(1))/2. + rmap_marg(1)
       ENDIF

       CALL mappos (x1pos, x2pos, y1pos, y2pos) ! position the map 
       CALL mpsetc ('OU' ,'CO'                ) ! continental outlines
       CALL mpseti ('PE' ,1                   ) ! draw perimeter
       CALL mpseti ('LA' ,0                   ) ! no labels for meridians

       ! corners are given with (Lat, Lon ) coordinates !!
       zcorner1(1) = rmap_marg(4)     ! limit of projection
       zcorner1(2) = rmap_marg(1)

       zcorner2(1) = rmap_marg(4)
       zcorner2(2) = rmap_marg(2)

       zcorner3(1) = rmap_marg(3)
       zcorner3(2) = rmap_marg(2)

       zcorner4(1) = rmap_marg(3)
       zcorner4(2) = rmap_marg(1) 
       IF ( opt_debug == 1 ) PRINT *, ' CORNERS ', zcorner1, zcorner2, zcorner3, zcorner4

       CALL mpsetr ('GR', xstep)  ! desired grid spacing

       ! cmap_zone is set according to how we specify the rectangular portion of the u/v plane 
       ! to be drawn. It depend on the projection
       !   'MA' : maximum possible according to projection
       !   'CO' : specify opposite corners
       !   'AN' : specify the angle of vision of the map
       !   'LI' : specify min/max of u/v : need some trigonopetric formulae
       !   'GR' : specify mi/max of lat/lon
       SELECT CASE ( cmap_proj )
       CASE  ( ' OR' )       ! Orthographic projection
          IF (cmap_zone /= 'MA') THEN
             cmap_zone = 'LI'
             ! Dans le cas d'une projection Orthographique, les limites
             ! de la projection sont plutot definies par LI, pour permettre
             ! une projection telle que dans la figure 3, p 90 de Contouring
             ! and Mapping Tutorial. Details page 91, formule utilisee :
             ! Orthographic, MP 1.5 page 71.
             ! Pourrait etre etendu a d'autres projections.

             zpi = ACOS ( -1.)
             zcorner1(1) = rmap_marg(1) - rlon_map
             zcorner2(1) = rmap_marg(2) - rlon_map
             zcorner3(1) = rmap_marg(3) - rlat_map
             zcorner4(1) = rmap_marg(4) - rlat_map

             ! deal with one hemisphere only
             IF (zcorner1(1) < -90 ) zcorner1(1) = -90
             IF (zcorner2(1) >  90 ) zcorner2(1) =  90

             zcorner1(1) = SIN ((zcorner1(1) * zpi)/180.0)
             zcorner2(1) = SIN ((zcorner2(1) * zpi)/180.0)
             zcorner3(1) = SIN ((zcorner3(1) * zpi)/180.0)
             zcorner4(1) = SIN ((zcorner4(1) * zpi)/180.0)
          ENDIF

       CASE ( 'CE'  )  ! Cylindrical equidistant
          rlat_map = 0.
          CALL mpsetr ('GR', 0. )   ! suppress the grid (will by drawn by its own )

       CASE ( 'ME' )   ! Mercator
          rlat_map = 0.

       CASE ( 'MO' , 'RO' )  ! Mollweide-type or Robertson (not in man but in ncl example)
          cmap_zone='GR'
          zcorner1(1)=rmap_marg(3)
          zcorner2(1)=rmap_marg(1)
          zcorner3(1)=rmap_marg(4)
          zcorner4(1)=rmap_marg(2)
          CALL mpseti ('PE',1)
          CALL mpseti ('C4', 22 )
          CALL mpseti('EL',1)   !  elliptic perimeter

       END SELECT

       CALL maproj (cmap_proj, rlat_map, rlon_map, 0.)
       CALL mapset (cmap_zone, zcorner1, zcorner2, zcorner3, zcorner4)

       ! JMM : these lines are in conflict with case (OR ) above ... check
       IF ( cmap_proj  ==  'OR' ) THEN 
          cmap_zone = 'AN'
          CALL mpseti ('EL', 1)   ! limit the map to an elliptic area inscribed within the
          ! normal rectangular perimeter
          IF ( rmap_coord(4)  <=  0 ) THEN   !  southern hemisphere
             zang = 90. + rmap_coord(4) + 2
          ELSE                               !  northern hemisphere
             zang = 90. - rmap_coord(3) + 2
          ENDIF
          CALL mapset (cmap_zone, zang, zang, zang, zang)
       ENDIF

       CALL mapint  

       ! choose the standard resolution ezmap data base or the higher one
       SELECT CASE ( opt_high )
       CASE ( 0 )  ! standard coast data base
          CALL mapbla(niama)      ! add boundary lines to area map
       CASE ( 1 )  ! higher resolution database  ( -hi option ) 
          CALL mplnam('Earth..2',1,niama)      ! add boundary lines to area map
       CASE ( 2 )  ! very high resolution database  ( -vhi option ) 
          CALL mplnam('Earth..4',1,niama)      ! add boundary lines to area map
       END SELECT

       !-----------------------------------------------
    CASE ( 1 )   ! projection is not be used
       !-----------------------------------------------
       ! ... opt_noproj = 1 (ie pas de projection)
       CALL set   (x1pos, x2pos, y1pos, y2pos,     &
            &      rmap_coord(1), rmap_coord(2),   &
            &      rmap_coord(3), rmap_coord(4), 1 )

       !                                       igi idl idr
       !                                        v   v   v
       CALL  aredam (niama, xperim, yperim, 5, 15, 30, 31)            

    END SELECT

    ! coordinate file is usefull for multi frame merging
    IF (opt_coords == 1) CALL CreateCoordsFile (rmap_marg, cf_coords)

  END SUBROUTINE InitMap


  SUBROUTINE ColorMap (pfld, bdimg, plimit, kncol)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ColorMap  ***
    !!
    !! ** Purpose :   Draw the color map ...
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(in) :: pfld    !  (NXX, NYY)
    TYPE( bimgfile ),             INTENT(in) :: bdimg
    REAL(KIND=4), DIMENSION(:),   INTENT(in) :: plimit  ! (NBOXMAX+1)
    INTEGER(KIND=4),              INTENT(in) :: kncol

    INTEGER(KIND=4)                              :: inx, iny
    INTEGER(KIND=4)                              :: ji, jj
    INTEGER(KIND=4)                              :: ilog
    INTEGER(KIND=4)                              :: incls = 300, iusr, jusr
    INTEGER(KIND=4)                              :: ioffset, icol, imap_flag, iszmax
    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE :: icra
    REAL(KIND=4)                                 :: zrl,  zrr,  zrb,  zrt
    REAL(KIND=4)                                 :: zur,  zul,  zut,  zub
    REAL(KIND=4)                                 :: zspv
    REAL(KIND=4)                                 :: zx, zy, zlat, zlon
    REAL(KIND=4)                                 :: zdx, zdy, zptr
    !!----------------------------------------------------------------------
    IF (opt_color == 0) RETURN

    iszmax = MAX (NXX, NYY )
    ALLOCATE ( icra ( iszmax, iszmax ) )

    CALL gflas1( jp_ib_color )

    inx  = bdimg%nxdata
    iny  = bdimg%nydata
    zspv = bdimg%spval

    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)

    zdx = REAL(inx) / (rmap_coord(2) - rmap_coord(1) )
    zdy = REAL(iny) / (rmap_coord(4) - rmap_coord(3) )

    SELECT CASE ( opt_print ) 
       !-------------------------------------------------------------------------
    CASE     ( 0 )    ! use a color cell array 
       !-------------------------------------------------------------------------
       SELECT CASE  (  opt_noproj ) 
          !..........................................................
       CASE     ( 0 )    !  Use a map projection for the color map
          !..........................................................
          CALL  flush(6)

          DO ji=1,incls
             zx = cfux( x1pos+(x2pos-x1pos)*(REAL(ji-1)+.5)/REAL(incls)  )
             DO jj=1,incls                                           
                zy = cfuy( y1pos+(y2pos-y1pos)*(REAL(jj-1)+.5)/REAL(incls) )

                CALL maptri (zx, zy, zlat, zlon)                          

                ! maptri return values between -180 and 180 deg. Some checking is needed
                IF ( (zlon < rmap_coord(1)) .AND. (zlon+360 <= rmap_coord(2)) ) THEN
                   zlon = zlon + 360.
                ENDIF

                ! also check if the map starts westward of -180.
                IF ( (zlon > rmap_coord(2)) .AND. (zlon >= rmap_coord(1)) ) THEN
                   zlon = zlon - 360.
                ENDIF

                IF (    (zlat /= rp_out_range ) .AND. &
                     &  (zlat >= rmap_coord(3)) .AND. &
                     &  (zlat <= rmap_coord(4)) .AND. &
                     &  (zlon >= rmap_coord(1)) .AND. &
                     &  (zlon <= rmap_coord(2)))       THEN

                   iusr = INT( REAL(zlon-rmap_coord(1))*zdx ) + 1
                   jusr = INT( REAL(zlat-rmap_coord(3))*zdy ) + 1

                   IF      (pfld(iusr,jusr) == zspv          ) THEN 
                      icra(ji,jj) = COLOR_SPVAL
                   ELSE IF (pfld(iusr,jusr) <= plimit(1)     ) THEN
                      icra(ji,jj) = COLOR_NRES
                   ELSE IF (pfld(iusr,jusr) >= plimit(kncol) ) THEN
                      icra(ji,jj) = kncol + COLOR_NRES -1
                   ELSE
                      ioffset = 0
                      zptr = float(kncol)

                      DO WHILE (zptr /= 1.0)
                         zptr = zptr / 2.0
                         IF (zptr <= 1.0) zptr = 1.0

                         icol = NINT(zptr) + ioffset

                         IF ( pfld(iusr,jusr) >= plimit(icol) ) THEN
                            IF (pfld(iusr,jusr) < plimit(icol+1))THEN
                               zptr = 1.0
                            ENDIF
                            ioffset = icol
                         ENDIF
                      ENDDO
                      icra(ji,jj) = ioffset + COLOR_NRES - 1
                   ENDIF
                ELSE                         
                   IF (zlat == rp_out_range ) THEN
                      icra(ji,jj)=COLOR_BACKGROUND
                   ELSE 
                      icra(ji,jj)=COLOR_OCEAN
                   ENDIF
                END IF

             ENDDO
          ENDDO

          CALL gca (cfux(x1pos), cfuy(y1pos), cfux(x2pos), cfuy(y2pos),   &
               &             iszmax, iszmax, 1, 1, incls, incls, icra) 
          !..........................................................
       CASE     ( 1 )    !  Do not use a map projection for the color map
          !..........................................................
          DO ji=1,inx
             DO jj=1,iny
                IF (pfld(ji,jj) == zspv) THEN 
                   icra(ji,jj) = COLOR_SPVAL
                ELSE IF (pfld(ji,jj) < plimit(1)) THEN
                   icra(ji,jj) = COLOR_NRES
                ELSE IF (pfld(ji,jj) >= plimit(kncol)) THEN
                   icra(ji,jj) = kncol + COLOR_NRES-1
                ELSE
                   ioffset = 0
                   zptr = float(kncol)

                   DO WHILE (zptr /= 1.0)
                      zptr = zptr / 2.0
                      icol = NINT(zptr)+ioffset
                      IF (pfld(ji,jj) >= plimit(icol)) THEN
                         IF (pfld(ji,jj) < plimit(icol+1)) THEN
                            zptr = 1.0
                         ENDIF
                         ioffset = icol
                      ENDIF
                   ENDDO

                   icra(ji,jj) = ioffset + COLOR_NRES-1

                ENDIF
             ENDDO
          ENDDO

          CALL set(x1pos, x2pos, y1pos, y2pos, 1., float(inx), 1., float(iny), 1)

          CALL gca(1.,1.,float(inx),float(iny),iszmax, iszmax,1,1,inx,iny,icra )

       END SELECT

       !-------------------------------------------------------------------------
    CASE     ( 1 )    ! use a color isocontours 
       !-------------------------------------------------------------------------
       CALL  flush(6)

       IF (bdimg%ngrid  ==  3 ) THEN
          imap_flag = 6
       ELSE 
          imap_flag = 1
       ENDIF

       IF (opt_noproj  ==  1) imap_flag = 0

       CALL ColorContour (pfld, bdimg, plimit, rmap_coord, imap_flag)
       CALL set ( zrl, zrr, zrb, zrt, 0.0, 1.0, 0.0, 1.0, 1)
       CALL aredam (niama, xperim, yperim, 5, 15, 30, 31)            
       CALL arscam (niama, xcra, ycra, jp_cra, niaia, nigia, jp_agid, colcontours)

    END SELECT

    CALL set   ( zrl, zrr, zrb, zrt, zur, zul, zut, zub, 1)

    CALL gflas2

    DEALLOCATE ( icra )

  END SUBROUTINE ColorMap


  SUBROUTINE CreateCoordsFile (pcoords, cd_filecoords)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE <routine>  ***
    !!
    !! ** Purpose :  Create a file with the PLOTFRAME layout 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: pcoords       ! 4
    CHARACTER(LEN=*),           INTENT(in) :: cd_filecoords

    REAL(KIND=4)     :: zrl,  zrr,  zrb,  zrt
    REAL(KIND=4)     :: zur, zul, zut, zub
    INTEGER(KIND=4)  :: ilog, ilu = 88
    !!----------------------------------------------------------------------
    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)

    OPEN(ilu,file=cd_filecoords,form='formatted')
    WRITE(ilu,*)'PLOTFRAME'
    WRITE(ilu,'(e15.5)') zrl
    WRITE(ilu,'(e15.5)')pcoords(1)
    WRITE(ilu,'(e15.5)') zrr
    WRITE(ilu,'(e15.5)')pcoords(2)
    WRITE(ilu,'(e15.5)') zrb
    WRITE(ilu,'(e15.5)')pcoords(3)
    WRITE(ilu,'(e15.5)') zrt
    WRITE(ilu,'(e15.5)')pcoords(4)
    CLOSE(ilu)

  END SUBROUTINE CreateCoordsFile


  SUBROUTINE AddText ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE AddText  ***
    !!
    !! ** Purpose :   Add text to the plot ( all kind of options)
    !!
    !! ** Method  : The glas1 (jp_ib_palette ) is open in the calling program
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4)    :: iclip, ierr
    INTEGER(KIND=4)    :: ji
    INTEGER(KIND=4)    :: ilog
    INTEGER(KIND=4)    :: icolor0  ! original text color
    REAL(KIND=4)       :: zrec(4)
    REAL(KIND=4)       :: zrl, zrr, zrb, zrt,  zur, zul, zut, zub
    CHARACTER(LEN=24)  :: cldum
    CHARACTER(LEN=256) :: clOut
    !!----------------------------------------------------------------------
    CALL gqclip(ierr, iclip, zrec)
    CALL gsclip(0)
    CALL getset( zrl, zrr, zrb, zrt, zur, zul, zut, zub,ilog)
    CALL set (0., 1., 0., 1.,0., 1., 0., 1., 1)

    !  -dat option
    IF ( opt_dat == 1 ) THEN
       cldum = fdate()
       CALL plchhq( 0.01, 0.01, cldum, -0.35, 0., -1.)
    ENDIF

    ! -team option
    IF ( opt_team  ==  1 ) THEN
       cldum = 'LEGI-MEOM'
       CALL plchhq( 0.01, 0.02, cldum, -0.35, 0., -1.)
    ENDIF

    ! -string option
    IF (nstrcount /= 0) THEN
       DO ji = 1, nstrcount
          CALL ParseString (text(ji)%cstr, clOut)

          IF ( text(ji)%ypos  <   0. ) text(ji)%ypos =  zrt + 0.03
          CALL plchhq (text(ji)%xpos, text(ji)%ypos, TRIM(clOut),           &
               &          -(text(ji)%rcsize), 0.,               text(ji)%align )
       ENDDO
    ENDIF

    ! -stringr option
    IF (nstrcountr /= 0) THEN
       DO ji = 1, nstrcountr
          CALL ParseString (textr(ji)%cstr, clOut)

          CALL plchhq (textr(ji)%xpos, textr(ji)%ypos, TRIM(clOut),         &
               &          -(textr(ji)%rcsize), textr(ji)%angle, textr(ji)%align)
       ENDDO
    ENDIF

    ! -stringrc option
    IF (nstrcountrc /= 0) THEN
       DO ji = 1, nstrcountrc
          CALL ParseString (textrc(ji)%cstr, clOut)
          ! save actual CC color
          CALL pcgeti('CC',icolor0)
          ! fix required CC
          CALL pcseti('CC',textrc(ji)%icolor+19) ! in order to address color of palette
          CALL plchhq (textrc(ji)%xpos, textrc(ji)%ypos, TRIM(clOut),         &
               &          -(textrc(ji)%rcsize), textrc(ji)%angle, textrc(ji)%align)
          ! restore original color  CC(0) 
          CALL pcseti('CC',icolor0)
       ENDDO
    ENDIF


    CALL set( zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)
    CALL gsclip (iclip)

  END SUBROUTINE AddText


  SUBROUTINE AddPalette (plimit, kncol)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE AddPalette  ***
    !!
    !! ** Purpose :  Add a palette bar to the plot 
    !!
    !! ** Method  :  The position of the color bar is fixed by x1pal, etc ...
    !!               Call DrawPalette which does the main job
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: plimit    ! (NBOXMAX+1)
    INTEGER(KIND=4),            INTENT(in) :: kncol

    INTEGER(KIND=4)                         :: ji, jmark
    INTEGER(KIND=4)                         :: int_format
    INTEGER(KIND=4), DIMENSION(NBOXMAX+1)   :: ilfin
    INTEGER(KIND=4)                         :: ilog
    REAL(KIND=4)                            :: zrl, zrr, zrb, zrt
    REAL(KIND=4)                            :: zur, zul, zut, zub
    REAL(KIND=4)                            :: zx1per, zy1per, zx2per, zy2per
    REAL(KIND=4)                            :: zcoef, zcoeflab
    REAL(KIND=4)                            :: zxdeb, zxfin, zydeb, zyfin
    REAL(KIND=4)                            :: zval
    CHARACTER(LEN=20), DIMENSION(NBOXMAX+1) :: cllbs  
    !!----------------------------------------------------------------------
    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub,ilog)
    CALL set (0., 1., 0., 1., 0., 1., 0., 1., 1)

    zcoef      = 10.**cl_exp
    int_format = IDformat (int_table(ICLR_PAL)%format)

    DO ji=1,marks_count + 1
       ilfin(ji) = ji
       IF ( opt_clrmark ==  1 ) THEN 
          cllbs(ji)=' '
       ELSE IF (int_format == 1) THEN
          WRITE(cllbs(ji),int_table(ICLR_PAL)%format) NINT (plimit(marks_index(ji)-COLOR_NRES+1)/(zcoef))
          cllbs(ji) = ADJUSTL( cllbs(ji) )
       ELSE 
          WRITE(cllbs(ji),int_table(ICLR_PAL)%format)      (plimit(marks_index(ji)-COLOR_NRES+1)/(zcoef))
          cllbs(ji) = ADJUSTL( cllbs(ji) )
       ENDIF
    ENDDO


    CALL gsfais (1        )
    CALL lbsetr ('WBL', 0.)

    SELECT CASE ( opt_vertpal )
       !--------------------------------------------
    CASE ( 0 )    ! horizontal palette  (default)
       !--------------------------------------------
       CALL DrawPalette (0,           & ! horizontal bar &
            &          x1pal,         & ! left plotter frame coord &
            &          x2pal,         & ! right plotter frame coord &
            &          y1pal,         & ! bottom plotter frame coord &
            &          y2pal,         & ! top plotter frame coord &
            & opt_nobox*marks_count,  & ! number of color boxes &
            &          1.0,           & ! fill all color box width &
            &          1./4.,         & ! fill only part of box height &
            &          ilfin,         & ! color index array &
            &          1,             & !  &
            &          cllbs,         & ! label list &
            &          marks_count+1, & ! label centered in boxes &
            &          nlbpos)          ! position of label 1 below 2 above 3 both
       !                                ! 0 nolabel
       zx1per = x1pal
       zx2per = x2pal
       SELECT CASE ( nlbpos ) 
       CASE ( 0 ,  3 )
          zy1per = y1pal + (1. - 1/4.) /2. * (y2pal - y1pal)
          zy2per = y2pal - (1. - 1/4.) /2. * (y2pal - y1pal)
       CASE ( 1      )
          zy1per = y2pal - (y2pal - y1pal)/4.
          zy2per = y2pal
       CASE ( 2      )
          zy1per = y1pal
          zy2per = y1pal + (y2pal - y1pal)/4.
       END SELECT
       !--------------------------------------------
    CASE ( 1 )    ! vertical palette (-vertpal)
       !--------------------------------------------
       CALL DrawPalette (1,           & ! vertical bar &
            &          x1pal,         & ! left plotter frame coord &
            &          x2pal,         & ! right plotter frame coord &
            &          y1pal,         & ! bottom plotter frame coord &
            &          y2pal,         & ! top plotter frame coord &
            & opt_nobox*marks_count,  & ! number of color boxes &
            &          1.0/4.,        & ! fill all color box width &
            &          1.0,           & ! fill only part of box height &
            &          ilfin,         & ! color index array &
            &          1,             & !  &
            &          cllbs,         & ! label list &
            &          marks_count+1, & ! label centered in boxes &
            &          nlbpos)          ! 1 right, 2 left, 3 both 0 nolabel
       !
       zy1per = y1pal
       zy2per = y2pal
       SELECT CASE ( nlbpos ) 
       CASE ( 0 ,  3 )
          zx1per = x1pal + (1. - 1/4.) /2. * (x2pal - x1pal)
          zx2per = x2pal - (1. - 1/4.) /2. * (x2pal - x1pal)
       CASE ( 1      )
          zx1per = x1pal
          zx2per = x1pal + (x2pal - x1pal)/4.
       CASE ( 2      )
          zx1per = x2pal - (x2pal - x1pal)/4.
          zx2per = x2pal
       END SELECT
    END SELECT

    IF (opt_clrmark  ==  1 ) THEN
       IF ( opt_log /= 1  .AND. opt_clrlog /=1 .AND. opt_cntlog /=1 ) THEN
         DO ji=1,nmark
           IF (int_format == 1) THEN
              WRITE(cllbs(ji),int_table(ICLR_PAL)%format)  NINT (vclrmark(ji)/(zcoef))
              cllbs(ji) = ADJUSTL( cllbs(ji) )
           ELSE
              WRITE(cllbs(ji),int_table(ICLR_PAL)%format)       (vclrmark(ji)/(zcoef))
              cllbs(ji) = ADJUSTL( cllbs(ji) )
           ENDIF
         ENDDO
       ELSE
         DO ji=1,nmark
              zval= 10.**(vclrmark(ji)/(zcoef))
           IF (int_format == 1) THEN
              WRITE(cllbs(ji),int_table(ICLR_PAL)%format)  NINT (zval)
              cllbs(ji) = ADJUSTL( cllbs(ji) )
           ELSE
              WRITE(cllbs(ji),int_table(ICLR_PAL)%format)       (zval)
              cllbs(ji) = ADJUSTL( cllbs(ji) )
           ENDIF
         ENDDO
       ENDIF
    ENDIF

    IF ( opt_nobox  ==  -1 ) THEN   ! case -clrmark 
       CALL plotif(0.,0.,2)
       CALL frstpt( zx1per,zy1per)
       CALL vector( zx1per,zy1per)
       CALL vector( zx2per,zy1per)
       CALL vector( zx2per,zy2per)
       CALL vector( zx1per,zy2per)
       CALL vector( zx1per,zy1per)
       CALL plotif(0.,0.,2)

       SELECT CASE ( opt_vertpal )
       CASE   (  0  )   ! horizontal palette
          zcoeflab = (zx2per - zx1per)/(cl_max-cl_min)
          DO jmark=1,nmark
             xmark(jmark) = zx1per + zcoeflab*(vclrmark(jmark)-cl_min)
          END DO
          !  draw a 'tick' mark
          IF (nlbpos  ==  1 .OR. nlbpos  ==  3) THEN
             zydeb=zy1per
             zyfin=zy1per-0.15*(zy2per-zy1per)
             DO jmark=1,nmark
                !    
                CALL plotif(0.,0.,2)
                CALL frstpt(xmark(jmark),zydeb)
                CALL vector(xmark(jmark),zyfin)
                CALL plchhq(xmark(jmark),zyfin-0.02,TRIM(cllbs(jmark)),rlbsc*0.012,0.,0.)
             END DO
             CALL plotif(0.,0.,2)
          ENDIF
          IF (nlbpos  ==  2 .OR. nlbpos  ==  3) THEN
             zydeb=zy2per
             zyfin=zy2per+0.15*(zy2per-zy1per)
             DO jmark=1,nmark
                !   
                CALL plotif(0.,0.,2)
                CALL frstpt(xmark(jmark),zydeb)
                CALL vector(xmark(jmark),zyfin)
                CALL plchhq(xmark(jmark),zyfin+0.02,TRIM(cllbs(jmark)),rlbsc*0.012,0.,0.)
             END DO
             CALL plotif(0.,0.,2)
          ENDIF
          !
       CASE   (  1  )   ! vertical palette
          zcoeflab = (zy2per - zy1per)/(cl_max-cl_min)
          DO jmark=1,nmark
             xmark(jmark) = zy1per + zcoeflab*(vclrmark(jmark)-cl_min)
          END DO
          IF (nlbpos  ==  1 .OR. nlbpos  ==  3) THEN
             zxdeb=zx2per
             zxfin=zx2per+0.10*(zx2per-zx1per)
             DO jmark=1,nmark
                CALL plotif(0.,0.,2)
                CALL frstpt(zxdeb,xmark(jmark))
                CALL vector(zxfin,xmark(jmark))
                CALL plchhq(zxfin+0.02,xmark(jmark),TRIM(cllbs(jmark)),rlbsc*0.012,0.,-1.)
             END DO
             CALL plotif(0.,0.,2)
          END IF
          IF (nlbpos  ==  2 .OR. nlbpos  ==  3) THEN
             zxdeb=zx1per
             zxfin=zx1per-0.10*(zx2per-zx1per)
             DO jmark=1,nmark
                CALL plotif(0.,0.,2)
                CALL frstpt(zxdeb,xmark(jmark))
                CALL vector(zxfin,xmark(jmark))
                CALL plchhq(zxfin-0.02,xmark(jmark),TRIM(cllbs(jmark)),rlbsc*0.012,0.,1.)
             END DO
             CALL plotif(0.,0.,2)
          ENDIF
       END SELECT
    END IF

    CALL set( zrl, zrr, zrb, zrt, zur, zul, zut, zub,ilog)

  END SUBROUTINE AddPalette


  SUBROUTINE DrawContinents()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DrawContinents  ***
    !!
    !! ** Purpose :   Draw the continent on the map
    !!
    !! ** Method  :  Work is done by colcontinent, argument of arscam
    !!
    !!----------------------------------------------------------------------
    CALL mpseti ('C5 - continental outlines color', COLOR_CONTINENT_PERIM )
    CALL gflas1 ( jp_ib_map )

    IF (opt_mapfill == 1) THEN
       CALL arscam (niama, xcra, ycra, jp_cra, niaia, nigia, jp_agid, colcontinents )
    ENDIF

    SELECT CASE ( opt_high )
    CASE ( 0 )
       CALL maplot
    CASE ( 1 )
       CALL mplndr('Earth..2',1)
    CASE ( 2 )   ! not working yet ...
       CALL mplndr('Earth..4',1)
    END SELECT

    CALL gflas2

  END SUBROUTINE DrawContinents


  SUBROUTINE DrawOceans()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE <routine>  ***
    !!
    !! ** Purpose :   Draw Oceans with the reserved color of the palette
    !!
    !! ** Method  :  Work is done by colocean, argument of arscam
    !!
    !! References :  
    !!----------------------------------------------------------------------
    CALL gflas1(jp_ib_ocean )
    IF ((opt_color == 1) .AND. (opt_noproj == 0)) THEN   
       CALL arscam (niama,xcra,ycra,jp_cra,niaia,nigia,jp_agid,colocean)
    ENDIF

    CALL gflas2

  END SUBROUTINE DrawOceans


  SUBROUTINE DrawMeridians ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DrawMeridians  ***
    !!
    !! ** Purpose :   Draw the meridians over the map, and label them
    !!
    !! References :  Important contribution by Christian Dietrich during
    !!               the DYNAMO project (1994-1998)
    !!----------------------------------------------------------------------
    ! this common cannot be eliminated
    ! it is a way to pass the respective variables
    ! to DessineMeridians which will label the map
    REAL(KIND=4)     ::     zrl, zrr, zrb, zrt,  zur, zul, zut, zub
    COMMON /special/  zrl, zrr, zrb, zrt,  zur, zul, zut, zub

    INTEGER(KIND=4)  :: ilog
    INTEGER(KIND=4)  :: ixtick, iytick
    INTEGER(KIND=4)  :: ixlb, iylb
    !!----------------------------------------------------------------------
    CALL gflas1( jp_ib_meridian )
    CALL pcsetr ('SA',scal_cslab)      ! change character size
    CALL getset( zrl, zrr, zrb, zrt, zul, zur, zub, zut,ilog)

    IF (cmap_proj == 'ME') THEN
       CALL mapgrm (niama,xcra,ycra,jp_cra,niaia,nigia,jp_agid,TraceMeridian)
    ELSE IF ((cmap_proj /= 'CE') .AND. (opt_grid == 1)) THEN
       CALL mapgrm (niama,xcra,ycra,jp_cra,niaia,nigia,jp_agid,TraceMeridiens)
    ENDIF

    CALL getset( zrl, zrr, zrb, zrt, zur, zul, zut, zub,ilog)
    CALL set   ( zrl, zrr, zrb, zrt, rmap_marg(1),rmap_marg(2),rmap_marg(3),rmap_marg(4),1)

    CALL labmod (int_table(ICLR_XAXIS)%format, &
         &       int_table(ICLR_YAXIS)%format, &
         &             0, 0, 1, 1, 10, 10, 0)

    IF ((opt_labx == 0) .OR. (cmap_proj /= 'CE')) THEN
       ixlb = 0
    ELSE 
       ixlb = 1
    ENDIF

    IF ((opt_laby == 0) .OR. (cmap_proj /= 'CE')) THEN
       iylb = 0
    ELSE 
       iylb = 1
    ENDIF

    IF (opt_xstep == 0) xstep = ABS(rmap_marg(1)-rmap_marg(2))/4.0
    IF (opt_ystep == 0) ystep = ABS(rmap_marg(3)-rmap_marg(4))/4.0


    IF ((opt_grad == 0).OR.(cmap_proj /= 'CE')) THEN
       ixtick = 0
       iytick = 0
    ELSE 
       ixtick = NINT (ABS(rmap_marg(1)-rmap_marg(2))/xstep)         
       iytick = NINT (ABS(rmap_marg(3)-rmap_marg(4))/ystep)
    ENDIF


    IF (opt_perim == 1) THEN
       IF (cmap_proj == 'CE') THEN                        
          IF (opt_grid == 1) THEN 
             IF (opt_xgrid == 1) THEN
                CALL gasetr ('WMJ - major tick line width',xlw)
                CALL gridal (ixtick,nxsub,0,0,ixlb,iylb,1,0.,0.)
                CALL gasetr ('WMJ - major tick line width',1.0)
             ENDIF
             IF (opt_ygrid == 1) THEN
                CALL gasetr ('WMJ - major tick line width',ylw)
                CALL gridal (0,0,iytick,nysub,ixlb,iylb,4,0.,0.)
                CALL gasetr ('WMJ - major tick line width',1.0)
             ENDIF
          ELSE 
             CALL gridal (ixtick,nxsub,iytick,nysub,ixlb,iylb,5,0.,0.)
          ENDIF
       ELSE 
          CALL gridal (0,0,0,0,ixlb,iylb,5,0.,0.)
       ENDIF
    ENDIF

    CALL pcsetr ('SA',0.88888888888) ! reset character size

    ! reset coordinated to their original state
    CALL set( zrl, zrr, zrb, zrt, zur, zul, zut, zub,ilog)

    CALL gflas2

  END SUBROUTINE DrawMeridians


  SUBROUTINE TraceMeridiens (pxc, pyc, kmcs, kareaid, kgrpid, kdsize)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE TraceMeridiens  ***
    !!
    !! ** Purpose : This routine is passed to MAPGRM as argument and is 
    !!              used to draw the meridians when the projection is
    !!              not Mercator projection.
    !!
    !! ** Method  :   
    !!
    !! References :  Contouring and Mapping Tutorial, Mp 4.6, page 128
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(kmcs),      INTENT(in) :: pxc   ! kmcs
    REAL(KIND=4), DIMENSION(kmcs),      INTENT(in) :: pyc   ! kmcs
    INTEGER(KIND=4),                    INTENT(in) :: kmcs
    INTEGER(KIND=4), DIMENSION(kdsize), INTENT(in) :: kareaid   ! (kdsize)
    INTEGER(KIND=4), DIMENSION(kdsize), INTENT(in) :: kgrpid    ! (kdsize)
    INTEGER(KIND=4),                    INTENT(in) :: kdsize

    INTEGER(KIND=4)  :: ji
    INTEGER(KIND=4)  :: id
    !!----------------------------------------------------------------------
    DO ji=1,kdsize
       IF (kgrpid(ji) == 1) id = kareaid(ji)
    ENDDO

    IF (opt_map == 1) THEN
       IF (opt_high  ==  0 ) THEN
          IF ((mapaci(id) == 1).AND.(kmcs >= 2)) THEN
             CALL curved (pxc,pyc,kmcs)
          ENDIF
       ELSE
          IF ((mpisci(id) == 1).AND.(kmcs >= 2)) THEN
             CALL curved (pxc,pyc,kmcs)
          ENDIF
       ENDIF
    ELSE 
       CALL curved (pxc,pyc,kmcs)
    ENDIF

  END SUBROUTINE TraceMeridiens
  !
  SUBROUTINE TraceMeridian (pxc, pyc, kmcs, kareaid, kgrpid, kdsize)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE TraceMeridian  ***
    !!
    !! ** Purpose :  Draw meridians and parallel in case of ME projection 
    !!
    !! ** Method  :  The original comments from C. Dieterich are kept as
    !!              an aknowledgment to his work, and in memory of a very
    !!              fruitfull european project : DYNAMO  
    !!
    !! Draw a curve along the the coordinates (pxc,pyc)(ji=1,kmcs). By default 
    !! the line is dotted. You can define other dash patterns with the 
    !! parameter idashid. Refer to the NCAR manual for valid parameters idashid. 
    !! The parameter maskid determinates whether the curves are drawn through 
    !! the whole area (maskid=0, default), over the ocean areas only (maskid=2) 
    !! or over land (maskid=1). Each curve is labelled by it's longitude or 
    !! latitude value. It is assumed that a map is to be labelled. Coordinate 
    !! values therefore must be included in the intervals -360.0<x<360.0 and
    !! -90.0<y<90.0, respectively. If these conditions are not met the line 
    !! is not labelled. The actual label is a string in the ranges '180W' to 
    !! '180E' or '90S' to '90N', depending on the average slope of the drawn 
    !! curve. For slopes in the interval [-1.0,1.0] labels of the form '90S' to 
    !! '90N' are used. Otherwise a meridian ('180W' to '180E') is labelled.
    !! The label is placed near the intersection of the curves with the perimeter 
    !! of the map. The character size may be controlled by zcs_label (default 0.8).
    !! Ch. Dieterich Wed Nov 15 10:42:43 MET 1995
    !! based on subroutine TraceMeridiens (pxc, pyc, kmcs, kareaid, kgrpid, kdsize)
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(kmcs),      INTENT(in) :: pxc     ! kmcs
    REAL(KIND=4), DIMENSION(kmcs),      INTENT(in) :: pyc     ! kmcs
    INTEGER(KIND=4),                    INTENT(in) :: kmcs
    INTEGER(KIND=4), DIMENSION(kdsize), INTENT(in) :: kareaid ! kdsize 
    INTEGER(KIND=4), DIMENSION(kdsize), INTENT(in) :: kgrpid  ! kdsize 
    INTEGER(KIND=4),                    INTENT(in) :: kdsize

    INTEGER(KIND=4)          :: ji
    INTEGER(KIND=4)          :: id
    REAL(KIND=4)             :: zx1w, zx2w, zx1u, zx2u
    REAL(KIND=4)             :: zy1w, zy2w, zy1u, zy2u
    REAL(KIND=4)             :: zlon, zlon0, zlon1, zlon2, zxstep
    REAL(KIND=4)             :: zlat, zlat0, zlat1, zlat2, zystep
    REAL(KIND=4)             :: zrange = 720.
    INTEGER(KIND=4)          :: ixstep, inxstep, inewx, ioldx
    INTEGER(KIND=4)          :: iystep, inystep, inewy, ioldy
    INTEGER(KIND=4)          :: ilab
    INTEGER(KIND=4)          :: ilog
    INTEGER(KIND=4)          :: isolid  = 65535
    INTEGER(KIND=4)          :: idashid = 43690
    INTEGER(KIND=4)          :: imaskid
    CHARACTER(LEN=80)        :: clrlab
    CHARACTER(LEN=80)        :: clab
    CHARACTER(LEN=80)        :: clfmt
    REAL(KIND=4)             :: zcs_label
    REAL(KIND=4)             :: zrl, zrr, zrb, zrt,  zur, zul, zut, zub
    COMMON /special/ zrl, zrr, zrb, zrt,  zur, zul, zut, zub
    !!----------------------------------------------------------------------
    imaskid   = 0   ! draw the lines over all regions
    zcs_label = 0.8

    ! Retrieve the group id.
    DO ji=1,kdsize
       IF (kgrpid(ji) == 1) id = kareaid(ji)
    ENDDO

    ! Set up the dash pattern.
    CALL dashdb(idashid)

    ! Draw the curve.
    IF (opt_grid  ==  1 ) THEN
       IF (opt_map == 1) THEN
          IF (imaskid == 0) THEN
             CALL curved (pxc, pyc, kmcs)
          ELSE IF (mapaci(id) == imaskid .AND. (kmcs >= 2)) THEN
             CALL curved (pxc, pyc, kmcs)
          ENDIF
       ELSE
          CALL curved (pxc, pyc, kmcs)
       ENDIF
    ENDIF

    ! Reset to solid lines.
    CALL dashdb(isolid)
    ! Get the perimeter of the map in world coordinates. 
    ! The calling routine MAPGRM has aligned world and user coordinates.
    CALL getset(zx1w, zx2w, zy1w, zy2w, zx1u, zx2u, zy1u, zy2u, ilog)

    ! Compute the average slope of the curve...
    zlon = ABS(pxc(kmcs) - pxc(1) )
    zlat = ABS(pyc(kmcs) - pyc(1) )
    IF(zlon == 0.0 .AND. zlat == 0.0) RETURN

    ! Find the geographical coordinate of the points pxc,pyc
    ! ... get the coordinate in u/v plan
    ! ... zul ...  zrl ... have been passed by common /special/
    zx1u= zul + (pxc(1)    -  zrl)/( zrr -  zrl)*( zur - zul)
    zy1u= zub + (pyc(1)    -  zrb)/( zrt -  zrb)*( zut - zub)
    zx2u= zul + (pxc(kmcs) -  zrl)/( zrr -  zrl)*( zur - zul)
    zy2u= zub + (pyc(kmcs) -  zrb)/( zrt -  zrb)*( zut - zub)

    ! ... perform inverse projection
    CALL maptri(zx1u, zy1u, zlat1, zlon1)
    CALL maptri(zx2u, zy2u, zlat2, zlon2)
    IF (zlat1  ==  rp_out_range  ) RETURN
    IF (zlat2  ==  rp_out_range  ) RETURN
    IF (zlon1  ==  rp_out_range  ) RETURN
    IF (zlon2  ==  rp_out_range  ) RETURN

    ! Disable clipping.
    CALL gsclip(0)

    ! ...and decide whether it is a meridian or not.
    IF (zlon == 0.0) go to 100
    IF (zlat/zlon > 1.0) go to 100
    ! It's not a meridian. 
    ! Compute a number proportional to the actual y-world coordinate.
    ! It should be an integer number to make sure that the statement 
    ! if(inewy .eq. ioldy)return works properly.
    ! The number of different latitudes recognized and labelled by 
    ! this routine is int((zy2w-zy1w)*zrange).
    inewy = NINT(pyc(1)*zrange)

    ! For the first call, get the grid spacing, the starting latitude
    ! and a positive or negative increment for latitudinal steps.
    IF(inystep == 0) THEN
       CALL mpgetr('GR',zystep)
       IF(pyc(1) < 0.5*(zy2w-zy1w)+zy1w) THEN
          iystep=1
          zlat0=zlat1
       ELSE
          iystep=-1
          zlat0=zlat2
       END IF
       ioldy=inewy*10
    END IF

    ! This y-position has already been labelled.
    IF(inewy == ioldy)    RETURN

    ! Step ahead.
    ioldy   = inewy
    zlat    = zlat0+inystep*zystep
    inystep = inystep+iystep
    ! This is not a valid latitude value.
    IF(ABS(zlat) > 90.0)  RETURN
    ! Labels for the northern or for the southern hemisphere?
    IF(zlat >= 0.0)THEN
       clab(4:4)='N'
    ELSE
       clab(4:4)='S'
    END IF
    ! Set up the labels.
    ! Set up the labels.
    ilab  = NINT(zlat)

    clfmt = '( '//int_table(ICLR_YAXIS)%format//',a)'
    !   look for integer of real format
    IF ( SCAN( clfmt,'Ii')   ==  0 ) THEN
       WRITE(clrlab,clfmt) zlat, clab(4:4)
    ELSE
       WRITE(clrlab,clfmt) NINT(zlat), clab(4:4)
    ENDIF
    clrlab = ADJUSTL( clrlab )

    ! Plot the characters.
    IF (opt_laby  /=  0 ) THEN
       CALL plchhq(zx1w-0.01,pyc(1),TRIM(clrlab),-ABS(zcs_label),0.0,1.0)
       IF (opt_grid  ==  0) CALL line(zx1w,pyc(1),zx1w+0.01,pyc(1))
    ENDIF

    RETURN
    ! It's a meridian. 
    ! Compute a number proportional to the actual x-world coordinate.
    ! It should be an integer number to make sure that the statement 
    ! if(inewx.eq.ioldx)return works properly.
    ! The number of different longitudes recogniced and labelled by 
    ! this routine is int((zx2w-zx1w)*zrange).
100 inewx = INT(pxc(1)*zrange)
    ! For the first call, get the grid spacing, the starting longitude
    ! and a positive or negative increment for longitudinal steps.
    IF (inxstep == 0) THEN
       CALL mpgetr('GR',zxstep)
       IF (pxc(1) < 0.5*(zx2w - zx1w) + zx1w) THEN
          ixstep = 1
          zlon0  = zlon1
       ELSE
          ixstep = -1
          zlon0  = zlon2
       END IF
       ioldx = inewx*10
    END IF
    ! This x-position has already been labelled.
    IF (inewx == ioldx) RETURN
    ! Step ahead.
    ioldx   = inewx
    zlon    = zlon0 + inxstep*zxstep
    inxstep = inxstep + ixstep

    ! This is not a valid longitude value.
    IF (ABS(zlon) > 360.0) RETURN
    zlon = zlon + 360.0
    zlon = MOD (zlon,360.0)
    ! Labels for 'West of Greenwich' or for 'East of Greenwich'?
    IF(NINT(zlon) /= NINT(amod(zlon,180.0)))THEN
       zlon=180.0-amod(zlon,180.0)
       clab(4:4)='W'
    ELSE
       clab(4:4)='E'
    END IF

    ! Set up the labels.
    ilab  = NINT(zlon)
    clfmt = '( '//int_table(ICLR_XAXIS)%format//',a)'

    !   look for integer of real format
    IF ( SCAN( clfmt,'Ii')   ==  0 ) THEN
       WRITE(clrlab,clfmt) zlon, clab(4:4)
    ELSE
       WRITE(clrlab,clfmt) NINT(zlon), clab(4:4)
    ENDIF
    clrlab = ADJUSTL( clrlab )

    ! Plot the characters.
    IF (opt_labx  /=  0 ) THEN
       CALL plchhq(pxc(1),zy1w-0.02,TRIM(clrlab),-ABS(zcs_label),0.0,0.0)
       IF (opt_grid  ==  0) CALL line(pxc(1),zy1w,pxc(1),zy1w+0.01)
    ENDIF

  END SUBROUTINE TraceMeridian


  SUBROUTINE mark (px, py, km, kn, kstp)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE mark  ***
    !!
    !! ** Purpose :  Mark model points over the map, with a little circle
    !!               in this version, do not worl for completery irregular
    !!               grid.
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: px
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: py
    INTEGER(KIND=4),            INTENT(in) :: km
    INTEGER(KIND=4),            INTENT(in) :: kn
    INTEGER(KIND=4),            INTENT(in) :: kstp

    INTEGER(KIND=4)  :: ji, jj
    REAL(KIND=4)     :: zx_map, zy_map
    !!----------------------------------------------------------------------

    CALL gsmksc(.3)

    DO  ji=1,km,kstp
       DO  jj=1,kn,kstp
          CALL maptra (py(jj), px(ji), zx_map, zy_map)
          IF (zx_map /= rp_out_range ) THEN
             CALL points (zx_map, zy_map, 1, -4, 0)
          ENDIF
       ENDDO
    ENDDO

  END SUBROUTINE mark

  SUBROUTINE SetPLotWindow ( bdimg )
    !!---------------------------------------------------------------------
    !!                  ***  SUBROUTINE SetPlotWindow  ***
    !!
    !! ** Purpose :   Set plot windows in order to have undistorted plot 
    !!              when performing a pixel plot ( option -pixel )
    !!
    !! ** Method  : take information in nxdata, nydata to set x1pos x2pos
    !!              y1pos y2pos in order that the plot window have the
    !!              same aspect ration than the data
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),             INTENT(in) :: bdimg

    REAL(KIND=8)    :: dl_gx, dl_gy, dl_dx0, dl_dy0, dl_rp, dl_dx, dl_dy
    INTEGER(KIND=4) :: inx, iny
    !!----------------------------------------------------------------------

    IF ( opt_debug == 1 ) PRINT *,  'SetPlotWindow  ...'
    inx = bdimg%nxdata
    iny = bdimg%nydata

    ! position of the center of the plot window (actual)
    dl_gx = ( x1pos + x2pos) / 2.d0
    dl_gy = ( y1pos + y2pos) / 2.d0

    ! actual size of the window 
    dl_dx0 = ( x2pos - x1pos )
    dl_dy0 = ( y2pos - y1pos )

    ! aspect ratio of the data
    dl_rp = inx * 1.d0 / iny

    ! first guess
    dl_dx = dl_rp * dl_dy0
    dl_dy = dl_dy0

    ! check if it fits in the actual window
    IF ( dl_dx >= dl_dx0 ) THEN
       dl_dx = dl_dx0
       dl_dy = dl_dx0 / dl_rp
    ENDIF

    x1pos = dl_gx - dl_dx /2.d0
    x2pos = dl_gx + dl_dx /2.d0
    y1pos = dl_gy - dl_dy /2.d0
    y2pos = dl_gy + dl_dy /2.d0

    CALL InitMap()
    CALL DrawMeridians()

  END SUBROUTINE SetPlotWindow

END MODULE tracecol
