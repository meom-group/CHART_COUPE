MODULE checkfiles
  !!======================================================================
  !!                     ***  MODULE  checkfiles  ***
  !! All routines concerning the coherency of options and files existence
  !! Set the major part of the data structure
  !!=====================================================================
  !! History : 1.0   ! 06/2010  ! E. Brown     : original code
  !!                 ! 11/2010  ! J.M. Molines : F90 and Dr form
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   OpenAndVerifyFiles :  Open files and read header, fill data structure
  !!   CheckOptions       :  check options consistency
  !!   ReadGrid           :  Read Grid files
  !!   ReadMask           :  Read Mask files 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  USE modcom
  USE modcolor
  USE util
  USE readbimg 

  IMPLICIT NONE

  PRIVATE

  PUBLIC  :: OpenAndVerifyFiles   ! used from chart/coupe
  PUBLIC  :: CheckOptions         ! used from chart/coupe
  PUBLIC  :: ReadGrid             ! used  from coupe

  PRIVATE :: ReadMask             ! internal use

CONTAINS

  SUBROUTINE OpenAndVerifyFiles (bdcolor, bdzlevel, bdcontours, bdvectors)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OpenAndVerifyFiles  ***
    !!
    !! ** Purpose :  Open all types of data files, read header and set 
    !!              caracteristics in the respective data structure
    !!
    !! ** Method  :  This routine is a key part for data managing and
    !!              mapping correspondence. bimgfile are defined in out
    !!              because they are allocated elsewhere
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile), INTENT(inout) :: bdcolor      ! color data structure
    TYPE( bimgfile), INTENT(inout) :: bdzlevel     ! zlevel data structure (sigma coordinates)
    TYPE( bimgfile), INTENT(inout) :: bdcontours   ! contour data structure 
    TYPE( bimgfile), INTENT(inout) :: bdvectors(3) ! vector data structure

    INTEGER(KIND=4) :: ji, jj              ! dummy loop index
    INTEGER(KIND=4) :: izoom_set
    INTEGER(KIND=4) :: inx, iny
    INTEGER(KIND=4) :: ikinf, iksup, iknear

    CHARACTER(LEN=255)  :: cmd
    !!----------------------------------------------------------------------
    izoom_set = 0
    ! ... initialize some stuff for NetCdf 
    !
    bdcolor%cvarname      = cv_clr
    bdcontours%cvarname   = cv_cnt
    bdvectors(1)%cvarname = cv_vecx
    bdvectors(2)%cvarname = cv_vecy
    !
    bdcolor%cmodifier     = cmodif_clr
    bdcontours%cmodifier  = cmodif_cnt
    bdvectors(1)%cmodifier= cmodif_vecx
    bdvectors(2)%cmodifier= cmodif_vecy
    !
    IF (opt_dep /= 0 ) THEN
       nlevel (1:NA) = 0
    ENDIF

    IF (opt_sigma == 1) THEN
       bdzlevel%cfname  = cf_zlevel
       bdzlevel%num = BimgOpenFile(bdzlevel)
       CALL BimgReadHeader(bdzlevel)
       IF (opt_clrgrid /= 0) THEN 
          CALL ReadGrid(bdzlevel,cf_clrgrid)
       ENDIF
    ENDIF

    IF (opt_single == 1) THEN
       max_lev   = 1
       nlevel(1) = 1
    ENDIF

    !     COLORS  ---------------------------------------------

    IF (opt_clrdata == 1) THEN
#ifdef linux
       ! Linux adaptation : a link on the data file is created with extension.tmpclr
       !    this is a workaround for some linux compiler (eg pgf) which cannot open twice
       !    the same file (ie for contour, for colors etc ... )
       CALL system('basename '//cf_clrdata//' > zzzbasenam') 
       OPEN(3,file='zzzbasenam')
       READ(3,'(a)') cmd
       CLOSE(3)
       CALL system('/bin/rm zzzbasenam')
       cmd=TRIM(cmd)//'.tmpclrnk'
       CALL system(' ln -s '//TRIM(cf_clrdata)//' '//cmd)
       cf_clrdata=cmd
#endif
       bdcolor%cfname  = cf_clrdata
       bdcolor%num = BimgOpenFile (bdcolor)
       IF ( .NOT. ALLOCATED ( bdcolor%depth ) ) CALL BimgAlloc(bdcolor)
       CALL BimgReadHeader(bdcolor)
       IF (bdcolor%spval == 0 ) PRINT *,' WARNING  CLRDATA :  spval = 0'

       IF (opt_dep /= 0) THEN
          ! find data levels corresponding to req_dep
          !      
          CALL LevelOfDepth(bdcolor, req_dep, iksup, ikinf, iknear)        
          nkup           = iksup
          nlevel(iknear) = 1
          max_lev        = iknear
       ENDIF

       ! time
       IF (max_tstp == 99999) THEN
          max_tstp = bdcolor%nt
       ENDIF

       IF (bdcolor%nt < max_tstp) THEN
          CALL PrintMessage (jp_Err, jp_IncorTim, TRIM(cf_clrdata))
       ENDIF

       ! level/layer

       IF (max_lev == 99999) THEN
          max_lev = bdcolor%nzfile
       ENDIF

       IF (bdcolor%nzfile < max_lev) THEN
          CALL PrintMessage (jp_Err, jp_IncorLev, TRIM(cf_clrdata))
       ENDIF

       ! mask option

       IF (opt_clrmask /= 0) THEN
          CALL ReadMask (bdcolor, cf_clrmask)
       ENDIF

       ! grid option

       IF (opt_clrgrid >= 1) THEN
          CALL ReadGrid (bdcolor, cf_clrgrid)
       ENDIF

       ! coordinates

       IF (opt_zoom == 1) THEN

          IF ( (bdcolor%x1mod+shift_map) > rmap_coord(1)) THEN
             PRINT *, 'Warning ... Zoom lon min < x1 --> mis a x1 +0.01'
             rmap_coord(1) = bdcolor%x1mod+shift_map +0.01
          ENDIF
          IF ( (bdcolor%x2mod+shift_map) < rmap_coord(2)) THEN
             PRINT *, 'Warning ... Zoom lon max > x2 --> mis a x2 -0.01'
             rmap_coord(2) = bdcolor%x2mod+shift_map -0.01
          ENDIF
          IF ( (bdcolor%y1mod) > rmap_coord(3)) THEN
             PRINT *, 'Warning ... Zoom lat min < y1  --> mis a y1 +0.01'
             rmap_coord(3) = bdcolor%y1mod + 0.01
          ENDIF
          IF ( (bdcolor%y2mod) < rmap_coord(4)) THEN
             PRINT *, 'Warning ... Zoom lat max > y2  --> mis a y2 -0.01'
             rmap_coord(4) = bdcolor%y2mod - 0.01
          ENDIF
       ELSE 
          rmap_coord(1) = bdcolor%x1mod 
          rmap_coord(2) = bdcolor%x2mod 
          rmap_coord(3) = bdcolor%y1mod
          rmap_coord(4) = bdcolor%y2mod
          izoom_set = 1
       ENDIF
    ENDIF

    !     CONTOURS ----------------------------------------------------

    IF (opt_contdata == 1) THEN
#ifdef linux
       ! Linux adaptation : a link on the data file is created with extension.tmpcnt
       !    this is a workaround for some linux compiler (eg pgf) which cannot open twice
       !    the same file (ie for contour, for colors etc ... )
       CALL system('basename '//cf_cntdata//' > zzzbasenam')
       OPEN(3,file='zzzbasenam')
       READ(3,'(a)') cmd
       CLOSE(3)
       CALL system('/bin/rm zzzbasenam')
       cmd=TRIM(cmd)//'.tmpcntlnk'
       CALL system(' ln -s '//TRIM(cf_cntdata)//' '//cmd)
       cf_cntdata=cmd
#endif
       bdcontours%cfname = cf_cntdata
       bdcontours%num = BimgOpenFile (bdcontours)
       CALL BimgReadHeader(bdcontours)
       IF (bdcontours%spval  ==  0 ) PRINT *,' WARNING  CNTDATA :  spval = 0'

       IF (opt_dep /= 0) THEN
          ! Look for depths correponding to req_depth
          !      
          CALL LevelOfDepth(bdcontours, req_dep, iksup, ikinf, iknear)        
          nkup=iksup
          nlevel(iknear)=1
          max_lev=iknear
       ENDIF

       !     temps

       IF (max_tstp == 99999) THEN
          max_tstp = bdcontours%nt
       ENDIF

       IF (bdcontours%nt < max_tstp) THEN
          CALL PrintMessage (jp_Err, jp_IncorTim, TRIM(cf_cntdata))
       ENDIF

       !     nlevel

       IF (max_lev == 99999) THEN
          max_lev = bdcontours%nzfile
       ENDIF

       IF (bdcontours%nzfile < max_lev) THEN
          CALL PrintMessage (jp_Err, jp_IncorLev, TRIM(cf_cntdata))
       ENDIF

       !     mask option

       IF (opt_contmask /= 0) THEN
          CALL ReadMask (bdcontours, cf_cntmask)
       ENDIF

       !     grid option

       IF (opt_contgrid >= 1) THEN
          CALL ReadGrid (bdcontours, cf_cntgrid)
       ENDIF

       !     coordonnees

       IF (opt_zoom == 1) THEN
          IF ( (bdcontours%x1mod+shift_map) > rmap_coord(1)) THEN
             PRINT *, 'Warning ... Zoom lon min < x1 --> set to x1 '
             rmap_coord(1) = bdcontours%x1mod+shift_map
          ENDIF
          IF ( (bdcontours%x2mod+shift_map) < rmap_coord(2)) THEN
             PRINT *, 'Warning ... Zoom lon max > x2 --> set to  x2 '
             rmap_coord(2) = bdcontours%x2mod+shift_map
          ENDIF
          IF ( (bdcontours%y1mod) > rmap_coord(3)) THEN
             PRINT *, 'Warning ... Zoom lat min < y1  --> set to y1 '
             rmap_coord(3) = bdcontours%y1mod
          ENDIF
          IF ( (bdcontours%y2mod) < rmap_coord(4)) THEN
             PRINT *, 'Warning ... Zoom lat max > y2  --> set to y2 '
             rmap_coord(4) = bdcontours%y2mod
          ENDIF
       ELSE 
          IF (izoom_set == 1) THEN
             IF (bdcontours%x1mod > rmap_coord(1))  rmap_coord(1) = bdcontours%x1mod
             IF (bdcontours%x2mod < rmap_coord(1))  rmap_coord(2) = bdcontours%x2mod
             IF (bdcontours%y1mod > rmap_coord(3))  rmap_coord(3) = bdcontours%y1mod
             IF (bdcontours%y2mod < rmap_coord(4))  rmap_coord(4) = bdcontours%y2mod
          ELSE 
             rmap_coord(1) = bdcontours%x1mod
             rmap_coord(2) = bdcontours%x2mod
             rmap_coord(3) = bdcontours%y1mod
             rmap_coord(4) = bdcontours%y2mod
             izoom_set = 1
          ENDIF
       ENDIF
    ENDIF

    !     VECTEURS ----------------------------------------------------

    IF (opt_vectors == 1) THEN
       IF ((opt_vect3D == 1) .OR. (opt_vect2D == 1)) THEN
          bdvectors(1)%cfname = cf_vecdata1
          bdvectors(1)%num = BimgOpenFile (bdvectors(1))
          CALL BimgReadHeader(bdvectors(1))
          IF (bdvectors(1)%spval  ==  0 ) PRINT *,' WARNING  VECTORS UV :  spval = 0'
          bdvectors(2)=bdvectors(1)

          IF (opt_dep /= 0) THEN
             ! Look for depths correponding to req_depth
             !      
             CALL LevelOfDepth(bdvectors(1),req_dep,iksup,ikinf,iknear)        
             nkup=iksup
             nlevel(iknear)=1
             max_lev=iknear
          ENDIF

       ELSE

          IF (opt_vectX == 1) THEN
             bdvectors(1)%cfname = cf_vecdata1
             bdvectors(1)%num = BimgOpenFile (bdvectors(1))
             CALL BimgReadHeader(bdvectors(1))
             IF (bdvectors(1)%spval  ==  0 ) PRINT *,' WARNING  VECTORS U :  spval = 0'
             IF (opt_dep /= 0) THEN
                ! Look for depths correponding to req_depth
                !      
                CALL LevelOfDepth(bdvectors(1),req_dep,iksup,ikinf,iknear)        
                nkup=iksup
                nlevel(iknear)=1
                max_lev=iknear
             ENDIF
          ENDIF

          IF (opt_vectY == 1) THEN
             bdvectors(2)%cfname = cf_vecdata2
             bdvectors(2)%num = BimgOpenFile (bdvectors(2))
             CALL BimgReadHeader(bdvectors(2))
             IF (bdvectors(2)%spval  ==  0 ) PRINT *,' WARNING  VECTORS V :  spval = 0'
             IF (opt_dep /= 0) THEN
                ! Look for depths correponding to req_depth
                !      
                CALL LevelOfDepth(bdvectors(2),req_dep,iksup,ikinf,iknear)        
                nkup=iksup
                nlevel(iknear)=1
                max_lev=iknear
             ENDIF
          ENDIF

          IF (opt_vectZ == 1) THEN
             bdvectors(3)%cfname = cf_vecdata3
             bdvectors(3)%num = BimgOpenFile (bdvectors(3) )
             CALL BimgReadHeader(bdvectors(3))
             IF (bdvectors(3)%spval  ==  0 ) PRINT *,' WARNING  VECTORS W:  spval = 0'
             IF (opt_dep /= 0) THEN
                ! Look for depths correponding to req_depth
                !      
                CALL LevelOfDepth(bdvectors(3),req_dep,iksup,ikinf,iknear)        
                nkup=iksup
                nlevel(iknear)=1
                max_lev=iknear
             ENDIF
          ENDIF

          IF ((opt_vectX == 1).AND.(opt_vectY == 1)) THEN
             IF ((bdvectors(1)%nxfile /= bdvectors(2)%nxfile).OR. &
                &             (bdvectors(1)%nyfile /= bdvectors(2)%nyfile).OR. &
                &             (bdvectors(1)%spval  /= bdvectors(2)%spval )) THEN
                PRINT *,' Cas 1'
                CALL PrintMessage (jp_Err, jp_VecMisMa, ' ')
             ENDIF
             ! patches pour l'option Cgrid qui travaille avec des fichiers
             ! n'ayant pas necessairement le meme layout...
             !
             IF (((bdvectors(1)%x1mod  /= bdvectors(2)%x1mod ).OR. &
                &             (bdvectors(1)%dx     /= bdvectors(2)%dx    ).OR. &
                &             (bdvectors(1)%dy     /= bdvectors(2)%dy    ).OR. &
                &             (bdvectors(1)%y1mod  /= bdvectors(2)%y1mod )).AND. &
                &             (opt_cgrid == 0)) THEN
                PRINT *,' Cas 2'
                CALL PrintMessage (jp_Err, jp_VecMisMa, ' ')
             ELSE
                bdvectors(1)%x1mod = bdvectors(2)%x1mod
                bdvectors(1)%y1mod = bdvectors(2)%y1mod
                bdvectors(1)%x2mod = bdvectors(2)%x2mod
                bdvectors(1)%y2mod = bdvectors(2)%y2mod
                bdvectors(1)%dx    = bdvectors(2)%dx
                bdvectors(1)%dy    = bdvectors(2)%dy
             ENDIF
          ENDIF

          IF ((opt_vectX == 1).AND.(opt_vectZ == 1)) THEN
             IF ((bdvectors(1)%nxfile /= bdvectors(3)%nxfile).OR. &
                &             (bdvectors(1)%nyfile /= bdvectors(3)%nyfile).OR. &
                &             (bdvectors(1)%dx     /= bdvectors(3)%dx    ).OR. &
                &             (bdvectors(1)%dy     /= bdvectors(3)%dy    ).OR. &
                &             (bdvectors(1)%x1mod  /= bdvectors(3)%x1mod ).OR. &
                &             (bdvectors(1)%y1mod  /= bdvectors(3)%y1mod ).OR. &
                &             (bdvectors(1)%spval  /= bdvectors(3)%spval )) THEN
                PRINT *,' Cas 3'
                CALL PrintMessage (jp_Err, jp_VecMisMa, ' ')
             ENDIF
          ENDIF

          IF ((opt_vectZ == 1).AND.(opt_vectY == 1)) THEN
             IF ((bdvectors(3)%nxfile /= bdvectors(2)%nxfile).OR. &
                &             (bdvectors(3)%nyfile /= bdvectors(2)%nyfile).OR. &
                &             (bdvectors(3)%dx     /= bdvectors(2)%dx    ).OR. &
                &             (bdvectors(3)%dy     /= bdvectors(2)%dy    ).OR. &
                &             (bdvectors(3)%x1mod  /= bdvectors(2)%x1mod ).OR. &
                &             (bdvectors(3)%y1mod  /= bdvectors(2)%y1mod ).OR. &
                &             (bdvectors(3)%spval  /= bdvectors(2)%spval )) THEN
                PRINT *,' Cas 4'
                CALL PrintMessage (jp_Err, jp_VecMisMa, ' ')
             ENDIF
          ENDIF
       ENDIF

       !     time

       IF (max_tstp == 99999) THEN
          max_tstp = bdvectors(1)%nt
       ENDIF

       IF (bdvectors(1)%nt < max_tstp) THEN
          CALL PrintMessage (jp_Err, jp_IncorTim, TRIM(cf_clrdata))
       ENDIF

       !     nlevel

       IF (max_lev == 99999) THEN
          max_lev = bdvectors(1)%nzfile
       ENDIF

       IF (bdvectors(1)%nzfile < max_lev) THEN
          CALL PrintMessage (jp_Err, jp_IncorLev, TRIM(cf_clrdata))
       ENDIF

       inx = bdvectors(1)%nxfile
       iny = bdvectors(1)%nyfile

       !     mask option

       IF (opt_vectmask /= 0) THEN
          CALL ReadMask (bdvectors(1), cf_vecmask)
       ENDIF

       !     grid option

       IF (opt_vectgrid >= 1) THEN
          bdvectors(2)%x1mod = bdvectors(1)%x1mod
          bdvectors(2)%x2mod = bdvectors(1)%x2mod
          bdvectors(2)%y1mod = bdvectors(1)%y1mod
          bdvectors(2)%y2mod = bdvectors(1)%y2mod
          bdvectors(2)%dx    = bdvectors(1)%dx
          bdvectors(2)%dy    = bdvectors(1)%dy

          CALL ReadGrid (bdvectors(1), cf_vecgrid)
          bdvectors(2)%ngrid = bdvectors(1)%ngrid
          DO ji = 1,inx
             bdvectors(2)%d_xgrid(ji) = bdvectors(1)%d_xgrid(ji)
          ENDDO
          DO jj = 1,iny
             bdvectors(2)%d_ygrid(jj) = bdvectors(1)%d_ygrid(jj)
          ENDDO

          IF (opt_zoom == 0) THEN
             rmap_coord(1) = bdvectors(1)%x1mod
             rmap_coord(2) = bdvectors(1)%x2mod
             rmap_coord(3) = bdvectors(1)%y1mod
             rmap_coord(4) = bdvectors(1)%y2mod
          ENDIF

          bdvectors(1)%x1mod = bdvectors(2)%x1mod
          bdvectors(1)%x2mod = bdvectors(2)%x2mod
          bdvectors(1)%y1mod = bdvectors(2)%y1mod
          bdvectors(1)%y2mod = bdvectors(2)%y2mod
          bdvectors(1)%dx    = bdvectors(2)%dx
          bdvectors(1)%dy    = bdvectors(2)%dy
       ENDIF

       !     coordonnees

       IF (opt_zoom == 1) THEN
          IF (opt_vectgrid == 0) THEN
             IF (((bdvectors(1)%x1mod+shift_map) > rmap_coord(1)).OR. &
                &             ((bdvectors(1)%x2mod+shift_map) < rmap_coord(2)).OR. &
                &             (bdvectors(1)%y1mod > rmap_coord(3)).OR. &
                &             (bdvectors(1)%y2mod < rmap_coord(4))) THEN
                CALL PrintMessage (jp_Err, jp_CoordOut, ' ')
                STOP
             ENDIF
          ELSE IF ((bdvectors(1)%d_xgrid(1) > rmap_coord(1)).OR. &
             &               (bdvectors(1)%d_xgrid(inx) < rmap_coord(2)).OR. &
             &               (bdvectors(1)%d_ygrid(1) > rmap_coord(3)).OR. &
             &               (bdvectors(1)%d_ygrid(iny) < rmap_coord(4))) THEN
             CALL PrintMessage (jp_Err, jp_CoordOut, ' ')
             !               stop
          ENDIF
       ELSE 
          IF (opt_vectgrid == 0) THEN
             IF (izoom_set == 1) THEN
                IF (bdvectors(1)%x1mod > rmap_coord(1)) &
                &                   rmap_coord(1) = bdvectors(1)%x1mod
                IF (bdvectors(1)%x2mod < rmap_coord(1)) &
                &                   rmap_coord(2) = bdvectors(1)%x2mod
                IF (bdvectors(1)%y1mod > rmap_coord(3)) &
                &                   rmap_coord(3) = bdvectors(1)%y1mod
                IF (bdvectors(1)%y2mod < rmap_coord(4)) &
                &                   rmap_coord(4) = bdvectors(1)%y2mod
             ELSE 
                rmap_coord(1) = bdvectors(1)%x1mod
                rmap_coord(2) = bdvectors(1)%x2mod
                rmap_coord(3) = bdvectors(1)%y1mod
                rmap_coord(4) = bdvectors(1)%y2mod
             ENDIF
          ENDIF
       ENDIF
    ENDIF

    IF (opt_zoom == 0) THEN
       rmap_coord(1) = rmap_coord(1) + shift_map
       rmap_coord(2) = rmap_coord(2) + shift_map
       opt_zoom = 1
    ENDIF


    IF (opt_marg == 0) THEN
       rmap_marg(1) = rmap_coord(1)
       rmap_marg(2) = rmap_coord(2)
       rmap_marg(3) = rmap_coord(3)
       rmap_marg(4) = rmap_coord(4)
    ENDIF

  END SUBROUTINE OpenAndVerifyFiles


  SUBROUTINE ReadMask (bdimg, cd_filename)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ReadMask
    !!
    !! ** Purpose : Read mask file and check dimensions with respect to 
    !!           the corresponding data file
    !!
    !! ** Method  : Replace spval by 1 and other values by 0 in the internal
    !!           array
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile), INTENT(inout) :: bdimg
    CHARACTER(LEN=*),   INTENT(in) :: cd_filename

    TYPE( bimgfile)                :: blmask
    !!----------------------------------------------------------------------
    CALL BimgAlloc (blmask)

    blmask%cfname = cd_filename
    blmask%num = BimgOpenFile (blmask)

    CALL BimgReadHeader(blmask)

    IF ((blmask%nxfile /= bdimg%nxfile).OR. &
       &    (blmask%nyfile /= bdimg%nyfile)) THEN
       PRINT *,'ERROR: mask size does not correspond to data size'
       STOP
    ENDIF

    bdimg%mask = 1
    CALL BimgGetLayer  (blmask, bdimg%d_mask, 1, 1, 1)
    CLOSE (blmask%num)
    CALL BimgDeAlloc (blmask)

  END SUBROUTINE ReadMask


  SUBROUTINE ReadGrid (bdimg, cd_filename)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ReadGrid  ***
    !!
    !! ** Purpose : Read external grid file and perform some checking on size 
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile), INTENT(inout) :: bdimg
    CHARACTER(LEN=*),   INTENT(in) :: cd_filename

    TYPE( bimgfile)                :: blgrid
    !!----------------------------------------------------------------------
    CALL BimgAlloc (blgrid)
    blgrid%cfname = cd_filename
    blgrid%num    = BimgOpenFile (blgrid)

    CALL BimgReadHeader(blgrid)

    IF ((blgrid%nxfile /= bdimg%nxfile).OR. &
       &    (blgrid%nyfile /= bdimg%nyfile)) THEN
       PRINT *,'ERROR: grid size does not correspond to data size'
       STOP
    ENDIF

    CALL BimgGetGridInfo (bdimg,blgrid)

    CLOSE (blgrid%num)
    CALL BimgDeAlloc (blgrid)

  END SUBROUTINE ReadGrid


  SUBROUTINE CheckOptions(bdimgclr, bdimgcnt, bdimgvec)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CheckOptions  ***
    !!
    !! ** Purpose :  Look for conflicts between options
    !!
    !! ** Method  :  Remark : so far, only bdimgvec is used !
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile), INTENT(in) ::  bdimgclr, bdimgcnt, bdimgvec(3)
    !!----------------------------------------------------------------------

    ! For chart, gridxy option impose opt_print = 1 in order to go through
    ! cpmpxy (which deals with irregular grids). Not necessary for coupe.
    IF (opt_chart  ==  1 .AND. opt_clrgrid  ==  3 ) THEN
       opt_print = 1
    ENDIF
     
    ! In case of cntshade, ncl_dim = nct_dim is required
    IF (opt_shade  ==  1 ) THEN
       ncl_dim = nct_dim
    ENDIF
     
    ! In case of vecshade, if an external irregular grid is specified,
    ! it should be done with option -gridxy
    IF (opt_vecshade  ==  1 .AND. opt_clrgrid /= 0) THEN
       IF (opt_clrgrid /= 3) THEN
          !  
          PRINT *,' ERROR : With vecshade, you must use -gridxy option.'
          STOP ' in CheckOptions'
       ENDIF
    ENDIF

    ! In case of vecclr option, if an external irregular grid is specified,
    ! it should be done with option -gridxy
    !
    IF (opt_vectclr  ==  1) THEN
       IF (opt_clrgrid == 1 .OR. opt_clrgrid  ==  2) THEN
          PRINT *,' ERROR : With vecclr, you must use -gridxy option.'
          STOP ' in CheckOptions'
          STOP 
       ENDIF
    ENDIF

    ! In case of vecclr, check that scalar fields to be used have the
    ! same dimensions than the vector field.
    ! JMM : this check was disable because of the C-grid and spem like files
    !      Because there are no more spem like files around here, I re-enable it 
    IF ((opt_vectclr == 1).AND.(opt_clrdata == 1)) THEN
       IF ((bdimgclr%nxfile /= bdimgvec(1)%nxfile).OR. &
          &       (bdimgclr%nyfile /= bdimgvec(1)%nyfile).OR. &
          &       (bdimgclr%dx /= bdimgvec(1)%dx).OR. &
          &       (bdimgclr%dy /= bdimgvec(1)%dy)) THEN
                   PRINT *, 'ERROR :  With vecclr, color data must have the same size '
                   PRINT *, '         than the vector data. '
                   STOP ' in CheckOptions'
       ENDIF
    ENDIF

    ! In case of vecclr, a clrdata file must be specified
    IF ((opt_vectclr == 1).AND.(opt_clrdata == 0)) THEN
       PRINT *, 'ERROR : With vecclr, you must specify a file with -clrdata '
       PRINT *, '        corresponding to the color field you want to use'
       STOP ' in CheckOptions'
    ENDIF

    ! Options -vecclr and -vecmod cannot be used together
    IF ((opt_vectclr == 1).AND.(opt_vectmod == 1)) THEN
       PRINT *, ' ERROR : You cannot specify -vecclr and -vecmod  at the same time'
       PRINT *, '        ( vectors cannot be colored with their module and a scalar'
       PRINT *, '        at the same time).'
       STOP ' in CheckOptions'
    ENDIF

    ! If vecclr is required, then disable color option because colored vectors
    ! upon a colored background are not visible.
    IF ((opt_vectclr == 1).AND.(opt_clrdata == 1)) THEN
       PRINT *, ' WARNING : color option desactivated, in favor of vecclr '
       opt_color = 0
    ENDIF
    !
    ! When using -clrmark, colors are to be choosen with a linear scale, clrmet = 1
    IF (opt_clrmark  ==  1 .AND. cl_met  /=  1 ) THEN
       cl_met = 1
       PRINT *, ' WARNING : clrmet enforced to 1, for clrmark to work properly'
    ENDIF
    IF (opt_clrmark  ==  1 ) opt_nobox = -1

  END SUBROUTINE CheckOptions

END MODULE checkfiles

