PROGRAM chart
  !!======================================================================
  !!                     ***  PROGRAM  chart  ***
  !! Main program for plotting charts of 2D fields. Contains the different
  !!  steps achieved to produce a plot.
  !!=====================================================================
  !! History :    1.0  !  06/1993  ! Eric Brown        : original code
  !!           2 -> 6  ! 1993-2010 ! Jean-Marc Molines :  improvements
  !!              7.0  !  11/2010  ! Jean-Marc Molines : F90 and Doctor
  !!----------------------------------------------------------------------

  USE modcom       !  define global variables 
  USE modcolor     !  
  USE modeflim     !
  USE modreadargs  !
  USE util         !
  USE calcul       !
  USE checkfiles   !
  USE readpal      !
  USE isocontours  !
  USE readbimg     !
  USE overlay      !
  USE tracecol     !
  USE vectors      !
  USE cdf, ONLY : PrintBimgStructure        !
  USE val_table    !

  IMPLICIT NONE

  INTEGER(KIND=4) :: jk, jt   ! dummy loop counter
  INTEGER(KIND=4) :: incol

  INTEGER(KIND=4) :: inarg
  INTEGER(KIND=4) :: imap_flag
  INTEGER(KIND=4) :: ierr

  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: zvecdata 
  REAL(KIND=4), DIMENSION(:,:),   ALLOCATABLE :: zcntdata 
  REAL(KIND=4), DIMENSION(:,:),   ALLOCATABLE :: zclrdata
  REAL(KIND=4), DIMENSION(:,:),   ALLOCATABLE :: zintdata

  REAL(KIND=4), DIMENSION(:),     ALLOCATABLE :: zlimit, zxover, zyover
  REAL(KIND=4), DIMENSION(:),     ALLOCATABLE :: zxoverm, zyoverm
  INTEGER(KIND=4)                             :: inover, inoverm

  TYPE( bimgfile) :: blimgclr    ! local bimg structure for color
  TYPE( bimgfile) :: blimgcnt    ! local bimg structure for contours
  TYPE( bimgfile) :: blimgvec(3) ! local bimg structure for vectors (3 components)
  TYPE( bimgfile) :: blimgint    ! local bimg structure for interpolated fields
  TYPE( bimgfile) :: blimgzlevel ! local bimg structure for zlevel (sigma coordinates)
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  PRINT *,' '
  PRINT *, 'Chart version 7.1 (F90)  19/02/2012'
  PRINT *,' (doc web http://www-meom.hmg.inpg.fr/WEB_CHART/)'
  PRINT *,' '
  PRINT *,' In this compilation    NXX = ', NXX
  PRINT *,'                        NYY = ', NYY
  PRINT *,'                         NZ = ', NA
  PRINT *,'                   NOVERPTS = ', NOVERPTS
  PRINT *,'                   Nmaxtime = ', Nmaxtime
!-------------------------------------------------------------------------
  ALLOCATE ( nlevel(NA),  ntime(Nmaxtime) )  ! global variables declared in modom.f90
  ALLOCATE ( xygr(0:NXX+1,0:NYY+1,2))

  ALLOCATE ( zlimit(NBOXMAX+1) )
  ALLOCATE ( zxover(NOVERPTS), zyover(NOVERPTS), zxoverm(NOVERPTS), zyoverm(NOVERPTS) )
  ALLOCATE ( zvecdata (NXX,NYY,2), zcntdata (NXX,NYY), zclrdata (NXX,NYY), zintdata (NXX,NYY) )

  CALL BimgAlloc(blimgclr)
  CALL BimgAlloc(blimgcnt)
  CALL BimgAlloc(blimgvec(1))
  CALL BimgAlloc(blimgvec(2))
  CALL BimgAlloc(blimgvec(3))
  CALL BimgAlloc(blimgint)
  CALL BimgAlloc(blimgzlevel)

  inarg = iargc()  ! number of arguments on the command line

  IF (inarg == 0) THEN
     ! just stop, the header of chart in chart_version.f90 is only
     ! printed
     STOP
  ENDIF

  opt_chart = 1
  opt_coupe = 0

  CALL InitValTable()   ! initialize int_table, real_table and string_table
  CALL SetDefaults()    ! initialize default options

  CALL ReadArgs (inarg) ! Parse command line : a huge task !

  CALL OpenNCAR_GKS()   ! Inialize the plotting file (gmeta or cf_cgm if -o option)

  CALL OpenAndVerifyFiles (blimgclr, blimgzlevel, blimgcnt, blimgvec)

  CALL CheckOptions (blimgclr, blimgcnt, blimgvec)

  CALL SetValTable (blimgclr, blimgcnt, blimgvec(1))

  IF (opt_pal == 1) THEN
     CALL ReadPaletteFile (cf_pal, incol, opt_reverse)
  ELSE
     CALL DefaultPalette (incol, opt_reverse,opt_pal)
  ENDIF

  IF ( opt_pixel == 0 ) THEN
    CALL InitMap()   ! map initialization
    ! Draw fixed elements of the map : argument of the routine are gflas selector id
    CALL DrawMeridians ()
  ELSE
    ! done later in SetPlotWindow
  ENDIF

  IF (opt_ocean    == 1) CALL DrawOceans ()
  IF (opt_map      == 1) CALL DrawContinents ()
  IF (opt_showgrid == 1) CALL ShowGrid (cf_showgrid )

  IF (opt_overdata == 1) THEN
     PRINT *,'Reading overlay points ...'
     CALL OverReadData(zxover,zyover,inover)
     PRINT *,'      Done: ',inover,' points read.'
  ENDIF

  IF (opt_overmark == 1) THEN
     PRINT *,'Reading overmark points ...'
     CALL OverMarkReadData(zxoverm,zyoverm,inoverm)
     PRINT *,'      Done: ',inoverm,' points read.'
  ENDIF

  ! 
  DO jt = 1, max_tstp  ! Loop on time step

     IF (ntime(jt) == 1) THEN  ! check if time jt must be plotted 

        DO jk=1,max_lev ! Loop on levels
           CALL InitMap() ! required to avoid problem in deeper layers ( label of upper layers
                          ! were still visible ...

           IF (nlevel(jk) == 1) THEN  ! check if level jk must be plotted
              IF (opt_single == 0) THEN
                 nkclr = jk
                 nkcnt = jk
                 nkvec = jk
              ENDIF
              IF (opt_dep == 1) THEN
                 nkclr=nkup
                 nkcnt=nkup
                 nkvec=nkup
              ENDIF

              ! Prepare data for plotting
              IF (opt_clrdata == 1) THEN
                 blimgclr%lclr=.TRUE. ; blimgclr%lcnt=.FALSE.
                 CALL BimgReadData (zclrdata,blimgclr,rmap_coord,jt,nkclr,ncl_dim, opt_scaleclr, dscaleclr,  &
                   &                                                  opt_meanclr, vmean0clr, opt_absclr     )
                 IF (blimgclr%lspval0 ) blimgclr%spval = rp_defspval
                 CALL SetClrCNESday (blimgclr%time)
              ENDIF
               IF (lo_debug2) ierr=PrintBimgStructure(blimgclr)

              IF (opt_contours == 1) THEN
                 blimgcnt%lcnt=.TRUE. ; blimgcnt%lclr=.FALSE.
                 CALL BimgReadData (zcntdata,blimgcnt,rmap_coord,jt,nkcnt,nct_dim, opt_scalecnt, dscalecnt, &
                   &                                                 opt_meancnt,  vmean0cnt , opt_abscnt   ) 
                 CALL SetClrCNESday (blimgcnt%time)
                 IF (blimgcnt%lspval0) blimgcnt%spval = rp_defspval
              ENDIF

              IF (opt_vectors == 1) THEN
                 PRINT *,'Vectors are being prepared ..'
                 CALL VectorPrepareData (blimgvec, blimgclr, zclrdata, zvecdata, &
                    &  rmap_coord, jt, nkvec)
              ENDIF

              CALL UpdateValTable (jt, nkclr, nkcnt, nkvec, blimgclr, blimgcnt, blimgvec(1))

              ! creating the plot
              IF (opt_color == 1) THEN   ! color plot
                 IF (opt_clrmark == 1 )  CALL ClrGetMark() 
                 IF (opt_noint   == 0 ) THEN  ! data are being interpolated
                    blimgint = blimgclr
                    CALL interp (zintdata, zclrdata, blimgint)  ! does the interpolation

                    CALL ClrGetLimits (zintdata,blimgint,zlimit,incol)
                    IF (opt_clrlout == 1) CALL WriteLimits (cf_clrlout, zlimit, incol+1)
                    IF ( lo_debug2) ierr=PrintBimgStructure(blimgint)
                    CALL ColorMap (zintdata, blimgint, zlimit, incol)  ! does the map
                 ELSE    ! no data interpolation
                    CALL ClrGetLimits (zclrdata,blimgclr,zlimit,incol)
                    IF (opt_clrlout == 1) CALL WriteLimits (cf_clrlout, zlimit, incol+1)
                    IF ( lo_debug2) ierr=PrintBimgStructure(blimgclr)
                    CALL ColorMap (zclrdata, blimgclr, zlimit, incol)  ! does the map
                 ENDIF
              ENDIF

              IF (opt_contours == 1) THEN ! contour plot
                 IF (opt_noproj == 1) THEN
                    imap_flag=0   ! no projection used
                 ELSE
                    imap_flag=1   ! some projection used
                 ENDIF

                 IF (blimgcnt%ngrid == 3) THEN  ! totaly irregular grid cpmpxy will be used
                    imap_flag=6
                 ENDIF

                 CALL TraceContours (zcntdata,blimgcnt,rmap_coord,imap_flag) ! does the contour
              ENDIF

              IF (opt_vecnotr == 0 .AND. opt_vectors == 1) THEN  ! vector plot
                 IF (opt_clrmark  ==  1 )  CALL ClrGetMark()
                 CALL VectorTrace (blimgvec, blimgclr, zclrdata, zvecdata, incol, rmap_coord)
              ENDIF

              ! overlay data
              IF (opt_overdata == 1) CALL OverTrace(zxover,zyover,inover)

              ! overmark data 
              IF (opt_overmark == 1) CALL OverMarkTrace(zxoverm,zyoverm,inoverm)

              CALL gflas1( jp_ib_palette )
              CALL AddText ()

              IF ((opt_palbar == 1) .AND. (opt_color == 1)) THEN
                 CALL AddPalette (zlimit, incol)
              ENDIF

              IF (opt_marks == 1) THEN
              !   "  CECI EST UNE HORRIBLE PATCHE (pour Bernard) " Eric Brown, 1993
              !  " This horrible patch is still there in 2010"  JMM 
                 CALL mark (blimgclr%d_xgrid, blimgclr%d_ygrid, blimgclr%nxfile, blimgclr%nyfile, mark_step )
              ENDIF
              CALL gflas2

              !     assemblage des differentes parties du dessin

              IF (opt_ocean    == 1)                        CALL gflas3( jp_ib_ocean    )
!             IF (opt_color    == 1 .AND. opt_overclr == 0) CALL gflas3( jp_ib_color    )
              IF (opt_color    == 1 )                       CALL gflas3( jp_ib_color    )
              IF (opt_contours == 1)                        CALL gflas3( jp_ib_contours )
              IF (opt_vecnotr  == 0 .AND. opt_vectors == 1) CALL gflas3( jp_ib_vectors  )
              IF (opt_map      == 1)                        CALL gflas3( jp_ib_map      )
              IF (opt_overdata == 1)                        CALL gflas3( jp_ib_overdata )
              IF (opt_overmark == 1)                        CALL gflas3( jp_ib_overmark )
              IF (opt_showgrid == 1)                        CALL gflas3( jp_ib_showgrid )
                                                            CALL gflas3( jp_ib_meridian )
                                                            CALL gflas3( jp_ib_palette  )
              CALL frame   ! advance to the next picture 
           ENDIF
           IF (blimgclr%lspval0) blimgclr%spval = 0  ! restore true spval for next plots
           IF (blimgcnt%lspval0) blimgcnt%spval = 0
        ENDDO    ! layers
     ENDIF
  ENDDO    ! time step

  CALL gclwk(9)
  CALL clsgks

#ifdef linux
  !     cleaning of temporary links
  CALL system ('/bin/rm -f *.tmpclrnk')
  CALL system ('/bin/rm -f *.tmpcntlnk')
#endif

  PRINT *,' Chart ended successfully'
  PRINT *,' '
  PRINT *,' '
  CALL flush(6)

END PROGRAM chart
