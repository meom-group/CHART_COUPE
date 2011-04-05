PROGRAM coupe
  !!======================================================================
  !!                     ***  PROGRAM  coupe  ***
  !! This is the main program to perform sections accross data files
  !!=====================================================================
  !! History :    1.0  !  06/1993      Eric Brown        : Original code
  !!         : 2 -> 6  ! 1994 - 2009   Jean-Marc Molines : Various improvement
  !!         :    7.0  ! 12/2010       Jean-Marc Molines : F90 and Doctor
  !!----------------------------------------------------------------------

   USE modcom
   USE modcolor
   USE modeflim
   USE modreadargs
   USE util
   USE checkfiles
   USE readpal
   USE sigma
   USE isocontours
   USE overlay
   USE tracecol
   USE val_table
   USE modcoupe

  IMPLICIT NONE

  INTEGER(KIND=4) :: jt
  INTEGER(KIND=4) :: incol

  INTEGER(KIND=4) :: inarg
  INTEGER(KIND=4) :: imap_flag

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: cntdata
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: clrdata

  REAL(KIND=4), DIMENSION(:),   ALLOCATABLE :: zlimit, zxover, zyover
  REAL(KIND=4), DIMENSION(:),   ALLOCATABLE :: zxoverm, zyoverm
  INTEGER(KIND=4)                           :: inover, inoverm

  REAL(KIND=4), DIMENSION(4)                :: zcoords
  REAL(KIND=4)                              :: zr1,zr2

  TYPE( bimgfile ) :: blimgclr
  TYPE( bimgfile ) :: blimgcnt
  TYPE( bimgfile ) :: blimgvec(3)
  TYPE( bimgfile ) :: blimgzlevel

  INTEGER(KIND=4)  :: ifrst = 0
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  !
  PRINT *,' '
  PRINT *, 'Coupe version 7.0 (F90)  24/11/2010'
  PRINT *,' (doc web http://www-meom.hmg.inpg.fr/WEB_CHART/)'
  PRINT *,' '
  PRINT *,' In this  compilation   NXX = ', NXX
  PRINT *,'                        NYY = ', NYY
  PRINT *,'                         NZ = ', NA
  PRINT *,'                   NOVERPTS = ', NOVERPTS
  PRINT *,'                   Nmaxtime = ', Nmaxtime
  !!----------------------------------------------------------------------
  ALLOCATE ( nlevel(NA),  ntime(Nmaxtime) )
  ALLOCATE ( zlimit(NBOXMAX+1), zxover(NOVERPTS), zyover(NOVERPTS))
  ALLOCATE ( zxoverm(NOVERPTS), zyoverm(NOVERPTS) )
  ALLOCATE ( cntdata (NXX,NYY), clrdata (NXX,NYY))
  ALLOCATE ( xygr(0:NXX+1,0:NYY+1,2))

  zcoords(1) = 1.
  zcoords(2) = float(NXX)
  zcoords(3) = 1.
  zcoords(4) = float(NYY)

  inarg = iargc()

  IF (inarg == 0) THEN
     CALL PrintOptions ()
     STOP
  ENDIF

  opt_chart = 0
  
  ! default value and plot initialization
  CALL InitValTable()
  CALL SetDefaults()
  CALL SetCoupeDefaults()

  CALL ReadArgs (inarg)

  IF ( opt_lev == 0 ) CALL SetLevels ()
                      CALL OpenNCAR_GKS()
                      CALL OpenAndVerifyFiles (blimgclr, blimgzlevel, blimgcnt, blimgvec)
                      CALL CheckOptions (blimgclr, blimgcnt, blimgvec)
                      CALL SetValTable (blimgclr, blimgcnt, blimgvec(1))
  IF (opt_pal == 1) THEN
                      CALL ReadPaletteFile (cf_pal, incol, opt_reverse)
  ELSE
                      CALL DefaultPalette (incol, opt_reverse, opt_pal)
  ENDIF
                      CALL CoupeInitNCAR(blimgclr, blimgcnt)
  IF (opt_map == 1) THEN
                      CALL gflas1( jp_ib_bathy )
                      CALL CreateBathy ()
                      CALL gflas2
  ENDIF

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

  DO jt = 1, max_tstp   ! loop on time step

     IF (ntime(jt) == 1) THEN

        !  colors
        IF (opt_clrdata == 1) THEN
           PRINT *,' CLRDATA...'
           blimgclr%lclr = .TRUE. 
           blimgclr%lcnt = .FALSE.
           CALL CalculateCutPlane (clrdata, blimgclr, blimgzlevel, jt, ncl_dim, opt_scaleclr, dscaleclr, &
              &                                                                 opt_meanclr, vmean0clr )
        ENDIF

        IF (opt_color == 1) THEN
           IF (opt_clrmark == 1 ) CALL ClrGetMark()
                                  CALL ClrGetLimits (clrdata,blimgclr,zlimit,incol)

           IF (opt_clrlout == 1 ) CALL WriteLimits (cf_clrlout, zlimit, incol+1)
                                  CALL ColorCoupe (clrdata, blimgclr, incol, zlimit )
                                  CALL SetClrCNESday (blimgclr%time)
        ENDIF

        !  Isocontours
        IF (opt_sigma == 0) THEN
           imap_flag=0
        ELSE
           imap_flag=5  ! In this case we will use cpmpxy inspired from SPEM
        ENDIF

        IF (opt_contours == 1) THEN
           PRINT *,' CNTDATA...'
           blimgcnt%lcnt = .TRUE. 
           blimgcnt%lclr = .FALSE.
           CALL CalculateCutPlane (cntdata, blimgcnt, blimgzlevel, jt, nct_dim, opt_scalecnt, dscalecnt, &
              &                                                                 opt_meancnt, vmean0cnt )

           IF(opt_marg == 0) THEN
              CALL set(x1pos, x2pos, y1pos, y2pos, 1., float(NXX), 1., float(NYY), 1)
           ELSE 
              IF(rmap_coord(3) == rmap_coord(4)) THEN 
                 ! zonal section
                 zr2 = 1 + ( rmap_marg(2) - rmap_coord(1) ) / ( rmap_coord(2) - rmap_coord(1) ) * (NXX-1)
                 zr1 = 1 + ( rmap_marg(1) - rmap_coord(1) ) / ( rmap_coord(2) - rmap_coord(1) ) * (NXX-1)
              ELSEIF (rmap_coord(1) == rmap_coord(2)) THEN         
                 ! meridian  section
                 zr2 = 1 + ( rmap_marg(4) - rmap_coord(3) ) / ( rmap_coord(4) - rmap_coord(3) ) * (NXX-1) 
                 zr1 = 1 + ( rmap_marg(3) - rmap_coord(3) ) / ( rmap_coord(4) - rmap_coord(3) ) * (NXX-1) 
              ELSE 
                 PRINT *,' ERROR: -marg option cannot be used with oblique sections '
                 STOP
              ENDIF
              CALL set(x1pos, x2pos, y1pos, y2pos, zr1, zr2, 1., float(NYY), 1)
           ENDIF
           CALL TraceContours (cntdata, blimgcnt, zcoords, imap_flag)
        ENDIF
        CALL UpdateValTable (jt, 1,1,1, blimgclr, blimgcnt, blimgvec(1))

        CALL AddText ()
        IF (opt_contours  ==  1 ) CALL gflas3( jp_ib_contours )

        IF (opt_sigmatr == 1) THEN
           IF(ifrst == 0) THEN
              ifrst=1
              CALL gflas1( jp_ib_sigmalev )
              CALL PlotSigmaLevel(blimgzlevel%nzfile)
              CALL gflas2
           ENDIF
        ENDIF

        IF (opt_map      == 1 ) CALL gflas3( jp_ib_bathy )

        CALL AddGrid ()

        IF (opt_sigmatr  == 1 ) CALL gflas3( jp_ib_sigmalev )
        IF (opt_overdata == 1 ) CALL OverTrace(zxover, zyover, inover)
        IF (opt_overmark == 1 ) CALL OverMarkTrace(zxoverm, zyoverm, inoverm)
        IF (opt_overdata == 1 ) CALL gflas3( jp_ib_overdata )
        IF (opt_overmark == 1 ) CALL gflas3( jp_ib_overmark )
        CALL frame
     ENDIF
  ENDDO

  CALL gclwk(9)
  CALL clsgks
#ifdef linux
  !
  !     LINUX ONLY:
  ! ---------------

  !     nettoyage des fichiers intermediaires

  CALL system ('/bin/rm -f *.tmpclrnk')
  CALL system ('/bin/rm -f *.tmpcntlnk')                         
#endif
  !      call system ('/bin/rm GNFB*')

  PRINT *,'Coupe program ended successfully'
  CALL  flush(6)

  END PROGRAM coupe
