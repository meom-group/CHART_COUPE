MODULE modreadargs
  !!======================================================================
  !!                     ***  MODULE  modreadargs  ***
  !! Parser for the command line for chart and coupe. Set default values
  !!=====================================================================
  !! History : 1.0  !  06/1993 ! E. Brown     : Original code
  !!         : 1->6 !  ????    ! J.M. Molines : new options developped
  !!         : 7.0  !  11/2010 ! J.M. Molines : F90 + Dr norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------

  USE modcom
  USE modcolor
  USE util
  USE readbimg
  USE calcul
  USE val_table

  IMPLICIT NONE

  PRIVATE

  INTEGER(KIND=4), PARAMETER :: jp_from_file = 1 , jp_from_line = 0

  PUBLIC  :: SetDefaults 
  PUBLIC  :: SetCoupeDefaults
  PUBLIC  :: SetLevels
  PUBLIC  :: ReadArgs
  PUBLIC  :: PrintOptions 
 
  PRIVATE :: GetMajorMinor
  PRIVATE :: ParseArgString
  PRIVATE :: GetString
  PRIVATE :: Get2String
  PRIVATE :: Get1real
  PRIVATE :: Get2real
  PRIVATE :: Get4real
  PRIVATE :: Get1integer
  PRIVATE :: Get2integer

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE ReadArgs (knarg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE <routine>  ***
    !!
    !! ** Purpose : Read and interpret the command line and/or the setup file
    !!
    !! ** Method  : Call ParseOptionLine
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: knarg

    INTEGER(KIND=4)     :: istatus        ! status for reading file till the end
    INTEGER(KIND=4)     :: iopt, ilenopt  ! option counter, length of the option string
    INTEGER(KIND=4)     :: idum           ! dummy integer
    INTEGER(KIND=4)     :: ilu = 20       ! logical unit for setup file
    CHARACTER(LEN=256)  :: cline          ! a string corresponding to a line in setup
    CHARACTER(LEN=30)   :: cl_opt         ! string containing the option name
    CHARACTER(LEN=256)  :: cl_arg         ! string containing the option argument 
    CHARACTER(LEN=30)   :: cldum          ! dummy variable
    CHARACTER(LEN=256)  :: clf_setup      ! setup file name if any
    CHARACTER(LEN=256)  :: cl_cmd         ! string used to fork a system call with vecrot
    !!----------------------------------------------------------------------
    iopt=1                  ! option counter
    ldefaultxyplot = .TRUE.  
    ldefaultxypal  = .TRUE.

    DO WHILE (iopt <= knarg)  ! loop till the end of the command line
       CALL getarg(iopt, cldum)

       ! look for a setup file to process if any
       SELECT CASE ( cldum )
        CASE (                                               '-setup' , '-s' )
          iopt=iopt+1
          CALL getarg(iopt, clf_setup) ;  iopt = iopt+1

          IF (FindFile ( clf_setup, jp_file_stp ) == 1 ) THEN 
             CALL PrintMessage (jp_Err, jp_NoFile, clf_setup)
          ELSE 
             OPEN (ilu,file=clf_setup,iostat=istatus,status='old') 

             DO WHILE ( istatus == 0 )
               READ (ilu, '(A)',IOSTAT=istatus ) cline
               ilenopt = ParseOptionLine (cline, cl_opt, cl_arg)

               IF (ilenopt /= 0 .AND. istatus == 0 ) THEN                  
                 idum = FindArg (cl_opt, idum, jp_from_file, cl_arg)
               ENDIF
             ENDDO
             CLOSE (ilu)

          ENDIF
        CASE DEFAULT   ! on line arguments are processed
          iopt = FindArg (cldum, iopt, jp_from_line, cl_arg)
       END SELECT
    ENDDO

    ! perform external command if necessary
    IF (opt_vecrot == 1) THEN
       WRITE(cl_cmd,100) angled, TRIM(cf_vecrotdata)
100    FORMAT('vecrot ',f8.2,1x,a)
       PRINT *,' performing : ',TRIM(cl_cmd)
       CALL system(cl_cmd)
    ENDIF

    IF (opt_vecrot == 2) THEN
       WRITE(cl_cmd,101) angled, TRIM(cf_vecdata1),TRIM(cf_vecdata2)
101    FORMAT('vecrot_opa ',f8.2,1x,a,1x,a)
       PRINT *,' performing: ',TRIM(cl_cmd)
       CALL system(cl_cmd)
    ENDIF

    ! Adjustment of some default values according to specific options
    IF (opt_vertpal == 1) THEN
       IF (ldefaultxypal) THEN
          x1pal = 0.8
          x2pal = 1.0
          y1pal = 0.1
          y2pal = 0.9
       ENDIF
       IF (ldefaultxyplot) THEN
          x1pos = 0.1
          x2pos = 0.78
          y1pos = 0.1
          y2pos = 0.9
          ylon = 0.08 
          ylat = 0.06
          rct_ilx = 0.78
          rct_ily = 0.05

          IF (opt_coupe == 1 ) THEN
             ylon  = 0.08
             ylat = 0.05
             rct_ilx = 0.78
             rct_ily = 0.02
          ENDIF
       ENDIF
    ENDIF

    ! Clear the Information Label text when using cntlib as contour interval
    ! is not known in this case
    IF (opt_cntlim == 1 ) THEN
       cct_ilt =  ' '
    ENDIF

    IF (opt_shade == 1) THEN
       IF (opt_contours /= 1) THEN
          CALL PrintMessage(jp_Err, jp_ErrCntSh,' ')
       ELSE
          cf_clrdata=cf_cntdata
       ENDIF
       IF (opt_pal /= 3) THEN
          CALL PrintMessage(jp_Warn, jp_ErrPal,' ')
          opt_pal = 3
       ENDIF
    ENDIF
  END SUBROUTINE ReadArgs


  INTEGER FUNCTION FindArg (cd_opt, ki, ktype, cd_arg)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION FindArg  ***
    !!
    !! ** Purpose :   
    !!
    !! ** Method  :  cd_opt  : option parsed from command line of file
    !!               ki      : current index on the command line
    !!               ktype   : type of command : jp_from_line or jp_from_file
    !!               cd_arg  : parameters of the option in case of jp_from_file
    !!                       : else empty string
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(in) :: cd_opt
    INTEGER(KIND=4),  INTENT(inout) :: ki
    INTEGER(KIND=4),     INTENT(in) :: ktype
    CHARACTER(LEN=*), INTENT(inout) :: cd_arg

    INTEGER(KIND=4)     :: ilen_opt
    INTEGER(KIND=4)     :: ilen_arg
    REAL(KIND=4)        :: zdum
    CHARACTER(LEN=256)  :: cl_cmd
    CHARACTER(LEN=30)   :: clValue, clFormat, clValFmt
    CHARACTER(LEN=64)   :: cdum, cline
  ! ---------------------------------------------------------------------
    ilen_arg = LEN_TRIM (cd_arg)

  SELECT CASE ( cd_opt ) 
    CASE (                                                       '-debug' )
       opt_debug  = 1  ; lo_debug  = .TRUE.
       opt_debug2 = 0  ; lo_debug2 = .FALSE.

    CASE (                                                       '-debug2' )
       opt_debug  = 1  ; lo_debug  = .TRUE.
       opt_debug2 = 1  ; lo_debug2 = .TRUE.

    CASE (                                                     '-nodebug' )
       opt_debug  = 0  ; lo_debug  = .FALSE.
       opt_debug2 = 0  ; lo_debug2 = .FALSE.

    CASE (                                                           '-p' )
       opt_pal=1
       CALL GetString (ki, ktype, cd_arg, cf_pal)
       IF (FindFile (cf_pal, jp_file_pal ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_pal))
       ENDIF

    CASE (                                                        '-prev' )
       opt_prev = 1

    CASE (                                                   '-greyscale' )
       opt_pal = 2

    CASE (                                                      '-jetpal' )
       opt_pal = 5
       CALL Get1integer (ki, ktype, cd_arg, ncol_jetpal)

    CASE (                                                       '-zebre' )
       opt_pal = 6
       CALL Get1integer (ki, ktype, cd_arg, ncol_jetpal)

    CASE (                                                      '-palout' )
       opt_palout = 1

    CASE (                                                     '-english' )
       opt_english = 1
       cct_ilt =  'MIN:  $CMN$ MAX: $CMX$ C.I.: $CIU$'
       cvv_mnt = 'Minimum vector'
       cvv_mxt = 'Maximum vector'

    CASE (                                                      '-ioipsl' )
       cmodif_vecx = 'deptht=depthu'
       cmodif_vecy = 'deptht=depthv'

    CASE (                                                       '-sesam' )
       cmodif_clr  = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
       cmodif_cnt  = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
       cmodif_vecx = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
       cmodif_vecy = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'

    CASE (                                                        '-orca' )
       opt_orca=1

    CASE (                                                       '-spval' )
       opt_spval=1
       CALL Get1real (ki, ktype, cd_arg, spval_new)

    CASE (                                                       '-scale' )
       opt_scale=1 ; opt_scalecnt=1 ; opt_scaleclr=1
       CALL Get1real (ki, ktype, cd_arg, dscale)
       dscalecnt = dscale
       dscaleclr = dscale

    CASE (                                                    '-cntscale' )
       opt_scalecnt=1
       CALL Get1real (ki, ktype, cd_arg, dscalecnt)
    CASE (                                                    '-clrscale' )
       opt_scaleclr=1
       CALL Get1real (ki, ktype, cd_arg, dscaleclr)

    CASE (                                                        '-mean' )
       opt_mean=1 ; opt_meancnt=1; opt_meanclr=1
       CALL Get1real (ki, ktype, cd_arg, vmean0)

    CASE (                                                     '-cntmean' )
       opt_meancnt=1
       CALL Get1real (ki, ktype, cd_arg, vmean0cnt)

    CASE (                                                     '-clrmean' )
       opt_meanclr=1
       CALL Get1real (ki, ktype, cd_arg, vmean0clr)

    CASE (                                                       '-log10' )
       opt_log=1

    CASE (                                                    '-clrlog10' )
       opt_clrlog=1

    CASE (                                                    '-cntlog10' )
       opt_cntlog=1

    CASE (                                                           '-b' )
       opt_map=1
       CALL GetString (ki, ktype, cd_arg, cf_bathy)

       IF (FindFile (cf_bathy, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_bathy ))
       ENDIF

    CASE (                                                       '-bgrid' )
       opt_batgrid = 1
       CALL GetString (ki, ktype, cd_arg, cf_batgrid)

       IF (FindFile (cf_batgrid, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_batgrid ))
       ENDIF

    CASE (                                                      '-direct' )
       PRINT *, '-direct option is obsolete (warning)'

    CASE (                                             '-lout', '-clrout' )
       opt_clrlout=1
       CALL GetString (ki, ktype, cd_arg, cf_clrlout)

    CASE (                                                           '-c' )
       opt_coords=1
       CALL GetString (ki, ktype, cd_arg, cf_coords)

    CASE (                                                      '-cntsav' )
       opt_cntsav=1
       CALL GetString (ki, ktype, cd_arg, cf_cntsav)

    CASE (                                                           '-o' )
       opt_outfile=1
       CALL GetString (ki, ktype, cd_arg, cf_cgm   )

    CASE (                                                        '-icod' )
       PRINT *, '-icod option is no longer supported' ; stop
       !     -----------------------------------------------

    CASE (                                                    '-showgrid' )
       opt_showgrid = 1
       CALL GetString (ki, ktype, cd_arg, cf_showgrid)

       IF (FindFile (cf_showgrid, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_showgrid ))
       ENDIF

    CASE (                                                        '-grid' )
       opt_batgrid   = 1
       opt_clrgrid   = 1
       opt_contgrid  = 1
       opt_vectgrid  = 1

       CALL GetString (ki, ktype, cd_arg, cf_batgrid)

       IF (FindFile (cf_batgrid, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_batgrid ))
       ENDIF

       cf_clrgrid = cf_batgrid
       cf_cntgrid = cf_batgrid
       cf_vecgrid = cf_batgrid

    CASE (                                                      '-ijgrid' )
       opt_ijgrid = 1
       opt_noproj = 1
       opt_map    = 0

    CASE (                                                      '-pixel' )
       opt_ijgrid = 1
       opt_noproj = 1
       opt_map    = 0
       opt_noint  = 1

    CASE (                                                     '-forcexy' )
       opt_forcexy = 1

    CASE (                                                      '-gridxy' )
       opt_batgrid  = 3
       opt_clrgrid  = 3
       opt_contgrid = 3
       opt_vectgrid = 3
       opt_noint    = 1

       CALL GetString (ki, ktype, cd_arg, cf_batgrid)

       IF (FindFile (cf_batgrid, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_batgrid ))
       ENDIF

       cf_clrgrid = cf_batgrid
       cf_cntgrid = cf_batgrid
       cf_vecgrid = cf_batgrid

    CASE (                                                    '-gridslow' )
       opt_batgrid  = 2
       opt_clrgrid  = 2
       opt_contgrid = 2
       opt_vectgrid = 2

       CALL GetString (ki, ktype, cd_arg, cf_batgrid)

       IF (FindFile (cf_batgrid, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_batgrid ))
       ENDIF

       cf_clrgrid = cf_batgrid
       cf_cntgrid = cf_batgrid
       cf_vecgrid = cf_batgrid

    CASE (                                                        '-mask' )
       opt_clrmask  = 1
       opt_contmask = 1
       opt_vectmask = 1
       CALL GetString (ki, ktype, cd_arg, cf_clrmask)

       IF (FindFile (cf_clrmask, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrmask))
       ENDIF

       cf_cntmask = cf_clrmask
       cf_vecmask = cf_clrmask

    CASE (                                                     '-maskopa' )
       opt_clrmask  = 2
       opt_contmask = 2
       opt_vectmask = 2
       CALL GetString (ki, ktype, cd_arg, cf_clrmask)

       IF (FindFile (cf_clrmask, jp_file_dta  ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrmask))
       ENDIF

       cf_cntmask = cf_clrmask
       cf_vecmask = cf_clrmask
       !
       ! shell
       !
    CASE (                                                      '-system' )
       CALL GetString (ki, ktype, cd_arg, cl_cmd)
       CALL system(cl_cmd)

    CASE (                                                        '-mark' )
       opt_marks = 1
       mark_step = 1

    CASE (                                                       '-mark1' )
       opt_marks = 1
       CALL Get1integer (ki, ktype, cd_arg, mark_step)

    CASE (                                                         '-rev' )
       opt_reverse=1

    CASE (                                                       '-nomap' )
       opt_map = 0

    CASE (                                                         '-map' )
       opt_map = 1

    CASE (                                                          '-hi' )
       opt_high = 1

    CASE (                                                         '-vhi' )
       opt_high = 2

    CASE (                                                          '-lo' )
       opt_high = 0

    CASE (                                                       '-ocean' )
       opt_ocean = 1

    CASE (                                                     '-noperim' )
       opt_perim = 0

    CASE (                                                       '-perim' )
       opt_perim = 1

    CASE (                                                        '-grad' )
       opt_grad = 1
       opt_labx = 1
       opt_laby = 1
       opt_labz = 1

    CASE (                                                      '-nograd' )
       opt_grad = 0
       opt_labx = 0
       opt_laby = 0
       opt_labz = 0
       opt_nolon = 1
       opt_nolat = 1

    CASE (                                                      '-noxlab' )
       opt_labx = 0

    CASE (                                                      '-noylab' )
       opt_laby = 0

    CASE (                                                      '-nozlab' )
       opt_labz = 0

    CASE (                                                      '-noproj' )
       opt_noproj = 1
       opt_map = 0

    CASE (                                                        '-outl' )
       opt_map     = 1
       opt_mapfill = 0

    CASE (                                                   '-nolinelab' )
       opt_nollab=1
       nct_llp = 0

    CASE (                                         '-nobar' , '-clrnopal' )
       opt_palbar=0

    CASE (                                                      '-clrpal' )
       opt_palbar=1

    CASE (                                                     '-vertpal' )
       opt_vertpal=1

    CASE (                                                       '-nobox' )
       opt_nobox=-1

    CASE (                                                       '-lbpal' )
       CALL Get1integer (ki, ktype, cd_arg, nlbpos)

    CASE (                                                        '-lbsc' )
       CALL Get1real (ki, ktype, cd_arg, rlbsc)

    CASE (                                           '-nolat', '-noxaxis' )
       opt_nolat=1

    CASE (                                           '-nolon', '-noyaxis' )
       opt_nolon=1

    CASE (                                                       '-noint' )
       opt_noint = 1

    CASE (                                                          '-km' )
       opt_km = 1

    CASE (                                                    '-clrnoint' )
       opt_noint = 1

    CASE (                                                         '-int' )
       opt_noint = 0 

    CASE (                                                       '-zgrid' )
       opt_grid=1
       opt_zgrid=1

    CASE (                                                       '-xgrid' )
       opt_grid=1
       opt_xgrid=1

    CASE (                                                       '-ygrid' )
       opt_grid=1
       opt_ygrid=1

    CASE (                                           '-print' , '-clriso' )
       opt_print=1

    CASE (                                                '-fn' , '-font' )
       CALL GetString (ki, ktype, cd_arg, c_font)

    CASE (                                                       '-shift' )
       opt_shift = 1
       CALL Get1real (ki, ktype, cd_arg, shift_map)

    CASE (                                                         '-360' )
       opt_360 = 1

    CASE (                                                         '-lev' )
       opt_lev   = 1
       nlevel(1) = 0
       max_lev   = -2 ! flag value to indicate level treatment in ParseArgString

       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL ParseArgString (cdum,   nlevel, max_lev )
       ELSE 
          CALL ParseArgString (cd_arg, nlevel, max_lev )
       ENDIF

    CASE (                                                         '-dep' )
       opt_dep   = 1
       CALL Get1real (ki, ktype, cd_arg, req_dep)

    CASE (                                                      '-clrlev' )
       opt_single   = 1
       CALL Get1integer (ki, ktype, cd_arg, nkclr)

    CASE (                                                      '-cntlev' )
       opt_single   = 1
       CALL Get1integer (ki, ktype, cd_arg, nkcnt)

    CASE (                                                      '-veclev' )
       opt_single   = 1
       CALL Get1integer (ki, ktype, cd_arg, nkvec)

    CASE (                                                        '-ndep' )
       opt_dep   = 2
       CALL Get1real (ki, ktype, cd_arg, req_dep)

    CASE (                                                        '-time' )
       ntime(1) = 0
       max_tstp = -1  ! flag value to indicate time treatment in ParseArgString

       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL ParseArgString (cdum,   ntime, max_tstp)
       ELSE 
          CALL ParseArgString (cd_arg, ntime, max_tstp)
       ENDIF

    CASE (                                                      '-clrexp' )
       CALL Get1integer (ki, ktype, cd_arg, cl_exp)
       CALL SetClrExp (cl_exp)


    CASE (                                             '-met' , '-clrmet' )
       CALL Get1integer (ki, ktype, cd_arg, cl_met)

       IF ((cl_met < 1).OR.(cl_met > 2)) THEN
          CALL PrintMessage (jp_Err, jp_ChoosMet, ' ')
       ENDIF

    CASE (                                                        '-pmin' )
       opt_pmin = 1
       CALL Get1real (ki, ktype, cd_arg, prof_min)
       prof_min = -prof_min

    CASE (                                                        '-pmax' )
       opt_pmax = 1
       CALL Get1real (ki, ktype, cd_arg, prof_max)
       prof_max = -prof_max

    CASE (                                                       '-xstep' )
       opt_xstep = 1
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL GetMajorMinor (cdum, xstep, zdum, xlw)
       ELSE 
          READ (cd_arg,'(A)') cline
          CALL GetMajorMinor (cline, xstep, zdum, xlw)
       ENDIF

       nxsub = INT(zdum)

    CASE (                                                       '-ystep' )
       opt_ystep = 1
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL GetMajorMinor (cdum, ystep, zdum, ylw)
       ELSE 
          READ (cd_arg,'(A)') cline
          CALL GetMajorMinor (cline, ystep, zdum, ylw)
       ENDIF

       nysub = INT(zdum)

    CASE (                                                       '-zstep' )
       opt_zstep = 1
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL GetMajorMinor (cdum, zstep, zdum, zlw)
       ELSE 
          READ (cd_arg,'(A)') cline
          CALL GetMajorMinor (cline, zstep, zdum, zlw)
       ENDIF

       nzsub = INT(zdum)

    CASE (                                                      '-kmstep' )
       opt_kmstep = 1
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL GetMajorMinor (cdum, kmstep, zdum, zlw)
       ELSE 
          READ (cd_arg,'(A)') cline
          CALL GetMajorMinor (cline, kmstep, zdum, zlw)
       ENDIF

       nkmsub = INT(zdum)

    CASE (                                                        '-conv' )
       PRINT *,'l''option -conv n''est plus disponible'

    CASE (                                                        '-marg' )
       opt_marg = 1
       CALL Get4real (ki, ktype, cd_arg, rmap_marg(1),rmap_marg(2), rmap_marg(3),rmap_marg(4))

    CASE (                                                        '-zoom' )
       opt_zoom = 1
       CALL Get4real (ki, ktype, cd_arg, &
          &  rmap_coord(1),rmap_coord(2), rmap_coord(3),rmap_coord(4))
       IF ((rmap_coord(2) < rmap_coord(1)).OR.&
          &  (rmap_coord(4) < rmap_coord(3))) THEN
          PRINT *,'ERROR : coordinates mus be given in ascending order '
          STOP
       ENDIF

    CASE (                                                         '-pts' )
       opt_zoom = 1
       CALL Get4real (ki, ktype, cd_arg, &
          & rmap_coord(1),rmap_coord(2), rmap_coord(3),rmap_coord(4))
       CALL CalculateCutHeading(rmap_coord)
       CALL CalculateCutDistance(rmap_coord, NXX)

    CASE (                                                       '-cslab' )
       CALL Get1real (ki, ktype, cd_arg, scal_cslab)

    CASE (                                                       '-sigma' )
       opt_sigma = 1
       opt_print = 1
       CALL GetString (ki, ktype, cd_arg, cf_zlevel)

       IF (FindFile (cf_zlevel, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_zlevel))
       ENDIF

    CASE (                                                     '-sigmatr' )
       opt_sigmatr = 1

    CASE (                                                      '-format' )
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,clValue)
          ki=ki+1 ;  CALL getarg(ki,clFormat)

          CALL SetFormatString (clValue, clFormat)
       ELSE 
          READ (cd_arg(1:30),'(A)') clValFmt
          CALL SplitValFmt (clValFmt,clValue, clFormat)
          CALL SetFormatString (clValue, clFormat)
       ENDIF

       !     carte -----------------------------------------------------

    CASE (                                                        '-rlat' )
       opt_rlat = 1
       CALL Get1real (ki, ktype, cd_arg, rlat_map)

    CASE (                                                        '-rlon' )
       opt_rlon = 1
       CALL Get1real (ki, ktype, cd_arg, rlon_map)

    CASE (                                                        '-proj' )
       CALL GetString (ki, ktype, cd_arg, cmap_proj)

       IF (cmap_proj == 'GL' .OR. cmap_proj  ==  'OR' ) THEN
          cmap_proj = 'OR'
          cmap_zone = 'MA'
       ENDIF

       !     couleur ---------------------------------------------------

    CASE (                                                  '-clrdatamod' )
       opt_color  = 1
       opt_clrmod = 1

    CASE (                                                       '-color' )
       opt_color=1

    CASE (                                       '-clrnocol' , '-nocolor' )
       opt_color=0

    CASE (                                               '-l' , '-clrlim' )
       cl_met = 3
       CALL GetString (ki, ktype, cd_arg, cf_clrlim)

       IF (FindFile (cf_clrlim, jp_file_lim) == 1 ) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrlim))
       ENDIF

    CASE (                                                     '-clrmark' )
       cl_met = 1
       opt_clrmark = 1
       CALL GetString (ki, ktype, cd_arg, cf_clrmark)

       IF (FindFile (cf_clrmark, jp_file_lim) == 1 ) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrmark))
       ENDIF

    CASE (                                                     '-clrmask' )
       opt_clrmask = 1
       CALL GetString (ki, ktype, cd_arg, cf_clrmask)

       IF (FindFile (cf_clrmask, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrmask))
       ENDIF

    CASE (                                                     '-clrdata' )
       opt_clrdata = 1
       opt_color   = 1
       IF (opt_shade == 1) THEN
          CALL PrintMessage (jp_Warn, jp_NoClrDta,' ')
       ELSE
          CALL GetString (ki, ktype, cd_arg, cf_clrdata)
          IF (cf_clrdata /= 'vecrot.bimg' .AND. cf_clrdata  /=  'vecrot.nc' ) THEN           
             IF (FindFile (cf_clrdata, jp_file_dta ) == 1) THEN
                CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrdata))
             ENDIF
          ENDIF
       ENDIF

    CASE (                                                     '-clrgrid' )
       opt_clrgrid = 1
       CALL GetString (ki, ktype, cd_arg, cf_clrgrid)

       IF (FindFile(cf_clrgrid, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrgrid))
       ENDIF

    CASE (                                            '-vmin' , '-clrmin' )
       opt_min = 1
       CALL Get1real (ki, ktype, cd_arg, cl_min)

    CASE (                                             '-vmax', '-clrmax' )
       opt_max = 1
       CALL Get1real (ki, ktype, cd_arg, cl_max)

    CASE (                                                      '-clrmsk' )
       opt_msk = 1

    CASE (                                                      '-clrdim' )
       CALL Get1integer (ki, ktype, cd_arg, ncl_dim)

    CASE (                                                      '-clrvar' )
       CALL GetString(ki, ktype, cd_arg, cv_clr)

    CASE (                                                    '-clrmodif' )
       CALL GetString(ki, ktype, cd_arg, cmodif_clr)


       !     contours --------------------------------------------------

    CASE (                                                     '-contour' )
       opt_contours = 1

    CASE (                                                      '-cntnum' )
       opt_contours = 1
       CALL Get1integer (ki, ktype, cd_arg, nct_cls)
       nct_cls = -nct_cls

    CASE (                                                       '-cntlw' )
       IF (ktype == jp_from_line) THEN
          ki=ki+1 ;  CALL getarg(ki,cdum)
          CALL GetMajorMinor (cdum, rct_cll1, rct_cll2, zdum)
       ELSE 
          READ (cd_arg,'(A)') cline
          CALL GetMajorMinor (cline, rct_cll1, rct_cll2, zdum)
       ENDIF

    CASE (                                                      '-cntlib' )
       CALL Get1integer (ki, ktype, cd_arg, nct_ilb)

    CASE (                                                      '-cntilc' )
       CALL Get1integer (ki, ktype, cd_arg, nct_ilc)

    CASE (                                                      '-cntill' )
       CALL Get1real (ki, ktype, cd_arg, rct_ill)

    CASE (                                                      '-cntrc1' )
       CALL Get1real (ki, ktype, cd_arg, rct_rc1)

    CASE (                                                      '-cntrc2' )
       CALL Get1real (ki, ktype, cd_arg, rct_rc2)

    CASE (                                                      '-cntcwm' )
       CALL Get1real (ki, ktype, cd_arg, rct_cwm)

    CASE (                                                      '-cntlbc' )
       CALL Get1integer (ki, ktype, cd_arg, nct_lbc)

    CASE (                                                      '-cntllb' )
       CALL Get1integer (ki, ktype, cd_arg, nct_llb)

    CASE (                                                      '-cntlis' )
       CALL Get1integer (ki, ktype, cd_arg, nct_lis)

    CASE (                                                     '-cntlabc' )
       CALL Get1integer (ki, ktype, cd_arg, nct_llc)

    CASE (                                                      '-cntllp' )
       CALL Get1integer (ki, ktype, cd_arg, nct_llp)

    CASE (                                                      '-cntexp' )
       CALL Get1real (ki, ktype, cd_arg, rct_sfs)
       CALL SetCntExp (INT(rct_sfs))

    CASE (                                                      '-cntilp' )
       CALL Get1integer (ki, ktype, cd_arg, nct_ilp)

    CASE (                                             '-cont', '-cntint' )
       opt_contours = 1
       opt_cntint   = 1
       CALL Get1real (ki, ktype, cd_arg, rct_cis)

    CASE (                                                      '-cntlim' )
       opt_contours = 1
       opt_cntlim=1
       CALL GetString (ki, ktype, cd_arg, cf_cntlim)

       IF (FindFile (cf_cntlim, jp_file_lim) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_cntlim))
       ENDIF

    CASE (                                                     '-cntlout' )
       opt_cntlout=1
       CALL GetString (ki, ktype, cd_arg, cf_cntlout)

    CASE (                                                     '-cntdata' )
       opt_contours = 1
       opt_contdata = 1
       CALL GetString (ki, ktype, cd_arg, cf_cntdata)
       IF (cf_cntdata /= 'vecrot.bimg') THEN            
          IF (FindFile (cf_cntdata, jp_file_dta ) == 1) THEN
             CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_cntdata))
          ENDIF
       ENDIF

    CASE (                                                     '-cntmask' )
       opt_contmask = 1
       CALL GetString (ki, ktype, cd_arg, cf_cntmask)

       IF (FindFile (cf_cntmask, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_cntmask))
       ENDIF

    CASE (                                                     '-cntdash' )
       opt_dash = 1

    CASE (                                                    '-cntshade' )
       opt_shade = 1
       opt_pal = 3
       ! supression affichage de la palette
       opt_palbar = 0
       ! force les clrdata (sans clrdata option), option clriso activee
       opt_clrdata = 1
       opt_color = 1
       opt_print = 1
       ! force un mini et maxi symetrique pour clrdata, clrmet 1 pour
       ! que l'on ait cl_min  0 cl_max dans les limites de la palette bicolore
       opt_min = 1
       opt_max = 1
       cl_met = 1
       cl_min = -1.e19
       cl_max = 1.e19

    CASE (                                                     '-cntgrid' )
       opt_contgrid = 1
       CALL GetString (ki, ktype, cd_arg, cf_cntgrid)

       IF (FindFile (cf_cntgrid, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_cntgrid))
       ENDIF

    CASE (                                                      '-cntmin' )
       opt_minc = 1
       CALL Get1real (ki, ktype, cd_arg, rct_cmn)

    CASE (                                                      '-cntmax' )
       opt_maxc = 1
       CALL Get1real (ki, ktype, cd_arg, rct_cmx)

    CASE (                                         '-xyinfo' , '-cntilxy' )
       CALL Get2real (ki, ktype, cd_arg, rct_ilx, rct_ily)

    CASE (                                                      '-cntils' )
       CALL Get1real (ki, ktype, cd_arg, rct_ils)

    CASE (                                                      '-cntlls' )
       CALL Get1real (ki, ktype, cd_arg, rct_lls)

    CASE (                                                      '-cntilt' )
       CALL GetString (ki, ktype, cd_arg, cct_ilt)

    CASE (                                                      '-cntdim' )
       CALL Get1integer (ki, ktype, cd_arg, nct_dim)         

    CASE (                                                      '-cntvar' )
       CALL GetString(ki, ktype, cd_arg, cv_cnt)

    CASE (                                                    '-cntmodif' )
       CALL GetString(ki, ktype, cd_arg, cmodif_cnt)


       !     vecteurs ---------------------------------------------------

    CASE (                                                      '-vecclr' )
       opt_vectclr = 1

    CASE (                                                     '-vectorx' )
       opt_vectX=1

    CASE (                                                     '-vectory' )
       opt_vectY=1

    CASE (                                                    '-vectorxy' )
       opt_vect2D=1

    CASE (                                                    '-vector3d' )
       opt_vect3D=1

    CASE (                                                   '-vecclrmod' )
       opt_vectmod = 1

    CASE (                                                  '-vecnotrace' )
       opt_vecnotr = 1

    CASE (                                                       '-Cgrid' )
       opt_cgrid =1

    CASE (                                                    '-vecshade' )
       opt_pal = 4
       opt_palbar = 0
       opt_vecshade =1
       opt_color = 1
       opt_ocean = 1
       cl_met = 1

    CASE (                                                      '-veclim' )
       opt_vectlim=1
       nvv_met = 3
       !        call GetString (ki, ktype, cd_arg, cf_veclim)
       CALL GetString (ki, ktype, cd_arg, cf_clrlim)

       IF (FindFile (cf_clrlim, jp_file_lim) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_clrlim))
       ENDIF


    CASE (                                    '-vecdata3d' , '-vecdata3D' )
       IF (opt_vectors == 1) THEN
          PRINT *, 'erreur : un fichier 3D doit etre specifie seul'
          STOP
       ENDIF

       opt_vectors = 1
       opt_vect3D  = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata1)

       IF (FindFile (cf_vecdata1, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata1))
       ENDIF

    CASE (                     '-vecdata2d' , '-vecdataxy' , '-vecdata2D' )
       IF (opt_vectors == 1) THEN
          PRINT *, 'erreur : un fichier 2D doit etre specifie seul'
          STOP
       ENDIF

       opt_vectors = 1
       opt_vect2D  = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata1)

       IF (FindFile (cf_vecdata1, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata1))
       ENDIF

    CASE (                                                    '-vecrotxy' )
       opt_vecrot = 1

       CALL GetString (ki, ktype, cd_arg, cf_vecrotdata)

       IF (FindFile (cf_vecrotdata, jp_file_dta ) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecrotdata))
       ENDIF

    CASE (                                                    '-vecrotuv' )
       ! cas ou l'on passe les fichiers U et V separement sur grille C opa !!!
       opt_vecrot = 2

       CALL Get2String (ki, ktype, cd_arg, cf_vecdata1, cf_vecdata2)

       IF (FindFile (cf_vecdata1,  jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata1))
       ENDIF

       IF (FindFile (cf_vecdata2, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata2))
       ENDIF


    CASE (                                                  '-vecdatapsi' )
       IF ((opt_vect2D == 1).OR.(opt_vect3D == 1)) THEN
          CALL PrintMessage (jp_Err, jp_NoMoreCo, ' ')
       ENDIF

       opt_vectors = 1
       opt_vectX   = 1
       opt_vecpsi  = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata1)

       IF (FindFile (cf_vecdata1, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata1))
       ENDIF

    CASE (                                      '-vecdataX' , '-vecdatax' )
       IF ((opt_vect2D == 1).OR.(opt_vect3D == 1)) THEN
          CALL PrintMessage (jp_Err, jp_NoMoreCo, ' ')
       ENDIF

       opt_vectors = 1
       opt_vectX   = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata1)

       IF (FindFile (cf_vecdata1, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata1))
       ENDIF

    CASE (                                      '-vecdataY' , '-vecdatay' )
       IF ((opt_vect2D == 1).OR.(opt_vect3D == 1)) THEN
          CALL PrintMessage (jp_Err, jp_NoMoreCo, ' ')
       ENDIF

       opt_vectors = 1
       opt_vectY   = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata2)

       IF (FindFile (cf_vecdata2, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata2))
       ENDIF

    CASE (                                      '-vecdataZ' , '-vecdataz' )
       IF ((opt_vect2D == 1).OR.(opt_vect3D == 1)) THEN
          CALL PrintMessage (jp_Err, jp_NoMoreCo, ' ')
       ENDIF

       opt_vectors = 1
       opt_vectZ   = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecdata3)

       IF (FindFile (cf_vecdata3, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecdata3))
       ENDIF

    CASE (                                                     '-vecmask' )
       opt_vectmask = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecmask)

       IF (FindFile (cf_vecmask, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecmask))
       ENDIF

    CASE (                                                     '-vecgrid' )
       opt_vectgrid = 1
       CALL GetString (ki, ktype, cd_arg, cf_vecgrid)

       IF (FindFile (cf_vecgrid, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_vecgrid))
       ENDIF


    CASE (                                                     '-veclout' )
       opt_veclout=1
       CALL GetString (ki, ktype, cd_arg, cf_veclout)

    CASE (                                                      '-vecmet' )
       CALL Get1integer (ki, ktype, cd_arg, nvv_met)

       IF ((nvv_met < 1).OR.(nvv_met > 2)) THEN
          CALL PrintMessage (jp_Err, jp_ChoosMet, ' ')
       ENDIF

    CASE (                                                      '-vecsub' )
       CALL Get2integer (ki, ktype, cd_arg, nvv_subx, nvv_suby)

    CASE (                                                      '-vecvpo' )
       CALL Get1integer (ki, ktype, cd_arg, nvv_vpo)

    CASE (                                                      '-vecvrl' )
       CALL Get1real (ki, ktype, cd_arg, rvv_vrl)

    CASE (                                                      '-vecvfr' )
       CALL Get1real (ki, ktype, cd_arg, rvv_vfr)

    CASE (                                                      '-vecvmd' )
       CALL Get1real (ki, ktype, cd_arg, rvv_vmd)

    CASE (                                                      '-veclwd' )
       CALL Get1real (ki, ktype, cd_arg, rvv_lwd)

    CASE (                                                      '-vecmns' )
       CALL Get1real (ki, ktype, cd_arg, rvv_mns)

    CASE (                                                     '-vecmnxy' )
       CALL Get2real (ki, ktype, cd_arg, rvv_mnx, rvv_mny)

    CASE (                                                      '-vecmnp' )
       CALL Get1integer (ki, ktype, cd_arg, nvv_mnp)

    CASE (                                                      '-vecmxs' )
       CALL Get1real (ki, ktype, cd_arg, rvv_mxs)

    CASE (                                                     '-vecmxxy' )
       CALL Get2real (ki, ktype, cd_arg, rvv_mxx, rvv_mxy)

    CASE (                                                      '-vecmxp' )
       CALL Get1integer (ki, ktype, cd_arg, nvv_mxp)

    CASE (                                                      '-vecvlc' )
       opt_vecmin = 1
       CALL Get1real (ki, ktype, cd_arg, rvv_vlc)

    CASE (                                                      '-vecvhc' )
       opt_vecmax = 1
       CALL Get1real (ki, ktype, cd_arg, rvv_vhc)

    CASE (                                                      '-vecmnt' )
       CALL GetString (ki, ktype, cd_arg, cvv_mnt)

    CASE (                                                      '-vecmxt' )
       CALL GetString (ki, ktype, cd_arg, cvv_mxt)

    CASE (                                                     '-vecvarx' )
       CALL GetString(ki, ktype, cd_arg, cv_vecx)

    CASE (                                                     '-vecvary' )
       CALL GetString(ki, ktype, cd_arg, cv_vecy)

    CASE (                                                   '-vecmodifx' )
       CALL GetString(ki, ktype, cd_arg, cmodif_vecx)

    CASE (                                                   '-vecmodify' )
       CALL GetString(ki, ktype, cd_arg, cmodif_vecy)

       !   overlay 
    CASE (                                                    '-overdata' )
       opt_overdata = 1
       CALL GetString (ki, ktype, cd_arg, cf_overdata)

       IF (FindFile (cf_overdata, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_overdata))
       ENDIF

    CASE (                                                      '-overlw' )
       CALL Get1real (ki, ktype, cd_arg, rover_lw)

    CASE (                                                     '-overclr' )
       opt_color = 1
       opt_overclr = 1
       CALL Get1integer (ki, ktype, cd_arg, nover_ic)
       nover_ic = nover_ic + 19

    CASE (                                                    '-overmark' )
       opt_overmark = 1
       CALL GetString (ki, ktype, cd_arg, cf_overmark)

       IF (FindFile (cf_overmark, jp_file_dta) == 1) THEN
          CALL PrintMessage (jp_Err, jp_NoFile, TRIM(cf_overmark))
       ENDIF

    CASE (                                                      '-overmk' )
       CALL Get1integer (ki, ktype, cd_arg, nover_mk)

    CASE (                                                    '-overmksc' )
       CALL Get1real (ki, ktype, cd_arg, rover_mksc)

    CASE (                                                   '-overmkclr' )
       opt_color = 1
       opt_overclr = 1
       CALL Get1integer (ki, ktype, cd_arg, nover_mkic)
       nover_mkic = nover_mkic + 19

    CASE (                                                     '-overout' )
       opt_overout = 1
       CALL GetString (ki, ktype, cd_arg, cf_overout)



    CASE (                                         '-xybar' , '-clrxypal' )
       CALL Get4real (ki, ktype, cd_arg, x1pal, x2pal, y1pal, y2pal)
       ldefaultxypal=.FALSE.

    CASE (                                                      '-xyplot' )
       CALL Get4real (ki, ktype, cd_arg, x1pos, x2pos, y1pos, y2pos)
       ldefaultxyplot=.FALSE.

    CASE (                                                       '-xybat' )
       opt_xybat = 1
       CALL Get4real (ki, ktype, cd_arg, x1bat, x2bat, y1bat, y2bat)

    CASE (                                            '-ylon' , '-xaxisy' )
       CALL Get1real (ki, ktype, cd_arg, ylon)

    CASE (                                            '-ylat' , '-yaxisy' )
       CALL Get1real (ki, ktype, cd_arg, ylat)

    CASE (                                                         '-ykm' )
       opt_km = 1
       CALL Get1real (ki, ktype, cd_arg, ykm)

    CASE (                                                      '-xaxist' )
       CALL GetString (ki, ktype, cd_arg, cxaxist)

    CASE (                                                      '-yaxist' )
       CALL GetString (ki, ktype, cd_arg, cyaxist)

    CASE (                                                         '-dat' )
       opt_dat = 1

    CASE (                                                       '-nodat' )
       opt_dat = 0

    CASE (                                                        '-team' )
       opt_team = 1

    CASE (                                                      '-noteam' )
       opt_team = 0

    CASE (                                                       '-title' )
       nstrcount              = nstrcount + 1
       text(nstrcount)%xpos   = 0.5
       text(nstrcount)%ypos   = -1.
       text(nstrcount)%rcsize = 1.3
       text(nstrcount)%align  = 0.
       text(nstrcount)%angle  = 0.

       IF (ktype == jp_from_line) THEN
          ki=ki+1 ; CALL getarg(ki,text(nstrcount)%cstr)
       ELSE
          CALL GetStringtParameters (cd_arg, text(nstrcount))
       ENDIF

    CASE (                                                      '-string' )
       IF (ktype == jp_from_line) THEN
          nstrcount = nstrcount + 1
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) text(nstrcount)%xpos
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) text(nstrcount)%ypos
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) text(nstrcount)%rcsize
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) text(nstrcount)%align
          ki=ki+1 ;  CALL getarg(ki,text(nstrcount)%cstr)
          text(nstrcount)%angle=0.
       ELSE 
          nstrcount = nstrcount + 1
          CALL GetStringParameters (cd_arg, text(nstrcount))
          text(nstrcount)%angle=0.
       ENDIF

    CASE (                                                     '-stringr' )
       IF (ktype == jp_from_line) THEN
          nstrcountr = nstrcountr + 1
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) textr(nstrcountr)%xpos
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) textr(nstrcountr)%ypos
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) textr(nstrcountr)%rcsize
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) textr(nstrcountr)%align
          ki=ki+1 ;  CALL getarg(ki,cdum)
          READ(cdum,*) textr(nstrcountr)%angle
          ki=ki+1 ;  CALL getarg(ki,textr(nstrcountr)%cstr)
       ELSE
          nstrcountr = nstrcountr + 1
          CALL GetStringrParameters (cd_arg, textr(nstrcountr))
       ENDIF

    CASE DEFAULT
       IF ( cd_opt(1:1) == '#' ) THEN
         ki = ki  !skip
       ELSE
         ilen_opt = LEN_TRIM(cd_opt)
         IF (ilen_opt /= 0 ) THEN
            PRINT *,'ERROR : unknown parameter or option : ',TRIM(cd_opt)
            CALL flush(6)
            STOP
         ENDIF
       ENDIF
    END SELECT

    ki      = ki + 1
    FindArg = ki

  END FUNCTION FindArg


  SUBROUTINE SetDefaults ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetDefaults  ***
    !!
    !! ** Purpose :  Initialisation of the option
    !!
    !!----------------------------------------------------------------------

    !  level initialization
    nlevel (1)   = 1
    max_lev      = 1
    nlevel(2:NA) = 0  ! only level 1 is to be plotted, bu default

    ! time frame initialization
    ntime (1)         = 1
    max_tstp          = 1
    ntime(2:Nmaxtime) = 0 ! only time frame 1 is to be plottes by default

    ! default options 
    opt_english  = 0
    opt_dat      = 0
    opt_team     = 1   
    opt_debug    = 0
    opt_single   = 0

    opt_pal      = 0
    opt_prev     = 0
    opt_vertpal  = 0
    opt_nobox    = 1
    opt_palout   = 0
    opt_lev      = 0
    opt_dep      = 0
    opt_clrlout  = 0
    opt_palbar   = 1
    opt_zoom     = 0
    opt_noint    = 0
    opt_km       = 0
    opt_coords   = 0
    opt_cntsav   = 0
    opt_min      = 0
    opt_max      = 0
    opt_msk      = 0
    opt_minc     = 0
    opt_maxc     = 0
    opt_clrmask  = 0
    opt_clrmark  = 0
    opt_contmask = 0
    opt_vectmask  = 0
    opt_contours = 0
    opt_cntlim  = 0
    opt_contdata = 0
    opt_nollab   = 0
    opt_color    = 0
    opt_grid     = 0
    opt_ijgrid   = 0
    opt_forcexy  = 0
    opt_xgrid    = 0
    opt_ygrid    = 0
    opt_zgrid    = 0
    opt_satur    = 0
    opt_reverse  = 0
    opt_marg     = 0
    opt_orca     = 0
    opt_spval    = 0
    opt_scale    = 0
    opt_scalecnt = 0
    opt_scaleclr = 0
    opt_mean     = 0
    opt_meancnt  = 0
    opt_meanclr  = 0
    opt_log      = 0
    opt_clrlog   = 0
    opt_cntlog   = 0
    opt_map      = 1
    opt_high     = 0
    opt_ocean    = 0
    opt_mapfill  = 1
    opt_rlat     = 0
    opt_rlon     = 0
    opt_perim    = 1
    opt_print    = 0
    opt_grad     = 1
    opt_labx     = 1
    opt_laby     = 1
    opt_labz     = 1
    opt_clrmod   = 0
    opt_shift    = 0
    opt_360      = 0
    opt_noproj   = 0

    opt_xstep    = 0
    opt_ystep    = 0
    opt_zstep    = 0

    opt_vectdata = 0
    opt_vect3D   = 0
    opt_vect2D   = 0
    opt_vectX    = 0
    opt_vectY    = 0
    opt_vectZ    = 0
    opt_vecmin   = 0
    opt_vecmax   = 0
    opt_veclout  = 0
    opt_vecpsi   = 0
    opt_vecnotr  = 0 
    opt_vecrot   = 0
    opt_cgrid    = 0
    opt_vecshade = 0

    opt_overdata = 0
    opt_overmark = 0
    opt_overout  = 0
    opt_overclr  = 0

    opt_cntlout  = 0

    opt_marks    = 0
    opt_dash     = 0
    opt_shade    = 0

    cl_met       = 2
    nvv_met       = 2
    nvv_vpo       = 0
    cl_min       = 0
    cl_max       = 0
    ncl_dim       = 1
    nct_dim       = 1
    rct_rc1       = 0.10
    rct_rc2       = 0.15

    rover_lw      = 1.0
    nover_mk      = 4
    rover_mksc    = 1.0
    nover_ic      = 1
    nover_mkic    = 1

    ! plot layout
    x1pos   = 0.10  ! map area
    x2pos   = 0.95
    y1pos   = 0.20
    y2pos   = 0.90

    x1pal   = 0.10  ! color bar area
    x2pal   = 0.95
    y1pal   = 0.00
    y2pal   = 0.12

    ! contours defaults
    CALL cpgeti ('LLB - Line Label Box Flag          ', nct_llb)
    CALL cpgetr ('LLS - Line Label Size              ', rct_lls)
    CALL cpgeti ('LIS - Label Interval Specifier     ', nct_lis)
    CALL cpgeti ('LBC - Label Box Color index        ', nct_lbc)

    CALL cpgeti ('ILB - Information Label Box flag   ', nct_ilb)
    CALL cpgetr ('ILL - Information Label Line width ', rct_ill)
    CALL cpgetr ('ILS - Information Label Size       ', rct_ils)
    CALL cpgeti ('ILP - Information Label Positioning', nct_ilp)

    CALL cpgetr ('CWM - Character Width Multiplier   ', rct_cwm)
    CALL cpgeti ('CLS - Contour Level Selection flag ', nct_cls)

    rct_sfs  = 0.0
    rct_cis = -1.     
    nct_llc  = COLOR_ISOCONTOUR
    nct_llp  = 2
    nct_ilc  = 1

    rct_cll1  = 1.0    ! contour line line width
    rct_cll2  = 1.0    ! label line line width

    rct_ilx   = 0.95   ! right limit of the contour information label
    rct_ily   = 0.16   ! upper position 

    ! default is french, modified with -english 
    cct_ilt =  'Contours de $CMN$ a $CMX$ par intervalles de $CIU$'

    ! Vectors
    rvv_vmd  = 0.
    nvv_subx = 1
    nvv_suby = 1
    rvv_vrl  = 1.0
    rvv_vfr  = 0.2
    rvv_mny  = 0.2
    rvv_mxy  = 0.2

    CALL vvgetr ('LWD - vector Line WiDth              ', rvv_lwd)
    CALL vvgetr ('MNS - MiN vector text block char Size', rvv_mns)
    CALL vvgetr ('MNX - MiN vector text block X coord  ', rvv_mnx)
    CALL vvgeti ('MNP - MiN vector text block Pos. mode', nvv_mnp)

    CALL vvgetr ('MXS - MaX vector text block char Size', rvv_mxs)
    CALL vvgetr ('MXX - MaX vector text block X coord  ', rvv_mxx)
    CALL vvgeti ('MXP - MaX vector text block Pos. mode', nvv_mxp)

    ! this is the french default, modified with -english option
    cvv_mnt = 'Vecteur minimum'
    cvv_mxt = 'Vecteur maximum'

    ! Configuration
    scal_cslab = 0.8
    nlbpos     = 1
    rlbsc      = 1.

    ! Map
    cmap_proj = 'CE'      
    cmap_zone = 'PO'
    shift_map= 0
    c_font   = 'PWRITX DATABASE   '

    xstep = 5.
    ystep = 5.

    nxsub = 0
    nysub = 0

    xlw = 1.0
    ylw = 1.0

    rmap_coord(1) = 0.0
    rmap_coord(2) = 360.0
    rmap_coord(3) = -90.0
    rmap_coord(4) = 90.0

    rmap_marg(1) = 0.0
    rmap_marg(2) = 360.0
    rmap_marg(3) = -90.0
    rmap_marg(4) = 90.0

    ! output file-name
    cf_cgm = 'gmeta'

    ! single frame 
    nkclr = 1
    nkcnt = 1
    nkvec = 1

    ! NetCdf
    cv_clr  = 'none'
    cv_cnt  = 'none'
    cv_vecx = 'none'
    cv_vecy = 'none'
    !    
    cmodif_clr  = 'none'
    cmodif_cnt  = 'none'
    cmodif_vecx = 'none'
    cmodif_vecy = 'none'

  END SUBROUTINE SetDefaults


  SUBROUTINE SetCoupeDefaults ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetCoupeDefaults  ***
    !!
    !! ** Purpose :   Set default options for coupe
    !!
    !!----------------------------------------------------------------------
    opt_map      = 0
    opt_noproj   = 1
    opt_nolat    = 0
    opt_nolon    = 0
    opt_km       = 0
    opt_xybat    = 0
    opt_batgrid  = 0
    opt_showgrid = 0
    opt_pmin     = 0
    opt_pmax     = 0
    opt_sigma    = 0
    opt_sigmatr  = 0

    prof_min     = 0.0
    prof_max     = 4000.0

    y1pos        = 0.30

    x1bat = x1pos
    x2bat = x2pos
    y1bat = y1pos
    y2bat = y2pos

    ylon    = 0.25
    ylat    = 0.20
    ykm     = 0.16

    rct_ilx   = 0.95   ! right limit of the contour information label
    rct_ily   = 0.02   ! upper position 

    zstep  = 200.
    kmstep = 200.     ! km

    nzsub  = 0
    nkmsub = 0
    zlw    = 1.0

    cxaxist = 'lon'
    cyaxist = 'lat'
    ckmaxist= 'Km '

  END SUBROUTINE SetCoupeDefaults


  SUBROUTINE SetLevels ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetLevels ***
    !!
    !! ** Purpose :  Activates all levels. Used by coupe only
    !!
    !!----------------------------------------------------------------------
    max_lev   = 99999
    nlevel(:) = 1  ! all levels activated
  END SUBROUTINE SetLevels


  SUBROUTINE PrintOptions()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE PrintOptions  ***
    !!
    !! ** Purpose :   dummy routine 
    !!
    !!----------------------------------------------------------------------
    ! Dummy routine
  END SUBROUTINE PrintOptions


  SUBROUTINE GetMajorMinor (cdarg, pmajor, pminor, plw)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetMajorMinor  ***
    !!
    !! ** Purpose :  Analyse a given string where fields are separated by :
    !!
    !! ** Method  : For instance:
    !!                  -xstep 10       -> a division every 10 degres
    !!                  -xstep 10:2     -> a division every 5 degress
    !!                                     labelled every second 
    !!                  -xstep 10:2:1.5 -> same as above, with labeled line
    !!                                     thicker by a factor of 1.5
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdarg          ! argument
    REAL(KIND=4),    INTENT(out) :: pmajor, pminor ! major tick, minor tick
    REAL(KIND=4),    INTENT(out) :: plw            ! line width

    INTEGER(KIND=4)              :: ilen, ji, isep1, isep2
    ! ---------------------------------------------------------------------
    ilen  = LEN_TRIM (cdarg)
    pminor= 0.   ;   plw   = 1.
    isep1 =  INDEX( cdarg,':', .false. )
    isep2 =  INDEX( cdarg,':', .true.  )

    IF ( isep1 == 0 ) THEN
       READ(cdarg                 ,*) pmajor
    ELSE IF (isep1 == isep2 )  THEN
       READ(cdarg(1      :isep1-1),*) pmajor
       READ(cdarg(isep1+1:ilen   ),*) pminor
    ELSE 
       READ(cdarg(1      :isep1-1),*) pmajor
       READ(cdarg(isep1+1:isep2-1),*) pminor
       READ(cdarg(isep2+1:ilen   ),*) plw
    ENDIF

  END SUBROUTINE GetMajorMinor


  INTEGER(KIND=4) FUNCTION ParseOptionLine (cd_in, cd_opt, cd_arg)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION ParseOptionLine  ***
    !!
    !! ** Purpose :   This function parse an input line containing an
    !!            option followed eventally by arguments. It return
    !!            separatly the option name (cd_opt) and the option
    !!            arguments, cd_arg. Function return value is the
    !!            length of cd_in
    !!
    !! ** Method  : Example : if cd_in = -zoom -80 -20 20 40
    !!                           cd_opt = "-zoom"
    !!                           cd_arg = " -80 -20 20 40 "
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(inout) :: cd_in
    CHARACTER(LEN=*), INTENT(out):: cd_opt
    CHARACTER(LEN=*), INTENT(out):: cd_arg

    INTEGER(KIND=4) :: ilen_line
    INTEGER(KIND=4) :: ilen_opt
    !!----------------------------------------------------------------------
    cd_in = ADJUSTL( TRIM (cd_in) )

    cd_opt = ' '
    cd_arg = ' '

    ilen_line = LEN_TRIM(cd_in)
    ParseOptionLine = ilen_line

    IF (ilen_line == 0) RETURN

    ilen_opt = INDEX( cd_in ,' ') -1

    cd_opt   = ADJUSTL( cd_in (1         :ilen_opt) )
    cd_arg   = ADJUSTL( cd_in (ilen_opt+1:        ) )

  END FUNCTION ParseOptionLine


  SUBROUTINE ParseArgString (cd_lev, kdraw_level, kmax )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ParseArgString  ***
    !!
    !! ** Purpose :  Analyse the argument of the -lev and -time option 
    !!
    !! ** Method  :    -lev 1-2,6-9,21 will set kdraw_level to 1 for 
    !!             indices 1,2,6,7,8,9,21 other to 0
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),            INTENT(inout) :: cd_lev
    INTEGER(KIND=4), DIMENSION(:), INTENT(out) :: kdraw_level
    INTEGER(KIND=4),             INTENT(inout) :: kmax 

    CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE :: clbloc
    INTEGER(KIND=4)        :: ji, jb
    INTEGER(KIND=4)        :: icoma, i1, i2, ic
    INTEGER(KIND=4)        :: ilen, istatus
    INTEGER(KIND=4)        :: isiz
    LOGICAL                :: ll_level, ll_time
    CHARACTER(LEN=80)      :: cldum
    !!----------------------------------------------------------------------

    cd_lev = ADJUSTL ( TRIM(cd_lev) )
    ilen   = LEN_TRIM(cd_lev)
    isiz   = SIZE ( kdraw_level )   

    ll_level = .FALSE. ; ll_time = .FALSE.
    SELECT CASE ( kmax )
     CASE ( -2 ) ; ll_level = .TRUE.
     CASE ( -1 ) ; ll_time  = .TRUE.
     CASE DEFAULT 
         PRINT *,'WARNING : ParseArgString not able to determine '
         PRINT *,'          if time or level are involved in this call'
    END SELECT

    kmax   = 0

    ! count the number of comas in cl_lev
    icoma = 0
    DO ji = 1, ilen
       IF ( cd_lev(ji:ji) == ',' ) THEN
          icoma = icoma + 1
       ENDIF
    ENDDO
    ALLOCATE ( clbloc(icoma+1) )  ! one more bloc than coma

    ! fill clbloc
    DO jb = 1, icoma
       ic         = INDEX( cd_lev,',') - 1
       clbloc(jb) = cd_lev(1    : ic )
       cd_lev     = cd_lev(ic+2 :    )  ! shift cd_lev
    ENDDO
       clbloc(icoma+1) = cd_lev
    
    ! Now decode each bloc ( i1-i2 or i1 )
    DO jb = 1, icoma + 1
       ic = INDEX ( clbloc(jb),'-')
       IF ( ic /= 0 ) clbloc(jb)(ic:ic) = ' '  ! replace dash by blank
       i1 = 0   ;  i2 = 0
       ! use iostat just to be able to read i1, even if i2 is N/A
       READ( clbloc(jb), FMT=*, IOSTAT=istatus ) i1, i2
       IF ( i2 /= 0 ) THEN
          kdraw_level(i1:i2) = 1
          kmax = MAX ( kmax, i2 )
       ELSE
          IF ( i1 == 0 ) THEN  ! all level are to be taken
            kdraw_level(:)  = 1 ; kmax = 99999
          ELSE
            kdraw_level(i1) = 1
            kmax = MAX (kmax, i1 )
          ENDIF
       ENDIF
    ENDDO

    IF (lo_debug ) THEN 
       cldum = 'Time or Level '
       IF ( ll_time  ) cldum = 'Time'
       IF ( ll_level ) cldum = 'Level'
       PRINT *, '#PAS ParseArgString :'
        DO ji = 1, isiz
          IF (  kdraw_level(ji) == 1 ) THEN
            PRINT *,'#PAS ',TRIM(cldum),' ',ji,' selected '
          ENDIF
        ENDDO
        PRINT *, '#PAS    MAX = ', kmax
    ENDIF

    DEALLOCATE (clbloc )

  END SUBROUTINE ParseArgString


  SUBROUTINE GetString (ki, ktype, cd_arg, cd_string)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetString  ***
    !!
    !! ** Purpose :   Return the string argument required by an option
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki        ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype     ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg    ! already argument if file
    CHARACTER(LEN=*),  INTENT(out) :: cd_string ! output string
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ; CALL getarg(ki,cd_string)
    ELSE 
       cd_string=TRIM(cd_arg)
    ENDIF

  END SUBROUTINE GetString

  SUBROUTINE Get2String (ki, ktype, cd_arg, cd_string1, cd_string2)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get2String  ***
    !!
    !! ** Purpose :  Return 2 strings argument required by an option 
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    CHARACTER(LEN=*),  INTENT(out) :: cd_string1 ! output string
    CHARACTER(LEN=*),  INTENT(out) :: cd_string2 ! output string

    INTEGER(KIND=4)      :: iblank 
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ;  CALL getarg(ki,cd_string1)
       ki=ki+1 ;  CALL getarg(ki,cd_string2)
    ELSE 
       ! Look for a blank character as separator
       iblank = INDEX(TRIM(cd_arg),' ')
       IF ( iblank ==  0 ) THEN
          PRINT *,' -vecrotuv or ... requires 2 strings as arguments'
          STOP 'Get2String'
       ENDIF
       cd_string1 =          TRIM(cd_arg(1       :iblank) )
       cd_string2 = ADJUSTL( TRIM(cd_arg(iblank+1:        )) )
    ENDIF

  END SUBROUTINE Get2String


  SUBROUTINE Get1real (ki, ktype, cd_arg, pValue)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get1real  ***
    !!
    !! ** Purpose : Get 1 real argument of an option
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    REAL(KIND=4),      INTENT(out) :: pValue     ! output Real Value

    CHARACTER(LEN=64)  ::  cldum
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ; CALL getarg(ki,cldum) ; READ(cldum,*) pValue
    ELSE 
       READ (cd_arg,*) pValue
    ENDIF

  END SUBROUTINE Get1real


  SUBROUTINE Get2real (ki, ktype, cd_arg, pValue1, pValue2)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get2real  ***
    !!
    !! ** Purpose :  Get 2 real argument of an option
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    REAL(KIND=4),      INTENT(out) :: pValue1    ! output Real Value 1
    REAL(KIND=4),      INTENT(out) :: pValue2    ! output Real Value 2

    CHARACTER(LEN=64)  ::  cldum
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ; CALL getarg(ki,cldum) ; READ(cldum,*) pValue1
       ki=ki+1 ; CALL getarg(ki,cldum) ; READ(cldum,*) pValue2
    ELSE 
       READ (cd_arg,*) pValue1, pValue2
    ENDIF

  END SUBROUTINE Get2real


  SUBROUTINE Get1integer (ki, ktype, cd_arg, kValue)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get1integer  ***
    !!
    !! ** Purpose :  Get 1 integer argument of an option 
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    INTEGER(KIND=4),   INTENT(out) :: kValue     ! output Integer  Value 

    CHARACTER(LEN=64)  ::  cldum
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ; CALL getarg(ki,cldum) ; READ(cldum,*) kValue
    ELSE 
       READ (cd_arg,*) kValue
    ENDIF

  END SUBROUTINE Get1integer


  SUBROUTINE Get2integer (ki, ktype, cd_arg, kValue1, kValue2)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get2integer ***
    !!
    !! ** Purpose :  Get 2 integer arguments of an option 
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    INTEGER(KIND=4),   INTENT(out) :: kValue1    ! output Integer  Value 1
    INTEGER(KIND=4),   INTENT(out) :: kValue2    ! output Integer  Value 2

    CHARACTER(LEN=64)  ::  cldum
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) kValue1
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) kValue2
    ELSE 
       READ (cd_arg,*) kValue1, kValue2
    ENDIF

  END SUBROUTINE Get2integer


  SUBROUTINE Get4real (ki, ktype, cd_arg, pValue1, pValue2, pValue3, pValue4)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Get4real  ***
    !!
    !! ** Purpose :  Get 2 real argument of an option
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(inout) :: ki         ! argument index on the line
    INTEGER(KIND=4),    INTENT(in) :: ktype      ! file or line
    CHARACTER(LEN=*),   INTENT(in) :: cd_arg     ! already argument if file
    REAL(KIND=4),      INTENT(out) :: pValue1    ! output Real Value 1
    REAL(KIND=4),      INTENT(out) :: pValue2    ! output Real Value 2
    REAL(KIND=4),      INTENT(out) :: pValue3    ! output Real Value 3
    REAL(KIND=4),      INTENT(out) :: pValue4    ! output Real Value 4

    CHARACTER(LEN=64)  ::  cldum
    !!----------------------------------------------------------------------
    IF (ktype == jp_from_line) THEN
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) pValue1
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) pValue2
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) pValue3
       ki=ki+1 ;  CALL getarg(ki,cldum) ; READ(cldum,*) pValue4
    ELSE 
       READ (cd_arg,*) pValue1, pValue2, pValue3, pValue4
    ENDIF

  END SUBROUTINE Get4real

END MODULE modreadargs



