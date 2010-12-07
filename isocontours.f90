MODULE isocontours 
  !!======================================================================
  !!                     ***  MODULE  isocontours  ***
  !!  Hold the routine to plot isocontours are in this module
  !!=====================================================================
  !! History : 1.0   :  06/1993  :  E. Brown     : Original code
  !!           7.0   :  11/2010  :  J.M. Molines : F90 and doctor norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   TraceContours  :
  !!   ColorContour   :
  !!   FilterQuotes   :
  !!----------------------------------------------------------------------
  USE modcom
  USE modcolor
  USE modmapncar
  USE modexternal
  USE val_table
  USE nclinfo
  USE util

  IMPLICIT NONE

  PRIVATE

  PUBLIC  :: TraceContours   ! called by chart and coupe
  PUBLIC  :: ColorContour    ! called by coupe and tracecol

  PRIVATE :: FilterQuotes    ! local call
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS


  SUBROUTINE TraceContours (pfld, bdimg, pcoords, kmap_flag)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE TraceContours  ***
    !!
    !! ** Purpose : Trace contours on the map
    !!
    !! ** Method  :   pfld    : data array 
    !!                bdimg   : data structure 
    !!                pcoords : location of the area to be plotted
    !!                kmap    : 1 : isocontours are projected on a map
    !!                        : 0 : isocontours are not projected 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(in) :: pfld      ! NXX x NYY
    TYPE( bimgfile ),             INTENT(in) :: bdimg     ! data structure
    REAL(KIND=4), DIMENSION(:),   INTENT(in) :: pcoords   ! dim = 4
    INTEGER(KIND=4),              INTENT(in) :: kmap_flag !  map flag

    INTEGER(KIND=4)                              :: ix, iy
    !                                                         !    for cntlim option :
    INTEGER(KIND=4)                              :: ilu = 20  ! logical unit for cnt_lim
    INTEGER(KIND=4),SAVE                         :: icontour  ! counter for isocontour in file
    REAL(KIND=4),SAVE, DIMENSION(:), ALLOCATABLE :: zcontour  ! contour level read in file (max : jp_cntlim)
    !
    INTEGER(KIND=4)                  :: ierr                  ! error flag
    INTEGER(KIND=4)                  :: ilen                  ! length of strings
    INTEGER(KIND=4)                  :: icount                ! counter for iso lines
    INTEGER(KIND=4)                  :: icol_index            !
    INTEGER(KIND=4)                  :: illnat                ! linear -log nature of the mapping
    INTEGER(KIND=4)                  :: iclu, iclv, icln
    INTEGER(KIND=4)                  :: istatus
    !
    REAL(KIND=4)                     :: zspv                  ! local special value
    REAL(KIND=4)                     :: zrl, zrr, zrb, zrt    ! real world window
    REAL(KIND=4)                     :: zur, zul, zut, zub    ! plotter world window
    REAL(KIND=4)                     :: zvmin, zvmax          ! Min and Max value of pfld
    REAL(KIND=4)                     :: zcntval               !

    CHARACTER(LEN=256)               :: clIn, clOut           ! 
    !!----------------------------------------------------------------------

    IF ( lo_debug ) PRINT *, '#ISO Tracecontour computation ...'

    CALL gflas1( jp_ib_contours )

    ix  = bdimg%nxdata
    iy  = bdimg%nydata
    zspv = bdimg%spval

    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, illnat)

    CALL FindMinMax (bdimg, pfld, zvmin, zvmax)
    
    IF (opt_minc == 0) rct_cmn = zvmin  ! no minimum specified on command line: take data minimum
    IF (opt_maxc == 0) rct_cmx = zvmax  ! no maximum specified on command line: take data maximum

    IF ( (opt_minc == 1 .OR. opt_maxc == 1 ) .AND. (opt_cntint == 0) ) THEN
       ! contour interval set with option -cntint
       rct_cis = (rct_cmx - rct_cmn) / float(nct_cls)
    ENDIF

    CALL SetCntMin (zvmin)
    CALL SetCntMax (zvmax)

    CALL gqtxci (ierr, icol_index)  ! save txt color index before change
    CALL gstxci (nct_llc)           ! set txt color index to Label Line Color

    CALL cpseti ('SET - do SET call flag             ', 0        )
    CALL cpseti ('MAP - Mapping Flag                 ', kmap_flag)

    IF (bdimg%ngrid == 3 .AND. opt_chart == 1) THEN
       CALL cpsetr ('XC1 - X coordinate at index 1      ', float(nimin) )
       CALL cpsetr ('XCM - X coordinate at index max    ', float(nimax) )
       CALL cpsetr ('YC1 - Y coordinate at index 1      ', float(njmin) )
       CALL cpsetr ('YCN - Y coordinate at index max    ', float(njmax) )
    ELSE
       CALL cpsetr ('XC1 - X coordinate at index 1      ', pcoords(1) )
       CALL cpsetr ('XCM - X coordinate at index max    ', pcoords(2) )
       CALL cpsetr ('YC1 - Y coordinate at index 1      ', pcoords(3) )
       CALL cpsetr ('YCN - Y coordinate at index max    ', pcoords(4) )
    ENDIF

    CALL cpsetc ('HLT - High Low label Text          ', ' '          )
    CALL cpsetr ('SPV - SPecial Value                ', zspv         )
    CALL cpsetr ('SFS - Scale Factor Selector        ', 10.**rct_sfs )

    CALL cpseti ('CLS - Contour Level Selection      ', nct_cls )
    CALL cpsetr ('CIS - Contour Interval Specifier   ', rct_cis )

    CALL cpsetr ('CMN - Contour MiNimum              ', rct_cmn )
    CALL cpsetr ('CMX - Contour MaXimum              ', rct_cmx )
    CALL cpsetr ('CWM - Character Width Multip.      ', rct_cwm )
    CALL cpseti ('NOF - Numeric Omission Flag        ', 7       )  ! message as short as possible

    CALL cpseti ('LLP - Line Label Positioning       ', nct_llp )
    CALL cpseti ('LLB - Line Label Box Flag          ', nct_llb )
    CALL cpsetr ('LLS - Line Label Size              ', rct_lls )
    CALL cpseti ('LIS - Label Interval Specifier     ', nct_lis )
    CALL cpseti ('LBC - Label Box Color index        ', nct_lbc )

    CALL cpseti ('ILB - Information Label Box flag   ', nct_ilb )
    CALL cpseti ('ILC - Information Label Color index', nct_ilc )
    CALL cpsetr ('ILL - Information Label Line width ', rct_ill )
    CALL cpsetr ('ILS - Information Label Size       ', rct_ils )
    CALL cpseti ('ILP - Information Label Positioning', nct_ilp )

    CALL cpsetr ('RC1 - Regular Scheme Constant 1    ', rct_rc1 )
    CALL cpsetr ('RC2 - Regular Scheme Constant 1    ', rct_rc2 )


    ! Information label management : cct_ilt can have variables in it 
    CALL FilterQuotes (cct_ilt, clIn)
    CALL ParseString  (clIn, clOut)

    ilen = LEN_TRIM (clOut)
    IF ( ilen > 100 ) ilen = 100  ! arbitrary limitation
    cct_ilt = clOut(1:ilen)

    CALL cpsetc ('ILT - Information Label Text       ', cct_ilt                 )
    CALL cpsetr ('ILX - Information Label X coord    ',(rct_ilx-zrl)/ (zrr-zrl) )    
    CALL cpsetr ('ILY - Information Label Y coord    ',(rct_ily-zrb)/ (zrt-zrb) )      
    CALL cpsetr ('ORV - Out-of-Range Value           ', rp_out_range            )

    CALL cprect (pfld, NXX, ix, iy, rwrk, jp_RWRK, niwrk, jp_IWRK)

    IF (opt_cntlim == 1) THEN
       ! contour values are read in a file (-cntlim option)
       IF ( .NOT. ALLOCATED (zcontour ) )  THEN   ! do only once
          PRINT *, '  READ CNTLIM in file ', TRIM(cf_cntlim)
          icontour = 1
          ALLOCATE (zcontour(jp_cntlim))            ! allocate only if needed
          OPEN (ilu, file=cf_cntlim, status='old')  ! existence checked already

          DO    ! read the contour file
            READ(ilu, *, END=10) zcontour(icontour)
            icontour = icontour + 1
          ENDDO

10        CONTINUE
          CLOSE (ilu)
          icontour = icontour - 1
       ENDIF

       CALL cpseti ('CLS - Contour Level Selection ', 0        ) ! conpack will not try to find values
       CALL cpseti ('NCL - Number of Contour Levels', icontour )

       DO iclv = 1, icontour
          CALL cpseti ('PAI - parameter array index    ', iclv             )
          CALL cpsetr ('CLV - Contour Level Values     ', zcontour(iclv)   )
          CALL cpseti ('CLC - Contour Line Color Index ', COLOR_ISOCONTOUR )
          CALL cpseti ('LLC - Line Label Color Index   ', nct_llc          )


          IF (MOD( float(iclv-1),float(nct_lis)) == 0.0 ) THEN  ! every Label Interval Specifier (LIS)
             PRINT *,'line label for:',zcontour(iclv)
             CALL cpsetr ('CLL - Contour Line Line Width ',rct_cll2 )
             CALL cpseti ('CLU - Contour Level Use Flags ', 3       )  ! draw line + label
          ELSE 
             CALL cpsetr ('CLL - Contour Line Line Width ',rct_cll1 )
             CALL cpseti ('CLU - Contour Level Use Flags ', 1       )  ! draw line only
          ENDIF
       ENDDO

    ELSE
       ! automatic contouring (conpack choose the lines)
       icount=0

       CALL cppkcl (pfld, rwrk, niwrk)     ! pick contour lines
       CALL cppklb (pfld, rwrk, niwrk)     ! pick labeled lines
       CALL cpgeti ('NCL - Number of Contour Levels',  icln  )

       DO iclv = 1, icln
          CALL cpseti ('PAI - Parameter Array Index', iclv   )

          IF (opt_cntint == 0) THEN   ! why ??
            CALL cpgeti ('CLU - Contour Level Use  ', iclu   )
            IF ( iclu /= 3 .AND.  icount == 0 )  THEN
              ! skip still the first automatically labeled line
            ELSE
             icount =icount + 1
             IF ( MOD (icount, nct_lis) == 1 ) THEN
                 CALL cpseti ('CLU - Contour Level Use Flags ', 3 )
             ELSE 
                 CALL cpseti ('CLU - Contour Level Use Flags ', 1 )
             ENDIF
            ENDIF
          ENDIF           ! ? why 

          CALL cpgeti ('CLU - Contour Level Use Flags ', iclu             )

          CALL cpseti ('CLC - Contour Line Color      ', COLOR_ISOCONTOUR )
          CALL cpseti ('LLC - Line Label Color Index  ', nct_llc          )

          IF (iclu == 3) THEN
             CALL cpsetr ('CLL - Contour Line Line Width', rct_cll2       )
          ELSE 
             CALL cpsetr ('CLL - Contour Line Line Width', rct_cll1       )
          ENDIF
       ENDDO

       IF (opt_cntlout == 1) THEN
          OPEN(ilu,file=cf_cntlout,form='formatted')

          DO iclv = 1, icln                 
             CALL cpseti ('PAI - Parameter Array Index', iclv   )
             CALL cpgetr ('CLV - Contour Level Value  ', zcntval )
             WRITE(ilu,'(e15.5)') zcntval
          ENDDO

          CLOSE(ilu)               
       ENDIF
    ENDIF

    IF (nct_llp  <  1) THEN
       CALL cplbam (pfld, rwrk, niwrk, niama)
    ENDIF

    CALL cplbdr (pfld, rwrk, niwrk)        
    CALL cpcldm (pfld, rwrk, niwrk, niama, drawcl)

    ! restore original values
    CALL gstxci (icol_index)   
    CALL set(zrl, zrr, zrb, zrt, zur, zul, zut, zub, illnat)

    PRINT *,'TraceContours : Space used :' ,jp_iama - (niama(6) - niama(5) - 1)

    CALL gflas2

  END SUBROUTINE TraceContours


  SUBROUTINE ColorContour (pfld, bdimg, plimit, pcoords, kmap_flag )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ColorContour  ***
    !!
    !! ** Purpose :   
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(in) :: pfld      ! NXX x NYY
    TYPE( bimgfile ),             INTENT(in) :: bdimg     ! data structure
    REAL(KIND=4), DIMENSION(:),   INTENT(in) :: plimit    ! NBOXMAX +1
    REAL(KIND=4), DIMENSION(:),   INTENT(in) :: pcoords   ! dim = 4
    INTEGER(KIND=4),              INTENT(in) :: kmap_flag !  map flag

    INTEGER(KIND=4)  :: ix, iy
    INTEGER(KIND=4)  :: inval
    INTEGER(KIND=4)  :: iclv
    REAL(KIND=4)     :: zspv
    !!----------------------------------------------------------------------

    ix   = bdimg%nxdata
    iy   = bdimg%nydata
    zspv = bdimg%spval

    CALL cpseti ('SET - do SET call flag         ', 0        )
    CALL cpseti ('MAP - mapping flag             ', kmap_flag)

    IF (bdimg%ngrid == 3 .AND. opt_chart == 1) THEN
       CALL cpsetr ('XC1 - X coordinate at index 1      ', float(nimin))
       CALL cpsetr ('XCM - X coordinate at index max    ', float(nimax))
       CALL cpsetr ('YC1 - Y coordinate at index 1      ', float(njmin))
       CALL cpsetr ('YCN - Y coordinate at index max    ', float(njmax))
    ELSE
       CALL cpsetr ('XC1 - X coordinate at index 1      ', pcoords(1))
       CALL cpsetr ('XCM - X coordinate at index max    ', pcoords(2))
       CALL cpsetr ('YC1 - Y coordinate at index 1      ', pcoords(3))
       CALL cpsetr ('YCN - Y coordinate at index max    ', pcoords(4))
    ENDIF

    CALL cpsetr ('SPV - SPecial Value                   ', zspv      )

    CALL cpsetc ('HLT - High Low label Text             ', ' '       )
    CALL cpsetc ('ILT - Information Label Text          ', ' '       )

    CALL cprect (pfld, NXX, ix, iy, rwrk, jp_RWRK, niwrk, jp_IWRK)

    CALL cppkcl (pfld, rwrk, niwrk)

    CALL cpseti ('NCL - number of contour levels', marks_count-1)

    DO iclv = 1, marks_count-1
       CALL cpseti ('PAI - parameter array index', iclv)
       CALL cpsetr ('CLV - contour level', plimit(marks_index(iclv)-COLOR_NRES+2))
       CALL cpseti ('CLU - contour level use', 1)
       CALL cpseti ('AIB - area identifier below', iclv+COLOR_NRES-1)
       CALL cpseti ('AIA - area identifier above', iclv+COLOR_NRES)

    ENDDO


    IF (lo_debug2 ) THEN
       CALL showmp_param('isocontours')
       CALL showar_param('isocontours')
       CALL showcp_param('isocontours')
    ENDIF

    CALL cpclam (pfld, rwrk, niwrk, niama)

    IF (lo_debug ) THEN
           CALL cpgeti ('IWU', inval) ;  PRINT *,'#ISCC IWRK size :',inval
           CALL cpgeti ('RWU', inval) ;  PRINT *,'#ISCC RWRK size :',inval, jp_iama
    ENDIF

    PRINT *,'ColorContour  : Space used :' ,jp_iama - (niama(6) - niama(5) - 1)

  END SUBROUTINE ColorContour


  SUBROUTINE FilterQuotes (cdIn, cdOut)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE FilterQuotes  ***
    !!
    !! ** Purpose :   Eliminates " and ' at the begining and end of text
    !!
    !! ** Method  : Copy cdIn into cdOut with eventual truncation of 
    !!             first and last character.
    !!
    !! References :  
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(in) :: cdIn
    CHARACTER(LEN=256), INTENT(out) :: cdOut

    INTEGER(KIND=4)  :: ifrst_char, ilast_char  ! position of 1rst and last char in cdIn
    !!----------------------------------------------------------------------

    ifrst_char = 1
    ilast_char = LEN_TRIM (cdIn)

    ! leading ' or " are detected 
    IF (cdIn(ifrst_char:ifrst_char) == '''' .OR. cdIn(ifrst_char:ifrst_char) == '"') THEN
       ifrst_char = 2
    ENDIF

    ! trailing ' or " are detected 
    IF (cdIn(ilast_char:ilast_char) == '''' .OR. cdIn(ilast_char:ilast_char) == '"') THEN
       ilast_char = ilast_char-1
    ENDIF

    cdOut = cdIn(ifrst_char:ilast_char)

  END SUBROUTINE FilterQuotes

END MODULE isocontours
