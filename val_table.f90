MODULE val_table
  !!======================================================================
  !!                     ***  MODULE  val_table  ***
  !! Management of the  internal variable of chart/coupe accessible to the
  !! user. Interpret strings that use those variables (eg CLR_DEP ...)
  !!=====================================================================
  !! History : 1.0  : 06/1993  ! E. Brown     : Original code
  !!         : 7.0  : 12/2010  ! J.M. Molines : F90 and Doctor norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !!   InitValTable       !
  !!   ParseString        !
  !!   GetNumValue        !
  !!   SetFormatString    !
  !!   SplitValFmt        !
  !!   SetValTable        !
  !!   UpdateValTable     !
  !!   SetClrMin          !
  !!   SetClrMax          !
  !!   SetVecMin          !
  !!   SetVecMax          !
  !!   SetCntExp          !
  !!   SetClrExp          !
  !!   SetClrCNESday      !
  !!   SetCLIPPERday      !
  !!   SetCntMin          !
  !!   SetCntMax          ! 
  !!   ConvCNES           !
  !!   CalDat             !

  !!   IDformat           !
  !!   ClipperDat         !

  !!----------------------------------------------------------------------
  USE modcom
  USE modvaltable
  USE util

  IMPLICIT NONE

  PRIVATE

  PUBLIC ::  InitValTable    ! called by chart and coupe
  PUBLIC ::  ParseString     ! called by tracecol and isocontours
  PUBLIC ::  IDformat        ! called by tracecol
  PUBLIC ::  SetFormatString ! Called by modreadargs
  PUBLIC ::  SplitValFmt     ! Called by modreadargs
  PUBLIC ::  SetValTable     ! called by chart and coupe
  PUBLIC ::  UpdateValTable  ! called by chart and coupe
  PUBLIC ::  SetClrMin       ! called by modeflim
  PUBLIC ::  SetClrMax       ! called by modeflim
  PUBLIC ::  SetVecMin       ! called by modeflim 
  PUBLIC ::  SetVecMax       ! called by modeflim
  PUBLIC ::  SetCntExp       ! called by modreadargs
  PUBLIC ::  SetClrExp       ! called by modreadargs
  PUBLIC ::  SetClrCNESday   ! called in chart and coupe
  PUBLIC ::  SetCntMin       ! called vy isocontours
  PUBLIC ::  SetCntMax       ! called by isocontours

  PRIVATE ::  GetNumValue    !  local call
  PRIVATE ::  SetCLIPPERday  ! local call
  PRIVATE ::  ConvCNES       ! local call
  PRIVATE ::  CalDat         !  local call

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------


CONTAINS

  SUBROUTINE InitValTable ()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE InitValTable  ***
    !!
    !! ** Purpose :   Initialize values and formats in data sructure
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4)  ::  iday, imon, iyear
    !!----------------------------------------------------------------------
    int_table (ICOUCHE   )%id  = 'COUCHE' 
    int_table (ITSTEP    )%id  = 'TSTEP'  

    int_table (ICLR_NX   )%id = 'CLR_NX'    
    int_table (ICLR_NY   )%id = 'CLR_NY'    
    int_table (ICLR_NZ   )%id = 'CLR_NZ'    
    int_table (ICLR_EXP  )%id = 'CLR_EXP'

    int_table (ICNT_NX   )%id = 'CNT_NX'    
    int_table (ICNT_NY   )%id = 'CNT_NY'    
    int_table (ICNT_NZ   )%id = 'CNT_NZ'    
    int_table (ICNT_EXP  )%id = 'CNT_EXP'

    int_table (IVEC_NX   )%id = 'VEC_NX'    
    int_table (IVEC_NY   )%id = 'VEC_NY'    
    int_table (IVEC_NZ   )%id = 'VEC_NZ'    
    int_table (IVEC_EXP  )%id = 'VEC_EXP'

    int_table (ICLR_XAXIS)%id = 'X_AXIS'    
    int_table (ICLR_YAXIS)%id = 'Y_AXIS'    
    int_table (ICLR_ZAXIS)%id = 'Z_AXIS'    
    int_table (ICLR_PAL  )%id = 'PALETTE'    


    real_table(ICLR_MIN  )%id = 'CLR_MIN'   
    real_table(ICLR_MAX  )%id = 'CLR_MAX'   
    real_table(ICLR_SPVAL)%id = 'CLR_SPVAL' 
    real_table(ICLR_DX   )%id = 'CLR_DX'    
    real_table(ICLR_DY   )%id = 'CLR_DY'    
    real_table(ICLR_TIME )%id = 'CLR_TIME'     
    real_table(ICLR_TIMEDAY )%id = 'CLR_TIMEDAY'     
    real_table(ICLR_DEPTH )%id= 'CLR_DEPTH' 

    real_table(ICNT_MIN  )%id = 'CNT_MIN'   
    real_table(ICNT_MAX  )%id = 'CNT_MAX'   
    real_table(ICNT_SPVAL)%id = 'CNT_SPVAL' 
    real_table(ICNT_DX   )%id = 'CNT_DX'    
    real_table(ICNT_DY   )%id = 'CNT_DY'    
    real_table(ICNT_TIME )%id = 'CNT_TIME'     
    real_table(ICNT_DEPTH )%id= 'CNT_DEPTH' 

    real_table(IVEC_MIN  )%id = 'VEC_MIN'   
    real_table(IVEC_MAX  )%id = 'VEC_MAX'   
    real_table(IVEC_SPVAL)%id = 'VEC_SPVAL' 
    real_table(IVEC_DX   )%id = 'VEC_DX'    
    real_table(IVEC_DY   )%id = 'VEC_DY'    
    real_table(IVEC_TIME )%id = 'VEC_TIME'     
    real_table(IVEC_DEPTH )%id= 'VEC_DEPTH' 

    string_table(ICLR_STR1)%id = 'CLR_STR1'
    string_table(ICLR_STR2)%id = 'CLR_STR2'
    string_table(ICLR_STR3)%id = 'CLR_STR3'
    string_table(ICLR_STR4)%id = 'CLR_STR4'

    string_table(ICNT_STR1)%id = 'CNT_STR1'
    string_table(ICNT_STR2)%id = 'CNT_STR2'
    string_table(ICNT_STR3)%id = 'CNT_STR3'
    string_table(ICNT_STR4)%id = 'CNT_STR4'

    string_table(IVEC_STR1)%id = 'VEC_STR1'
    string_table(IVEC_STR2)%id = 'VEC_STR2'
    string_table(IVEC_STR3)%id = 'VEC_STR3'
    string_table(IVEC_STR4)%id = 'VEC_STR4'

    string_table(I_DATE)%id = 'DATE'
    string_table(CLR_ICDATE)%id = 'CLR_JCNES'
    string_table(CNT_ICDATE)%id = 'CNT_JCNES'
    string_table(VEC_ICDATE)%id = 'VEC_JCNES'
    string_table(CLIPPER_DATE)%id = 'CLIPPER_DAT'

    int_table (ICOUCHE   )%format = '(i10)'
    int_table (ITSTEP    )%format = '(i10)'

    int_table (ICLR_NX   )%format = '(i10)'
    int_table (ICLR_NY   )%format = '(i10)'
    int_table (ICLR_NZ   )%format = '(i10)'
    int_table (ICLR_EXP  )%format = '(i10)'

    int_table (ICNT_NX   )%format = '(i10)'
    int_table (ICNT_NY   )%format = '(i10)'
    int_table (ICNT_NZ   )%format = '(i10)'
    int_table (ICNT_EXP  )%format = '(i10)'

    int_table (IVEC_NX   )%format = '(i10)'
    int_table (IVEC_NY   )%format = '(i10)'
    int_table (IVEC_NZ   )%format = '(i10)'
    int_table (IVEC_EXP  )%format = '(i10)'

    int_table (ICLR_XAXIS)%format = '(i10)'    
    int_table (ICLR_YAXIS)%format = '(i10)'    
    int_table (ICLR_ZAXIS)%format = '(i10)'    
    int_table (ICLR_PAL  )%format = '(f10.2)'     


    real_table(ICLR_MIN  )%format = '(f10.2)'
    real_table(ICLR_MAX  )%format = '(f10.2)'
    real_table(ICLR_SPVAL)%format = '(f10.2)'
    real_table(ICLR_DX   )%format = '(f10.2)'
    real_table(ICLR_DY   )%format = '(f10.2)'
    real_table(ICLR_TIME )%format = '(f10.2)'
    real_table(ICLR_TIMEDAY )%format = '(f10.2)'
    real_table(ICLR_DEPTH)%format = '(f10.2)'

    real_table(ICNT_MIN  )%format = '(f10.2)'
    real_table(ICNT_MAX  )%format = '(f10.2)'
    real_table(ICNT_SPVAL)%format = '(f10.2)'
    real_table(ICNT_DX   )%format = '(f10.2)'
    real_table(ICNT_DY   )%format = '(f10.2)'
    real_table(ICNT_TIME )%format = '(f10.2)'
    real_table(ICNT_DEPTH)%format = '(f10.2)'

    real_table(IVEC_MIN  )%format = '(f10.2)'
    real_table(IVEC_MAX  )%format = '(f10.2)'
    real_table(IVEC_SPVAL)%format = '(f10.2)'
    real_table(IVEC_DX   )%format = '(f10.2)'
    real_table(IVEC_DY   )%format = '(f10.2)'
    real_table(IVEC_TIME )%format = '(f10.2)'
    real_table(IVEC_DEPTH)%format = '(f10.2)'

    string_table(ICLR_STR1)%format = '(A)'
    string_table(ICLR_STR2)%format = '(A)'
    string_table(ICLR_STR3)%format = '(A)'
    string_table(ICLR_STR4)%format = '(A)'

    string_table(ICNT_STR1)%format = '(A)'
    string_table(ICNT_STR2)%format = '(A)'
    string_table(ICNT_STR3)%format = '(A)'
    string_table(ICNT_STR4)%format = '(A)'

    string_table(IVEC_STR1)%format = '(A)'
    string_table(IVEC_STR2)%format = '(A)'
    string_table(IVEC_STR3)%format = '(A)'
    string_table(IVEC_STR4)%format = '(A)'

    string_table(I_DATE)%format = '(A)'

    WRITE (string_table(I_DATE)%cstr,100) iday,imon,iyear

100 FORMAT (i2.2,'/',i2.2,'/',i2.2)

  END SUBROUTINE InitValTable


  SUBROUTINE ParseString (cd_in, cd_out)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ParseString  ***
    !!
    !! ** Purpose : Interpret special meaning of some expression in 
    !!              a string given on input. Return a vision of the
    !!              data structure  with special expression interpreted. For
    !!              instance in a string " depth = @CLR_DEPTH@ " the expression
    !!              @CLR_DEPTH@ will be replaced by le depts of the color data.
    !! 
    !! ** Method : The string is composed like 
    !!              " ....text... @TABLE@ ....text... @TABLE@ ...etc..."
    !!              and the tricks is to get all field within @ @ in order to
    !!              replace it by the corresponding value.
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),  INTENT(in) :: cd_In
    CHARACTER(LEN=*), INTENT(out) :: cd_Out

    INTEGER(KIND=4)            :: ji
    INTEGER(KIND=4)            :: icount, i0, i1, i2, i3
    CHARACTER(LEN=1)           :: cl_ctl
    CHARACTER(LEN=256)         :: cl_val, cl_dum
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: clblk
    INTEGER(KIND=4),   DIMENSION(:), ALLOCATABLE :: ictlpos
    !!----------------------------------------------------------------------
    cl_ctl = '@'
    cl_dum = cd_In
    icount = 0

    ! Initialize
    cd_Out  = ' '
    IF ( LEN_TRIM ( cd_In ) == 0 )  RETURN  ! nothing to do, empty string in input
   
    ! count the number of cl_ctl chars
    DO ji = 1, LEN_TRIM(cd_In)
       IF ( cd_In(ji:ji) == cl_ctl ) THEN
         icount = icount + 1
       ENDIF
    ENDDO

    ! Perform some verifications
    IF (MOD (icount,2 ) /= 0 ) THEN 
       CALL PrintMessage (jp_Err,jp_SyntStri ,' Missing @ in text string ')
    ENDIF

    ! if nothing particular to interpret, return
    IF (icount == 0  ) THEN
       cd_Out = cd_In
       RETURN
    ENDIF

    ALLOCATE ( clblk(icount/2), ictlpos(icount+1) )

    ! get @ postions in ictlpos array
    DO ji = 1, icount
      i1=INDEX(cl_dum ,cl_ctl)
      ictlpos(ji) = i1
      cl_dum(i1:i1)=' '  ! replace @ by blank for next iteration
    ENDDO

    i0     = ictlpos(1)-1
    cd_Out = cd_In(1:i0 )
    ictlpos( icount + 1 ) = LEN_TRIM(cd_in)

    DO ji = 1, icount/2
      i1 = ictlpos(2*ji - 1 ) + 1
      i2 = ictlpos(2*ji     ) - 1
      clblk(ji)= cd_in(i1:i2)
      ! some tricks on clblk to be performed
      CALL GetNumValue (clblk(ji), cl_val)
      clblk(ji)=TRIM(cl_val)
      i3 = i2 + 2
      cd_Out = cd_Out(1:i0)//TRIM(clblk(ji) )//cd_In(i3:)
      i0 = i0 + LEN_TRIM(clblk(ji)) + ictlpos(2*ji+1 ) - i3
    END DO

    DEALLOCATE ( clblk, ictlpos )

  END SUBROUTINE ParseString


  SUBROUTINE GetNumValue (cd_In, cd_Out )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetNumValue  ***
    !!
    !! ** Purpose :  Exchange input string ( variable id ) with its numerical
    !!               value according to the format for this variable 
    !!
    !! ** Method  :  Accepted format are I, F, E
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),  INTENT(in) :: cd_In
    CHARACTER(LEN=*), INTENT(out) :: cd_Out

    INTEGER(KIND=4)    :: lStrIn, lStrOut
    INTEGER(KIND=4)    :: ji
    CHARACTER(LEN=256) :: cl_work
    LOGICAL            :: llfound
    !!----------------------------------------------------------------------
    ! initialize working string
    cl_work = ' '

    llfound = .FALSE.
    ! scan int_table for matching cd_In
    DO ji=1, NUM_INT_VALS
       IF (TRIM(cd_In) == int_table(ji)%id ) THEN
          llfound = .TRUE.

          IF (IDformat (int_table(ji)%format) == 1) THEN
             WRITE (cl_work,int_table(ji)%format) int_table(ji)%iVal
          ELSE 
             WRITE (cl_work,int_table(ji)%format) float (int_table(ji)%iVal)
          ENDIF
       ENDIF
    ENDDO

    ! scan real_table for matching cd_In
    IF ( .NOT. llfound ) THEN
       DO ji=1, NUM_REAL_VALS
          IF (TRIM(cd_in) == real_table(ji)%id ) THEN
             llfound = .TRUE.
             IF (IDformat (real_table(ji)%format) == 1) THEN
                WRITE (cl_work,real_table(ji)%format) INT(real_table(ji)%rVal)
             ELSE 
                WRITE (cl_work,real_table(ji)%format) real_table(ji)%rVal
             ENDIF
          ENDIF
       ENDDO
    ENDIF

    ! scan string_table for matching cd_In	
    IF ( .NOT. llfound ) THEN
       DO ji=1, NUM_STR_VALS
          IF (TRIM(cd_In) == string_table(ji)%id) THEN
             llfound = .TRUE.

             WRITE (cl_work,'(A)') string_table(ji)%cstr
          ENDIF
       ENDDO
    ENDIF

    ! if no matching is found in known table, quote the faulty expression between $
    IF ( .NOT. llfound ) THEN
       WRITE (cl_work,100) TRIM( cd_In )
100    FORMAT('$',a,'$')
    ENDIF

    !     enleve les caracteres d'espacement au debut et a la fin

    cd_Out = ADJUSTL ( cl_work )

  END SUBROUTINE GetNumValue


  INTEGER FUNCTION IDformat (cdFmt)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION IDformat  ***
    !!
    !! ** Purpose :  identify if the format os for Integer or Real
    !!
    !! ** Method  :    return 1 for integer, 0 for real. This function is useful
    !!              for writing a real with integer format
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=10), INTENT(in) :: cdFmt

    INTEGER(KIND=4)   :: fmtType
    !!----------------------------------------------------------------------
    IF ( SCAN( cdFmt,'Ii'   ) /= 0 ) fmtType = 1
    IF ( SCAN( cdFmt,'FfeE' ) /= 0 ) fmtType = 0

    IDformat = fmtType

  END FUNCTION IDformat


  SUBROUTINE SetFormatString (cdVal, cdFormat)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetFormatString  ***
    !!
    !! ** Purpose :  Set the %format in a text structure according to 
    !!              the value passed as argument 
    !!
    !! ** Method  :  It change the format associated to a value 
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdVal
    CHARACTER(LEN=*), INTENT(in) :: cdFormat

    INTEGER(KIND=4)  :: ji
    LOGICAL          :: llfound
    !!----------------------------------------------------------------------
    llfound = .FALSE.

    IF (.NOT. llfound ) THEN
       DO ji=1, NUM_INT_VALS
          IF (TRIM(cdVal) == int_table(ji)%id) THEN
             llfound = .TRUE.
             int_table(ji)%format = '('//TRIM(cdFormat)//')'
          ENDIF
       ENDDO
    ENDIF

    IF (.NOT. llfound ) THEN
       DO ji=1, NUM_REAL_VALS
          IF (TRIM(cdVal) == real_table(ji)%id) THEN
             llfound = .TRUE.
             real_table(ji)%format = '('//TRIM(cdFormat)//')'
          ENDIF
       ENDDO
    ENDIF

    IF (.NOT. llfound ) THEN
       PRINT *,'Unknown field : ',cdVal
    ENDIF

  END SUBROUTINE SetFormatString


  SUBROUTINE SplitValFmt (cdValFmt, cdValue, cdFormat)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SplitValFmt  ***
    !!
    !! ** Purpose :  Interpret the option -format VALUE FORMAT  
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),  INTENT(in) :: cdValFmt
    CHARACTER(LEN=*), INTENT(out) :: cdValue
    CHARACTER(LEN=*), INTENT(out) :: cdFormat
    
    INTEGER(KIND=4)       :: ib
    !!----------------------------------------------------------------------
    ib       = index (cdValFmt,' ') 
    cdValue  = TRIM(ADJUSTL(cdValFmt(1:ib) ) ) 
    cdFormat = TRIM(ADJUSTL(cdValFmt(ib:) ) ) 

  END SUBROUTINE SplitValFmt


  SUBROUTINE SetValTable (bdimgclr, bdimgcnt, bdimgvec)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetValTable  ***
    !!
    !! ** Purpose :  Initialise the table value associated to the 3 data
    !!              structures given as argumment 
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(in) :: bdimgclr
    TYPE( bimgfile ), INTENT(in) :: bdimgcnt
    TYPE( bimgfile ), INTENT(in) :: bdimgvec

    int_table (ICLR_NZ)%iVal = bdimgclr%nzfile
    int_table (ICNT_NZ)%iVal = bdimgcnt%nzfile
    int_table (IVEC_NZ)%iVal = bdimgvec%nzfile

    real_table(ICLR_SPVAL)%rVal = bdimgclr%spval
    real_table(ICLR_DX   )%rVal = bdimgclr%dx
    real_table(ICLR_DY   )%rVal = bdimgclr%dy

    real_table(ICNT_SPVAL)%rVal = bdimgcnt%spval
    real_table(ICNT_DX   )%rVal = bdimgcnt%dx
    real_table(ICNT_DY   )%rVal = bdimgcnt%dy

    real_table(IVEC_SPVAL)%rVal = bdimgvec%spval
    real_table(IVEC_DX   )%rVal = bdimgvec%dx
    real_table(IVEC_DY   )%rVal = bdimgvec%dy


    string_table(ICLR_STR1)%cstr = bdimgclr%cstr1
    string_table(ICLR_STR2)%cstr = bdimgclr%cstr2
    string_table(ICLR_STR3)%cstr = bdimgclr%cstr3
    string_table(ICLR_STR4)%cstr = bdimgclr%cstr4

    string_table(ICNT_STR1)%cstr = bdimgcnt%cstr1
    string_table(ICNT_STR2)%cstr = bdimgcnt%cstr2
    string_table(ICNT_STR3)%cstr = bdimgcnt%cstr3
    string_table(ICNT_STR4)%cstr = bdimgcnt%cstr4

    string_table(IVEC_STR1)%cstr = bdimgvec%cstr1
    string_table(IVEC_STR2)%cstr = bdimgvec%cstr2
    string_table(IVEC_STR3)%cstr = bdimgvec%cstr3
    string_table(IVEC_STR4)%cstr = bdimgvec%cstr4

  END SUBROUTINE SetValTable


  SUBROUTINE UpdateValTable (ktstp, kclr_lev, kcnt_lev, kvec_lev, bdimgclr, bdimgcnt, bdimgvec)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE UpdateValTable  ***
    !!
    !! ** Purpose :   Update the tables with dynalically veolving values
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4),  INTENT(in) :: ktstp
    INTEGER(KIND=4),  INTENT(in) :: kclr_lev
    INTEGER(KIND=4),  INTENT(in) :: kcnt_lev
    INTEGER(KIND=4),  INTENT(in) :: kvec_lev
    TYPE( bimgfile ), INTENT(in) :: bdimgclr
    TYPE( bimgfile ), INTENT(in) :: bdimgcnt
    TYPE( bimgfile ), INTENT(in) :: bdimgvec
    !!----------------------------------------------------------------------
    int_table (ICOUCHE)%iVal  = kclr_lev
    int_table (ITSTEP )%iVal  = ktstp
    
    ! color data structure
    int_table (ICLR_NX)%iVal  = bdimgclr%nxdata
    int_table (ICLR_NY)%iVal  = bdimgclr%nydata

    real_table(ICLR_TIME   )%rVal = bdimgclr%time
    real_table(ICLR_TIMEDAY)%rVal = bdimgclr%time/86400.
    real_table(ICLR_DEPTH  )%rVal = ABS(bdimgclr%depth(kclr_lev))

    ! contour data stucture
    int_table (ICNT_NX)%iVal   = bdimgcnt%nxdata
    int_table (ICNT_NY)%iVal   = bdimgcnt%nydata
    real_table(ICNT_TIME)%rVal = bdimgcnt%time
    IF (ALLOCATED (bdimgcnt%depth) ) real_table(ICNT_DEPTH)%rVal = ABS(bdimgcnt%depth(kcnt_lev))

    ! vecor data structure
    int_table (IVEC_NX)%iVal   = bdimgvec%nxdata
    int_table (IVEC_NY)%iVal   = bdimgvec%nydata
    real_table(IVEC_TIME)%rVal = bdimgvec%time
    IF (ALLOCATED (bdimgvec%depth) ) real_table(IVEC_DEPTH)%rVal = ABS(bdimgvec%depth(kvec_lev))

    IF (opt_dep == 1) THEN
       real_table(ICLR_DEPTH)%rVal = ABS(req_dep)
       real_table(ICNT_DEPTH)%rVal = ABS(req_dep)
       real_table(IVEC_DEPTH)%rVal = ABS(req_dep)
    ENDIF

  END SUBROUTINE UpdateValTable

  ! ---------------------------------------------------------------------
  ! 
  ! Nom         :  SetClrMin, SetClrMax
  ! 
  ! Parametres  :  vmin
  ! 
  ! Description :  Met la valeur fournie en argument dans la table
  !                des valeurs affichables
  ! 
  ! ---------------------------------------------------------------------

  SUBROUTINE SetClrMin (pvmin)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetClrMin SetClrMax ***
    !!                  ***  ROUTINE SetVecMin SetVecMax ***
    !!                  ***  ROUTINE SetCntMin SetCntMax ***
    !!
    !! ** Purpose :  Fill the ICLR_MIN%rval value of the table 
    !!            :  Fill the ICLR_MAX%rval value of the table 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmin
    !!----------------------------------------------------------------------
    real_table(ICLR_MIN)%rVal = pvmin

  END SUBROUTINE SetClrMin

  SUBROUTINE SetClrMax (pvmax)
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmax
    !!----------------------------------------------------------------------
    real_table(ICLR_MAX)%rVal = pvmax

  END SUBROUTINE SetClrMax

  SUBROUTINE SetVecMin (pvmin)
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmin
    !!----------------------------------------------------------------------
    real_table(IVEC_MIN)%rVal = pvmin

  END SUBROUTINE SetVecMin

  SUBROUTINE SetVecMax (pvmax)
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmax
    !!----------------------------------------------------------------------
    real_table(IVEC_MAX)%rVal = pvmax

  END SUBROUTINE SetVecMax

  SUBROUTINE SetCntMin (pvmin)
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmin
    !!----------------------------------------------------------------------
    real_table(ICNT_MIN)%rVal = pvmin
  END SUBROUTINE SetCntMin

  SUBROUTINE SetCntMax (pvmax)
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pvmax
    !!----------------------------------------------------------------------
    real_table(ICNT_MAX)%rVal = pvmax

  END SUBROUTINE SetCntMax


  SUBROUTINE SetCntExp (pexp)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetCntExp SetClrExp  ***
    !!
    !! ** Purpose :   Put exp in INCT_EXP table index
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: pexp
    !!----------------------------------------------------------------------
    int_table(ICNT_EXP)%iVal = pexp

  END SUBROUTINE SetCntExp

  SUBROUTINE SetClrExp (pexp)
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: pexp
    !!----------------------------------------------------------------------

    int_table(ICLR_EXP)%iVal = pexp

  END SUBROUTINE SetClrExp


  SUBROUTINE SetClrCNESday (pday)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetClrCNESday  ***
    !!
    !! ** Purpose :  Fill the ICDATE field of the table with the calendar day
    !!               corresponding to the pday in jcnes given as argument
    !!               
    !!
    !! ** Method  :   use ConvCNES to transform jcnes in calendar date
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), INTENT(in) :: pday
    CHARACTER(LEN=17) :: cl_jcnes
    INTEGER(KIND=4)   :: ji
    !!----------------------------------------------------------------------

    CALL ConvCNES (NINT(pday),cl_jcnes)

    string_table(CLR_ICDATE)%cstr = ' '
    string_table(CNT_ICDATE)%cstr = ' '
    string_table(VEC_ICDATE)%cstr = ' '

    string_table(CLR_ICDATE)%cstr = cl_jcnes
    string_table(CNT_ICDATE)%cstr = cl_jcnes
    string_table(VEC_ICDATE)%cstr = cl_jcnes

  END SUBROUTINE SetClrCNESday

  ! ---------------------------------------------------------------------
  ! ---------------------------------------------------------------------
  ! 
  ! Nom         :  SetCLIPPERday
  ! 
  ! Parametres  :  day - jour cray
  ! 
  ! Description :  cette fonction calcule le jour CNES a partir d'un 
  !                nombre et l'exprime sous forme de chaine de caracteres
  ! 
  ! ---------------------------------------------------------------------

  SUBROUTINE SetCLIPPERday (day)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE SetCLIPPERday  ***
    !!
    !! ** Purpose :  Fill the CLIPPER_DATE field of the tables 
    !!
    !! ** Method  :   
    !!
    !! References :  
    !!----------------------------------------------------------------------

    REAL(KIND=4)       :: day
    CHARACTER(LEN=10)  :: clDay 
    INTEGER(KIND=4)    :: ji

    clDay = ClipperDat (NINT(day))

    DO ji=1,80
       string_table(CLIPPER_DATE)%cstr(ji:ji) = ' '
    ENDDO

    string_table(CLIPPER_DATE)%cstr(1:17) = clDay

  END SUBROUTINE SetCLIPPERday


  SUBROUTINE ConvCNES(kjcnes, cd_date)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ConvCNES  ***
    !!
    !! ** Purpose :  Convert a jcnes in calendar date 
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4),    INTENT(in) :: kjcnes
    CHARACTER(LEN=17), INTENT(out) :: cd_date

    INTEGER(KIND=4)            :: idayspm(12)   ! number of days per month
    CHARACTER(LEN=9)           :: cl_month_fr(12)
    CHARACTER(LEN=9)           :: cl_month_en(12)
    INTEGER(KIND=4), PARAMETER :: jp_Jan01_1950 = 2433283  ! origin of day CNES

    INTEGER(KIND=4)         :: ijul, iyr_c, imon_c, iday_c
    !!----------------------------------------------------------------------
    DATA idayspm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
    DATA cl_month_fr/'  JANVIER','  FEVRIER','     MARS','    AVRIL', &
      &              '      MAI','     JUIN','  JUILLET','     AOUT', &
      &              'SEPTEMBRE', ' OCTOBRE',' NOVEMBRE',' DECEMBRE'/ 

    DATA cl_month_en/'  January',' February','    March','    April', &
      &              '      May','     June','     July','   August', &
      &              'September', ' October',' November',' December'/ 
    
    ijul = kjcnes + jp_Jan01_1950
    CALL CalDat ( ijul, imon_c, iday_c, iyr_c )

    IF (opt_english == 1 ) THEN
      WRITE(cd_date,67) cl_month_en(imon_c), iday_c, iyr_c
    ELSE
      WRITE(cd_date,66) iday_c, cl_month_fr(imon_c), iyr_c
    ENDIF

66  FORMAT(i2,1x,a9,1x,i4)
67  FORMAT(a9,1x,i2,1x,i4)

  END SUBROUTINE ConvCNES

  CHARACTER(LEN=10) FUNCTION ClipperDat(kjclip)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION ClipperDat  ***
    !!
    !! ** Purpose :  This function takes a date as clipper adatrj number 
    !!              and return the date in calendar format (ndastp style )
    !!
    !! ** Method  : The various off-set used in this tricky routine are 
    !!              linked to old CLIPPER stories ... Do not use this 
    !!              function if you do not know about CLIPPER !
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: kjclip
    
    INTEGER(KIND=4)            :: iday_c, imon_c, iyr_c
    INTEGER(KIND=4)            :: idayj
    INTEGER(KIND=4), PARAMETER :: jp_Feb15_1950 = 2433328  ! julian day
    INTEGER(KIND=4), PARAMETER :: jp_Feb15_1958 = 2436250  ! julian day
    INTEGER(KIND=4), PARAMETER :: jp_Feb15_1971 = 2440998  ! julian day
    !!----------------------------------------------------------------------
    idayj= kjclip + jp_Feb15_1950  ! 

    IF ( idayj >  jp_Feb15_1958 )   idayj = kjclip + jp_Feb15_1971
    CALL CalDat (idayj, imon_c, iday_c, iyr_c)

    WRITE(ClipperDat,'(i4.4,1h/,i2.2,1h/,i2.2)') iyr_c,imon_c,iday_c

  END FUNCTION ClipperDat


  SUBROUTINE CalDat(kjulian, kmm, kid, kiyyy)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CalDat  ***
    !!
    !! ** Purpose : This routine convert a julian day in calendar date.
    !!              It is the inverse of the function julday.
    !!
    !! ** Method  : This routine comes directly from the Numerical Recipe Book.
    !!
    !! References :  Press et al., numerical recipes, cambridge univ. press, 1986
    !!----------------------------------------------------------------------
    INTEGER(KIND=4),  INTENT(in) ::  kjulian
    INTEGER(KIND=4), INTENT(out) ::  kmm
    INTEGER(KIND=4), INTENT(out) ::  kid
    INTEGER(KIND=4), INTENT(out) ::  kiyyy

    INTEGER(KIND=4)             ::  ia, ialpha, ib, ic, id, ie
    INTEGER(KIND=4), PARAMETER  ::  jpgreg = 2299161  ! October 15, 1582
    !!----------------------------------------------------------------------
    ! ... Cross over to Gregorian Calendar produces this correction:
    IF ( kjulian >= jpgreg) THEN
       ialpha = INT ((( kjulian - 1867216) - 0.25)/36524.25 )
       ia     = kjulian +1 + ialpha -INT (0.25*ialpha)
    ELSE
       ia = kjulian
    END IF
    !
    ib = ia + 1524
    ic = INT (6680. + (( ib -2439870) - 122.1)/365.25 )
    id = 365* ic + INT (0.25*ic)
    ie = INT (( ib - id )/30.6001)
    !
    kid = ib - id - INT (30.6001*ie)
    kmm = ie -1
    IF ( kmm > 12 ) kmm = kmm - 12
    kiyyy = ic - 4715
    IF ( kmm     >  2 ) kiyyy = kiyyy - 1
    IF ( kiyyy  <=  0 ) kiyyy = kiyyy - 1

  END SUBROUTINE CalDat

END MODULE val_table
