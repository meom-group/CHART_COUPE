MODULE modvaltable
  !!======================================================================
  !!                     ***  MODULE  modvaltable  ***
  !! Define structure type linked with all text in chart, an in particular
  !! the values of what can be inserted between $ $
  !!=====================================================================
  !! History : 1.0  : 06/1993  :  E. Brown     : Original code
  !!           7.0  : 12/2010  :  J.M. Molines : F90 and Doctor
  !!----------------------------------------------------------------------
  !! Type defined :
  !!    INTEGER_TABLE_STRUCT
  !!    REAL_TABLE_STRUCT
  !!    STRING_TABLE_STRUCT
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE
  
  PUBLIC

  INTEGER(KIND=4), PARAMETER :: NUM_INT_VALS  = 18   !:
  INTEGER(KIND=4), PARAMETER :: NUM_REAL_VALS = 22   !:
  INTEGER(KIND=4), PARAMETER :: NUM_STR_VALS  = 17   !:

  TYPE  INTEGER_TABLE_STRUCT
     SEQUENCE
     CHARACTER(LEN=16) :: id        !# string id
     CHARACTER(LEN=32) :: FORMAT    !# format 
     INTEGER(KIND=4)   :: iVal      !# integer value
  END TYPE  INTEGER_TABLE_STRUCT

  TYPE(INTEGER_TABLE_STRUCT)  :: int_table(NUM_INT_VALS)     !:

  TYPE  REAL_TABLE_STRUCT
     SEQUENCE
     CHARACTER(LEN=16) :: id        !# string id
     CHARACTER(LEN=32) :: FORMAT    !# format
     REAL(KIND=4)      :: rVal      !# real value
  END TYPE  REAL_TABLE_STRUCT

  TYPE( REAL_TABLE_STRUCT )   :: real_table(NUM_REAL_VALS)   !:

  TYPE  STRING_TABLE_STRUCT
     SEQUENCE
     CHARACTER(LEN=16) :: id        !# string id
     CHARACTER(LEN=32) :: FORMAT    !# format
     CHARACTER(LEN=80) :: cstr      !# character value
  END TYPE  STRING_TABLE_STRUCT

  TYPE( STRING_TABLE_STRUCT ) :: string_table(NUM_STR_VALS)  !:

  INTEGER(KIND=4), PARAMETER :: ICOUCHE       = 1   !: int_table  indexes
  INTEGER(KIND=4), PARAMETER :: ITSTEP        = 2 
  INTEGER(KIND=4), PARAMETER :: ICLR_NX       = 3
  INTEGER(KIND=4), PARAMETER :: ICLR_NY       = 4
  INTEGER(KIND=4), PARAMETER :: ICLR_NZ       = 5
  INTEGER(KIND=4), PARAMETER :: ICLR_XAXIS    = 6
  INTEGER(KIND=4), PARAMETER :: ICLR_YAXIS    = 7
  INTEGER(KIND=4), PARAMETER :: ICLR_ZAXIS    = 8
  INTEGER(KIND=4), PARAMETER :: ICLR_PAL      = 9
  INTEGER(KIND=4), PARAMETER :: ICNT_NX       = 10
  INTEGER(KIND=4), PARAMETER :: ICNT_NY       = 11
  INTEGER(KIND=4), PARAMETER :: ICNT_NZ       = 12
  INTEGER(KIND=4), PARAMETER :: IVEC_NX       = 13
  INTEGER(KIND=4), PARAMETER :: IVEC_NY       = 14
  INTEGER(KIND=4), PARAMETER :: IVEC_NZ       = 15
  INTEGER(KIND=4), PARAMETER :: ICLR_EXP      = 16
  INTEGER(KIND=4), PARAMETER :: ICNT_EXP      = 17
  INTEGER(KIND=4), PARAMETER :: IVEC_EXP      = 18

  INTEGER(KIND=4), PARAMETER :: ICLR_SPVAL    = 1   !: real_table indexes
  INTEGER(KIND=4), PARAMETER :: ICLR_DX       = 2
  INTEGER(KIND=4), PARAMETER :: ICLR_DY       = 3
  INTEGER(KIND=4), PARAMETER :: ICLR_TIME     = 4 
  INTEGER(KIND=4), PARAMETER :: ICLR_MIN      = 5
  INTEGER(KIND=4), PARAMETER :: ICLR_MAX      = 6
  INTEGER(KIND=4), PARAMETER :: ICLR_DEPTH    = 7
  INTEGER(KIND=4), PARAMETER :: ICNT_SPVAL    = 8
  INTEGER(KIND=4), PARAMETER :: ICNT_DX       = 9
  INTEGER(KIND=4), PARAMETER :: ICNT_DY       = 10
  INTEGER(KIND=4), PARAMETER :: ICNT_TIME     = 11 
  INTEGER(KIND=4), PARAMETER :: ICNT_MIN      = 12
  INTEGER(KIND=4), PARAMETER :: ICNT_MAX      = 13
  INTEGER(KIND=4), PARAMETER :: ICNT_DEPTH    = 14
  INTEGER(KIND=4), PARAMETER :: IVEC_SPVAL    = 15
  INTEGER(KIND=4), PARAMETER :: IVEC_DX       = 16
  INTEGER(KIND=4), PARAMETER :: IVEC_DY       = 17
  INTEGER(KIND=4), PARAMETER :: IVEC_TIME     = 18 
  INTEGER(KIND=4), PARAMETER :: IVEC_MIN      = 19
  INTEGER(KIND=4), PARAMETER :: IVEC_MAX      = 20
  INTEGER(KIND=4), PARAMETER :: IVEC_DEPTH    = 21
  INTEGER(KIND=4), PARAMETER :: ICLR_TIMEDAY  = 22 

  INTEGER(KIND=4), PARAMETER :: ICLR_STR1     = 1  !: character table indexes
  INTEGER(KIND=4), PARAMETER :: ICLR_STR2     = 2
  INTEGER(KIND=4), PARAMETER :: ICLR_STR3     = 3
  INTEGER(KIND=4), PARAMETER :: ICLR_STR4     = 4
  INTEGER(KIND=4), PARAMETER :: ICNT_STR1     = 5
  INTEGER(KIND=4), PARAMETER :: ICNT_STR2     = 6
  INTEGER(KIND=4), PARAMETER :: ICNT_STR3     = 7
  INTEGER(KIND=4), PARAMETER :: ICNT_STR4     = 8
  INTEGER(KIND=4), PARAMETER :: IVEC_STR1     = 9
  INTEGER(KIND=4), PARAMETER :: IVEC_STR2     = 10
  INTEGER(KIND=4), PARAMETER :: IVEC_STR3     = 11
  INTEGER(KIND=4), PARAMETER :: IVEC_STR4     = 12
  INTEGER(KIND=4), PARAMETER :: I_DATE        = 13
  INTEGER(KIND=4), PARAMETER :: CLR_ICDATE    = 14
  INTEGER(KIND=4), PARAMETER :: CNT_ICDATE    = 15
  INTEGER(KIND=4), PARAMETER :: VEC_ICDATE    = 16
  INTEGER(KIND=4), PARAMETER :: CLIPPER_DATE  = 17
  !!----------------------------------------------------------------------

END MODULE modvaltable
