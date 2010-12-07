MODULE modcolor
  !!======================================================================
  !!                     ***  MODULE  modcolor  ***
  !!  define parameters and some variables used with colors
  !!=====================================================================
  !! History : 1.0  :    1993  : E. Brown     : Original code
  !!         : 7.0  : 11/2010  : J.M. Molines : F90 and Doctor norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  INTEGER(KIND=4), PARAMETER :: NBOXMAX               = 256
  INTEGER(KIND=4), PARAMETER :: COLOR_NRES            = 20
  INTEGER(KIND=4), PARAMETER :: COLOR_BACKGROUND      = 0
  INTEGER(KIND=4), PARAMETER :: COLOR_FOREGROUND      = 1
  INTEGER(KIND=4), PARAMETER :: COLOR_CONTINENT       = 2
  INTEGER(KIND=4), PARAMETER :: COLOR_SPVAL           = 3
  INTEGER(KIND=4), PARAMETER :: COLOR_OCEAN           = 4
  INTEGER(KIND=4), PARAMETER :: COLOR_ISOCONTOUR      = 5
  INTEGER(KIND=4), PARAMETER :: COLOR_VECTOR          = 6
  INTEGER(KIND=4), PARAMETER :: COLOR_CONTINENT_PERIM = 7

  ! color limit  and method
  INTEGER(KIND=4) :: cl_met, cl_exp
  REAL(KIND=4)    :: cl_max, cl_min
  !!----------------------------------------------------------------------

END MODULE modcolor
     
