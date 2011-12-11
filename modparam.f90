MODULE modparam
  !!======================================================================
  !!                     ***  MODULE  modparam  ***
  !! Define the size parameters for the data and plot area
  !!=====================================================================
  !! History : 1.O  : 06/1993  : E. Brown  : Original code
  !!         : 7.0  : 11/2010  : J.M. Molines : F90 
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
      ! ORCA12
      INTEGER, PARAMETER :: NXX     = 4322   !: X size for the data and plot
      INTEGER, PARAMETER :: NYY     = 3059   !: Y size for the data and plot
      INTEGER, PARAMETER :: NA      = 75     !: Z size for the data
      INTEGER, PARAMETER :: Nmaxtime= 700    !: Maximum number of time step in data file

      ! ORCA025
!     INTEGER, PARAMETER :: NXX     = 1500   !: X size for the data and plot
!     INTEGER, PARAMETER :: NYY     = 1600   !: Y size for the data and plot
!     INTEGER, PARAMETER :: NA      = 100    !: Z size for the data
!     INTEGER, PARAMETER :: Nmaxtime= 700    !: Maximum number of time step in data file

      ! ORCA2
!     INTEGER, PARAMETER :: NXX     = 500    !: X size for the data and plot
!     INTEGER, PARAMETER :: NYY     = 550    !: Y size for the data and plot
!     INTEGER, PARAMETER :: NA      = 46     !: Z size for the data
!     INTEGER, PARAMETER :: Nmaxtime= 700    !: Maximum number of time step in data file

      ! PERIANT8
!     INTEGER, PARAMETER :: NXX     = 2882   !: X size for the data and plot
!     INTEGER, PARAMETER :: NYY     = 799    !: Y size for the data and plot
!     INTEGER, PARAMETER :: NA      = 200    !: Z size for the data
!     INTEGER, PARAMETER :: Nmaxtime= 700    !: Maximum number of time step in data file


      INTEGER, PARAMETER :: jp_txt=50 !: max number of -string '...'  options
      INTEGER, PARAMETER :: NOVERPTS=500000 !: Max number of overlay points
      INTEGER, PARAMETER :: NCLRMARK=256 !: max number of colors in clrmark

      REAL(KIND=4), PARAMETER :: rp_defspval=999999999. !: default spval

      ! internal buffer for gflas  (jp_ib_xxxx )
      INTEGER(KIND=4), PARAMETER :: jp_ib_ocean    = 1
      INTEGER(KIND=4), PARAMETER :: jp_ib_color    = 2
      INTEGER(KIND=4), PARAMETER :: jp_ib_contours = 3
      INTEGER(KIND=4), PARAMETER :: jp_ib_meridian = 4
      INTEGER(KIND=4), PARAMETER :: jp_ib_map      = 5
      INTEGER(KIND=4), PARAMETER :: jp_ib_palette  = 6
      INTEGER(KIND=4), PARAMETER :: jp_ib_vectors  = 7
      INTEGER(KIND=4), PARAMETER :: jp_ib_bathy    = 8   ! bathy in coupe
      INTEGER(KIND=4), PARAMETER :: jp_ib_overdata = 55
      INTEGER(KIND=4), PARAMETER :: jp_ib_overmark = 56
      INTEGER(KIND=4), PARAMETER :: jp_ib_showgrid = 57
      INTEGER(KIND=4), PARAMETER :: jp_ib_sigmalev = 58  ! sigma levels in coupe

      ! dimension of  cntlim
      INTEGER(KIND=4), PARAMETER :: jp_cntlim = 200      ! max number of limits in cntlim

      REAL(KIND=4), PARAMETER    :: rp_out_range = 1.e12 ! Out of Range value for conpack
END MODULE modparam
