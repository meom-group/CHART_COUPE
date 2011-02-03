MODULE modmapncar
  !!======================================================================
  !!                     ***  MODULE  modmapncar  ***
  !! Define the variables used for map initialization
  !!=====================================================================
  !! History : 1.0   !  06/1993  E. Brown     : Original code
  !!         : 7.0   !  11/2010  J.M. Molines : F90 + Dr norme
  !!----------------------------------------------------------------------
  PUBLIC 

  INTEGER(KIND=4), PARAMETER :: jp_iama = 25000000  !: size of area map integer worspace
!INTEGER(KIND=4), PARAMETER :: jp_iama = 250000000  !: size of area map integer worspace
  INTEGER(KIND=4), PARAMETER :: jp_cra =  10000000  !: size of area map real workspace
!INTEGER(KIND=4), PARAMETER :: jp_cra =  10000000  !: size of area map real workspace
  INTEGER(KIND=4), PARAMETER :: jp_agid =  10      !: max number of area/group id

  INTEGER(KIND=4),DIMENSION(jp_iama) :: niama      !: area map integer workspace
  INTEGER(KIND=4),DIMENSION(jp_agid) :: niaia      !: area identifiers
  INTEGER(KIND=4),DIMENSION(jp_agid) :: nigia      !: group identifiers
  REAL(KIND=4)   ,DIMENSION(jp_cra)  :: xcra       !: Area map workspace for X-coord
  REAL(KIND=4)   ,DIMENSION(jp_cra)  :: ycra       !: Area map worksapce for Y-coord

  INTEGER(KIND=4), PARAMETER :: jp_IWRK=190000     !: size of integer contour workspace
  INTEGER(KIND=4), PARAMETER :: jp_RWRK=190000     !: size of real contour workspace

  INTEGER(KIND=4), DIMENSION(jp_IWRK) :: niwrk     !: integer contour workspace
  REAL(KIND=4),    DIMENSION(jp_IWRK) :: rwrk      !: real contour workspace

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

END MODULE modmapncar
