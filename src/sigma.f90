MODULE sigma
  !!======================================================================
  !!                     ***  MODULE  sigma  ***
  !! PlotSigma level, for overlaying sigma coordinate level on the z-plot
  !!=====================================================================
  !! History : 2.0  !  05/1994    J.M. Molines  : Original code (DYNAMO)
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   PlotSigmaLevel
  !!----------------------------------------------------------------------
  USE modcom
  IMPLICIT NONE

  PRIVATE

  PUBLIC ::  PlotSigmaLevel

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS
  ! cpmpxy has been remove from this module because it is required as a single object (not in module)
  ! in order to properly link with NCL F77 library :(

  SUBROUTINE PlotSigmaLevel( ksiglev )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE PlotSigmaLevel  ***
    !!
    !! ** Purpose :   Draw sigma level on the section
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: ksiglev

    INTEGER(KIND=4)                         :: jk, ji
    INTEGER(KIND=4)                         :: ilog 
    REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: zxtmp, zytmp ! NXX
    REAL(KIND=4)                            :: zul, zur, zub, zut
    REAL(KIND=4)                            :: zwl, zwr, zwb, zwt
    !!----------------------------------------------------------------------
    ALLOCATE ( zxtmp(NXX), zytmp(NXX) )

    CALL getset(zul, zur, zub, zut, zwl, zwr, zwb, zwt, ilog)
    CALL set (zul, zur, zub, zut, float(1), float(NXX), prof_max, prof_min, ilog)

    zxtmp(:) = (/ (float(ji), ji=1,NXX) /)

    DO jk=1,ksiglev
       zytmp(:) = -rzs (:,jk)
       CALL curve(zxtmp, zytmp, NXX)
    ENDDO

    CALL set (zul, zur, zub, zut, zwl, zwr, zwb, zwt, ilog)

    DEALLOCATE ( zxtmp, zytmp )

  END SUBROUTINE PlotSigmaLevel

END MODULE sigma
