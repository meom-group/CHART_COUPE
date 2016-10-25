PROGRAM vecrot
  !!======================================================================
  !!                     ***  PROGRAM vecrot  ***
  !! Used as external program in coupe, this program perform a rotation
  !! of an opa vector field giving the tangent and normal velocity.
  !! this works only with bimg files
  !!=====================================================================
  !! History :  1.0  !  1995    ! Jean-Marc Molines  : Original code
  !!                 !  02/2007 : J.M. Molines       :  F90
  !!            7.0  !  12/2010 ! J.M. Molines       : Full Doctor norm
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4)                           :: ji, jj, jk
  INTEGER(KIND=4)                           :: narg, ilu=10, iluo=11
  INTEGER(KIND=4)                           :: ni, nj, nk, ndim, nt, icod
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: u, v, urot, vrot
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE   :: h1d
  REAL(KIND=4)                              :: pi, angle, alfa, angled
  REAL(KIND=4)                              :: x1, y1, dx, dy, spval
  REAL(KIND=4)                              :: time_tag1
  CHARACTER(LEN=80)                         :: cline1, cline2, cline3, cline4
  CHARACTER(LEN=80)                         :: cf_filuv, cdum
  !!----------------------------------------------------------------------
  pi   = ACOS(-1.)
  narg = iargc()

  IF (narg < 2) THEN
     PRINT *,'USAGE: vecrot <angle>  vecxyfile'
     PRINT *,' ie sur vecrot.bimg>'
     STOP
  ENDIF
  !
  CALL getarg(1,cdum    )   ;   READ(cdum,*)angled
  angle = angled*pi/180.
  alfa  = angle-pi/2.

  CALL getarg(2,cf_filuv)

  OPEN(ilu,file=cf_filuv,form='unformatted')
120 FORMAT(a,i3.3)

  OPEN(iluo,file='vecrot.bimg',form='unformatted')
  READ(ilu) cline1
  READ(ilu) cline2
  READ(ilu) cline3
  READ(ilu) cline4

  WRITE(cline4,120)'Rotation of axes  ',angled
  WRITE(iluo) cline1
  WRITE(iluo) cline2
  WRITE(iluo) cline3
  WRITE(iluo) cline4
  !
  !
  READ(ilu) ni, nj, nk, nt, ndim, icod
  ALLOCATE (u(ni,nj), v(ni,nj), urot(ni,nj), vrot(ni,nj) )
  ALLOCATE (h1d(nk)                                      )

  READ(ilu) x1, y1, dx, dy, spval
  WRITE(iluo) ni, nj, nk, nt, ndim, icod
  WRITE(iluo) x1, y1, dx, dy, spval
  !
  !
  READ(ilu) (h1d(jk),jk=1,nk)
  READ(ilu) time_tag1
  WRITE(iluo) (h1d(jk),jk=1,nk)
  WRITE(iluo) time_tag1

  DO jk=1,nk
     READ(ilu)((u(ji,jj),ji=1,ni),jj=1,nj)
     READ(ilu)((v(ji,jj),ji=1,ni),jj=1,nj)

     WHERE ( u /= spval .AND. v /= spval ) 
       urot = u * COS(alfa) - v * SIN(alfa)
       vrot = u * SIN(alfa) + v * COS(alfa)
     ELSE WHERE
       urot = spval
       vrot = spval
     END WHERE

     WRITE(iluo)((urot(ji,jj),ji=1,ni),jj=1,nj)
     WRITE(iluo)((vrot(ji,jj),ji=1,ni),jj=1,nj)
  ENDDO

  CLOSE(ilu)
  CLOSE(iluo)

END PROGRAM vecrot
