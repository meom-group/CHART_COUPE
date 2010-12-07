!!======================================================================
!!                     ***  COLLECTION of routines  ***
!!  This is a file where all NCAR user modified routines are
!! It cannot be in a module ( ??) because the linker needs to find them
!! before the standard version proposed in libncarg
!!=====================================================================
!! History : 1.0  ! 06/1993  ! E. Brown     : original codes
!!           7.0  ! 11/2010  ! J.M. Molines : F90, Doctor norm, gathered
!!                                            in a single file
!!----------------------------------------------------------------------

!!----------------------------------------------------------------------
!!   cpmpxy  :  replace standard CPMPXY routine
!!   cpchil  :  internally called by conpack routines
!!   vvumxy  :  internally called by  VVMPXY
!!----------------------------------------------------------------------

!!----------------------------------------------------------------------
!! CHART/COUPE 7.0 , MEOM 2010
!! $Id$
!! Copyright (c) 2010, J.-M. Molines.
!! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
!!----------------------------------------------------------------------

SUBROUTINE cpmpxy(kimap, px, py, pfx, pfy)
  !!---------------------------------------------------------------------
  !!                  ***  ROUTINE cpmpxy  ***
  !!
  !! ** Purpose :  This is a replacement routine for NCL standard routine 
  !!         it ensures a particular mapping for special grid used in chart
  !!
  !! ** Method  :  This routine must not be in a module :( At link time it
  !!        must be found before the default NCL routine in libncarg 
  !!
  !! References :  
  !!----------------------------------------------------------------------
  USE modcom

  IMPLICIT NONE

  INTEGER(KIND=4),  INTENT(in) :: kimap
  REAL(KIND=4),     INTENT(in) :: px, py 
  REAL(KIND=4),    INTENT(out) :: pfx, pfy 

  INTEGER(KIND=4) :: i1, i2, j1, j2  ! integer position of the current point
  REAL(KIND=4)    :: zx, zy          ! fractional position refered to (i1,j1)
  REAL(KIND=4)    :: zlat, zlon      ! position of point in world unit (chart)
  REAL(KIND=4)    :: zdep            ! depth of point in world unit (coupe)
  !!----------------------------------------------------------------------
  SELECT CASE ( kimap )
  CASE ( 0 )
   !  the object of the call is to ask CPMPXY about its mapping capabilities
  CASE ( 1 ) 
     CALL maptra(py,px,pfx,pfy)

  CASE ( 5 )   ! coupe for sigma coordinates
     i1 = INT(px)    ;  i2 = i1 + 1
     j1 = INT(py)    ;  j2 = j1 + 1

     zx = px - i1
     zy = py - j1
     !
     zdep =  rys(i1,j1) * (1. - zx ) * (1. - zy ) + &
          &  rys(i1,j2) * (1. - zx ) *     zy     + &
          &  rys(i2,j1) * (     zx ) * ( 1. -zy ) + &
          &  rys(i2,j2) *  zx * zy
     zdep=-zdep

     pfx = px
     pfy = 1 + (zdep-prof_max)/(prof_min-prof_max)*(NYY -1)

  CASE ( 6 )
     i1 = INT(px)    ;  i2 = i1 + 1
     j1 = INT(py)    ;  j2 = j1 + 1

     zx = px - i1   
     zy = py - j1
     !
     zlat =  xygr(i1,j1,2) * (1. - zx ) * (1. - zy ) + &
          &  xygr(i1,j2,2) * (1. - zx ) *    zy      + &
          &  xygr(i2,j1,2) * (     zx ) * ( 1. -zy ) + &
          &  xygr(i2,j2,2) *  zx * zy

     zlon =  xygr(i1,j1,1) * (1. - zx ) * (1. - zy ) + &
          &  xygr(i1,j2,1) * (1. - zx ) * zy         + &
          &  xygr(i2,j1,1) * (     zx ) * ( 1. -zy ) + &
          &  xygr(i2,j2,1) *  zx * zy

     CALL maptra(zlat, zlon, pfx, pfy)  ! return pfx, pfy 

  CASE DEFAULT 
     PRINT *,'Unknown mapping in cpmpxy ',kimap
  END SELECT

END SUBROUTINE cpmpxy


SUBROUTINE cpchil (kflag)
  !!---------------------------------------------------------------------
  !!                  ***  ROUTINE cpchil  ***
  !!
  !! ** Purpose :   This routine is called by NCARG, just before and
  !!          just after the drawing of the information label. kflag = 3
  !!          is the value we are interested in, it is at this time when
  !!          plchhq is called.
  !!
  !! ** Method  :   Change the color for plotchar, and set it to nct_ilc
  !!
  !! References : Conpack, a contouring package,NCAR Graphics Programmer Document
  !!                   page 27, 3.5 
  !!----------------------------------------------------------------------
  USE modcom, ONLY : nct_ilc 
  IMPLICIT NONE

  INTEGER(KIND=4), INTENT(in) :: kflag
  INTEGER(KIND=4)  ::  iold_color
  SAVE    iold_color  ! because it is needed in a subsequent call 
  !!----------------------------------------------------------------------
  SELECT CASE ( kflag )
  CASE (  3 )  ! change plotchar color
     CALL pcgeti ('CC', iold_color)
     CALL pcseti ('CC', nct_ilc)
  CASE ( -3 )  ! restore iold_color for subsequent use of plchhq
     CALL pcseti ('CC', iold_color)
  END SELECT

END SUBROUTINE cpchil


SUBROUTINE vvumxy (px, py, pu, pv, puvm, pxb, pyb, pxe, pye, kist)
  !!---------------------------------------------------------------------
  !!                  ***  ROUTINE vvumxy  ***
  !!
  !! ** Purpose :   Similar than cpmpxy above but for vectors. Allow a 
  !!           re mapping for totaly irregular grid
  !!
  !! ** Method  :   
  !!
  !! References : man vvumxy
  !!----------------------------------------------------------------------
  USE modcom
  USE vectors

  IMPLICIT NONE

  REAL(KIND=4),  INTENT(inout) :: px, py   ! position of the vector
  REAL(KIND=4),     INTENT(in) :: pu, pv   ! vector component
  REAL(KIND=4),     INTENT(in) :: puvm     ! vector magnitude
  REAL(KIND=4),    INTENT(out) :: pxb, pyb ! position of the vector starting point
  REAL(KIND=4),    INTENT(out) :: pxe, pye ! position of the vector ending  point
  INTEGER(KIND=4), INTENT(out) :: kist     ! status of the vector mapping operation

  REAL(KIND=4)    :: zdx, zdy,   zx, zy,  zlat, zlon
  INTEGER(KIND=4) :: i1, i2, j1, j2
  INTEGER(KIND=4) :: ii, ij
  !!----------------------------------------------------------------------

  pxb = 0. ;   pyb = 0.
  pxe = 0. ;   pye = 0.

  zdx = (rmap_coord(2) - rmap_coord(1)) / float (nimax-nimin-1)
  zdy = (rmap_coord(4) - rmap_coord(3)) / float (njmax-njmin-1)

  ii = NINT ((px - bimgwk%d_xgrid(nimin))/ zdx) 
  ij = NINT ((py - bimgwk%d_ygrid(njmin))/ zdy) 

  SELECT CASE ( bimgwk%ngrid )
  CASE (3 ) 
     i1 = INT(px)  ;  i2 = i1 + 1
     j1 = INT(py)  ;  j2 = j1 + 1

     zx = px - i1
     zy = py - j1

     IF ( lo_debug ) THEN
        IF ((i1 == NXX).OR.(i1 == 1)) PRINT *,' vvmuxy : i1 out of bound i1 = ',i1, NXX
        IF ((i2 == NXX).OR.(i2 == 1)) PRINT *,' vvmuxy : i2 out of bound i2 = ',i2, NXX
        IF ((j1 == NYY).OR.(j1 == 1)) PRINT *,' vvmuxy : j1 out of bound j1 = ',j1, NYY
        IF ((j2 == NYY).OR.(j2 == 1)) PRINT *,' vvmuxy : j2 out of bound j2 = ',j2, NYY
     ENDIF
     !
     zlat =  xygr(i1,j1,2) * (1. - zx ) * (1. - zy ) + &
          &  xygr(i1,j2,2) * (1. - zx ) *    zy      + &
          &  xygr(i2,j1,2) * (     zx ) * ( 1. -zy ) + &
          &  xygr(i2,j2,2) * zx * zy

     zlon =  xygr(i1,j1,1) * (1. - zx ) * (1. - zy ) + &
          &  xygr(i1,j2,1) * (1. - zx ) *    zy      + &
          &  xygr(i2,j1,1) * (     zx ) * ( 1. -zy ) + &
          &  xygr(i2,j2,1) * zx * zy

     px=zlon
     py=zlat

  CASE DEFAULT

     px = bimgwk%d_xgrid(nimin+ii)
     py = bimgwk%d_ygrid(njmin+ij)

  END SELECT

  ! In vvmuxy, we only change px, py and let vvmpxy do the work for pxb, pyb, pxe, pye;
  ! vvmpxy is thus called with IMAP = 1 

  CALL vvseti ('MAP - mapping flag             ', 1)
  CALL vvmpxy (px, py, pu, pv, puvm, pxb, pyb, pxe, pye, kist)
  CALL vvseti ('MAP - mapping flag             ', 3)

END SUBROUTINE vvumxy
