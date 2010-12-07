MODULE   moddrawpalette
  !!======================================================================
  !!                     ***  MODULE  moddrawpalette  ***
  !!  Draw palette for color plots
  !!=====================================================================
  !! History : 1.0   !  06/1993  E. Brown     Original code
  !!         : 7.0   !  11/2010  J.M. Molines F90 (Keep NCAR coding)
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description : see below
  !!----------------------------------------------------------------------
  USE  modcom
  IMPLICIT NONE

  PRIVATE
  
  PUBLIC  :: DrawPalette  ! Draw palette bar (from LBLBAR)

  PRIVATE :: lbfill       ! a user version of lbfill
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS


  SUBROUTINE DrawPalette (khov, pxleb, pxreb, pybeb, pyteb, knbox, pwsfb, &
       &                        phsfb, klfin, kftp, cdllbs, knlbs, klbab )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DrawPalette  ***
    !!
    !! ** Purpose : This is an adaptation of LBLBAR NCARG routine. This
    !!         adaptation implement a top limit for the size of characters
    !!
    !! ** Method  :  See NCAT Fundamentals (LBLBAR).
    !!
    !! References :   NCAR documentation
    !!----------------------------------------------------------------------
    INTEGER(KIND=4),  INTENT(in) :: khov         ! Horizontal (0) Or Vertical (/=0)  indicator
    REAL(KIND=4),     INTENT(in) :: pxleb, pxreb ! Rectangular area where the palette + label
    REAL(KIND=4),     INTENT(in) :: pybeb, pyteb !   is drawn
    INTEGER(KIND=4),  INTENT(in) :: knbox        ! Number of boxes in the palette
    !                                            ! if < 0 the palette is not outlined
    REAL(KIND=4),     INTENT(in) :: pwsfb, phsfb ! Width and heigth of the sub box filled
    INTEGER(KIND=4),  INTENT(in) :: klfin(:)     ! arrays of limits
    INTEGER(KIND=4),  INTENT(in) :: kftp         ! Fill technique 0 :intenal  /=0 : use lbfill
    CHARACTER(LEN=*), INTENT(in) :: cdllbs(:)    ! Character array with labels 
    INTEGER(KIND=4),  INTENT(in) :: knlbs        ! Number of label in cdllbs
    INTEGER(KIND=4),  INTENT(in) :: klbab        ! specifies on which side or sides of the bar 
    !                                            ! the labels are to be written

    INTEGER(KIND=4)   :: icbl, icfl, iclb
    REAL(KIND=4)      :: zwobl, zwofl, zwolb
    REAL(KIND=4)      :: zxlvp, zxrvp, zybvp, zytvp
    REAL(KIND=4)      :: zxlwd, zxrwd, zybwd, zytwd
    REAL(KIND=4)      :: zwsob, zwinc, zhsob, zhinc
    REAL(KIND=4)      :: zxlb1, zxrb1, zybb1, zytb1
    REAL(KIND=4)      :: zstlw, zwmax, zhmax
    REAL(KIND=4)      :: zdstl, zdstr, zdstb, zdstt
    REAL(KIND=4)      :: zhola, wchr, zwola

    INTEGER(KIND=4)   :: ji                        ! dummy loop counter
    INTEGER(KIND=4)   :: ilnlg, ierr, isfc, ispc
    INTEGER(KIND=4)   :: itex, inclb, iscl, isct
    REAL(KIND=4)      :: zxcra(5), zycra(5) ! Define local arrays to hold X and Y coordinates of boxes.
    REAL(KIND=4)      :: zwrk(6)            ! Define local arrays for use as work arrays by the routine SFSGFA.
    INTEGER(KIND=4)   :: iwrk(8)  ! Define local arrays for use as work arrays by the routine SFSGFA.
    !!----------------------------------------------------------------------
    ! get parameter from NCAR common to maintain compatibility with LPSETI and LBSETR.
    CALL lbgeti ('CBL',icbl)
    CALL lbgeti ('CFL',icfl)
    CALL lbgeti ('CLB',iclb)
    CALL lbgetr ('WBL',zwobl)
    CALL lbgetr ('WFL',zwofl)
    CALL lbgetr ('WLB',zwolb)

    ! Save the current SET parameters and arrange for the use of normalized
    ! device coordinates.
    CALL getset (zxlvp, zxrvp, zybvp, zytvp, zxlwd, zxrwd, zybwd, zytwd, ilnlg)
    CALL    set (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)

    ! Compute the width and height of each section of the bar and the
    ! coordinates of the edges of the first solid-filled box.
    IF (khov == 0) THEN
       zwsob=(pxreb-pxleb)/REAL(ABS(knbox))
       zwinc=zwsob
       zhsob=pyteb-pybeb
       zhinc=0.
       zxlb1=pxleb+.5*(1.-pwsfb)*zwsob
       zxrb1=zxlb1+pwsfb*zwsob
       IF (klbab == 1) THEN
          zybb1=pyteb-phsfb*zhsob
          zytb1=pyteb
       ELSE IF (klbab == 2) THEN
          zybb1=pybeb
          zytb1=pybeb+phsfb*zhsob
       ELSE
          zybb1=pybeb+.5*(1.-phsfb)*zhsob
          zytb1=pyteb-.5*(1.-phsfb)*zhsob
       END IF
    ELSE
       zwsob=pxreb-pxleb
       zwinc=0.
       zhsob=(pyteb-pybeb)/REAL(ABS(knbox))
       zhinc=zhsob
       IF (klbab == 1) THEN
          zxlb1=pxleb
          zxrb1=pxleb+pwsfb*zwsob
       ELSE IF (klbab == 2) THEN
          zxlb1=pxreb-pwsfb*zwsob
          zxrb1=pxreb
       ELSE
          zxlb1=pxleb+.5*(1.-pwsfb)*zwsob
          zxrb1=pxreb-.5*(1.-pwsfb)*zwsob
       END IF
       zybb1=pybeb+.5*(1.-phsfb)*zhsob
       zytb1=zybb1+phsfb*zhsob
    END IF

    ! Draw the bar by filling all of the individual boxes.
    CALL gqfaci (ierr,isfc)
    IF (ierr /= 0) THEN
       CALL seter ('lblbar - error exit from gqfaci',1,2)
       STOP
    END IF

    IF (icfl >= 0) THEN
       CALL gqplci (ierr,ispc)
       IF (ierr /= 0) THEN
          CALL seter ('lblbar - error exit from gqplci',2,2)
          STOP
       END IF
       CALL gsplci (icfl)
    END IF

    IF (zwofl > 0.) THEN
       CALL gqlwsc (ierr,zstlw)
       IF (ierr /= 0) THEN
          CALL seter ('lblbar - error exit from gqlwsc',3,2)
          STOP
       END IF
       CALL gslwsc (zwofl)
    END IF

    DO  ji=1,ABS(knbox)
       zxcra(1)=zxlb1+REAL(ji-1)*zwinc
       zycra(1)=zybb1+REAL(ji-1)*zhinc
       zxcra(2)=zxrb1+REAL(ji-1)*zwinc
       zycra(2)=zycra(1)
       zxcra(3)=zxcra(2)
       zycra(3)=zytb1+REAL(ji-1)*zhinc
       zxcra(4)=zxcra(1)
       zycra(4)=zycra(3)
       zxcra(5)=zxcra(1)
       zycra(5)=zycra(1)
       IF (kftp == 0) THEN
          CALL sfsgfa (zxcra,zycra,4,zwrk,6,iwrk,8,klfin(ji))
       ELSE
          CALL lbfill (kftp,zxcra,zycra,5,klfin(ji))
       END IF
    ENDDO

    CALL gsfaci (isfc)
    IF (icfl >= 0) CALL gsplci (ispc)
    IF (zwofl > 0.) CALL gslwsc (zstlw)

    ! If it is to be done, outline the boxes now.

    IF (knbox > 0) THEN

       IF (icbl >= 0) THEN
          CALL gqplci (ierr,ispc)
          IF (ierr /= 0) THEN
             CALL seter ('lblbar - error exit from gqplci',4,2)
             STOP
          END IF
          CALL gsplci (icbl)
       END IF

       IF (zwobl > 0.) THEN
          CALL gqlwsc (ierr,zstlw)
          IF (ierr /= 0) THEN
             CALL seter ('lblbar - error exit from gqlwsc',5,2)
             STOP
          END IF
          CALL gslwsc (zwobl)
       END IF

       DO  ji=1,ABS(knbox)
          zxcra(1)=zxlb1+REAL(ji-1)*zwinc
          zycra(1)=zybb1+REAL(ji-1)*zhinc
          zxcra(2)=zxrb1+REAL(ji-1)*zwinc
          zycra(2)=zycra(1)
          zxcra(3)=zxcra(2)
          zycra(3)=zytb1+REAL(ji-1)*zhinc
          zxcra(4)=zxcra(1)
          zycra(4)=zycra(3)
          zxcra(5)=zxcra(1)
          zycra(5)=zycra(1)
          IF (khov == 0) THEN
             IF (ji == 1.OR.pwsfb /= 1.) THEN
                CALL gpl (5,zxcra,zycra)
             ELSE
                CALL gpl (4,zxcra,zycra)
             END IF
          ELSE
             IF (ji == 1.OR.phsfb /= 1.) THEN
                CALL gpl (5,zxcra,zycra)
             ELSE
                CALL gpl (4,zxcra(2),zycra(2))
             END IF
          END IF
       END DO

       IF (icbl >= 0) CALL gsplci (ispc)
       IF (zwobl > 0.) CALL gslwsc (zstlw)

    END IF

    ! If labelling is to be done at all ...

    IF (klbab /= 0) THEN

       ! ... save the current setting of the PLOTCHAR "text extent" parameter
       ! and reset it to force computation of "text extent" quantities.

       CALL pcgeti ('te - text extent flag',itex)
       CALL pcseti ('te - text extent flag',1)

       ! Find the dimensions of the largest label in the list of labels.

       zwmax=0.
       zhmax=0.

       DO  ji=1,knlbs
          inclb=LEN(cdllbs(ji))
103       IF (cdllbs(ji)(inclb:inclb) == ' ') THEN
             inclb=inclb-1
             IF (inclb /= 0) go to 103
          END IF
          IF (inclb /= 0) THEN
             CALL plchhq (.5,.5,cdllbs(ji)(1:inclb),.01,360.,0.)
             CALL pcgetr ('dl - distance to left edge'  ,zdstl)
             CALL pcgetr ('dr - distance to right edge' ,zdstr)
             CALL pcgetr ('db - distance to top edge'   ,zdstb)
             CALL pcgetr ('dt - distance to bottom edge',zdstt)
             zwmax=MAX(zwmax,zdstl+zdstr+.02)
             zhmax=MAX(zhmax,zdstb+zdstt+.02)
          END IF
       ENDDO

       ! If the maximum height and width are undefined, quit.

       IF (zwmax <= .02.OR.zhmax <= .02) go to 107

       ! Determine the character width to be used and the resulting offset
       ! distance to the bottom or top of the label.

       IF (khov == 0) THEN
          zhola=(1.-phsfb)*zhsob
          IF (klbab >= 3) zhola=zhola/2.
          wchr=.01*MIN(zwsob/zwmax,zhola/zhmax)

!############  Voici la ligne pour laquelle on a fait tout ce travail !
          IF (wchr > 0.012) wchr = 0.012
!############

          zdstb=(zdstb+.01)*(wchr/.01)
          zdstt=(zdstt+.01)*(wchr/.01)
       ELSE
          zwola=(1.-pwsfb)*zwsob
          IF (klbab >= 3) zwola=zwola/2.
          wchr=.01*MIN(zwola/zwmax,zhsob/zhmax)

!############  Voici la ligne pour laquelle on a fait tout ce travail !
          IF (wchr > 0.012) wchr = 0.012
!############

       END IF

       ! Draw the labels.

       CALL gqplci (ierr,iscl)
       IF (ierr /= 0) THEN
          CALL seter ('lblbar - error exit from gqplci',6,2)
          STOP
       END IF
       CALL gqtxci (ierr,isct)
       IF (ierr /= 0) THEN
          CALL seter ('lblbar - error exit from gqtxci',7,2)
          STOP
       END IF
       IF (iclb < 0) THEN
          CALL gsplci (isct)
       ELSE
          CALL gsplci (iclb)
          CALL gstxci (iclb)
       END IF
       IF (zwolb > 0.) THEN
          CALL gqlwsc (ierr,zstlw)
          IF (ierr /= 0) THEN
             CALL seter ('lblbar - error exit from gqlwsc',8,2)
             STOP
          END IF
          CALL gslwsc (zwolb)
       END IF

       IF (knlbs < ABS(knbox)) THEN
          zxlb1=zxlb1+zwinc
          zybb1=zybb1+zhinc
       ELSE IF (knlbs == ABS(knbox)) THEN
          zxlb1=zxlb1+pwsfb*zwinc/2.
          zybb1=zybb1+phsfb*zhinc/2.
       END IF

       DO  ji=1,knlbs
          inclb=LEN(cdllbs(ji))
105       IF (cdllbs(ji)(inclb:inclb) == ' ') THEN
             inclb=inclb-1
             IF (inclb /= 0) go to 105
          END IF
          IF (inclb /= 0) THEN
             IF (khov == 0) THEN
                IF (klbab == 1.OR.klbab >= 3) &
                &            CALL plchhq (zxlb1+REAL(ji-1)*zwsob,zybb1-zdstt, &
                &                            cdllbs(ji)(1:inclb),wchr,0.,0.)
                IF (klbab == 2.OR.klbab >= 3) &
                &            CALL plchhq (zxlb1+REAL(ji-1)*zwsob,zytb1+zdstb, &
                &                            cdllbs(ji)(1:inclb),wchr,0.,0.)
             ELSE
                IF (klbab == 1.OR.klbab >= 3) &
                &            CALL plchhq (zxrb1+wchr,zybb1+REAL(ji-1)*zhsob, &
                &                            cdllbs(ji)(1:inclb),wchr,0.,-1.)
                IF (klbab == 2.OR.klbab >= 3) &
                &            CALL plchhq (zxlb1-wchr,zybb1+REAL(ji-1)*zhsob, &
                &                            cdllbs(ji)(1:inclb),wchr,0.,+1.)
             END IF
          END IF
       ENDDO

       CALL gsplci (iscl)
       IF (iclb >= 0) CALL gstxci (isct)
       IF (zwolb > 0.) CALL gslwsc (zstlw)

       ! Restore the original setting of the PLOTCHAR text extent flag.

107    CALL pcseti ('te - text extent flag',itex)

    END IF

    ! Restore the original SET parameters.

    CALL set (zxlvp, zxrvp, zybvp, zytvp, zxlwd, zxrwd, zybwd, zytwd, ilnlg)

    ! done.

  END SUBROUTINE DrawPalette

  ! ---------------------------------------------------------------------
  ! 
  ! Nom              :  LBFILL
  ! Date de creation :  4 mai 1993
  ! Parametres       :  
  ! Description      :  remplace la fonction par defaut appelee
  !                     par LBLBAR, pour remplir les boites selon
  !                     ma propre technique...
  ! 
  ! ---------------------------------------------------------------------

  SUBROUTINE lbfill (kftp, pxcra, pycra, kncra, kindx)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE lbfill  ***
    !!
    !! ** Purpose :  Replace default function called by LBLBAR, for filling
    !!             the color boxes with Eric Brown technique. 
    !!
    !! ** Method  :  Ask Eric Brown ... 
    !!
    !! References :  
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) :: kftp, kindx, kncra
    REAL(KIND=4),    INTENT(inout) :: pxcra(:), pycra(:)

    REAL(KIND=4)      :: zxincr, zyincr
    INTEGER(KIND=4)   :: ji, icolor_num, icolor_index
    !!----------------------------------------------------------------------

    ! kindx is an index in the array marks_index, not a color index   
    icolor_index = marks_index(kindx) 
    icolor_num   = marks_index(kindx+1) - marks_index(kindx)

    !      PRINT *,'#### COLOR_NUM ####=',color_num
    IF (icolor_num == 0) THEN
       icolor_num = 1.0
    ENDIF

    IF (opt_vertpal  ==  0) THEN
       zxincr = (pxcra(2) - pxcra(1))/(float(icolor_num))

       pxcra(2) = pxcra(1) + zxincr
       pxcra(3) = pxcra(4) + zxincr

       DO ji=0,icolor_num-1
          CALL gsfaci (icolor_index+ji)

          CALL gfa    (kncra-1, pxcra, pycra)
          pxcra(1) = pxcra(2)
          pxcra(2) = pxcra(2) + zxincr
          pxcra(4) = pxcra(3)
          pxcra(3) = pxcra(3) + zxincr
       ENDDO
    ELSE
       zyincr = (pycra(4) - pycra(1))/(float(icolor_num))

       pycra(3) = pycra(2) + zyincr
       pycra(4) = pycra(1) + zyincr

       DO ji=0,icolor_num-1
          CALL gsfaci (icolor_index+ji)
          CALL gfa    (kncra-1, pxcra, pycra)
          pycra(1) = pycra(4)
          pycra(2) = pycra(3)
          pycra(3) = pycra(2) + zyincr
          pycra(4) = pycra(1) + zyincr
       ENDDO
    ENDIF
  END SUBROUTINE lbfill

END MODULE moddrawpalette
