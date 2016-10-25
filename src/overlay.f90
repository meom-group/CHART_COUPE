MODULE overlay
  !!======================================================================
  !!                     ***  MODULE  overlay  ***
  !!   Allow superposition of lines of marks on the chart
  !!=====================================================================
  !! History :       ! 07-1996 :  J.M Molines : original code
  !!         :  7.0  ! 11-2010 :  J.M Molines :  Fortran 90 +Doctor rules
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   OverReadData     :  read overlay data file (-overdata)
  !!   OverTrace        :  Trace overlay data (line)
  !!   OverMarkReadData :  read overmark data file (-overmark)
  !!   OverMarkTrace    :  plot overmarks
  !!   ShowGrid         :  superimpose the model grid lines on the plot
  !!----------------------------------------------------------------------
  USE modparam
  USE modcom
  USE readbimg , ONLY : IsDirect
  USE cdf,       ONLY : CdfReadHgr

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: OverReadData  
  PUBLIC :: OverTrace
  PUBLIC :: OverMarkReadData
  PUBLIC :: OverMarkTrace
  PUBLIC :: ShowGrid

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS

  SUBROUTINE OverReadData(pxover,pyover,knover)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OverReadData  ***
    !!
    !! ** Purpose : Read text file with overlat data.  
    !!
    !! ** Method  : It reads a sequence of x,y point from cf_overdata
    !!        Lines starting with # are skipped
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    INTENT(out) :: pxover(*)  ! array of x position of overlay
    REAL(KIND=4),    INTENT(out) :: pyover(*)  ! array of y position of overlay
    INTEGER(KIND=4), INTENT(out) :: knover     ! number of valid points found

    INTEGER(KIND=4)    :: ilu=55    !  logical unit used for the file
    CHARACTER(LEN=80 ) :: cline     !  a line of data in the file
    !!----------------------------------------------------------------------
    OPEN(ilu,file=cf_overdata)
    knover=0
    DO 
       READ(ilu,'(a)',END=100) cline
       IF (cline(1:1) /= '#' ) THEN
          knover = knover +1
          READ(cline,*) pxover(knover),pyover(knover)
       END IF
    ENDDO
100 CONTINUE
    CLOSE(ilu)

  END SUBROUTINE OverReadData


  SUBROUTINE OverTrace(pxover,pyover,knover)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OverTrace  ***
    !!
    !! ** Purpose : Trace the lines over the actual plot
    !!
    !! ** Method  : Draw a continuous sequence of x,y points until
    !!              a end of line point is found ( 9999,9999). Then
    !!              continue for next sequence
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    INTENT(in) :: pxover(*)  !  x data in
    REAL(KIND=4),    INTENT(in) :: pyover(*)  !  y data in
    INTEGER(KIND=4), INTENT(in) :: knover     !  number of points

    REAL(KIND=4)    :: zco(4)
    REAL(KIND=4)    :: zlwidth
    REAL(KIND=4)    :: zrl, zrr, zrb, zrt, zul, zur, zub, zut
    REAL(KIND=4)    :: zuval,zvval
    REAL(KIND=4)    :: CUFX, CUFY   ! NCAR function name do not modify

    INTEGER(KIND=4) :: ierr, iclip
    INTEGER(KIND=4) :: il
    INTEGER(KIND=4) :: ipt, incoul, incoul0
    INTEGER(KIND=4) :: ilu=88       ! Logical unit for data output
    !!----------------------------------------------------------------------
    IF ( opt_overout == 1 ) OPEN (ilu, FILE=cf_overout)

    CALL gqclip( ierr,iclip,zco)
    CALL gsclip(1)
    CALL gqlwsc(ierr,zlwidth)
    CALL gqplci(ierr,incoul0)
    CALL gsplci(nover_ic)

    CALL gslwsc(rover_lw)
    PRINT *,' OVER TRACE : opt_noproj =',opt_noproj
    IF (opt_coupe == 1 ) THEN
       PRINT *,' OVERLAY non prevu pour coupe'
       PRINT *,' en chantier ...'
       opt_overdata = 0
       RETURN

       CALL getset (zrl,zrr,zrb,zrt,zul,zur,zub,zut,il)
       CALL set  (zrl,zrr,zrb,zrt, &
            &      1.,float(NXX),-prof_max,-prof_min,il)
       PRINT *,'OVERLAY :', zrl,zrr,zrb,zrt, &
            &      rmap_marg, prof_min,prof_max,il
    ENDIF
    incoul=20
    CALL gflas1( jp_ib_overdata )
    !       call gsplci(incoul)
    IF (opt_noproj == 0 ) THEN
       CALL mapiq
       CALL mapit(pyover(1),pxover(1),0)
       ipt=0
       DO WHILE (ipt < knover)
          ipt=ipt+1
          IF (pyover(ipt) >= 9999) THEN
             !       incoul=incoul+1
             !if (incoul > ncol_jetpal) incoul=20
             !       call gsplci(incoul)
             CALL mapiq
             ipt=ipt+1
             ! in case many 99999 lines are in sequence
             DO WHILE ( pyover(ipt) >= 9999 .AND. ipt < knover )
              ipt=ipt+1
             END DO
             CALL mapit(pyover(ipt),pxover(ipt),0)
          ENDIF
          CALL mapit(pyover(ipt),pxover(ipt),1)
          IF (opt_overout == 1 ) THEN
             CALL MAPTRN(pyover(ipt),pxover(ipt), zuval,zvval)
             PRINT *, &
                  &  'OVEROUT ',ipt,pxover(ipt),pyover(ipt), zuval,zvval
             WRITE(ilu, '(a,i3,1x,2G10.5,2f10.7)') &
                  &  'OVEROUT ',ipt,pxover(ipt),pyover(ipt), zuval,zvval
          END IF
       ENDDO
       CALL mapiq
    ELSE
       CALL frstpt (pxover(1),pyover(1))
       ipt=0
       DO WHILE ( ipt < knover )
          ipt = ipt + 1
          IF (pyover(ipt) >= 9999) THEN
             CALL plotif(0.,0.,2)
             ipt = ipt + 1
             CALL  frstpt (pxover(ipt),pyover(ipt))
          ENDIF
          CALL vector (pxover(ipt),pyover(ipt))
          IF (opt_overout == 1 ) THEN
             PRINT *, &
                  &  'OVEROUT ',ipt,pxover(ipt),pyover(ipt), CUFX(pxover(ipt)),CUFY(pyover(ipt))
             WRITE(ilu,'(a,i3,1x,2G10.5,2f10.7)') &
                  & 'OVEROUT ',ipt,pxover(ipt),pyover(ipt), CUFX(pxover(ipt)),CUFY(pyover(ipt))
          ENDIF
       END DO
       CALL plotif(0.,0.,2)
    END IF
    CALL gslwsc(zlwidth)
    CALL gsclip(iclip)
    CALL gsplci(incoul0)
    CALL gflas2
    IF (opt_coupe == 1 ) THEN
       CALL set (zrl,zrr,zrb,zrt,zul,zur,zub,zut,il)
    ENDIF
    !
    IF (opt_overout == 1 ) CLOSE (ilu)

  END SUBROUTINE OverTrace


  SUBROUTINE OverMarkReadData(xoverm,yoverm,knoverm)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OverMarkReadData  ***
    !!
    !! ** Purpose : Read text file with overmark data.
    !!
    !! ** Method  : It reads a sequence of x,y point from cf_overmark
    !!        Lines starting with # are skipped
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4),    INTENT(out) :: xoverm(*) ! x data in
    REAL(KIND=4),    INTENT(out) :: yoverm(*) ! y data in
    INTEGER(KIND=4), INTENT(out) :: knoverm   ! number of points

    CHARACTER(LEN=80 )  :: cline              ! line of data 
    INTEGER(KIND=4)     :: ilu=56             ! logical unit for cf_overmark
    !!----------------------------------------------------------------------
    OPEN(ilu,file=cf_overmark)
    knoverm=0
    DO 
       READ(ilu,'(a)',END=100) cline
       IF (cline(1:1) /= '#' ) THEN
          knoverm = knoverm +1
          READ(cline,*) xoverm(knoverm),yoverm(knoverm)
       END IF
    ENDDO
100 CONTINUE
    CLOSE(ilu)

  END SUBROUTINE OverMarkReadData


  SUBROUTINE OverMarkTrace(xoverm,yoverm,knoverm)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OverMarkTrace  ***
    !!
    !! ** Purpose : Draw the marker over the actual plot
    !!
    !! ** Method  : Draw  x,y points. (9999 9999) are skipped
    !!
    !! References :  
    !!---------------------------------------------------------------------
    REAL(KIND=4),    INTENT(in) :: xoverm(*) ! x data in
    REAL(KIND=4),    INTENT(in) :: yoverm(*) ! y data in
    INTEGER(KIND=4), INTENT(in) :: knoverm   ! number of points

    REAL(KIND=4)    :: zmsz, zuval, zvval
    INTEGER(KIND=4) :: imtype
    INTEGER(KIND=4) :: ierr
    INTEGER(KIND=4) :: ji
    !!---------------------------------------------------------------------
    CALL gflas1( jp_ib_overmark )
    CALL gqmk(ierr,imtype)
    CALL gqmksc(ierr,zmsz)

    CALL gsmk(nover_mk)
    CALL gsmksc(rover_mksc)
    CALL gspmci(nover_mkic)

    DO ji=1,knoverm
       IF (xoverm(ji) < 9999 ) THEN
          IF ( opt_noproj == 1 ) THEN
             zuval = xoverm(ji)
             zvval = yoverm(ji)
          ELSE
             CALL maptrn(yoverm(ji),xoverm(ji),zuval,zvval)
          END IF
          CALL GPM(1,zuval,zvval)
       END IF
    ENDDO

    CALL gsmk(imtype)
    CALL gsmksc(zmsz)
    CALL gflas2
  END SUBROUTINE OverMarkTrace
   

  SUBROUTINE ShowGrid(cdshowfile)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ShowGrid  ***
    !!
    !! ** Purpose : Draw a model grid upon the actual plot
    !!
    !! ** Method  : Read the grid points in a bimg file 
    !!
    !! References :  
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdshowfile  ! name of input bimg file
    ! local
    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zgrx, zgry 
    REAL(KIND=4), DIMENSION(:),   ALLOCATABLE :: zpx, zpy 
    ! bimg stuff 
    REAL(KIND=4)       :: zq, zspval, ztag, ztmpx,ztmpy
    REAL(KIND=4)       :: zx1, zx2, zy1, zy2
    INTEGER(KIND=4)    :: inxg, inyg, inzg, intg, indg, icod
    CHARACTER(LEN=80 ) :: cldum

    INTEGER(KIND=4) :: ji, jj, jk               ! loop counter
    INTEGER(KIND=4) :: iseg, ii, ij, ilu=10     
    !!----------------------------------------------------------------------
    ALLOCATE ( zpx(NYY), zpy(NYY) )
    CALL gflas1( jp_ib_showgrid )
    zx1 =  rmap_coord(1)
    zx2 =  rmap_coord(2)
    zy1 =  rmap_coord(3)
    zy2 =  rmap_coord(4)

   IF (IsDirect (cdshowfile ) /= -1) THEN 

    OPEN(ilu, file=cdshowfile, form='unformatted')
    READ(ilu) cldum
    READ(ilu) cldum
    READ(ilu) cldum
    READ(ilu) cldum
    READ(ilu) inxg,inyg,inzg,intg,indg,icod

    ALLOCATE ( zgrx(inxg,inyg), zgry(inxg,inyg) )

    READ(ilu) zq,zq,zq,zq,zspval
    READ(ilu) (zq, jk=1,inzg)
    READ(ilu) ztag
    READ(ilu) zgrx(:,:)
    READ(ilu) zgry(:,:)
    CLOSE(ilu)
    
    ELSE ! assume a NEMO netcdf file reading glamf gphif
       CALL CdfReadHgr (zgrx,zgry, cdshowfile, inxg,inyg )
    ENDIF

    ! Draw lines at  j = constant
    DO jj=1,inyg
       ii=0
       DO ji=1,inxg
          ztmpx=zgrx(ji,jj)
          ztmpy=zgry(ji,jj)
          IF ( zx1 < ztmpx .AND. ztmpx < zx2) THEN
             IF ( zy1 < ztmpy .AND. ztmpy  < zy2) THEN
                ii=ii+1
                zpx(ii)=ztmpx
                zpy(ii)=ztmpy
             ENDIF
          ENDIF

       ENDDO
       CALL mapiq
       CALL mapit(zpy(1),zpx(1),0)
       iseg=0
       DO WHILE (iseg < ii)
          iseg=iseg+1
          IF (zpy(iseg) >= 999) THEN
             CALL mapiq
             iseg=iseg+1
             CALL mapit(zpy(iseg),zpx(iseg),0)
          ENDIF
          CALL mapit(zpy(iseg),zpx(iseg),1)
       ENDDO
       CALL mapiq
    ENDDO

    ! Draw lines at i = constant
    DO ji=1,inxg
       ij=0
       DO jj=1,inyg
          ztmpx=zgrx(ji,jj)
          ztmpy=zgry(ji,jj)
          IF ( zx1 < ztmpx .AND. ztmpx < zx2) THEN
             IF ( zy1 < ztmpy .AND. ztmpy  < zy2) THEN
                ij=ij+1
                zpx(ij)=ztmpx
                zpy(ij)=ztmpy
             ENDIF
          ENDIF
       ENDDO
       CALL mapiq
       CALL mapit(zpy(1),zpx(1),0)
       iseg=0
       DO WHILE (iseg < ij)
          iseg=iseg+1
          IF (zpy(iseg) >= 999) THEN
             CALL mapiq
             iseg=iseg+1    
             CALL mapit(zpy(iseg),zpx(iseg),0)
          ENDIF
          CALL mapit(zpy(iseg),zpx(iseg),1)
       ENDDO
       CALL mapiq
    ENDDO

    CALL gflas2
    DEALLOCATE ( zpx, zpy, zgrx, zgry )

  END SUBROUTINE ShowGrid

END MODULE overlay
