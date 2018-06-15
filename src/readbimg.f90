MODULE readbimg
  !!======================================================================
  !!                     ***  MODULE  readbimg  ***
  !! Contains all functions to read data files (not only bimg !)
  !!=====================================================================
  !! History : 1.O  !  06/1993   E. Brown     : Original code for bimg
  !!         : 3.0  !  06/1998   J.M. Molines : Add dimg support
  !!         : 5.0  !  06/2003   J.M. Molines : Add netcdf support
  !!         : 7.0  !  11/2010   J.M. Molines : F90 + Dr 
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : see below
  !!----------------------------------------------------------------------
  USE modcom
  USE cdf
  USE util
  USE tracecol

  IMPLICIT NONE

  PRIVATE

  REAL(KIND=4), PRIVATE, DIMENSION(:,:), ALLOCATABLE :: work_array

  PUBLIC :: BimgReadData
  PUBLIC :: BimgOpenFile
  PUBLIC :: BimgReadHeader
  PUBLIC :: BimgGetLayer
  PUBLIC :: BimgGetGridInfo
  PUBLIC :: BimgMaskData
  PUBLIC :: BimgShiftGrid
  PUBLIC :: BimgShift
  PUBLIC :: BimgAlloc
  PUBLIC :: BimgDeAlloc
  PUBLIC :: DirectGetLayer
  PUBLIC :: IsDirect

  PRIVATE :: BimgRegularGridSlow
  PRIVATE :: BimgRegularGrid
  PRIVATE :: BimgRegularGridx
  PRIVATE :: BimgRegularGridy

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS
  INTEGER(KIND=4) FUNCTION BimgOpenFile (bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION  BimgOpenFile  ***
    !!
    !! ** Purpose : Open  data file and initialize bimg structure 
    !!
    !! ** Method  : Detect file format and call appropriate statement 
    !!              Logical unit numbimg is initialize in modcom
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) ::  bdimg

    CHARACTER(LEN=256)  :: cfilename
    INTEGER(KIND=4)     :: ierr, irecl
    !!----------------------------------------------------------------------

    cfilename= bdimg%cfname
    ierr = FindFile(cfilename, jp_file_dta )      
    IF (ierr == 1) THEN
       PRINT *,' Error in BimgOpenFile : file does not exist '
       PRINT *,'    FILE : ', TRIM(cfilename)
       STOP
    ENDIF
    bdimg%nrecl = IsDirect(cfilename)

    irecl = bdimg%nrecl

    IF (irecl  ==  0 ) THEN
       ! Open bimg sequential file
       OPEN(numbimg,file=cfilename,form='unformatted')
    ELSE IF (irecl  >  0 ) THEN
       ! Open dimg direct access file
       OPEN(numbimg,file=cfilename,form='unformatted', access='direct',recl=irecl)
    ELSE IF (irecl  ==  -1 ) THEN
       ! Open NetCdf file 
        ierr=CdfOpen(cfilename,bdimg)
    ELSE
       PRINT *,' BimgOpenFile : This type of data file is not supported '
       PRINT *,'    FILE : ', TRIM(cfilename)
       STOP 
    ENDIF

    BimgOpenFile  = numbimg
    numbimg = numbimg + 1

  END FUNCTION BimgOpenFile


  SUBROUTINE BimgReadHeader (bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgReadHeader  ***
    !!
    !! ** Purpose :  Read header of data file and initialize data size
    !!
    !! ** Method  : Use the appropriate statement for reading the header
    !!              depending on file format. Fill the bimg structure with
    !!              header data.
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg

    INTEGER(KIND=4)   ::  jk, jt
    INTEGER(KIND=4)   ::  ierr
    !!----------------------------------------------------------------------
    IF (bdimg%nrecl  ==  0 ) THEN
       PRINT *,'BimgReadHeader :  Read BIMG file (sequential)'
       CALL flush(6)

       !     RECORDS 1-4 -- strings ----------------------------
       READ(bdimg%num) bdimg%cstr1
       READ(bdimg%num) bdimg%cstr2
       READ(bdimg%num) bdimg%cstr3
       READ(bdimg%num) bdimg%cstr4

       !     RECORD 5 ------------------------------------------
       READ(bdimg%num) bdimg%nxfile,bdimg%nyfile,bdimg%nzfile, &
       &                  bdimg%nt,bdimg%ndim,bdimg%icod

       bdimg%nxdata = bdimg%nxfile
       bdimg%nydata = bdimg%nyfile

       ! check size and report errors
       ierr=0
       IF (bdimg%nxfile > NXX) THEN
          PRINT *,'ERROR: NXX too small (Max : ',NXX,')'
          ierr=1
       ELSE IF (bdimg%nyfile > NYY) THEN
          PRINT *,'ERROR: NYY too small (Max : ',NYY,')'
          ierr=1
       ELSE IF (bdimg%nzfile > NA) THEN
          PRINT *,'ERROR: NA too small (Max : ',NA,')'
          ierr=1
       ENDIF

       IF (ierr == 1) STOP

       !     RECORD 6 ------------------------------------------

       READ(bdimg%num) bdimg%x1mod,bdimg%y1mod, &
       &                  bdimg%dx,bdimg%dy,bdimg%spval

       bdimg%ngrid = 0
       bdimg%mask = 0

       bdimg%x2mod = bdimg%x1mod + (float(bdimg%nxfile-1) &
       &                            * bdimg%dx)
       bdimg%y2mod = bdimg%y1mod + (float(bdimg%nyfile-1) &
       &                            * bdimg%dy)
       IF (opt_ijgrid  ==  1 ) THEN
          ! ... Use ij coordinates
          opt_print = 0
          bdimg%x1mod = 1
          bdimg%y1mod = 1
          bdimg%dx = 1
          bdimg%dy = 1
          bdimg%ngrid = 0
          bdimg%mask = 0
          bdimg%x2mod = bdimg%nxfile
          bdimg%y2mod = bdimg%nyfile
       ENDIF

       bdimg%nlast_time  = 0
       bdimg%nlast_layer = bdimg%nzfile
       bdimg%nlast_dim   = bdimg%ndim
       bdimg%nlast_rect  = 10000000
       bdimg%nlast_recr  = 0

       !     RECORD 7 ------------------------------------------

       READ(bdimg%num)(bdimg%depth(jk),jk=1,bdimg%nzfile)
       ! in chart/coupe depth are forced to be negative
       DO jk=1,bdimg%nzfile
          bdimg%depth(jk)=-1.*ABS(bdimg%depth(jk))
       ENDDO

    ELSE IF (bdimg%nrecl  >  0 ) THEN
       PRINT *,'BimgReadHeader : Read DIMG file (direct access)'
       READ(bdimg%num,rec=1) bdimg%cver,bdimg%cstr1,bdimg%nrecl,                        &
       &     bdimg%nxfile,bdimg%nyfile,bdimg%nzfile,                                    &
       &     bdimg%nt,bdimg%ndim,bdimg%x1mod,bdimg%y1mod,bdimg%dx,bdimg%dy,bdimg%spval, &
       &     (bdimg%depth(jk),jk=1,bdimg%nzfile),(bdimg%timea(jt),jt=1,bdimg%nt)   
       !
       bdimg%cstr2 = 'N/A'
       bdimg%cstr3 = 'N/A'
       bdimg%cstr4 = 'N/A'
       bdimg%icod  =  0

       bdimg%nxdata = bdimg%nxfile
       bdimg%nydata = bdimg%nyfile

       bdimg%ngrid = 0
       bdimg%mask  = 0

       bdimg%x2mod = bdimg%x1mod + (float(bdimg%nxfile-1) * bdimg%dx)
       bdimg%y2mod = bdimg%y1mod + (float(bdimg%nyfile-1) * bdimg%dy)

       bdimg%nlast_time  = 0
       bdimg%nlast_layer = bdimg%nzfile
       bdimg%nlast_dim   = bdimg%ndim

       ! in chart/coupe depth are forced to be negative
       DO jk=1,bdimg%nzfile
          bdimg%depth(jk)=-1.*ABS(bdimg%depth(jk))
       ENDDO
    ELSE IF (bdimg%nrecl  ==  -1 ) THEN
       PRINT *, 'BimgReadHeader : Read NetCdf file '
       CALL CdfHeader(bdimg)
    ELSE
       !     likely to be never reached, program should have stopped before
    ENDIF

    !  All file format go through this common end of routine
    IF ( opt_spval == 1 ) bdimg%spval=spval_new  ! change spval from default

    IF (bdimg%spval  ==  0) THEN
       bdimg%lspval0 = .TRUE.
    ELSE
       bdimg%lspval0 = .FALSE.
    ENDIF
    bdimg%nshift = 0

  END SUBROUTINE BimgReadHeader


  SUBROUTINE BimgReadData (pdata_out, bdimg, pcoords, ktstep, klayer, kdim, kopt_scale, pscale, kopt_mean, pmean0, kopt_abs)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgReadData  ***
    !!
    !! ** Purpose :  Read a layer of data from according to the structure
    !!               passed as argument 
    !!
    !! ** Method  :   pdata_out  : array of data
    !!                bdimg      : structure of data
    !!                pcoords    : coordinates of the data
    !!                ktstep     : time step to read
    !!                klayer     : layer to read
    !!                kdim       : dimension to read
    !!
    !! References :  
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: pdata_out ! tableau fourni par la fonction d'appel
    TYPE( bimgfile ),           INTENT(inout) :: bdimg     ! data structure
    REAL(KIND=4), DIMENSION(:),  INTENT(inout):: pcoords   ! coordinates of the corner of the data
    INTEGER(KIND=4),               INTENT(in) :: ktstep    ! time step number to read
    INTEGER(KIND=4),               INTENT(in) :: klayer    ! layer number to read
    INTEGER(KIND=4),               INTENT(in) :: kdim      ! dimension number to read
    INTEGER(KIND=2), OPTIONAL,     INTENT(in) :: kopt_scale! scaling of the data flag
    REAL(KIND=4),    OPTIONAL,     INTENT(in) :: pscale    ! scaling of the data flag
    INTEGER(KIND=2), OPTIONAL,     INTENT(in) :: kopt_mean ! substract mean value flag
    REAL(KIND=4),    OPTIONAL,     INTENT(in) :: pmean0    ! mean value of the output field
    INTEGER(KIND=2), OPTIONAL,     INTENT(in) :: kopt_abs  ! mean value of the output field

    INTEGER(KIND=4)   :: ji, jj, inmean, inx, iny
    INTEGER(KIND=4)   :: iksup, ikinf
    REAL(KIND=4)      :: zlocal_data(NXX,NYY)  ! same size as pdata_out
    REAL(KIND=8)      :: zmean                 ! double precision for accumulation
    REAL(KIND=4)      :: zlat, zlon, zcof, zcof1, zspval
    LOGICAL           :: llgood_point
    LOGICAL           :: ll_scale , ll_mean, ll_abs
    REAL(KIND=4)      :: zscale , zmean0
    !!----------------------------------------------------------------------
    ll_scale=.false. ; zscale = 1.
    IF ( PRESENT ( kopt_scale ) ) THEN
     IF ( kopt_scale == 1 ) THEN
       ll_scale=.true.
     ELSE 
       ll_scale=.false.
     ENDIF
    ENDIF

    IF ( PRESENT ( pscale ) ) THEN
       zscale=pscale
    ELSE 
       zscale=1.
    ENDIF

    ll_mean=.false. ; zmean0 = 0
    IF ( PRESENT ( kopt_mean ) ) THEN
      IF ( kopt_mean == 1 ) THEN
       ll_mean=.true.
      ELSE 
       ll_mean=.false.
      ENDIF
    ENDIF
      
    IF ( PRESENT ( pmean0 ) ) THEN
       zmean0=pmean0
    ELSE 
       zmean0=0
    ENDIF

    ll_abs=.false.
    IF ( PRESENT ( kopt_abs ) ) THEN
      IF ( kopt_abs == 1 ) THEN
       ll_abs=.true.
      ELSE
       ll_abs=.false.
      ENDIF
    ENDIF


    PRINT '( 3(a,i5) )', ' READ DATA : Time step :', ktstep, &
       &                  ' Layer :', klayer, &
       &                  ' Dimension :', kdim
    CALL flush(6)

    IF (opt_dep /= 1) THEN  ! read a layer at klayer for ktstep and kdim
       IF (bdimg%nrecl  ==  0 )       THEN
          CALL BimgGetLayer   (bdimg, zlocal_data, ktstep, klayer, kdim)
       ELSE IF (bdimg%nrecl  >  0 )   THEN
          CALL DirectGetLayer (bdimg, zlocal_data, ktstep, klayer, kdim)
       ELSE IF (bdimg%nrecl  ==  -1 ) THEN
          CALL CdfGetLayer    (bdimg, zlocal_data, ktstep, klayer, kdim)
       ELSE
          !  likely to be never reached
       ENDIF
    ELSE
       ! interpolation between 2 levels to fit exact required depth
       iksup=klayer
       ikinf=klayer + 1
       PRINT *,'Interpolation between layers ',iksup ,' and ',ikinf 
       PRINT *,' weight :',bdimg%alphasup, 1.- bdimg%alphasup
       PRINT *,' depth  :',bdimg%depth(iksup), req_dep, bdimg%depth(ikinf)

       ! Read upper level
       IF (bdimg%nrecl  ==  0 ) THEN
          CALL BimgGetLayer   (bdimg, zlocal_data, ktstep, iksup, kdim)
       ELSE IF (bdimg%nrecl  >  0 ) THEN  
          CALL DirectGetLayer (bdimg, zlocal_data, ktstep, iksup, kdim)
       ELSE IF (bdimg%nrecl  ==  -1 ) THEN
          CALL CdfGetLayer    (bdimg, zlocal_data, ktstep, iksup, kdim)
       ELSE
          !  likely to be never reached
       ENDIF
       ! Read lower llevel
       IF (bdimg%nrecl  ==  0  ) THEN
          CALL BimgGetLayer   (bdimg, pdata_out, ktstep, ikinf, kdim)
       ELSE IF (bdimg%nrecl  >  0 ) THEN
          CALL DirectGetLayer (bdimg, pdata_out, ktstep, ikinf, kdim)
       ELSE IF (bdimg%nrecl  ==  -1 ) THEN
          CALL CdfGetLayer    (bdimg, pdata_out, ktstep, ikinf, kdim)
       ELSE
          !  likely to be never reached
       ENDIF

       ! interpolation taking care of special values
       zcof = bdimg%alphasup
       zcof1 = 1. - zcof
       inx = bdimg%nxdata ; iny = bdimg%nydata 
       zspval = bdimg%spval

       WHERE ( pdata_out(1:inx,1:iny) /= zspval .AND. zlocal_data(1:inx,1:iny) /= zspval ) 
          zlocal_data(1:inx, 1:iny) = zcof * zlocal_data(1:inx, 1:iny) + zcof1 *  pdata_out(1:inx,1:iny)
       ELSEWHERE 
          zlocal_data(1:inx, 1:iny) = zspval
       END WHERE

    ENDIF

    ! If spval is 0, contouring program fails. So the 0 values are changed to another
    ! rp_defspval, with the risk of flagging good data points with 0 value ...
    IF (bdimg%lspval0) THEN
       bdimg%spval=rp_defspval
       WHERE ( zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile) == 0. ) &
          &    zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile)=bdimg%spval
    ENDIF

    ! Apply masking if required : replace masked values by spval
    IF (bdimg%mask == 1) THEN
       PRINT *, 'masking data'
       CALL BimgMaskData (bdimg, zlocal_data)
    ENDIF

    ! option -scale -scaleclr -scalecnt
    IF (ll_scale ) THEN
       PRINT *,' Scaling input data by ', zscale 
       WHERE ( zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile) /= bdimg%spval ) &
          &    zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile)=zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile) * zscale
    ENDIF
    
    ! option -abs -clrabs -cntabs
    IF (ll_abs ) THEN
       PRINT *,' Taking absolute value of iniput data '
       WHERE ( zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile) /= bdimg%spval ) &
          &    zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile)=ABS(zlocal_data(1:bdimg%nxfile,1:bdimg%nyfile))
    ENDIF

    ! option -shift
    IF (opt_shift == 1) THEN
                                CALL BimgShift (bdimg, zlocal_data, shift_map, pcoords)
       IF (opt_clrgrid  >= 1)   CALL BimgShiftGrid (bdimg, shift_map)
       bdimg%x1mod = bdimg%x1mod + shift_map
    ENDIF

    ! if a grid was specified (-grid ), data are interpolated on a NXX x NYY grid with
    ! a bilinear interpolation (Case 1) or a higher order (and slower) interpolation (case 2)
    SELECT CASE ( bdimg%ngrid )
      CASE ( 1 )
       IF (bdimg%nxfile == 1) THEN
          CALL BimgRegularGridy(zlocal_data, bdimg)
          bdimg%nxdata = 1
          bdimg%nydata = NYY
          bdimg%dx = 0
          bdimg%dy = (bdimg%d_ygrid(bdimg%nyfile) - bdimg%d_ygrid(1)) &
          &             / float (NYY-1)
       ELSEIF (bdimg%nyfile == 1) THEN
          CALL BimgRegularGridx(zlocal_data, bdimg)
          bdimg%nxdata = NXX
          bdimg%nydata = 1
          bdimg%dx = (bdimg%d_xgrid(bdimg%nxfile) - bdimg%d_xgrid(1)) &
          &             / float (NXX-1)
          bdimg%dy = 0
       ELSE
          CALL BimgRegularGrid (zlocal_data, bdimg)
          bdimg%nxdata  = NXX
          bdimg%nydata  = NYY
          bdimg%dx = (bdimg%d_xgrid(bdimg%nxfile) - bdimg%d_xgrid(1)) &
          &             / float (NXX-1)
          bdimg%dy = (bdimg%d_ygrid(bdimg%nyfile) - bdimg%d_ygrid(1))  &
          &             / float (NYY-1)
       ENDIF

      CASE ( 2 )
       PRINT *,'Irregular grid  option -gridslow,  be patient ...'
       CALL BimgRegularGridSlow (zlocal_data, bdimg)
       bdimg%nxdata  = NXX
       bdimg%nydata  = NYY
       bdimg%dx = (bdimg%d_xgrid(bdimg%nxfile) - bdimg%d_xgrid(1)) &
       &             / float (NXX-1)
       bdimg%dy = (bdimg%d_ygrid(bdimg%nyfile) - bdimg%d_ygrid(1))  &
       &             / float (NYY-1)
      CASE DEFAULT
       ! This case includes the case of completly irregular grids, as in this case, 
       ! no previous interpolation is done. It will be performed by cpmpxy at the
       ! time of charting the data
       bdimg%nxdata = bdimg%nxfile
       bdimg%nydata = bdimg%nyfile
    END SELECT

    !  Take only the part of the data corresponding to the region of interest, and 
    !  transfert it to the output array.  pcoords array hold xmin xmax ymin ymax 
    !  of the selected area, either from the data or from the zoom option. In case
    !  of an irregular grid -grid or -gridxy), x1mod, x2mod, y1mod, y2mod are updated
    !  when reading the grid (GetGridInfo) and passed to pcoords in OpenAndVerifyFiles.
    SELECT CASE ( bdimg%ngrid )
     CASE ( 3 )  ! completely irregular grid

       nimin=1
       nimax=bdimg%nxdata
       njmin=1
       njmax=bdimg%nydata
       ! In this case, when using zoom, points out of the selected area are masked
       ! using spval, in order to have a correct computation of min and max computed
       ! globaly othewise.
       IF (opt_zoom == 1 .AND. opt_chart == 1) THEN
          DO ji=nimin,nimax
             DO jj=njmin,njmax
                zlat=xygr(ji,jj,2)
                zlon=xygr(ji,jj,1)
                llgood_point=.TRUE.
                llgood_point=(zlat < pcoords(4))
                llgood_point=llgood_point .AND. (zlat > pcoords(3))
                llgood_point=llgood_point .AND. (zlon < pcoords(2))
                llgood_point=llgood_point .AND. (zlon > pcoords(1))
                IF (.NOT.llgood_point) zlocal_data(ji,jj)=bdimg%spval
             ENDDO
          ENDDO
       ENDIF

     CASE DEFAULT
       IF (bdimg%dx == 0.) THEN
          nimin=1
          nimax=1
       ELSE
          nimin = NINT((pcoords(1) - bdimg%x1mod)/bdimg%dx)+1
          nimax = NINT((pcoords(2) - bdimg%x1mod)/bdimg%dx)+1

       ENDIF

       IF (bdimg%dy == 0.) THEN
          njmin=1
          njmax=1
       ELSE
          njmin = NINT((pcoords(3) - bdimg%y1mod)/bdimg%dy)+1
          njmax = NINT((pcoords(4) - bdimg%y1mod)/bdimg%dy)+1
       ENDIF

       ! pcoords is adjusted to the exact location corresponing to nimin, nimax etc..
       ! Rem : Eric Brown original message here :" ATTENTION, ca va causer des pb le jour 
       !        ou il y aura des donnees avec des pas de grille differents......"
       pcoords(1)=bdimg%x1mod+bdimg%dx*(nimin-1)
       pcoords(2)=bdimg%x1mod+bdimg%dx*(nimax-1)
       pcoords(3)=bdimg%y1mod+bdimg%dy*(njmin-1)
       pcoords(4)=bdimg%y1mod+bdimg%dy*(njmax-1)

    END SELECT

    ! check that the domain limited by nimin, nimax, njmin, njmax lays within the
    ! data domain 
    IF (    (nimin > bdimg%nxdata) .OR. (nimax > bdimg%nxdata) .OR. &
       &    (njmin > bdimg%nydata) .OR. (njmax > bdimg%nydata) ) THEN
       PRINT *,'ERROR in BimgReadData  : coordinated out of the domain  '
       PRINT *,' Domain is : ', pcoords(1), pcoords(2), pcoords(3), pcoords(4)
       PRINT *,nimin,bdimg%nxdata
       PRINT *,nimax,bdimg%nxdata
       PRINT *,njmin,bdimg%nydata
       PRINT *,njmax,bdimg%nydata
       STOP
    ENDIF

    ! Now  check the order of the domain limit : in chart it should ne always in correct order
    ! but for coupe, a section can be described in reverse order... In this latter case, the
    ! loops are to be processed in the opposit order ...
    IF ((nimin <= nimax).AND.(njmin <= njmax)) THEN   ! always the case for chart
       bdimg%nxdata = nimax - nimin ! + 1
       bdimg%nydata = njmax - njmin ! + 1
       !     option -mean : change the mean value of local data set
       IF ( ll_mean ) THEN
          PRINT *, 'Changing mean value of extracted data to ', zmean0
          inmean = COUNT ( zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval )
          zmean  = SUM   ( zlocal_data(nimin:nimax,njmin:njmax) ,                 &
             &             zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval ) / inmean
          PRINT *, '     NMEAN = ', inmean
          PRINT *, '     ZMEAN = ', zmean

          WHERE ( zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval )           &
             &    zlocal_data(nimin:nimax,njmin:njmax) =                          &
                  zlocal_data(nimin:nimax,njmin:njmax) -zmean +zmean0 
       END IF

       IF ( opt_log == 1 .OR. ( opt_cntlog == 1 .AND. bdimg%lcnt ) .OR. (opt_clrlog == 1 .AND. bdimg%lclr) ) THEN
          WHERE(  zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval .AND.      &
             &    zlocal_data(nimin:nimax,njmin:njmax) > 0.                 ) 
             zlocal_data(nimin:nimax,njmin:njmax) = LOG10( zlocal_data(nimin:nimax,njmin:njmax))
          ELSEWHERE
             zlocal_data(nimin:nimax,njmin:njmax) =  bdimg%spval 
          ENDWHERE
       ENDIF

       IF ( opt_low == 1 .OR. (opt_cntlow == 1 .AND. bdimg%lcnt ) .OR. (opt_clrlow == 1 .AND. bdimg%lclr) ) THEN
          WHERE(  zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval .AND.      &
             &    zlocal_data(nimin:nimax,njmin:njmax) > 0.                 )
             zlocal_data(nimin:nimax,njmin:njmax) = SQRT(SQRT( zlocal_data(nimin:nimax,njmin:njmax)))
          ELSEWHERE
             zlocal_data(nimin:nimax,njmin:njmax) =  bdimg%spval
          ENDWHERE
       ENDIF

       IF ( opt_hig == 1 .OR. (opt_cnthig == 1 .AND. bdimg%lcnt ) .OR. (opt_clrhig == 1 .AND. bdimg%lclr) ) THEN
          WHERE(  zlocal_data(nimin:nimax,njmin:njmax) /= bdimg%spval .AND.      &
             &    zlocal_data(nimin:nimax,njmin:njmax) > 0.                 )
             zlocal_data(nimin:nimax,njmin:njmax) =  zlocal_data(nimin:nimax,njmin:njmax)**4
          ELSEWHERE
             zlocal_data(nimin:nimax,njmin:njmax) =  bdimg%spval
          ENDWHERE
       ENDIF


       pdata_out(1:nimax-nimin+1 , 1:njmax-njmin+1 ) = zlocal_data (nimin:nimax ,njmin:njmax )

    ! Longitudes and latitudes inverted   ( coupe only  )
    ELSE IF ( (nimax < nimin) .AND. (njmax < njmin) ) THEN
       bdimg%nxdata = nimin - nimax !  + 1
       bdimg%nydata = njmin - njmax !  + 1

       pdata_out(1:nimin-nimax+1 , 1:njmin-njmax+1 ) = zlocal_data (nimin:nimax:-1 ,njmin:njmax:-1 )

    ! Longitudes inverted
    ELSEIF ((nimax < nimin).AND.(njmin <= njmax)) THEN
       bdimg%nxdata = nimin - nimax !  + 1
       bdimg%nydata = njmax - njmin !  + 1

       pdata_out(1:nimin-nimax+1 , 1:njmax-njmin+1 ) = zlocal_data (nimin:nimax:-1 ,njmin:njmax:1 )

    ! Latitudes inverted
    ELSEIF ((nimin <= nimax).AND.(njmax < njmin)) THEN
       bdimg%nxdata = nimax - nimin !  + 1
       bdimg%nydata = njmin - njmax !  + 1

       pdata_out(1:nimax-nimin+1 , 1:njmin-njmax+1 ) = zlocal_data (nimin:nimax:1 ,njmin:njmax:-1 )

    ENDIF

    ! in case of nimin=nimax or njmin=njmax (meridional or zonal sections )
    IF (bdimg%nxdata == 0) bdimg%nxdata = 1
    IF (bdimg%nydata == 0) bdimg%nydata = 1


    IF (opt_shift == 1) THEN
       bdimg%x1mod = bdimg%x1mod - shift_map
    ENDIF

    ! restore original spval for next layer. 
    IF (bdimg%lspval0) bdimg%spval = 0.

    ! set plot window in case of -pixel option
    IF (opt_pixel == 1 ) THEN
      CALL SetPlotWindow(bdimg)
    ENDIF

  END SUBROUTINE BimgReadData


  SUBROUTINE BimgRegularGridSlow (pdata_in_out, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgRegularGridSlow  ***
    !!
    !! ** Purpose :  This routine considers that data are on the grid read
    !!               by BimgGetGridInfo and uses NCL CPSPS2 routine to 
    !!               interpolate data on a regular grid 
    !!
    !! ** Method  :  Nice but expensive 
    !!
    !! References :  NCAR MANUAL
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pdata_in_out   ! (NXX,NYY)
    TYPE( bimgfile ),                INTENT(in) :: bdimg

    INTEGER(KIND=4)   ::  ji,jj
    !     see Contouring and Mapping tutorial page 227
    REAL(KIND=4),    DIMENSION(:), ALLOCATABLE :: zwrk
    INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: iwrk
    INTEGER(KIND=4)                            :: izwk, iiwk, izrg
    INTEGER(KIND=4)                            :: inx, iny
    INTEGER(KIND=4)                            :: ilog
    REAL(KIND=4)                               :: zrl, zrr, zrb, zrt
    REAL(KIND=4)                               :: zur, zul, zut, zub
    !!----------------------------------------------------------------------
    izwk = 100 * ((NXX*NYY + 99)/100) + 3*NYY*NXX + NYY + 2*NXX
    iiwk = 100 * ((NXX*NYY + 99)/100)
    izrg = NXX*NYY
    ALLOCATE (work_array(NXX,NYY) )
    ALLOCATE (zwrk(izwk)        )
    ALLOCATE (iwrk(iiwk)        )
    !     see Contouring and Mapping tutorial page 227

    CALL cpseti ('ZDS - Z data array Dimension Selector', 0)
    CALL cpseti ('ZD1 - Z data array Dimension 1', NXX)
    CALL cpseti ('ZDM - Z data array Dimension M', NXX)
    CALL cpseti ('ZDN - Z data array Dimension N', NYY)
    CALL cpsetr ('SPV - SPecial Value', bdimg%spval)

    inx = bdimg%nxfile
    iny = bdimg%nyfile

    ! calls to getset and set are necessary because CPSPS2 also does
    ! a call to set, which produces unwanted effects
    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)
    CALL cpsps2 (bdimg%d_xgrid, bdimg%d_ygrid, pdata_in_out, &
    &             NXX, inx, iny, zwrk, izwk, &
    &             iwrk, iiwk, work_array, izrg)
    CALL set(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)

    pdata_in_out(:,:) = work_array(:,:)

    DEALLOCATE ( work_array , iwrk, zwrk )

  END SUBROUTINE BimgRegularGridSlow


  SUBROUTINE BimgRegularGrid (pdata_in_out, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgRegularGrid  ***
    !!
    !! ** Purpose : This routine considers that data are on the grid read
    !!               by BimgGetGridInfo and uses a bilinear interpolation to
    !!               interpolate data on a regular grid
    !!
    !! ** Method  :  Much faster 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pdata_in_out   ! (NXX,NYY)
    TYPE( bimgfile ),                INTENT(in) :: bdimg

    INTEGER(KIND=4)       :: ji, jj
    INTEGER(KIND=4)       :: ii, ij, inx, iny
    INTEGER(KIND=4), SAVE :: ifrst = 0
    INTEGER(KIND=4), SAVE :: i0(NXX), j0(NYY)
    REAL(KIND=4),    SAVE :: zfacx(NXX), zfacy(NYY)
    REAL(KIND=4)          :: zdx, zdy, zP1, zP2, zP3, zP4
    REAL(KIND=4)          :: zxx, zyy
    !!----------------------------------------------------------------------
    inx = bdimg%nxfile
    iny = bdimg%nyfile

    ALLOCATE (work_array (NXX,NYY) )

    IF (ifrst == 0) THEN
       ifrst=1
       ! Initialize arrays i0, j0 and zfacx, zfacy at first passage
       zdx      = (bdimg%d_xgrid(inx)-bdimg%d_xgrid(1))/(NXX-1)
       zdy      = (bdimg%d_ygrid(iny)-bdimg%d_ygrid(1))/(NYY-1)
       i0(1)    = 1
       j0(1)    = 1
       zfacx(1) = 0
       zfacy(1) = 0
       i0(NXX)  = inx
       j0(NYY)  = iny
       zfacx(NXX) = 0
       zfacy(NYY) = 0

       ii=1 ; ij=1
       DO ji=2,NXX-1
          zxx = (ji-1)*zdx + bdimg%d_xgrid(1)
          DO WHILE ( zxx >= bdimg%d_xgrid(ii) )
             ii=ii+1
          ENDDO
          ii=ii-1
          i0(ji)    = ii
          zfacx(ji) =( zxx - bdimg%d_xgrid(ii) ) / ( bdimg%d_xgrid(ii+1) - bdimg%d_xgrid(ii) )
       ENDDO

       DO jj=2,NYY-1
          zyy = (jj-1)*zdy + bdimg%d_ygrid(1)
          DO WHILE (zyy >= bdimg%d_ygrid(ij))
             ij=ij+1
          ENDDO
          ij=ij-1
          j0(jj)    = ij
          zfacy(jj) = ( zyy - bdimg%d_ygrid(ij) ) / ( bdimg%d_ygrid(ij+1) - bdimg%d_ygrid(ij) )
       ENDDO

    ENDIF
    work_array(1  ,1  ) = pdata_in_out(1  ,1  )
    work_array(NXX,NYY) = pdata_in_out(inx,iny)

    DO ji=2,NXX-1
       DO jj=2,NYY-1
          zP1 = pdata_in_out( i0(ji)  ,j0(jj)   )
          zP2 = pdata_in_out( i0(ji)+1,j0(jj)   )
          zP3 = pdata_in_out( i0(ji)+1,j0(jj)+1 )
          zP4 = pdata_in_out( i0(ji)  ,j0(jj)+1 )
          IF (  zP1 == bdimg%spval .OR. zP2 == bdimg%spval .OR. &
             &  zP3 == bdimg%spval .OR. zP4 == bdimg%spval ) THEN
             work_array(ji,jj) = bdimg%spval
          ELSE
             work_array(ji,jj) =                       &
             &   (1-zfacx(ji)) * (1-zfacy(jj)) * zP1 + &
             &   (  zfacx(ji)) * (1-zfacy(jj)) * zP2 + &
             &   (  zfacx(ji)) * (  zfacy(jj)) * zP3 + &
             &   (1-zfacx(ji)) * (  zfacy(jj)) * zP4
          ENDIF
       ENDDO
    ENDDO
    !
    ! Special treatment at the limit of the domain (2 points only )
    DO jj=1,NYY,NYY-1
      DO ji=2,NXX-1
         zP1 = pdata_in_out( i0(ji)  ,j0(jj) )
         zP2 = pdata_in_out( i0(ji)+1,j0(jj) )
         IF (  zP1 == bdimg%spval .OR. zP2 == bdimg%spval ) THEN
            work_array(ji,jj) = bdimg%spval
         ELSE
            work_array(ji,jj) = (1-zfacx(ji))*zP1 + zfacx(ji)*zP2 
         ENDIF
      ENDDO
    ENDDO
    !
    DO ji=1,NXX,NXX-1
      DO jj=2,NYY-1
         zP1 = pdata_in_out( i0(ji),j0(jj)   )
         zP4 = pdata_in_out( i0(ji),j0(jj)+1 )
         IF ( zP1 == bdimg%spval .OR. zP4 == bdimg%spval ) THEN
            work_array(ji,jj) = bdimg%spval
         ELSE
            work_array(ji,jj) = (1-zfacy(jj))*zP1 + zfacy(jj)*zP4
         ENDIF
      ENDDO
    ENDDO

    pdata_in_out(:,:) = work_array(:,:)

    DEALLOCATE ( work_array )

  END SUBROUTINE BimgRegularGrid


  SUBROUTINE BimgRegularGridy (pdata_in_out, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgRegularGridy  ***
    !!
    !! ** Purpose :   This routine considers that data are on the grid read
    !!               by BimgGetGridInfo and uses a bilinear interpolation to
    !!               interpolate data on a regular grid (1,NYY).
    !!
    !! ** Method  :   very fast
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:),INTENT(inout) :: pdata_in_out  !(NXX,NYY)
    TYPE( bimgfile ),               INTENT(in) :: bdimg

    INTEGER(KIND=4)       :: jj
    INTEGER(KIND=4)       :: ij, iny
    INTEGER(KIND=4), SAVE :: ifrst = 0
    INTEGER(KIND=4), SAVE :: j0(NYY)
    REAL(KIND=4),    SAVE :: zfacy(NYY)
    REAL(KIND=4)          :: zdy, zP1, zP4
    REAL(KIND=4)          :: zyy
    !!----------------------------------------------------------------------
    iny = bdimg%nyfile
    ALLOCATE (work_array (NXX,NYY) )

    IF (ifrst == 0) THEN
       ifrst=1

       ! initialize j0 and zfacy arrays
       zdy=(bdimg%d_ygrid(iny)-bdimg%d_ygrid(1))/(NYY-1)
       j0(1)      = 1
       zfacy(1)   = 0
       j0(NYY)    = iny
       zfacy(NYY) = 0

       ij=1
       DO jj=2,NYY-1
          zyy=(jj-1)*zdy+bdimg%d_ygrid(1)
          DO WHILE (zyy >= bdimg%d_ygrid(ij))
             ij=ij+1
          ENDDO
          ij=ij-1
          j0(jj)=ij
          zfacy(jj)=( zyy - bdimg%d_ygrid(ij) ) / ( bdimg%d_ygrid(ij+1) - bdimg%d_ygrid(ij) )
       ENDDO

    ENDIF

    work_array(1,1  ) = pdata_in_out(1,1)
    work_array(1,NYY) = pdata_in_out(1,iny)

    DO jj=2,NYY-1
       zP1=pdata_in_out( 1,j0(jj)  )
       zP4=pdata_in_out( 1,j0(jj)+1)
       IF (zP1 == bdimg%spval .OR. zP4 == bdimg%spval ) THEN
          work_array(1,jj)=bdimg%spval
       ELSE
          work_array(1,jj)= (1-zfacy(jj))*zP1 + zfacy(jj)*zP4
       ENDIF
    ENDDO

    pdata_in_out(1,:) = work_array(1,:)

    DEALLOCATE ( work_array )

  END SUBROUTINE BimgRegularGridy


  SUBROUTINE BimgRegularGridx (pdata_in_out, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgRegularGridx  ***
    !!
    !! ** Purpose :     This routine considers that data are on the grid read
    !!               by BimgGetGridInfo and uses a bilinear interpolation to
    !!               interpolate data on a regular grid (NXX,1).
    !!
    !! ** Method  :   very fast
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:),INTENT(inout) :: pdata_in_out  !(NXX,NYY)
    TYPE( bimgfile ),               INTENT(in) :: bdimg

    INTEGER(KIND=4)       :: ji
    INTEGER(KIND=4)       :: ii, inx
    INTEGER(KIND=4), SAVE :: ifrst = 0
    INTEGER(KIND=4), SAVE :: i0(NXX)
    REAL(KIND=4),    SAVE :: zfacx(NXX)
    REAL(KIND=4)          :: zdx, zP1, zP2
    REAL(KIND=4)          :: zxx
    !!----------------------------------------------------------------------
    inx = bdimg%nxfile
    ALLOCATE (work_array (NXX,NYY) )

    IF (ifrst == 0) THEN
       ifrst=1
       !
       ! initialise les tableaux i0,j0, zfacx et zfacy
       !
       ! initialize i0 and zfacx arrays
       zdx        = (bdimg%d_xgrid(inx) - bdimg%d_xgrid(1) )/(NXX-1)
       i0(1)      = 1
       zfacx(1)   = 0
       i0(NXX)    = inx
       zfacx(NXX) = 0

       ii=1
       DO ji=2,NXX-1
          zxx=(ji-1)*zdx+bdimg%d_xgrid(1)
          DO WHILE (zxx >= bdimg%d_xgrid(ii))
             ii=ii+1
          ENDDO
          ii=ii-1
          i0(ji)=ii
          zfacx(ji)=(zxx-bdimg%d_xgrid(ii))/ &
          &            (bdimg%d_xgrid(ii+1)-bdimg%d_xgrid(ii))
       ENDDO

    ENDIF
    work_array(1,1)=pdata_in_out(1,1)
    work_array(NXX,1)=pdata_in_out(inx,1)

    DO ji=2,NXX-1
       zP1 = pdata_in_out( i0(ji)  ,1 )
       zP2 = pdata_in_out( i0(ji)+1,1 )
       IF (zP1 == bdimg%spval .OR. zP2 == bdimg%spval ) THEN
          work_array(ji,1) = bdimg%spval
       ELSE
          work_array(ji,1) = (1-zfacx(ji))*zP1 + zfacx(ji)*zP2 
       ENDIF
    ENDDO

    pdata_in_out(:,1) = work_array(:,1)

    DEALLOCATE ( work_array )

  END SUBROUTINE BimgRegularGridx


  SUBROUTINE BimgGetLayer (bdimg, plocal_data, ktstep, klev, kdim)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgGetLayer  ***
    !!
    !! ** Purpose :  Read a layer of data in a bimg file, for layer klev,
    !!               time ktstep, dim kdim. Data are output in plocal_data
    !!
    !! ** Method  : In this version, the bimg sequential access file is used
    !!              in a similar way than the direct access, assuming the file
    !!              stays open, and that we progress forward into the file. If
    !!              we have to go backwqrd, then the file is rescanned from the
    !!              beginning.
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),           INTENT(inout) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: plocal_data  ! NXX x NYY
    INTEGER(KIND=4),               INTENT(in) :: ktstep
    INTEGER(KIND=4),               INTENT(in) :: klev
    INTEGER(KIND=4),               INTENT(in) :: kdim

    INTEGER(KIND=4)   :: ji, jj
    INTEGER(KIND=4)   :: irect, irecr, irecdif, irecdift
    INTEGER(KIND=4)   :: idstp 
    !!----------------------------------------------------------------------
    irect    = 7 + (ktstep-1)*(bdimg%nzfile*bdimg%ndim + 1 )
    idstp    = ktstep - bdimg%nlast_time
    irecdift = irect - bdimg%nlast_rect

    irecr    = (klev-1)*bdimg%ndim + (kdim -1)
    irecdif  = irecr - bdimg%nlast_recr

    IF (irecdift  ==  0 .AND. irecdif  >  0 ) THEN
       DO ji=1,irecdif - 1
          READ(bdimg%num)
       ENDDO

       READ(bdimg%num) ((plocal_data(ji,jj), ji=1,bdimg%nxfile), jj=1,bdimg%nyfile )
    ELSE IF (irecdift > 0) THEN
       ! go to the end of the current block
       DO ji=1,bdimg%nzfile*bdimg%ndim -(bdimg%nlast_recr +1)
          READ(bdimg%num)
       ENDDO
       ! skip useless time blocks
       DO jj=1,idstp -1 
          DO ji=1, bdimg%nzfile*bdimg%ndim + 1
             READ(bdimg%num)
          ENDDO
       ENDDO
       ! read ad-hoc time
       READ(bdimg%num) bdimg%time
       ! Jump to irecr, skipping records
       DO ji=1,irecr
          READ(bdimg%num)
       ENDDO
       ! read data
       READ(bdimg%num) ((plocal_data(ji,jj), ji=1,bdimg%nxfile), jj=1,bdimg%nyfile )
    ELSE

       ! backward motion : need to re-synchronize at the beginning of the file
       REWIND(bdimg%num)

       DO ji=1,irect
          READ(bdimg%num)
       ENDDO

       READ(bdimg%num) bdimg%time

       DO ji=1,irecr
          READ(bdimg%num)
       ENDDO

       READ(bdimg%num) ((plocal_data(ji,jj), ji=1,bdimg%nxfile), jj=1,bdimg%nyfile )
    ENDIF

    ! save current cursor position for next access
    bdimg%nlast_time  = ktstep
    bdimg%nlast_layer = klev
    bdimg%nlast_dim   = kdim
    bdimg%nlast_rect  = irect
    bdimg%nlast_recr  = irecr

  END SUBROUTINE BimgGetLayer


  SUBROUTINE DirectGetLayer (bdimg, plocal_data, ktstep, klev, kdim)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE DirectGetLayer  ***
    !!
    !! ** Purpose :  Direct access read, standard f90 statements 
    !!
    !! ** Method  :  The file stays open between 2 call, and we assume that
    !!               the data pointer has not been moved between the 2 calls
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),           INTENT(inout) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: plocal_data  ! NXX x NYY
    INTEGER(KIND=4),               INTENT(in) :: ktstep
    INTEGER(KIND=4),               INTENT(in) :: klev
    INTEGER(KIND=4),               INTENT(in) :: kdim

    INTEGER(KIND=4)   :: ji,jj
    INTEGER(KIND=4)   :: irec
    INTEGER(KIND=4)   :: idstp 
    !!----------------------------------------------------------------------
    irec=2 +(ktstep-1)*bdimg%nzfile*bdimg%ndim + (klev -1)*bdimg%ndim +(kdim -1)
    PRINT *,'  read record ',irec, ktstep, klev, kdim

    READ(bdimg%num,rec=irec) ((plocal_data(ji,jj), ji=1,bdimg%nxfile), jj=1,bdimg%nyfile)

    ! save current cursor position for next access ( for compatibility)
    bdimg%time        = bdimg%timea(ktstep)        
    bdimg%nlast_time  = ktstep
    bdimg%nlast_layer = klev
    bdimg%nlast_dim   = kdim

  END SUBROUTINE DirectGetLayer


  SUBROUTINE BimgGetGridInfo (bdimg, bdgrid)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgGetGridInfo  ***
    !!
    !! ** Purpose :   Read a bimg gridfile, and fill the corresponding
    !!                bimg data structure. Fill in global xygr array
    !!
    !! ** Method  :  At this level, the file is already open and header
    !!               is already read and reported to the data structure.
    !!                
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg
    TYPE( bimgfile ),    INTENT(in) :: bdgrid

    INTEGER(KIND=4)   :: ji, jj
    REAL(KIND=4)      :: ztdum
    LOGICAL, SAVE     :: llprint = .FALSE.
    !!----------------------------------------------------------------------
    nigr=bdimg%nxfile
    njgr=bdimg%nyfile

    ALLOCATE (work_array (NXX,NYY) )
    ! read the whole file
    READ(bdgrid%num) ztdum
    ! Lon (x,y)
    READ(bdgrid%num) ((work_array(ji,jj),ji=1,nigr), jj=1,njgr)

    xygr(1:nigr,1:njgr,1) = work_array(1:nigr,1:njgr)
    bdimg%d_xgrid(1:nigr) = work_array(1:nigr,1     )

    ! Lat(x,y)
    READ(bdgrid%num) ((work_array(ji,jj),ji=1,nigr), jj=1,njgr)

    xygr(1:nigr,1:njgr,2) = work_array(1:nigr,1:njgr)
    bdimg%d_ygrid(1:njgr) = work_array(1     ,1:njgr)

    ! fill xygr extra halo with extrapolated values. This is usefill for coupe
    ! and CalculateCutPlane, particularly
    DO ji=1,nigr
       xygr(ji,0     ,1) = 2*xygr(ji,1   ,1) - xygr(ji,2     ,1)
       xygr(ji,0     ,2) = 2*xygr(ji,1   ,2) - xygr(ji,2     ,2)
       xygr(ji,njgr+1,1) = 2*xygr(ji,njgr,1) - xygr(ji,njgr-1,1)
       xygr(ji,njgr+1,2) = 2*xygr(ji,njgr,2) - xygr(ji,njgr-1,2)
    ENDDO

    DO jj=1,njgr
       xygr(0     ,jj,1) = 2*xygr(1   ,jj,1) - xygr(2     ,jj,1)
       xygr(0     ,jj,2) = 2*xygr(1   ,jj,2) - xygr(2     ,jj,2)
       xygr(nigr+1,jj,1) = 2*xygr(nigr,jj,1) - xygr(nigr-1,jj,1)
       xygr(nigr+1,jj,2) = 2*xygr(nigr,jj,2) - xygr(nigr-1,jj,2)
    ENDDO

    ! update data structure with exact position read from the grid file
    bdimg%ngrid  = opt_clrgrid

    bdimg%x1mod = bdimg%d_xgrid(1           )
    bdimg%x2mod = bdimg%d_xgrid(bdimg%nxdata)
    bdimg%y1mod = bdimg%d_ygrid(1           )
    bdimg%y2mod = bdimg%d_ygrid(bdimg%nydata)

    IF (opt_clrgrid == 3) THEN
       ! In this case of totaly irregular grid, min and max of the grid
       ! must be determined scanning all the grid points

       ! option -360 : add 360 degrees to negative longitudes
       IF (opt_360  ==  1 ) THEN
          PRINT *,' OPTION 360 ON'
          WHERE ( xygr(1:nigr,1:njgr,1) < 0. ) 
             xygr(1:nigr,1:njgr,1) = xygr(1:nigr,1:njgr,1) + 360.
          ENDWHERE
       ENDIF

       ! compute min and  max
       bdimg%x1mod = MINVAL ( xygr(1:nigr,1:njgr,1) )
       bdimg%x2mod = MAXVAL ( xygr(1:nigr,1:njgr,1) )
       bdimg%y1mod = MINVAL ( xygr(1:nigr,1:njgr,2) )
       bdimg%y2mod = MAXVAL ( xygr(1:nigr,1:njgr,2) )

       IF ( .NOT. llprint .AND. lo_debug ) THEN
          llprint = .false.
          PRINT *,'#GGI  Get Grid Info '
          PRINT *,'#GGI  Limits of irregular grid :'
          PRINT *,'#GGI    x1mod=',bdimg%x1mod
          PRINT *,'#GGI    x2mod=',bdimg%x2mod
          PRINT *,'#GGI    y1mod=',bdimg%y1mod
          PRINT *,'#GGI    y2mod=',bdimg%y2mod
       ENDIF

    ENDIF

    DEALLOCATE ( work_array )

  END SUBROUTINE BimgGetGridInfo


  SUBROUTINE BimgMaskData (bdimg, ptomask)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgMaskData  ***
    !!
    !! ** Purpose :  Set masked value  to spval 
    !!
    !! ** Method  :  Mask is already available in the data structure,
    !!               in %d_mask(:,:) from ReadMask in checkfiles.F90
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),              INTENT(in) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: ptomask  ! NXX x NYY 

    INTEGER(KIND=4)       :: inx, iny  ! used to ease code reading
    !!----------------------------------------------------------------------
    inx = bdimg%nxfile
    iny = bdimg%nyfile

    SELECT CASE ( opt_clrmask )
     CASE ( 1 )  ! mask DYNAMO  1 = land, 0 = sea
       WHERE ( bdimg%d_mask(1:inx,1:iny) == 1.0 ) ptomask(1:inx,1:iny) = bdimg%spval

     CASE ( 2 )  ! mask OPA     1 = sea, 0 = land
       WHERE ( bdimg%d_mask(1:inx,1:iny) == 0.0 ) ptomask(1:inx,1:iny) = bdimg%spval

     END SELECT

  END SUBROUTINE BimgMaskData


  SUBROUTINE BimgShift (bdimg, plocal_data, pshift, pcoords)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgShift  ***
    !!
    !! ** Purpose :  Shift the map eastward (>0)  by pshift degrees
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),                INTENT(in) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: plocal_data ! NXX x NYY
    REAL(KIND=4),                 INTENT(inout) :: pshift
    REAL(KIND=4), DIMENSION(:),   INTENT(inout) :: pcoords     ! 4

    INTEGER(KIND=4)             :: jj
    REAL(KIND=4),DIMENSION(NXX) :: ztmp    ! temporary
    INTEGER(KIND=4)             :: inx, iny
    INTEGER(KIND=4)             :: i1
    !!----------------------------------------------------------------------
    inx = bdimg%nxfile
    iny = bdimg%nyfile

    PRINT *,' SHIFT of ', pshift, ' required '

    IF ( pshift > 0 ) THEN
       i1 = NINT(pshift/bdimg%dx) + 1  ! assume %dx in deg for regular mesh
                                       ! It is likely not the case for %grid = 3
                                       ! (totaly irregular grid !) 
       DO jj=1,iny
          ! save a buffer of i1-1 points width
          ztmp(1:i1-1) = plocal_data(1:i1-1,jj)

          ! shift to the left
          plocal_data(1:inx-i1+1,jj) = plocal_data(i1:inx,jj)

          ! patch ztmp an the end
          plocal_data(inx-i1+2:inx,jj) = ztmp (1:i1-1) 
       ENDDO

       !  negative shift 
    ELSE IF (pshift < 0) THEN
       i1 = inx + NINT(pshift/bdimg%dx)  ! assume %dx in deg for regular mesh
                                         ! It is likely not the case for %grid = 3
                                         ! (totaly irregular grid !)
       DO jj=1,iny
          ! save a buffer of ji=1,inx-i1
          ztmp(1:inx-i1) = plocal_data(i1+1:inx,jj)

          ! shift to the right
          plocal_data(inx:inx-i1+1:-1,jj) = plocal_data(i1:1:-1,jj)

          ! patch ztmp an the begining
          plocal_data(1:inx-i1,jj) = ztmp(1:inx-i1)
       ENDDO
    ENDIF

    ! return the true pshift to calling program
    pshift = NINT(pshift/bdimg%dx) * bdimg%dx
    PRINT *,'     Really performed : ',   pshift

  END SUBROUTINE BimgShift


  SUBROUTINE BimgShiftGrid (bdimg, pshift)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgShiftGrid  ***
    !!
    !! ** Purpose :   Shift the grid the same amount as the data
    !!
    !! ** Method  :  We suppose that the data have already been shifted.
    !!               Therefore, pshit is the true shift
    !!               We also set a flag (%nshift=1) to shift the grid only
    !!               once !
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg
    REAL(KIND=4),        INTENT(in) :: pshift
    INTEGER(KIND=4)     :: inx
    !!----------------------------------------------------------------------
    inx = bdimg%nxfile
    IF (bdimg%nshift /= 1) THEN
       PRINT *,' SHIFT of irregular grid '
       bdimg%nshift=1
       bdimg%d_xgrid(1:inx) = bdimg%d_xgrid(1:inx) + pshift
    ENDIF
  END SUBROUTINE BimgShiftGrid

  INTEGER(KIND=4) FUNCTION IsDirect(cd_fname)
    !!---------------------------------------------------------------------
    !!                  ***  Function IsDirect  ***
    !!
    !! ** Purpose :   Determine the type of the file given in argument
    !!
    !! ** Method  :  Try to decode the first 4 char of the binary file. 
    !!               For dimg, it looks like @1xx where xx is a version number
    !!               For NetCdf it is just the string CDF
    !!               For bimg, nothing sensible ...
    !!               Return the record length for dimg file, -1 for CDF file or 
    !!               0 for other type (and we assume that this is bimg )
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_fname
    CHARACTER(LEN=4)     :: clver
    CHARACTER(LEN=80)    :: clheader
     
    INTEGER(KIND=4)      :: irecl, ilu = 110
    !!----------------------------------------------------------------------
    OPEN(ilu, FILE   = cd_fname,      &
      &       FORM   = 'UNFORMATTED', &
      &       ACCESS = 'DIRECT',      &
      &       RECL   = 88)
    READ(ilu,REC=1) clver,clheader,irecl
    PRINT *,'VER',clver
    CLOSE(ilu)
    !
    IF (clver == '@!01' .OR. clver == '@!02'  .OR. clver == '@!03' ) THEN
       IsDirect = irecl
    ELSEIF (clver(1:3) == 'CDF' .OR. clver(2:4) == 'HDF') THEN
       IsDirect = -1
    ELSE
       IsDirect = 0
    END IF

  END FUNCTION IsDirect

  SUBROUTINE BimgAlloc(bdimg)
    TYPE( bimgfile ), INTENT(inout) :: bdimg

    ALLOCATE ( bdimg%depth(NA) )
    ALLOCATE ( bdimg%timea(Nmaxtime) )
    ALLOCATE ( bdimg%d_mask(NXX,NYY) )
    ALLOCATE ( bdimg%d_xgrid(NXX) )
    ALLOCATE ( bdimg%d_ygrid(NYY) )
  END SUBROUTINE BimgAlloc

  SUBROUTINE BimgDeAlloc(bdimg)
    TYPE( bimgfile ), INTENT(inout) :: bdimg

    DEALLOCATE ( bdimg%depth )
    DEALLOCATE ( bdimg%timea )
    DEALLOCATE ( bdimg%d_mask  )
    DEALLOCATE ( bdimg%d_xgrid )
    DEALLOCATE ( bdimg%d_ygrid )
  END SUBROUTINE BimgDeAlloc


END MODULE readbimg
