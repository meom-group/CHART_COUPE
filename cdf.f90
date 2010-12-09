MODULE cdf
  !!======================================================================
  !!                     ***  MODULE  cdf  ***
  !! All CDF related subroutine are in this module
  !! debug information starts with #C 
  !!=====================================================================
  !! History : 5.0  !  10/2001  J.M. Molines  : original code
  !!         : 7.0  !  11/2010  J.M. Molines  : F90 and Doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  USE netcdf
  USE modcom

  IMPLICIT NONE

  PRIVATE

  INTEGER(KIND=4),PARAMETER  :: jpstand = 14

  CHARACTER(LEN=80), POINTER :: clat, clon, cdepth, ctime, cdim
  CHARACTER(LEN=80), POINTER :: ctitle ,ccom1, ccom2, ccom3, ccom4
  CHARACTER(LEN=80), POINTER :: cmiss, clongname, cunits, clonxy, clatxy
  CHARACTER(LEN=80), DIMENSION(jpstand), TARGET :: cstandlist

  PUBLIC :: PrintBimgStructure
  PUBLIC :: CdfOpen
  PUBLIC :: CdfHeader
  PUBLIC :: CdfGetLayer

  PRIVATE :: ERR_HDL
  PRIVATE :: cmodif

CONTAINS

  SUBROUTINE CdfInit()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CdfInit  ***
    !!
    !! ** Purpose :  Initialize cdf variable name at IOIPSL standards
    !!
    !! ** Method  :  Use an array for holding the standard name and pointer
    !!              that points on any particular name for readibility
    !!
    !!----------------------------------------------------------------------
    !
    IF (opt_debug == 1 ) PRINT *,'#CDF CdfInit ...'

    ! standard names 
    cstandlist(1 )= 'x'             ; clon      => cstandlist(1 )
    cstandlist(2 )= 'y'             ; clat      => cstandlist(2 )
    cstandlist(3 )= 'deptht'        ; cdepth    => cstandlist(3 )
    cstandlist(4 )= 'time_counter'  ; ctime     => cstandlist(4 )
    cstandlist(5 )= 'dim'           ; cdim      => cstandlist(5 )
    cstandlist(6 )= 'Conventions'   ; ctitle    => cstandlist(6 )
    cstandlist(7 )= 'file_name'     ; ccom2     => cstandlist(7 )
    cstandlist(8 )= 'production'    ; ccom3     => cstandlist(8 )
    cstandlist(9 )= 'TimeStamp'     ; ccom4     => cstandlist(9 )
    cstandlist(10)= 'missing_value' ; cmiss     => cstandlist(10)
    cstandlist(11)= 'long_name'     ; clongname => cstandlist(11)
    cstandlist(12)= 'units'         ; cunits    => cstandlist(12)
    cstandlist(13)= 'nav_lon'       ; clonxy    => cstandlist(13)
    cstandlist(14)= 'nav_lat'       ; clatxy    => cstandlist(14)

    IF (opt_debug == 1 ) PRINT *,'#CDF CdfInit Done.'
    !
  END SUBROUTINE CdfInit
  !
  INTEGER(KIND=4) FUNCTION CdfOpen(cd_filename, bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION CdfOpen  ***
    !!
    !! ** Purpose :    Open a NetCdf File for Chart and Coupe and fills up what
    !!              he can in the bimg structure.
    !!
    !! ** Method  :   call CdfInit for standard name and look for modifier if any
    !!           then look for the general content of the file, and report it to
    !!           the data structure. Do a second call to CdfInit in order to restore
    !!           standard name for next file to be read. Return the ncid.
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(in) ::  cd_filename
    TYPE( bimgfile ), INTENT(inout) ::  bdimg

    INTEGER(KIND=4)  ::  istatus, id_nc, indims, invars, ingatts, iunlimdimid
    !!----------------------------------------------------------------------

    CALL CdfInit
    IF ( opt_debug == 1 )  THEN 
      PRINT *, '#CDF In CdfOpen ...', TRIM(bdimg%cmodifier), TRIM(cd_filename)
    ENDIF

    IF ( bdimg%cmodifier /= 'none' ) CALL cmodif (bdimg)

    istatus=NF90_OPEN(cd_filename,NF90_NOWRITE,id_nc)
    CALL ERR_HDL(istatus)

    bdimg%ncid=id_nc
    ! ... Inquire general info on file
    istatus=NF90_INQUIRE(id_nc,indims,invars,ingatts,iunlimdimid)
    CALL ERR_HDL(istatus)

    bdimg%ndims        = indims
    bdimg%nvars        = invars
    bdimg%ngatts       = ingatts
    bdimg%nunlimdimid  = iunlimdimid

    CALL CdfInit
    CdfOpen=id_nc

    IF ( opt_debug == 1 )  PRINT *, '#CDF Done.'

  END FUNCTION CdfOpen


  SUBROUTINE CdfHeader (bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CdfHeader  ***
    !!
    !! ** Purpose :   Read the header of  NetCdf File for Chart and Coupe and fills up what
    !!     he can in the bimg structure.
    !!
    !! ** Method  :   This routine is quite complicated as it manage different type of
    !!             grid etc ...
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg
    !
    INTEGER(KIND=4) ::  istatus, id_nc                  ! error status, ncid
    INTEGER(KIND=4) ::  indims, invars                  ! number of dims, variables
    INTEGER(KIND=4) ::  id_dimdep, id_dimtim, id_dimdim !  id of dimensions
    INTEGER(KIND=4) ::  id_dimlat, id_dimlon            !   "        "
    INTEGER(KIND=4) ::  id_var, id_dep, id_tim          !  id of variables
    INTEGER(KIND=4) ::  idimv             ! number of dimensions for the variable
    INTEGER(KIND=4) ::  ier               ! error count
    INTEGER(KIND=4) ::  ji, jj, jvar  ! dummy loop index
    INTEGER(KIND=4) ::  ixf ,iyf      ! a local variable for %nxfile, %nyfile
    
    !
    CHARACTER(LEN=80) :: clvar, clnam, clongn
    !
    LOGICAL         :: llflag, ll_nodepth 
    LOGICAL         :: ll_print = .TRUE.       ! flag to print only once the grid.
    !
    REAL            :: zspval
    !!----------------------------------------------------------------------
    IF (lo_debug ) PRINT *,'#CRH  In ReadHeader', TRIM(bdimg%cmodifier)

    IF ( bdimg%cmodifier /= 'none' ) CALL cmodif (bdimg)
    clongn=' '

    id_nc  = bdimg%ncid
    invars = bdimg%nvars
    clvar  = bdimg%cvarname

    IF ( lo_debug ) PRINT *,'#CRH',  id_nc, invars, TRIM(clvar)
    istatus=NF90_INQ_VARID (id_nc,clvar,id_var)

    ! ... Enter interactive loop if the variable name is not
    !     given or if it is wrong.
    DO WHILE (istatus == NF90_ENOTVAR) 
       PRINT *,' >>>>> Give the netcdf variable name with :'
       PRINT *,'      option -clrvar ''varname'' '
       PRINT *,'      option -cntvar ''varname'' '
       PRINT *,'      option -vecvarx ''varname'' '
       PRINT *,'      option -vecvary ''varname'' '
       DO jvar = 1, invars
          istatus=NF90_INQUIRE_VARIABLE (id_nc, jvar, name=clnam)
          CALL ERR_HDL(istatus)
          istatus=NF90_GET_ATT(id_nc, jvar,clongname, clongn)
          IF ( istatus == NF90_ENOTATT ) THEN
             clongn='unknown'
          ELSE
             CALL ERR_HDL(istatus)
          END IF
          PRINT '(i4,3h : ,a15,3h : ,a)',jvar,TRIM(clnam), TRIM(clongn)
          clongn=' '
       END DO
       PRINT *,' >>>>> Enter the number corresponding to your variable ... '
       READ *, id_var
       istatus=NF90_INQUIRE_VARIABLE (id_nc, id_var, name=clnam) 
       !        CALL ERR_HDL(istatus)
       bdimg%cvarname=clnam
       clvar=clnam
    END DO
    !
    bdimg%nvarid=id_var
    !  Get informations on the required variable
    ! ...  Attributes ...
    istatus=NF90_GET_ATT (id_nc, bdimg%nvarid,clongname,bdimg%cstr1) 
    istatus=NF90_GET_ATT (id_nc, bdimg%nvarid,cunits,bdimg%cstr2) 
    istatus=NF90_GET_ATT (id_nc,NF90_GLOBAL,ccom2,bdimg%cstr3)
    bdimg%cstr4=' From a NetCdf File'
    ! ...  Number of dimensions ...
    istatus=NF90_INQUIRE_VARIABLE (id_nc,id_var,ndims=idimv)
    IF (idimv <  2 ) THEN
       PRINT *, '>>> variable : ',TRIM(clvar)
       PRINT *,' >>> this variable is not mapable: less than 2D ...'
       STOP
    END IF
    bdimg%ndimv = idimv
    ! ... special value (missing value/fill value)
    istatus=NF90_GET_ATT(id_nc,id_var,cmiss,zspval)
    IF ( istatus == NF90_ENOTATT ) THEN
       PRINT *, ' missing value not defined, take default value '
       zspval=rp_defspval  ! defined in modparam.f90 
    END IF
    bdimg%spval = zspval
    ! ... size of the horizontal 2D fields (nxfile, nyfile)
    ! ...... latitude or y or j direction  named clat
    IF ( lo_debug ) PRINT *,'#CRH Looking for ', TRIM(clat),' dimension'
    istatus=NF90_INQ_DIMID(id_nc,clat,id_dimlat)
    CALL ERR_HDL(istatus) 

    istatus=NF90_INQUIRE_DIMENSION(id_nc, id_dimlat, len=iyf)
    CALL ERR_HDL(istatus) 
    bdimg%nyfile=iyf
    IF ( lo_debug ) PRINT *,'#CRH  ... found ', bdimg%nyfile

    ! ...... longitude  or x or i direction named clon
    IF ( lo_debug ) PRINT *,'#CRH Looking for ', TRIM(clon),' dimension'
    istatus=NF90_INQ_DIMID(id_nc,clon,id_dimlon)
    CALL ERR_HDL(istatus)

    istatus=NF90_INQUIRE_DIMENSION(id_nc, id_dimlon, len=ixf)
    CALL ERR_HDL(istatus)
    bdimg%nxfile=ixf

    IF ( lo_debug ) PRINT *, '#CRH  ... found ', bdimg%nxfile

    bdimg%nxdata = bdimg%nxfile   ! at this level  nxdata and nxfile
    bdimg%nydata = bdimg%nyfile   ! are equal ...
    !
    ! ... Number of vertical levels, set to 1 if nothing found
    !
    ll_nodepth = .FALSE.
    istatus=NF90_INQ_DIMID(id_nc,cdepth,id_dimdep)
    IF (istatus == NF90_EBADDIM ) THEN
       PRINT *,' Only one level assumed ...'
       bdimg%nzfile = 1 ;  bdimg%depth(1) = 0.
       ll_nodepth=.TRUE.
    ELSE
       istatus=NF90_INQUIRE_DIMENSION(id_nc, id_dimdep, len=bdimg%nzfile)
       CALL ERR_HDL(istatus)
    ENDIF
    ! ...  Now check if there is enough space to store the variables
    !      JMM : this will disappear with dynamical allocation
       !
       ier=0
       IF (bdimg%nxfile > NXX) THEN
          PRINT *,'ERROR: NXX too small (Max : ',NXX,')'
          ier=ier + 1 
       ELSE IF (bdimg%nyfile > NYY) THEN
          PRINT *,'ERROR: NYY too small (Max : ',NYY,')'
          ier=ier + 1 
       ELSE IF (bdimg%nzfile > NA) THEN
          PRINT *,'ERROR: NA too small (Max : ',NA,')'
          ier=ier + 1 
       ENDIF

       IF (ier > 0) THEN
         PRINT * , ier,' errors found ..'
         STOP
       ENDIF
       !
     IF ( .NOT. ll_nodepth ) THEN  ! there is a depth variable to read
       istatus=NF90_INQ_VARID(id_nc,cdepth,id_dep)
       CALL ERR_HDL(istatus)

       istatus=NF90_GET_VAR(id_nc, id_dep, bdimg%depth, start=(/1/), count=(/bdimg%nzfile/))
       CALL ERR_HDL(istatus)
    END IF

    ! ... depth are forced to be negative 
       bdimg%depth(1:bdimg%nzfile) = -1. * ABS ( bdimg%depth(1:bdimg%nzfile))
    !
    ! ... Number of time steps, 1 if not defined
    !
    istatus=NF90_INQ_DIMID(id_nc,ctime,id_dimtim)
    IF (istatus == NF90_EBADDIM ) THEN
       PRINT *,' Only one time assumed ...'
       bdimg%nt = 1  ;   bdimg%timea(1) = 0.
    ELSE
       istatus=NF90_INQUIRE_DIMENSION(id_nc, id_dimtim, len=bdimg%nt)
       CALL ERR_HDL(istatus)
       !
       istatus=NF90_INQ_VARID(id_nc,ctime,id_tim)
       IF ( istatus == NF90_NOERR ) THEN
       !
          istatus=NF90_GET_VAR(id_nc, id_tim, bdimg%timea, start=(/1/),count=(/bdimg%nt/))
          CALL ERR_HDL(istatus)
       ELSE 
         IF ( bdimg%nt == 1 ) THEN
            PRINT *, 'No time variable ',TRIM(ctime),' found. Put 0 '
            bdimg%timea(1) = 0
         ELSE
            PRINT *, 'No time variable ',TRIM(ctime),' found. More than 1 time ..;'
          CALL ERR_HDL(istatus)
         ENDIF
       ENDIF
    END IF
    !
    ! ... Number of dimension per levels (likely to become obsolete, this is a remnant 
    !       of bimg/dimg format ). Set to 1 if nothing sensible found
    !
    istatus=NF90_INQ_DIMID(id_nc,cdim,id_dimdim)
    IF (istatus == NF90_EBADDIM ) THEN
       PRINT *,' Only one dim assumed ...'
       bdimg%ndim = 1
    ELSE
       istatus=NF90_INQUIRE_DIMENSION(id_nc, id_dimdim, len=bdimg%ndim)
       CALL ERR_HDL(istatus)
    END IF
    bdimg%icod = 0   ! also obsolete 
    !
    IF (opt_ijgrid == 1 ) THEN
       ! ... Use ij coordinates
       bdimg%x1mod = 1
       bdimg%y1mod = 1
       bdimg%dx = 1
       bdimg%dy = 1
       bdimg%ngrid = 0
       bdimg%mask = 0
       bdimg%x2mod = bdimg%nxfile
       bdimg%y2mod = bdimg%nyfile
    ELSE IF ( opt_clrgrid /= 0 ) THEN   ! an external grid file has been specified
       !  external grid file will be read later
       bdimg%ngrid = opt_clrgrid
    ELSE  
       ! ... Read the geographical coordinates from cdf File
       CALL CdfXYgr (bdimg,bdimg%nxfile,bdimg%nyfile)
       opt_print = 1
       ! in the following use ixf, iyf in place of bdimg%nxfile, bdimg%nyfile for 
       ! the sake of lisibility
       ! add an extra halo at xygr for easier interpolation procedure
       ! extra halo is extrapolated
       ! ... i-direction
       DO ji=1,ixf
          xygr(ji,0,1)=2*xygr(ji,1,1) - xygr(ji,2,1)
          xygr(ji,0,2)=2*xygr(ji,1,2) - xygr(ji,2,2)
          xygr(ji,iyf+1,1)= &
          &    2*xygr(ji,iyf,1) - xygr(ji,iyf-1,1)
          xygr(ji,iyf+1,2)= &
          &    2*xygr(ji,iyf,2) - xygr(ji,iyf-1,2)
       ENDDO
       ! ... j-direction
       DO jj=1,iyf
          xygr(0,jj,1)    =2*xygr(1,jj,1) - xygr(2,jj,1)
          xygr(0,jj,2)    =2*xygr(1,jj,2) - xygr(2,jj,2)
          xygr(ixf+1,jj,1)= &
          &    2*xygr(ixf,jj,1) - xygr(ixf-1,jj,1)
          xygr(ixf+1,jj,2)= &
          &    2*xygr(ixf,jj,2) - xygr(ixf-1,jj,2)
       ENDDO
       ! In this case the grid can be completely irregular and maximum 
       ! and minimum latitude (y1mod, y2mod)  and longitude (x1mod, x2mod)
       ! are determined by scanning all grid points
       !
       ! initialize
       bdimg%x1mod =  99999.0
       bdimg%y1mod =  99999.0
       bdimg%x2mod = -99999.0
       bdimg%y2mod = -99999.0
       ! When -360 option is used, 360 deg must be added to negative longitudes
       IF (opt_360 == 1 ) THEN
          PRINT *,' OPTION 360 ON'
          WHERE (  xygr(1:ixf,1:iyf,1) < 0 ) 
            xygr(1:ixf,1:iyf,1) = xygr(1:ixf,1:iyf,1) +360.
          END WHERE
       ENDIF
       ! compute minimum and maximum : a F90 statement will be used when dynamic allocation
       bdimg%x1mod=MINVAL( xygr(1:ixf,1:iyf,1))
       bdimg%y1mod=MINVAL( xygr(1:ixf,1:iyf,2))
       bdimg%x2mod=MAXVAL( xygr(1:ixf,1:iyf,1))
       bdimg%y2mod=MAXVAL( xygr(1:ixf,1:iyf,2))

       ! ... check if the grid is really an irregular grid 
       llflag=.TRUE.   ! llflag will be F if the grid is irregular
       nigr=bdimg%nxfile
       njgr=bdimg%nyfile
       !
       IF (nigr /= 1 .AND. njgr /= 1 ) THEN
          DO ji=1,bdimg%nxfile
             DO jj=1,bdimg%nyfile
                IF ( xygr(ji,jj-1,1) /= xygr(ji,jj,1) ) llflag = ( llflag .AND. .FALSE. )
             END DO
          END DO

          DO jj=1,bdimg%nyfile
             DO ji=1,bdimg%nxfile
                IF ( xygr(ji-1,jj,2) /= xygr(ji,jj,2) ) llflag = ( llflag .AND. .FALSE. )
             END DO
          END DO
       ENDIF
       ! JMM add option -forcexy
       IF ( opt_forcexy  == 1 ) THEN 
          IF ( llflag ) PRINT *, ' -forcexy active : llflag set to FALSE '
          llflag=.FALSE.
       ENDIF

       IF (llflag) THEN 
          bdimg%ngrid=1
       ELSE
          bdimg%ngrid=3
       END IF

       PRINT *,' BIMG.GRID = ', bdimg%ngrid
       bdimg%dx= (xygr(bdimg%nxfile,1,1)-xygr(1,1,1)) / bdimg%nxfile
       !     bdimg%dx = 1
       bdimg%dy = 1

       IF ( ll_print ) THEN
          ll_print = .FALSE.
          PRINT *,' LIMITES DE LA GRILLE IRREGULIERE:'
          PRINT *,'   x1mod=',bdimg%x1mod
          PRINT *,'   x2mod=',bdimg%x2mod
          PRINT *,'   y1mod=',bdimg%y1mod
          PRINT *,'   y2mod=',bdimg%y2mod
       ENDIF
    END IF
    !
    bdimg%nlast_time  = 0
    bdimg%nlast_layer = bdimg%nzfile
    bdimg%nlast_dim   = bdimg%ndim
    bdimg%nlast_rect  = 10000000
    bdimg%nlast_recr  = 0          

    CALL CdfInit
    IF ( opt_debug == 1 )  PRINT *,'#CDF  ReadHeader done.'
  END SUBROUTINE CdfHeader


  SUBROUTINE CdfXYgr (bdimg,kx,ky)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CdfXY  ***
    !!
    !! ** Purpose :   Read the coordinates in the header of NetCdf files
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg
    INTEGER(KIND=4),     INTENT(in) :: kx, ky

    INTEGER(KIND=4) ::  istatus, id_nc
    INTEGER(KIND=4) ::  id_var, id_dim(2)
    INTEGER(KIND=4) ::  id_dimlon, id_dimlat
    INTEGER(KIND=4) ::  ipos_lon, ipos_lat
    INTEGER(KIND=4) ::  istart(10),icount(10)
    INTEGER(KIND=4) ::  ji,jj
    REAL(KIND=4), DIMENSION(kx,ky) ::  zxyloc
    !!----------------------------------------------------------------------
    IF ( lo_debug ) PRINT *,'#CXY  In CdfXYgr', bdimg%cmodifier
    IF ( bdimg%cmodifier /= 'none' ) CALL cmodif (bdimg)
    id_nc=bdimg%ncid
    !
    ! ... clonxy  --> xygr(,,1)
    id_var    = 0 
    id_dim(:) = 0
    id_dimlon = 0
    id_dimlat = 0
    ! 
    istatus=NF90_INQ_VARID(id_nc,clonxy,id_var)
    CALL ERR_HDL(istatus)
    istatus=NF90_INQUIRE_VARIABLE(id_nc,id_var, dimids=id_dim)
    CALL ERR_HDL(istatus)    
    !
    istatus=NF90_INQ_DIMID(id_nc,clon,id_dimlon)
    CALL ERR_HDL(istatus)
    !
    istatus=NF90_INQ_DIMID(id_nc,clat,id_dimlat)
    CALL ERR_HDL(istatus)  
    !
    IF ( id_dim(2) == 0 ) THEN
       ! In this case, lon(ji) only ( 1D array )
       ipos_lon=1
       ipos_lat=2
       istart (ipos_lon) = 1
       icount (ipos_lon) = bdimg%nxfile
       istart (ipos_lat) = 1
       icount (ipos_lat) = 1
       istatus=NF90_GET_VAR(id_nc,id_var,zxyloc, start=(/1,1/),count=(/bdimg%nxfile,1/))
       CALL ERR_HDL(istatus)
       DO ji=1,bdimg%nxfile
          DO jj=1,bdimg%nyfile
             xygr(ji,jj,1) = zxyloc(ji,1)     
          END DO
       END DO
       bdimg%dx= (zxyloc(bdimg%nxfile,1)-zxyloc(1,1)) / bdimg%nxfile
    ELSE
       ! General case
       DO ji = 1,10
          istart(ji) = 1
          icount(ji) = 1
       END DO
       DO ji = 1 , 2
          IF      (id_dim(ji) == id_dimlon ) THEN
             ipos_lon = ji
          ELSE IF (id_dim(ji) == id_dimlat ) THEN
             ipos_lat = ji
          END IF
       END DO
       istart (ipos_lon) = 1
       icount (ipos_lon) = bdimg%nxfile
       istart (ipos_lat) = 1
       icount (ipos_lat) = bdimg%nyfile                                           
       istatus=NF90_GET_VAR(id_nc,id_var,zxyloc,start=(/1,1/) ,count=(/bdimg%nxfile,bdimg%nyfile/))
       CALL ERR_HDL(istatus)
       DO ji=1,bdimg%nxfile
          DO jj=1,bdimg%nyfile
             xygr(ji,jj,1) = zxyloc(ji,jj)     
          END DO
       END DO
    ENDIF
    !
    ! ... clatxy  --> xygr(,,2)
    !
    IF ( id_dim(2) == 0 ) THEN
       !   In this case lat(jj) only ( 1D array )
       ipos_lon=1
       ipos_lat=2
       istart (ipos_lon) = 1
       icount (ipos_lon) = bdimg%nyfile
       istart (ipos_lat) = 1
       icount (ipos_lat) = 1
       PRINT *, istart, icount
       istatus=NF90_INQ_VARID(id_nc,clatxy,id_var)
       CALL ERR_HDL(istatus)
       istatus=NF90_GET_VAR(id_nc,id_var,zxyloc, start=(/1,1/),count=(/1,bdimg%nyfile/))
       CALL ERR_HDL(istatus)
       DO ji=1,bdimg%nxfile
          DO jj=1,bdimg%nyfile
             xygr(ji,jj,2) = zxyloc(jj,1)
          END DO
       END DO
    ELSE
       ! General case

       istatus=NF90_INQ_VARID(id_nc,clatxy,id_var)
       CALL ERR_HDL(istatus)
       istatus=NF90_GET_VAR(id_nc,id_var,zxyloc,start=(/1,1/) ,count=(/bdimg%nxfile,bdimg%nyfile/))
       CALL ERR_HDL(istatus)

       DO ji=1,bdimg%nxfile
          DO jj=1,bdimg%nyfile
             xygr(ji,jj,2) = zxyloc(ji,jj)
          END DO
       END DO
    ENDIF

    DO ji =1,bdimg%nxfile
       bdimg%d_xgrid(ji)= xygr(ji,1,1)
    END DO
    !
    DO jj =1,bdimg%nyfile
       bdimg%d_ygrid(jj)= xygr(1,jj,2)
    END DO
    !    
    CALL CdfInit
    IF ( lo_debug ) PRINT *, '#CXY CdfXYgr Done.'
  END SUBROUTINE CdfXYgr


  SUBROUTINE CdfGetLayer (bdimg, plocal_data, ktstp, klev, kdim )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CdfGetLayer  ***
    !!
    !! ** Purpose :  Read the  required layer and update the bimg structure 
    !!
    !! ** Method  :   
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),           INTENT(inout) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(out) :: plocal_data  ! NXX x NYY
    INTEGER(KIND=4),               INTENT(in) :: ktstp
    INTEGER(KIND=4),               INTENT(in) :: klev
    INTEGER(KIND=4),               INTENT(in) :: kdim

    INTEGER(KIND=4)                :: istatus, id_nc
    INTEGER(KIND=4)                :: id_var
    INTEGER(KIND=4)                :: id_dimlon, id_dimlat, id_dimdep, id_dimtim, id_dimdim
    INTEGER(KIND=4)                :: istart(10), icount(10), id_dim(10)
    INTEGER(KIND=4)                :: ji, jj
    INTEGER(KIND=4)                :: ipos_lon, ipos_lat, ipos_dep, ipos_tim, ipos_dim
    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zwork
    !!----------------------------------------------------------------------
    IF ( lo_debug ) PRINT *,'#CGL  In CdfGetLayer ...' , bdimg%cmodifier

    ALLOCATE ( zwork ( bdimg%nxfile, bdimg%nyfile) )
    
    IF ( bdimg%cmodifier /= 'none' ) CALL cmodif (bdimg)
    id_nc     = bdimg%ncid
    id_var    = bdimg%nvarid
    id_dim(:) = 0

    istart(:) = -10
    icount(:) = -10
    !
    istatus=NF90_INQ_DIMID(id_nc,clon,id_dimlon)
    CALL ERR_HDL(istatus)    
    !
    istatus=NF90_INQ_DIMID(id_nc,clat,id_dimlat)
    CALL ERR_HDL(istatus)      
    !
    istatus=NF90_INQ_DIMID(id_nc,cdepth,id_dimdep)
    IF (istatus == NF90_EBADDIM ) THEN
       id_dimdep = -1
    ENDIF
    !
    istatus=NF90_INQ_DIMID(id_nc,ctime,id_dimtim)
    IF (istatus == NF90_EBADDIM ) THEN
       id_dimtim = -1
    END IF
    !
    istatus=NF90_INQ_DIMID(id_nc,cdim,id_dimdim)
    IF (istatus == NF90_EBADDIM ) THEN
       id_dimdim = -1
    END IF
    !
    istatus=NF90_INQUIRE_VARIABLE(id_nc,id_var,dimids=id_dim)
    CALL ERR_HDL(istatus)
    ! look for position of the various dimension in the variable
    ! JMM another way would be to suppose variables are x,y,z,t always
    !     and thus determine the name of the respective dimension !
    ipos_lon=0 ; ipos_lat=0; ipos_dep=0 ; ipos_tim=0 ; ipos_dim=0
    DO ji = 1 , bdimg%ndimv
       IF      (id_dim(ji) == id_dimlon ) THEN
          ipos_lon = ji
       ELSE IF (id_dim(ji) == id_dimlat ) THEN
          ipos_lat = ji
       ELSE IF (id_dim(ji) == id_dimdep ) THEN
          ipos_dep = ji
       ELSE IF (id_dim(ji) == id_dimtim ) THEN
          ipos_tim = ji
       ELSE IF (id_dim(ji) == id_dimdim ) THEN
          ipos_dim = ji
       END IF
    END DO

    !
    istart (ipos_lon) = 1
    icount (ipos_lon) = bdimg%nxfile
    istart (ipos_lat) = 1
    icount (ipos_lat) = bdimg%nyfile
    ! ... if some dimension missing they are assume to be 1
    IF ( ipos_dep  >  0 ) THEN
       istart (ipos_dep) = klev
       icount (ipos_dep) = 1
    END IF
    IF ( ipos_tim  >  0 ) THEN
       istart (ipos_tim) = ktstp
       icount (ipos_tim) = 1
    END IF
    IF ( ipos_dim  >  0 ) THEN
       istart (ipos_dim) = kdim
       icount (ipos_dim) = 1
    END IF
    !
    istatus=NF90_GET_VAR(id_nc,id_var,zwork,start=istart,count=icount)
    CALL ERR_HDL(istatus)          
    DO ji=1,bdimg%nxfile
       DO jj=1,bdimg%nyfile
          plocal_data(ji,jj) = zwork(ji,jj)
       END DO
    END DO
    !
    bdimg%time       = bdimg%timea(ktstp)
    bdimg%nlast_time  = ktstp
    bdimg%nlast_layer = klev
    bdimg%nlast_dim   = kdim   
    !
    CALL CdfInit

    DEALLOCATE ( zwork )
    IF (opt_debug == 1 ) PRINT *, ' CdfGetLayer done.'
    !
  END SUBROUTINE CdfGetLayer


  SUBROUTINE ERR_HDL(kstatus)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ERR_HDL  ***
    !!
    !! ** Purpose :   Error Handler for cdf calls
    !!
    !! ** Method  : Return the Netcdf error message is kstatus /= NF90_NOERR  
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(in) ::  kstatus
    !!----------------------------------------------------------------------
    IF (kstatus /= NF90_NOERR ) THEN
       PRINT *, 'ERROR in NETCDF routine, status=',kstatus
       PRINT *, NF90_STRERROR(kstatus)
       STOP
    END IF
  END SUBROUTINE ERR_HDL


  SUBROUTINE cmodif (bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE cmodif  ***
    !!
    !! ** Purpose : This routine allows to change some dimension or variable
    !!            standard name as specified by -clrmodif for instance
    !!
    !! ** Method  : It scan the standard name list to find the required modification,
    !          and save the new name. The modifier refer to non-standard name, it
    !!         reports an error.
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(in) ::  bdimg

    CHARACTER(LEN=80) :: cltmp, clcur, cltmp2
    CHARACTER(LEN=80) :: cla, clb
    INTEGER(KIND=4)   :: ji, jj, jk
    INTEGER(KIND=4)   :: ii
    INTEGER(KIND=4)   :: inmodif, ipp
    INTEGER(KIND=4)   :: ip(jpstand)
    !!----------------------------------------------------------------------
    ! ... modifier is a string such as 'old1=new1,old2=new2'
    !     Each change is separated from the former by a ,
    !
    IF (lo_debug ) PRINT *,'#CMO  In Cmodif'

    cltmp   = bdimg%cmodifier
    cltmp2  = cltmp
    inmodif = 0
    ipp     = 0

    DO WHILE ( INDEX(cltmp2,',') /= 0 )
       inmodif=inmodif+1
       ip(inmodif)= INDEX(cltmp2,',')+ipp
       ipp=ip(inmodif)
       cltmp2= cltmp(ipp+1:)
    END DO
    !
    ii=1
    DO ji=1,inmodif+1
       IF (ji /= inmodif+1) THEN
          jj=ip(ji)
          clcur=cltmp(ii:jj-1)      
          ii=jj+1
       ELSE
          clcur=cltmp(ii:)
       END IF
       ! ... Detect the '=' symbol
       ipp = INDEX (clcur,'=')
       IF ( ipp == 0 ) THEN
          PRINT *,' Erreur dans le modifier ',clcur 
          STOP
       ELSE
          cla=clcur(1:ipp-1)
          clb=clcur(ipp+1:)
       END IF
       PRINT *,TRIM(cla),'---->', TRIM(clb)

       ! Find the modifier and return once modif done
       DO jk = 1, jpstand
          IF (cla == cstandlist(jk) ) cstandlist(jk) = clb
       END DO
       ! ... loop on next modifier
    END DO
    IF ( lo_debug ) PRINT *,'#CMO cmodif done'

  END SUBROUTINE cmodif


  INTEGER(KIND=4) FUNCTION PrintBimgStructure (bdimg)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION PrintBimgStructure  ***
    !!
    !! ** Purpose :  Print the bimg structure passed as arguments
    !!
    !! ** Method  :   use only with debug2 level (-debug2 )
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(in) :: bdimg
    !!----------------------------------------------------------------------

    PRINT *,'cstr1 : ',TRIM( bdimg%cstr1)
    PRINT *,'cstr2 : ',TRIM( bdimg%cstr2)
    PRINT *,'cstr3 : ',TRIM( bdimg%cstr3)
    PRINT *,'cstr4 : ',TRIM( bdimg%cstr4)
    PRINT *,'cver  : ', bdimg%cver
    PRINT *,'cfname : ',TRIM( bdimg%cfname)
    PRINT *,'nxfile :', bdimg%nxfile
    PRINT *,'nyfile :', bdimg%nyfile
    PRINT *,'nzfile :', bdimg%nzfile
    PRINT *,'nt     :', bdimg%nt  
    PRINT *,'ndim   :', bdimg%ndim  
    PRINT *,'icod   :', bdimg%icod  
    PRINT *,'nxdata :', bdimg%nxdata
    PRINT *,'nydata :', bdimg%nydata
    PRINT *,'nrecl  :', bdimg%nrecl  
    PRINT *,'x1mod  :', bdimg%x1mod  
    PRINT *,'y1mod  :', bdimg%y1mod  
    PRINT *,'x2mod  :', bdimg%x2mod  
    PRINT *,'y2mod  :', bdimg%y2mod  
    PRINT *,'dx     :', bdimg%dx
    PRINT *,'dy     :', bdimg%dy
    PRINT *,'spval  :', bdimg%spval
    PRINT *,'lspval0 :', bdimg%lspval0
    PRINT *,'time   :', bdimg%time
    PRINT *,'num    :', bdimg%num
    PRINT *,'nlast_layer :', bdimg%nlast_layer
    PRINT *,'nlast_dim   :', bdimg%nlast_dim
    PRINT *,'nlast_time  :', bdimg%nlast_time
    PRINT *,'nlast_rect  :', bdimg%nlast_rect
    PRINT *,'nlast_recr  :', bdimg%nlast_recr
    PRINT *,'mask        :', bdimg%mask
    PRINT *,'ngrid       :', bdimg%ngrid
    PRINT *,'nshift      :', bdimg%nshift
!    print *,'d_xgrid   :', bdimg%d_xgrid
!    print *,'d_ygrid   :', bdimg%d_ygrid
    PRINT *,'alphasup  :', bdimg%alphasup
    PrintBimgStructure = 1
    !
  END FUNCTION PrintBimgStructure

END MODULE cdf
