PROGRAM vecrot_opa
  !!======================================================================
  !!                     ***  PROGRAM vecrot_opa  ***
  !! Used as external program in coupe, this program perform a rotation
  !! of an opa vector field giving the tangent and normal velocity.
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
  USE netcdf

  IMPLICIT NONE

  CHARACTER(LEN=80)  ::  cline1, cline2, cline3, cline4
  CHARACTER(LEN=80)  ::  cf_filu, cf_filv
  CHARACTER(LEN=80)  ::  ccom, ctype, cdum
  CHARACTER(LEN=4)   ::  cver

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: u, v, ua, va, urot, vrot
  REAL(KIND=4), DIMENSION(:)  , ALLOCATABLE :: h1d, time_tag1, time

  INTEGER(KIND=4)  :: ji, jj, jk, jtime
  INTEGER(KIND=4)  :: ni, nj, nk, nt
  INTEGER(KIND=4)  :: narg, iargc
  INTEGER(KIND=4)  :: ilui=10, iluo=11, ilvi=12   ! logical unit for I/O
  INTEGER(KIND=4)  :: irecl, irec, irec2
  INTEGER(KIND=4)  :: icod, ndim, idim
  REAL(KIND=4)     :: x1, y1, dx, dy, spval
  REAL(KIND=4)     :: angled, angle, alfa, pi

  ! CDF STUFF
  INTEGER(KIND=4)  :: istatus, ncid1, ncid2
  INTEGER(KIND=4)  :: id_dimx, id_dimy, id_dimz, id_dimt, id_dep1, id_u, id_v, id
  INTEGER(KIND=4)  :: istart(4), icount(4)
  !  CDF OUT
  INTEGER(KIND=4)  :: id_latin, id_lonin, ncout
  INTEGER(KIND=4)  :: id_x, id_y, id_z, id_t, id_lon, id_lat ,id_dep
  INTEGER(KIND=4)  :: id_tim, id_tang, id_norm
  !!----------------------------------------------------------------------
  pi      = ACOS(-1.)
  narg    = iargc()
  IF (narg < 2) THEN
     PRINT *,'USAGE: vecro_opat <angle>  vecU vecV'
     PRINT *,' output on  vecrot.bimg or vecrot.nc >'
     PRINT *,' If output on nc files variables are vtang and vnorm'
     STOP
  END IF
  !
  CALL getarg(1,cdum   )  ;  READ(cdum,*)angled

  angle = angled*pi/180.
  alfa  = angle - pi/2.

  CALL getarg(2,cf_filu)
  CALL getarg(3,cf_filv)

120 FORMAT(a,i3.3)
  OPEN(ilui,file=cf_filu,form='UNFORMATTED', ACCESS='DIRECT',recl=88)
  READ(ilui,rec=1) cver,ccom,irecl
  PRINT *,'file opened:',cf_filu
  PRINT *,cver
  CLOSE(ilui)

  SELECT CASE ( cver(1:3))
  CASE (    '@!0'  )
     ctype='dimg'
     OPEN(ilui,file=cf_filu,form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
     OPEN(ilvi,file=cf_filv,form='UNFORMATTED', ACCESS='DIRECT',recl=88)
     READ(ilvi,rec=1) cver,ccom,irecl
     PRINT *,'file opened:',cf_filv
     PRINT *,cver,ccom,irecl
     CLOSE(ilvi)
     IF (cver /= '@!01' ) THEN
        PRINT *, "Error: U and V files are not of the same type"
        STOP
     END IF

     OPEN(ilvi,file=cf_filv,form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)

     OPEN(iluo,file='vecrot.bimg',form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
  CASE (    'CDF', 'HDF' )
     PRINT *, ' NetCdf File U '
     ctype ='cdf'
     istatus=NF90_OPEN(cf_filu,NF90_NOWRITE,ncid1)
     istatus=NF90_INQ_DIMID(ncid1,'x',id_dimx)
     istatus=NF90_INQ_DIMID(ncid1,'y',id_dimy)
     istatus=NF90_INQ_DIMID(ncid1,'depthu',id_dimz)
     istatus=NF90_INQ_DIMID(ncid1,'time_counter',id_dimt)
     istatus=NF90_INQ_VARID(ncid1,'depthu',id_dep1 )
     istatus=NF90_INQ_VARID(ncid1,'vozocrtx',id_u )
     istatus=NF90_INQUIRE_DIMENSION(ncid1,id_dimx,len=ni)
     istatus=NF90_INQUIRE_DIMENSION(ncid1,id_dimy,len=nj)
     istatus=NF90_INQUIRE_DIMENSION(ncid1,id_dimz,len=nk)
     istatus=NF90_INQUIRE_DIMENSION(ncid1,id_dimt,len=nt)
     ALLOCATE( u(ni,nj), ua(ni,nj), urot(ni,nj) )
     ALLOCATE( v(ni,nj), va(ni,nj), vrot(ni,nj) )
     ALLOCATE( h1d(nk), time_tag1(nt), time(nt)  )

     istatus=NF90_OPEN(cf_filv,NF90_NOWRITE,ncid2)
     istatus=NF90_INQ_VARID(ncid2,'vomecrty',id_v )
     !   create a grid file associated WITH this cdf file
     !   ... uses u v etc as temporary arrays, released after...
     istatus=NF90_INQ_VARID(ncid1,'nav_lat',id_latin) ; 
     istatus=NF90_GET_VAR(ncid1,id_latin,v(1:ni,1:nj), start=(/1,1/), count=(/ni,nj/) ) ; 

     istatus=NF90_INQ_VARID(ncid2,'nav_lon',id_lonin) ; 
     istatus=NF90_GET_VAR(ncid2,id_lonin,urot(1:ni,1:nj),start=(/1,1/), count=(/ni,nj/) ) ; 
     !   ... now estimate the lon, lat of T point , in ua, va
     !  ...    T lon is mean V_lon
     DO ji = 1, ni
        DO jj = 2, nj
           ua(ji,jj) = 0.5*(urot(ji,jj)+urot(ji,jj-1) )
        END DO
     END DO
     !  ...    T lat is mean U-lat
     DO ji = 2, ni
        DO jj = 1, nj
           va(ji,jj) = 0.5*(v(ji,jj)+v(ji-1,jj) )
        END DO
     END DO
     ua(:,1)=ua(:,2)
     va(1,:)=va(2,:)

     !    output file still in dimg !!!
     irecl=ni*nj*4
     OPEN(iluo,file='vecrot.bimg',form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
     !  we suppose that file 2 has same ni,nj,nk than file 1
  CASE  DEFAULT
     ctype ='bimg'
     OPEN(ilui,file=cf_filu,form='UNFORMATTED')
     OPEN(ilvi,file=cf_filv,form='UNFORMATTED')
     OPEN(iluo,file='vecrot.bimg',form='UNFORMATTED')
  END SELECT


  SELECT CASE ( ctype )
  CASE ( 'bimg' )
     READ(ilui) cline1
     READ(ilui) cline2
     READ(ilui) cline3
     READ(ilui) cline4

     WRITE(cline4,120)'Rotation des axes de ',angled
     WRITE(iluo) cline1
     WRITE(iluo) cline2
     WRITE(iluo) cline3
     WRITE(iluo) cline4
     !
     !
     READ(ilui) ni,nj,nk,nt,ndim,icod

     ALLOCATE( u(ni,nj), ua(ni,nj), urot(ni,nj) )
     ALLOCATE( v(ni,nj), va(ni,nj), vrot(ni,nj) )
     ALLOCATE ( h1d(nk), time_tag1(nt), time(nt)  )

     READ(ilui) x1,y1,dx,dy,spval
     WRITE(iluo) ni,nj,nk,nt,2,icod
     WRITE(iluo) x1,y1,dx,dy,spval
     !
     !
     READ(ilui) (h1d(jk),jk=1,nk)
     READ(ilui) time_tag1(1)
     WRITE(iluo) (h1d(jk),jk=1,nk)
     WRITE(iluo) time_tag1(1)

     READ(ilvi) cline1
     READ(ilvi) cline2
     READ(ilvi) cline3
     READ(ilvi) cline4
     READ(ilvi) ni,nj,nk,nt,ndim,icod
     READ(ilvi) x1,y1,dx,dy,spval
     READ(ilvi) (h1d(jk),jk=1,nk)

  CASE ( 'dimg' )
     READ(ilui,rec=1) cver,ccom,irecl,ni,nj,nk,nt,ndim
     ALLOCATE( u(ni,nj), ua(ni,nj), urot(ni,nj) )
     ALLOCATE( v(ni,nj), va(ni,nj), vrot(ni,nj) )
     ALLOCATE ( h1d(nk), time_tag1(nt), time(nt)  )

     READ(ilui,rec=1) cver,ccom,irecl,ni,nj,nk,nt,ndim, &
          &                  x1,y1,dx,dy,spval, &
          &              (h1d(ji),ji=1,nk), &
          &              (time(ji),ji=1,nt)
     READ(ilvi,rec=1) cver,ccom,irecl,ni,nj,nk,nt,ndim, &
          &                  x1,y1,dx,dy,spval, &
          &              (h1d(ji),ji=1,nk), &
          &              (time(ji),ji=1,nt)
     WRITE(iluo,rec=1) cver,ccom,irecl,ni,nj,nk,nt,2, &
          &                  x1,x1,dx,dy,spval, &
          &              (h1d(ji),ji=1,nk), &
          &              (time(ji),ji=1,nt) 
     PRINT *,cver,ccom,irecl,ni,nj,nk,nt,2, x1,x1,dx,dy,spval
  CASE ( 'cdf' )
     !  create output file
     PRINT *,' Output file is vecrot.nc '
     istatus=NF90_CREATE('vecrot.nc',NF90_CLOBBER,ncout) ; 
     !define dims just as in input file
     istatus=NF90_DEF_DIM(ncout,'x',ni,id_x) ; 
     istatus=NF90_DEF_DIM(ncout,'y',nj,id_y) ; 
     istatus=NF90_DEF_DIM(ncout,'deptht',nk,id_z) ; 
     istatus=NF90_DEF_DIM(ncout,'time_counter',NF90_UNLIMITED,id_t) ; 
     ! define variables
     istatus=NF90_DEF_VAR(ncout,'nav_lon',NF90_FLOAT,(/id_x,id_y/),id_lon) ; 
     istatus=NF90_DEF_VAR(ncout,'nav_lat',NF90_FLOAT,(/id_x,id_y/),id_lat) ; 
     istatus=NF90_DEF_VAR(ncout,'deptht',NF90_FLOAT,(/id_z/),id_dep) ; 
     istatus=NF90_DEF_VAR(ncout,'time_counter',NF90_FLOAT,(/id_t/),id_tim) ; 

     istatus=NF90_DEF_VAR(ncout,'vtang',NF90_FLOAT,(/id_x,id_y,id_z,id_t/),id_tang) ; 
     istatus=NF90_DEF_VAR(ncout,'vnorm',NF90_FLOAT,(/id_x,id_y,id_z,id_t/),id_norm) ; 

     ! define attributes
     ! copy them for dim variables
     istatus=NF90_COPY_ATT(ncid1,id_lonin,'units',ncout,id_lon)
     istatus=NF90_COPY_ATT(ncid1,id_lonin,'valid_min',ncout,id_lon)
     istatus=NF90_COPY_ATT(ncid1,id_lonin,'valid_max',ncout,id_lon)
     istatus=NF90_COPY_ATT(ncid1,id_lonin,'long_name',ncout,id_lon)
     istatus=NF90_COPY_ATT(ncid1,id_lonin,'nav_model',ncout,id_lon)

     istatus=NF90_COPY_ATT(ncid1,id_latin,'units',ncout,id_lat)
     istatus=NF90_COPY_ATT(ncid1,id_latin,'valid_min',ncout,id_lat)
     istatus=NF90_COPY_ATT(ncid1,id_latin,'valid_max',ncout,id_lat)
     istatus=NF90_COPY_ATT(ncid1,id_latin,'long_name',ncout,id_lat)
     istatus=NF90_COPY_ATT(ncid1,id_latin,'nav_model',ncout,id_lat)

     ! or define them 
     istatus=NF90_PUT_ATT(ncout,id_dep,'units','m')
     istatus=NF90_PUT_ATT(ncout,id_dep,'positive','unknown')
     istatus=NF90_PUT_ATT(ncout,id_dep,'valid_min',0.)
     istatus=NF90_PUT_ATT(ncout,id_dep,'valid_max',46.)
     istatus=NF90_PUT_ATT(ncout,id_dep,'title','deptht')
     istatus=NF90_PUT_ATT(ncout,id_dep,'long_name','Vertical levels')

     istatus=NF90_PUT_ATT(ncout,id_tim,'calendar','gregorian')
     istatus=NF90_PUT_ATT(ncout,id_tim,'units','seconds since 0006-01-01 00:00:00')
     istatus=NF90_PUT_ATT(ncout,id_tim,'time_origin','0001-JAN-01 00:00:00')
     istatus=NF90_PUT_ATT(ncout,id_tim,'title','Time')
     istatus=NF90_PUT_ATT(ncout,id_tim,'long_name','Time axis')

     istatus=NF90_PUT_ATT(ncout,id_tang,'units','m/s')
     istatus=NF90_PUT_ATT(ncout,id_tang,'missing_value',0.)
     istatus=NF90_PUT_ATT(ncout,id_tang,'valid_min',-10.)
     istatus=NF90_PUT_ATT(ncout,id_tang,'valid_max',10.)
     istatus=NF90_PUT_ATT(ncout,id_tang,'long_name','Tangential velocity')
     istatus=NF90_PUT_ATT(ncout,id_tang,'short_name','vtang')
     istatus=NF90_PUT_ATT(ncout,id_tang,'online_operation','N/A')
     istatus=NF90_PUT_ATT(ncout,id_tang,'axis','TZYX')

     istatus=NF90_PUT_ATT(ncout,id_norm,'units','m/s')
     istatus=NF90_PUT_ATT(ncout,id_norm,'missing_value',0.)
     istatus=NF90_PUT_ATT(ncout,id_norm,'valid_min',-10.)
     istatus=NF90_PUT_ATT(ncout,id_norm,'valid_max',10.)
     istatus=NF90_PUT_ATT(ncout,id_norm,'long_name','Normal velocity')
     istatus=NF90_PUT_ATT(ncout,id_norm,'short_name','vnorm')
     istatus=NF90_PUT_ATT(ncout,id_norm,'online_operation','N/A')
     istatus=NF90_PUT_ATT(ncout,id_norm,'axis','TZYX')

     ! One global attribute
     istatus=NF90_PUT_ATT(ncout,NF90_GLOBAL,'rem:','temporary file')
     istatus=NF90_ENDDEF(ncout)

     DO jtime=1,nt
        time(jtime)=jtime
     END DO
     x1= 1 ; y1 = 1; dx=1; dy=1; spval=0.

     istatus=NF90_GET_VAR(ncid1,id_dep1,h1d(1:nk),start=(/1/), count=(/nk/) ) ; 

     istatus=NF90_PUT_VAR(ncout,id_lon,ua(1:ni,1:nj),start=(/1,1/),count=(/ni,nj/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_lat,va(1:ni,1:nj),start=(/1,1/),count=(/ni,nj/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_dep,h1d(1:nk),start=(/1/),count=(/nk/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_tim,time(1:nt),start=(/1/),count=(/nt/) ) ; 

  CASE DEFAULT
     PRINT *, ' CTYPE unknown :', TRIM(ctype),' ... ERROR :('
     STOP ' file format not supported'
  END SELECT

  irec=1
  DO jtime=1,nt
     DO jk=1,nk
        SELECT CASE ( ctype )
        CASE ( 'bimg' )
           READ(ilui) time_tag1(jtime)
           READ(ilvi) time_tag1(jtime)
           READ(ilui)((u(ji,jj),ji=1,ni),jj=1,nj)
           READ(ilvi)((v(ji,jj),ji=1,ni),jj=1,nj)
        CASE ( 'dimg' )
           idim=1
           irec2=2+ (jtime-1)*nk*ndim+(jk-1)*ndim+(idim-1)
           READ(ilui,rec=irec2)((u(ji,jj),ji=1,ni),jj=1,nj)
           READ(ilvi,rec=irec2)((v(ji,jj),ji=1,ni),jj=1,nj)
        CASE ( 'cdf' )
           istart(1)=1
           istart(2)=1
           istart(3)=jk
           istart(4)=jtime
           icount(1)=ni
           icount(2)=nj
           icount(3)=1
           icount(4)=1
           istatus = NF90_GET_VAR(ncid1,id_u,u(1:ni,1:nj),start=(/1,1,jk,jtime/),count=(/ni,nj,1,1/) ) ; 
           istatus = NF90_GET_VAR(ncid2,id_v,v(1:ni,1:nj),start=(/1,1,jk,jtime/),count=(/ni,nj,1,1/) ) ; 
        CASE DEFAULT
           PRINT *, ' CTYPE unknown :', TRIM(ctype),' ... ERROR :('
           STOP
        END SELECT

        ! interpolation on Cgrid
        WHERE ( u == spval ) u = 0.
        WHERE ( v == spval ) v = 0.

        DO jj=1,nj
           ua(1,jj)=0.
           DO ji=2,ni
              ua(ji,jj)=0.5*(u(ji-1,jj) + u(ji,jj))
           END DO
        END DO

        DO ji=1,ni
           va(ji,1)=0.
           DO jj=2,nj
              va(ji,jj)=0.5*(v(ji,jj-1) + v(ji,jj))
           END DO
        END DO

        urot = ua * COS(alfa) - va * SIN(alfa)
        vrot = ua * SIN(alfa) + va * COS(alfa)

        SELECT CASE ( ctype )
        CASE  ( 'bimg' )
           WRITE(iluo)((urot(ji,jj),ji=1,ni),jj=1,nj)
           WRITE(iluo)((vrot(ji,jj),ji=1,ni),jj=1,nj)
        CASE  ( 'dimg' )
           idim=1
           irec2=2+ (jtime-1)*nk*2+(jk-1)*2+(idim-1)
           WRITE(iluo,rec=irec2)((urot(ji,jj),ji=1,ni),jj=1,nj)
           idim=2
           irec2=2+ (jtime-1)*nk*2+(jk-1)*2+(idim-1) 
           WRITE(iluo,rec=irec2)((vrot(ji,jj),ji=1,ni),jj=1,nj)
        CASE  ( 'cdf' )
           istatus=NF90_PUT_VAR(ncout,id_tang,urot(1:ni,1:nj),start=(/1,1,jk,jtime/), count=(/ni,nj,1,1/) ) ; 
           istatus=NF90_PUT_VAR(ncout,id_norm,vrot(1:ni,1:nj),start=(/1,1,jk,jtime/), count=(/ni,nj,1,1/) ) ; 
        CASE DEFAULT
           PRINT *, ' CTYPE unknown :', TRIM(ctype),' ... ERROR :('
           STOP
        END SELECT
     END DO
  END DO

  CLOSE(ilui)
  CLOSE(iluo)
  CLOSE(ilvi)
  IF ( ctype == 'cdf' ) istatus=NF90_CLOSE(ncout)

END PROGRAM vecrot_opa

