PROGRAM vecrot_opa
  !!! -------------------------------------------------------------------------------------
  !!!             ***   PROGRAM  VECROTOPA   ***
  !!!
  !!!    ** Purpose : compute tangeant and normal velocity component for a section
  !!!
  !!!   history : ORIGINAL : ??? JMM
  !!!          02/2007 : port to F90
  !!! -------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE
  INCLUDE 'parameter.h'
  CHARACTER*80 line1, line2, line3, line4
  CHARACTER*80 fil1,fil2,line,filgrid,cmd
  REAL u(NXX,NYY), h1d(NA)
  REAL v(NXX,NYY)
  REAL ua(NXX,NYY),va(NXX,NYY)
  REAL urot(NXX,NYY)
  REAL vrot(NXX,NYY)

  CHARACTER*80 RECORD_str, ctype
  CHARACTER*4 VER
  LOGICAL DIRECT

  INTEGER k,i,j,ni,nj,nk, ji,jj
  INTEGER irecl, irec,IFich,narg,iargc,irec2
  INTEGER nt,icod,ndim,itime,idim
  REAL    x1, y1, dx, dy, spval, time_tag1(Ntime),time(Ntime)
  REAL angled,angle,alfa,pi
  ! CDF STUFF
  INTEGER istatus, ncid1, ncid2
  INTEGER id_dimx, id_dimy, id_dimz, id_dimt, id_dep1, id_u, id_v, id
  INTEGER istart(4), icount(4)
  !  CDF OUT
  INTEGER :: id_latin, id_lonin, ncout, id_x, id_y, id_z, id_t, id_lon, id_lat ,id_dep
  INTEGER :: id_tim, id_tang, id_norm

  pi=ACOS(-1.)
  narg=iargc()
  DIRECT=.FALSE.
  IF (narg.LT.2) THEN
     PRINT *,'USAGE: vecro_opat <angle>  vecU vecV'
     PRINT *,' sortie sur vecrot.bimg or vecrot.nc >'
     PRINT *,' If output on nc files variables are vtang and vnorm'
     STOP
  END IF
  !
  !
  CALL getarg(1,fil1)
  READ(fil1,*)angled
  angle=angled*pi/180.
  alfa=angle-pi/2.

  IFich=2
  CALL getarg(IFich,fil1)
  IFich=3
  CALL getarg(IFich,fil2)

120 FORMAT(a,i3.3)
  OPEN(10,file=fil1,form='UNFORMATTED', ACCESS='DIRECT',recl=88)
  READ(10,rec=1) VER,RECORD_str,irecl
  PRINT *,'file OPENed:',fil1
  PRINT *,VER
  CLOSE(10)

  IF (VER .EQ. '@!01')  THEN
     ctype='dimg'
     OPEN(10,file=fil1,form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
     OPEN(12,file=fil2,form='UNFORMATTED', ACCESS='DIRECT',recl=88)
     READ(12,rec=1) VER,RECORD_str,irecl
     PRINT *,'file OPENed:',fil2
     PRINT *,VER,RECORD_str,irecl
     CLOSE(12)
     IF (VER.NE.'@!01' ) THEN
        PRINT *, "Error: U and V files are not of the same type"
        STOP
     END IF

     OPEN(12,file=fil2,form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)

     OPEN(11,file='vecrot.bimg',form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
  ELSEIF (VER(1:3) .EQ. 'CDF')  THEN
     PRINT *, ' NetCdf File U '
     ctype ='cdf'
     istatus=NF90_OPEN(fil1,NF90_NOWRITE,ncid1)
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

     istatus=NF90_OPEN(fil2,NF90_NOWRITE,ncid2)
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
     OPEN(11,file='vecrot.bimg',form='UNFORMATTED', ACCESS='DIRECT',recl=irecl)
     !  we suppose that file 2 has same ni,nj,nk than file 1
  ELSE
     ctype ='bimg'
     OPEN(10,file=fil1,form='UNFORMATTED')
     OPEN(12,file=fil2,form='UNFORMATTED')
     OPEN(11,file='vecrot.bimg',form='UNFORMATTED')
  END IF


  IF ( ctype .EQ. 'bimg' ) THEN
     READ(10) line1
     READ(10) line2
     READ(10) line3
     READ(10) line4

     WRITE(line4,120)'Rotation des axes de ',angled
     WRITE(11) line1
     WRITE(11) line2
     WRITE(11) line3
     WRITE(11) line4
     !
     !
     READ(10) ni,nj,nk,nt,ndim,icod
     READ(10) x1,y1,dx,dy,spval
     WRITE(11) ni,nj,nk,nt,2,icod
     WRITE(11) x1,y1,dx,dy,spval
     !
     !
     READ(10) (h1d(k),k=1,nk)
     READ(10) time_tag1
     WRITE(11) (h1d(k),k=1,nk)
     WRITE(11) time_tag1

     READ(12) line1
     READ(12) line2
     READ(12) line3
     READ(12) line4
     READ(12) ni,nj,nk,nt,ndim,icod
     READ(12) x1,y1,dx,dy,spval
     READ(12) (h1d(k),k=1,nk)
     !        READ(12) time_tag1

  ELSEIF (ctype .EQ. 'dimg' ) THEN  ! DIRECT
     READ(10,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,ndim, &
     &                  x1,y1,dx,dy,spval, &
     &              (h1d(i),i=1,nk), &
     &              (time(i),i=1,nt)
     READ(12,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,ndim, &
     &                  x1,y1,dx,dy,spval, &
     &              (h1d(i),i=1,nk), &
     &              (time(i),i=1,nt)
     WRITE(11,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,2, &
     &                  x1,x1,dx,dy,spval, &
     &              (h1d(i),i=1,nk), &
     &              (time(i),i=1,nt) 
     PRINT *,VER,RECORD_str,irecl,ni,nj,nk,nt,2, x1,x1,dx,dy,spval
  ELSEIF ( ctype .EQ. 'cdf' ) THEN
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

     DO itime=1,nt
        time(itime)=itime
     END DO
     x1= 1 ; y1 = 1; dx=1; dy=1; spval=0.

     istatus=NF90_GET_VAR(ncid1,id_dep1,h1d(1:nk),start=(/1/), count=(/nk/) ) ; 

     istatus=NF90_PUT_VAR(ncout,id_lon,ua(1:ni,1:nj),start=(/1,1/),count=(/ni,nj/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_lat,va(1:ni,1:nj),start=(/1,1/),count=(/ni,nj/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_dep,h1d(1:nk),start=(/1/),count=(/nk/) ) ; 
     istatus=NF90_PUT_VAR(ncout,id_tim,time(1:nt),start=(/1/),count=(/nt/) ) ; 

  ELSE
     STOP ' file format not supported'
  END IF

  irec=1
  DO itime=1,nt
     DO k=1,nk
        IF (ctype .EQ. 'bimg' ) THEN
           READ(10) time_tag1(itime)
           READ(12) time_tag1(itime)
           READ(10)((u(i,j),i=1,ni),j=1,nj)
           READ(12)((v(i,j),i=1,ni),j=1,nj)
        ELSEIF ( ctype .EQ. 'dimg' ) THEN !  DIRECT
           !              irec=irec+1
           idim=1
           irec2=2+ (itime-1)*nk*ndim+(k-1)*ndim+(idim-1)
           READ(10,rec=irec2)((u(i,j),i=1,ni),j=1,nj)
           READ(12,rec=irec2)((v(i,j),i=1,ni),j=1,nj)
        ELSEIF ( ctype .EQ. 'cdf' ) THEN
           istart(1)=1
           istart(2)=1
           istart(3)=k
           istart(4)=itime
           icount(1)=ni
           icount(2)=nj
           icount(3)=1
           icount(4)=1
           istatus = NF90_GET_VAR(ncid1,id_u,u(1:ni,1:nj),start=(/1,1,k,itime/),count=(/ni,nj,1,1/) ) ; 
           istatus = NF90_GET_VAR(ncid2,id_v,v(1:ni,1:nj),start=(/1,1,k,itime/),count=(/ni,nj,1,1/) ) ; 
        END IF

        ! interpolation on Cgrid
        DO i=1,ni
           DO j=1,nj
              IF (u(i,j).EQ.spval) u(i,j)=0.
              IF (v(i,j).EQ.spval) v(i,j)=0.
           END DO
        END DO
        DO j=1,nj
           ua(1,j)=0.
           DO i=2,ni
              ua(i,j)=0.5*(u(i-1,j) + u(i,j))
           END DO
        END DO
        DO i=1,ni
           va(i,1)=0.
           DO j=2,nj
              va(i,j)=0.5*(v(i,j-1) + v(i,j))
           END DO
        END DO

        DO i=1,ni
           DO j=1,nj
              !                  IF (u(i,j).ne.spval.and.v(i,j).ne.spval) THEN
              urot(i,j)=ua(i,j)*COS(alfa)-va(i,j)*SIN(alfa)
              vrot(i,j)=ua(i,j)*SIN(alfa)+va(i,j)*COS(alfa)
              !                  ELSE
              !              urot(i,j)=spval
              !            vrot(i,j)=spval
              !                END IF
           END DO
        END DO
        IF (ctype == 'bimg' ) THEN
           WRITE(11)((urot(i,j),i=1,ni),j=1,nj)
           WRITE(11)((vrot(i,j),i=1,ni),j=1,nj)
        ELSE IF (ctype == 'dimg' ) THEN  ! DIRECT
           idim=1
           irec2=2+ (itime-1)*nk*2+(k-1)*2+(idim-1)
           WRITE(11,rec=irec2)((urot(i,j),i=1,ni),j=1,nj)
           idim=2
           irec2=2+ (itime-1)*nk*2+(k-1)*2+(idim-1) 
           WRITE(11,rec=irec2)((vrot(i,j),i=1,ni),j=1,nj)
        ELSE IF ( ctype == 'cdf' ) THEN
           istatus=NF90_PUT_VAR(ncout,id_tang,urot(1:ni,1:nj),start=(/1,1,k,itime/), count=(/ni,nj,1,1/) ) ; 
           istatus=NF90_PUT_VAR(ncout,id_norm,vrot(1:ni,1:nj),start=(/1,1,k,itime/), count=(/ni,nj,1,1/) ) ; 
        ELSE
           PRINT *, ' CTYPE unknown :', TRIM(ctype),' ... ERROR :('
           STOP
        END IF
     END DO
  END DO
  CLOSE(10)
  CLOSE(11)
  CLOSE(12)
  IF ( ctype == 'cdf' ) istatus=NF90_CLOSE(ncout)
END PROGRAM vecrot_opa

