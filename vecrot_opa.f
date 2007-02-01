      PROGRAM vecrot_opa
      IMPLICIT NONE

      INTEGER nimax,njmax,nkmax,ndimax,ntmax
      PARAMETER (nimax=780,njmax=1296,nkmax=43,ndimax=1,ntmax=1)
      CHARACTER*80 line1, line2, line3, line4
      CHARACTER*80 fil1,fil2,line,filgrid,cmd
      REAL u(nimax,njmax), h1d(nkmax)
      REAL v(nimax,njmax)
      REAL ua(nimax,njmax),va(nimax,njmax)
      REAL urot(nimax,njmax)
      REAL vrot(nimax,njmax)

      CHARACTER*80 RECORD_str, ctype
      CHARACTER*4 VER
            LOGICAL DIRECT

      INTEGER k,i,j,ni,nj,nk, ji,jj
      INTEGER irecl, irec,IFich,narg,iargc,irec2
      INTEGER nt,icod,ndim,itime,idim
      REAL    x1, y1, dx, dy, spval, time_tag1(ntmax),time(ntmax)
      REAL angled,angle,alfa,pi
      include 'netcdf.inc'
      INTEGER istatus, ncid1, ncid2
      INTEGER id_dimx, id_dimy, id_dimz, id_dimt, id_dep, id_u, id_v, id
      INTEGER istart(4), icount(4)

      pi=acos(-1.)
      narg=iargc()
      DIRECT=.false.
      IF (narg.lt.2) THEN
       PRINT *,'USAGE: vecro_opat <angle>  vecU vecV'
       PRINT *,' sortie sur vecrot.bimg>'
      STOP
      END IF
c
c
      CALL getarg(1,fil1)
      READ(fil1,*)angled
      angle=angled*pi/180.
      alfa=angle-pi/2.
      
      IFich=2
      CALL getarg(IFich,fil1)
      IFich=3
      CALL getarg(IFich,fil2)

120      FORMAT(a,i3.3)
      PRINT *,' Sortie sur vecrot.bimg'

        OPEN(10,file=fil1,form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=88)
        READ(10,rec=1) VER,RECORD_str,irecl
        PRINT *,'file OPENed:',fil1
c        PRINT *,VER,RECORD_str,irecl
        PRINT *,VER
        CLOSE(10)

        IF (VER .eq. '@!01')  THEN
         ctype='dimg'
         OPEN(10,file=fil1,form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=irecl)
         OPEN(12,file=fil2,form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=88)
         READ(12,rec=1) VER,RECORD_str,irecl
         PRINT *,'file OPENed:',fil2
         PRINT *,VER,RECORD_str,irecl
       CLOSE(12)
       IF (VER.ne.'@!01' ) THEN
        PRINT *,
     .      "Error: U and V files are not of the same type"
        STOP
       END IF

         OPEN(12,file=fil2,form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=irecl)

         OPEN(11,file='vecrot.bimg',form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=irecl)
        ELSEIF (VER(1:3) .eq. 'CDF')  THEN
         PRINT *, ' NetCdf File U '
         ctype ='cdf'
         istatus=NF_OPEN(fil1,NF_NOWRITE,ncid1)
         istatus=NF_INQ_DIMID(ncid1,'x',id_dimx)
         istatus=NF_INQ_DIMID(ncid1,'y',id_dimy)
         istatus=NF_INQ_DIMID(ncid1,'depthu',id_dimz)
         istatus=NF_INQ_DIMID(ncid1,'time_counter',id_dimt)
         istatus=NF_INQ_VARID(ncid1,'depthu',id_dep )
         istatus=NF_INQ_VARID(ncid1,'vozocrtx',id_u )
         istatus=NF_INQ_DIMLEN(ncid1,id_dimx,ni)
         istatus=NF_INQ_DIMLEN(ncid1,id_dimy,nj)
         istatus=NF_INQ_DIMLEN(ncid1,id_dimz,nk)
         istatus=NF_INQ_DIMLEN(ncid1,id_dimt,nt)

         istatus=NF_OPEN(fil2,NF_NOWRITE,ncid2)
         istatus=NF_INQ_VARID(ncid2,'vomecrty',id_v )
C   create a grid file associated with this cdf file
C   ... uses u v etc as temporary arrays, released after...
         istatus=NF_INQ_VARID(ncid1,'nav_lat',id)
         istatus=NF_GET_VAR_REAL(ncid1,id,v(1:ni,1:nj) )

         istatus=NF_INQ_VARID(ncid2,'nav_lon',id)
         istatus=NF_GET_VAR_REAL(ncid2,id,urot(1:ni,1:nj) )
C   ... now estimate the lon, lat of T point , in ua, va
C  ...    T lon is mean V_lon
         DO ji = 1, ni
          DO jj = 2, nj
            ua(ji,jj) = 0.5*(urot(ji,jj)+urot(ji,jj-1) )
          END DO
         END DO
C  ...    T lat is mean U-lat
         DO ji = 2, ni
          DO jj = 1, nj
            va(ji,jj) = 0.5*(v(ji,jj)+v(ji-1,jj) )
          END DO
         END DO
          ua(:,1)=ua(:,2)
          va(1,:)=va(2,:)
          line1='temporary vecrot grid file !!!'
          OPEN (21, file='vecrotgrid.bimg',form='UNFORMATTED')
          WRITE(21) line1
          WRITE(21) line1
          WRITE(21) line1
          WRITE(21) line1
          WRITE(21) ni,nj,1,1,2,0
          WRITE(21) 1.,1.,1.,1.,0.
          WRITE(21) 0.
          WRITE(21) 0.
          WRITE(21) (( ua(ji,jj), ji=1,ni),jj=1,nj )
          WRITE(21) (( va(ji,jj), ji=1,ni),jj=1,nj )
          CLOSE(21)
C    output file still in dimg !!!
         irecl=ni*nj*4
         OPEN(11,file='vecrot.bimg',form='UNFORMATTED',
     .      ACCESS='DIRECT',recl=irecl)
C  we suppose that file 2 has same ni,nj,nk than file 1
        ELSE
         ctype ='bimg'
         OPEN(10,file=fil1,form='UNFORMATTED')
         OPEN(12,file=fil2,form='UNFORMATTED')
       OPEN(11,file='vecrot.bimg',form='UNFORMATTED')
        END IF


        IF ( ctype .eq. 'bimg' ) THEN
      READ(10) line1
      READ(10) line2
      READ(10) line3
      READ(10) line4

      WRITE(line4,120)'Rotation des axes de ',angled
      WRITE(11) line1
      WRITE(11) line2
      WRITE(11) line3
      WRITE(11) line4
c
c
      READ(10) ni,nj,nk,nt,ndim,icod
      READ(10) x1,y1,dx,dy,spval
      WRITE(11) ni,nj,nk,nt,2,icod
      WRITE(11) x1,y1,dx,dy,spval
c
c
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
c        READ(12) time_tag1

      ELSEIF (ctype .EQ. 'dimg' ) THEN  ! DIRECT
        READ(10,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,ndim,
     .                  x1,y1,dx,dy,spval,
     .              (h1d(i),i=1,nk),
     .              (time(i),i=1,nt)
        READ(12,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,ndim,
     .                  x1,y1,dx,dy,spval,
     .              (h1d(i),i=1,nk),
     .              (time(i),i=1,nt)
        WRITE(11,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,2,
     .                  x1,x1,dx,dy,spval,
     .              (h1d(i),i=1,nk),
     .              (time(i),i=1,nt)
      PRINT *,VER,RECORD_str,irecl,ni,nj,nk,nt,2,
     .                  x1,x1,dx,dy,spval
      ELSEIF ( ctype .EQ. 'cdf' ) then
        VER='@!01'
        RECORD_str='vecrot file'
        DO itime=1,nt
           time(itime)=itime
        END DO
        x1= 1 ; y1 = 1; dx=1; dy=1; spval=0.
        
        istatus=NF_GET_VAR_REAL(ncid1,id_dep,h1d(1:nk) )
        WRITE(11,rec=1) VER,RECORD_str,irecl,ni,nj,nk,nt,2,
     .                  x1,x1,dx,dy,spval,
     .              (h1d(i),i=1,nk),
     .              (time(i),i=1,nt)

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
c              irec=irec+1
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
            istatus = NF_GET_VARA_REAL(ncid1,id_u,istart,icount,u(1:ni,1:nj) )
            istatus = NF_GET_VARA_REAL(ncid2,id_v,istart,icount,v(1:ni,1:nj) )
        END IF

c interpolation on Cgrid
            DO i=1,ni
        DO j=1,nj
          IF (u(i,j).eq.spval) u(i,j)=0.
          IF (v(i,j).eq.spval) v(i,j)=0.
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
c                  IF (u(i,j).ne.spval.and.v(i,j).ne.spval) THEN
              urot(i,j)=ua(i,j)*cos(alfa)-va(i,j)*sin(alfa)
            vrot(i,j)=ua(i,j)*sin(alfa)+va(i,j)*cos(alfa)
c                  ELSE
c              urot(i,j)=spval
c            vrot(i,j)=spval
c                END IF
               END DO
                END DO
           IF (ctype == 'bimg' ) THEN
              WRITE(11)((urot(i,j),i=1,ni),j=1,nj)
              WRITE(11)((vrot(i,j),i=1,ni),j=1,nj)
           ELSE   ! DIRECT
           idim=1
            irec2=2+ (itime-1)*nk*2+(k-1)*2+(idim-1)
                WRITE(11,rec=irec2)((urot(i,j),i=1,ni),j=1,nj)
           idim=2
            irec2=2+ (itime-1)*nk*2+(k-1)*2+(idim-1) 
                WRITE(11,rec=irec2)((vrot(i,j),i=1,ni),j=1,nj)
           END IF
        END DO
         END DO
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      END 

        FUNCTION lnblnk(string)
        CHARACTER string*(*)
        ll= len(string)
        ii= index(string,' ')
        IF (ll.eq.0.or.ii.eq.1) THEN
          lnblnk= 1
          RETURN
        ELSE IF (ii.eq.0) THEN
          lnblnk= ll
          RETURN
        END  IF
        DO 10 k= ll, ii-1, -1
          IF (string(k:k).ne.' ') go to 20
10      CONTINUE
20      lnblnk= k
        RETURN
        END 
