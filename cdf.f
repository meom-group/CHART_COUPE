      SUBROUTINE CdfInit
CCC----------------------------------------------------------------------------
CCC                    SUBROUTINE CDFINIT
CCC                    ******************
CCC
CCC      PURPOSE:
CCC      --------
CCC       Initialize names of cdf variable at the IPSL norm
CCC
CC       AUTHOR:
CC       -------
CC        Original : J.M. Molines,  October, 2001
CCC---------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'cdfname.h'
C
      PRINT *,'CdfInit ...'
      cstandlist(1 )= 'x'
      cstandlist(2 )= 'y'
      cstandlist(3 )= 'deptht'
      cstandlist(4 )= 'time_counter'
      cstandlist(5 )= 'dim'
      cstandlist(6 )= 'Conventions'
      cstandlist(7 )= 'file_name'
      cstandlist(8 )= 'production'
      cstandlist(9 )= 'TimeStamp'
      cstandlist(10)= 'missing_value'
      cstandlist(11)= 'long_name'
      cstandlist(12)= 'units'
      cstandlist(13)= 'nav_lon'
      cstandlist(14)= 'nav_lat'
C
      RETURN
      END
C
       FUNCTION CdfOpen(filename, bimg)
CCC----------------------------------------------------------------------------
CCC                     FUNCTION CDFOPEN
CCC                     ****************
CCC
CCC      PURPOSE:
CCC      --------
CCC         Open a NetCdf File for Chart and Coupe and fills up what
CCC      he can in the bimg structure.
CC
CC       AUTHOR:
CC       -------
CC           Original: J.M. Molines 10/2001
CCC-----------------------------------------------------------------------------
       IMPLICIT NONE
       INCLUDE 'common.h'
       INCLUDE 'netcdf.inc' 
       CHARACTER*(*) filename
       TYPE( bimgfile ) bimg
       INTEGER CdfOpen
       INTEGER STATUS,NCID,ndims,nvars,ngatts,unlimdimid
CC
CC 1. Check modifier for this file
CC -------------------------------
       Call CdfInit
       PRINT *, 'In CdfOpen ...', bimg%modifier, filename
       IF ( bimg%modifier .NE. 'none' ) CALL cmodif (bimg)
       STATUS=NF_OPEN(filename,NF_NOWRITE,NCID)
       CALL ERR_HDL(STATUS)
       bimg%ncid=NCID
C ... Inquire general info on file
       STATUS=NF_INQ(NCID,ndims,nvars,ngatts,unlimdimid)
       CALL ERR_HDL(STATUS)
       bimg%ndims       = ndims
       bimg%nvars       = nvars
       bimg%ngatts      = ngatts
       bimg%unlimdimid  = unlimdimid

       CALL CdfInit
       CdfOpen=NCID
       RETURN
       END
CC
CC
       SUBROUTINE CdfHeader (bimg)
CCC----------------------------------------------------------------------------
CCC                     SUBROUTINE CDFHEADER
CCC                     ********************
CCC
CCC      PURPOSE:
CCC      --------
CCC         Read the header of  NetCdf File for Chart and Coupe and fills up what
CCC      he can in the bimg structure.
CC
CC       AUTHOR:
CC       -------
CC           Original: J.M. Molines 10/2001
CCC-----------------------------------------------------------------------------       
CC 0. Declarations
CC ---------------
CC  
      IMPLICIT NONE
      INCLUDE 'common.h'
      INCLUDE 'netcdf.inc' 
      TYPE( bimgfile ) bimg
C
      INTEGER STATUS,NCID,ndims,nvars,ngatts,unlimdimid  
      INTEGER dimdepid, depid, dimtimid, timid,dimdimid
      INTEGER varid, dimlatid, dimlonid
      INTEGER ndimv,ni,nj,nk
      INTEGER ivar,lnam,lnblnk
      INTEGER jk,jt,jdim,ier,i,j
      INTEGER yet_printed
C
      CHARACTER*80 cvar,clnam,clongn
C
      LOGICAL lflag
C
      REAL spval
      INCLUDE 'cdfname.h'
CC
CC 1. Read Header
CC --------------
CC
      data yet_printed /0/ 
      PRINT *,' In ReadHeader', bimg%modifier
       IF ( bimg%modifier .NE. 'none' ) CALL cmodif (bimg)
C     clongname='long_name'
C     cmiss='missing_value'
      clongn=' '
      NCID=bimg%ncid
      nvars=bimg%nvars
      cvar=bimg%varname
      PRINT *, NCID, nvars,cvar
      STATUS=NF_INQ_VARID (NCID,cvar,varid)
      PRINT *, status
C ... Enter interactive loop if the variable name is not
C     given or if it is wrong.
      DO WHILE (STATUS .EQ. NF_ENOTVAR) 
       PRINT *,' >>>>> Donnez le nom de la variable du ncdf'
       PRINT *,'      Options -clrvar ''varname'' '
       PRINT *,'      Options -cntvar ''varname'' '
       PRINT *,'      Options -vecvarx ''varname'' '
       PRINT *,'      Options -vecvary ''varname'' '
        DO ivar = 1, nvars
         STATUS=NF_INQ_VARNAME (NCID, ivar, clnam)
         CALL ERR_HDL(STATUS)
         STATUS=NF_GET_ATT_TEXT (NCID, ivar,clongname, clongn)
         IF ( STATUS .EQ. NF_ENOTATT ) THEN
          clongn='unknown'
         ELSE
          CALL ERR_HDL(STATUS)
         END IF
         lnam=lnblnk(clnam)
         PRINT '(i4,3h : ,a15,3h : ,a)'
     $ ,ivar,clnam(1:lnam),clongn(1:lnblnk(clongn))
      clongn=' '
        END DO          
        PRINT *,' >>>>> Entrez maintenant le no choisi '
        read *, varid
        STATUS=NF_INQ_VARNAME (NCID, varid, clnam) 
C        CALL ERR_HDL(STATUS)
        bimg%varname=clnam
        cvar=clnam
      END DO
C
      bimg%varid=varid
C  Lire info sur la variable requise
C ...  Les lignes de commentaires
       STATUS=NF_GET_ATT_TEXT (NCID, bimg%varid,clongname,bimg%str1) 
       STATUS=NF_GET_ATT_TEXT (NCID, bimg%varid,cunits,bimg%str2) 
       STATUS=NF_GET_ATT_TEXT (NCID,NF_GLOBAL,ccom2,bimg%str3)
       bimg%str4=' From a NetCdf File'
C ...  Le nombre de dimensions:
      STATUS=NF_INQ_VARNDIMS (NCID,varid,ndimv)
      IF (ndimv .LT. 2 ) THEN
       PRINT *, '>>> Variable : ',cvar(1:lnblnk(cvar))
       PRINT *,' >>> la variable n''est pas cartographiable'
       stop
      END IF
      bimg%ndimv = ndimv
C ... La valeur speciale spval
      STATUS=NF_GET_ATT_REAL(NCID,varid,cmiss,spval)
      IF ( STATUS .EQ. NF_ENOTATT ) THEN
       PRINT *, ' Valeur spval non definie, valeur par defaut'
       spval=defspval
      END IF
      bimg%spval = spval
C ... Les indices des tableaux: nxfile, nyfile
      STATUS=NF_INQ_DIMID(NCID,clat,dimlatid)
      CALL ERR_HDL(STATUS)

      STATUS=NF_INQ_DIM(NCID, dimlatid, clnam,bimg%nyfile)
      CALL ERR_HDL(STATUS)
C ...
      STATUS=NF_INQ_DIMID(NCID,clon,dimlonid)
      CALL ERR_HDL(STATUS)
 
      STATUS=NF_INQ_DIM(NCID, dimlonid, clnam,bimg%nxfile)
      CALL ERR_HDL(STATUS)
      bimg%nxdata = bimg%nxfile
      bimg%nydata = bimg%nyfile
C
C ... Les nombres de niveaux verticaux, 1 si ils ne sont pas definis
C
      STATUS=NF_INQ_DIMID(NCID,cdepth,dimdepid)
      IF (STATUS .EQ. NF_EBADDIM ) THEN
       PRINT *,' Only one level assumed ...'
       bimg%ncou = 1
       bimg%depth(1) = 0.
      ELSE
       STATUS=NF_INQ_DIM(NCID, dimdepid, clnam,bimg%ncou)
       CALL ERR_HDL(STATUS)
C
      ier=0
      if (bimg%nxfile.gt.NXX) then
         PRINT *,'erreur: NXX insuffisant (Max : ',NXX,')'
         ier=1
      else if (bimg%nyfile.gt.NYY) then
         PRINT *,'erreur: NYY insuffisant (Max : ',NYY,')'
         ier=1
      else if (bimg%ncou.gt.NA) then
         PRINT *,'erreur: NA insuffisant (Max : ',NA,')'
         ier=1
      endif
 
      if (ier.eq.1) stop
C
       STATUS=NF_INQ_VARID(NCID,cdepth,depid)
       CALL ERR_HDL(STATUS)
 
       STATUS=NF_GET_VAR_REAL(NCID, depid, bimg%depth)
       CALL ERR_HDL(STATUS)
      END IF
cJMM force les profondeurs a etre negatives ...
       do jk=1,bimg%ncou
        bimg%depth(jk)=-1.*abs(bimg%depth(jk))
       enddo    
C
C ... Les nombres de pas de temps stockes, 1 il pas defini
C
       STATUS=NF_INQ_DIMID(NCID,ctime,dimtimid)
       IF (STATUS .EQ. NF_EBADDIM ) THEN
        PRINT *,' Only one time assumed ...'
        bimg%nt = 1
        bimg%timea(1) = 0.
       ELSE
        STATUS=NF_INQ_DIM(NCID, dimtimid, clnam,bimg%nt)
        CALL ERR_HDL(STATUS)
C
        STATUS=NF_INQ_VARID(NCID,ctime,timid)
        CALL ERR_HDL(STATUS)
C
        STATUS=NF_GET_VAR_REAL(NCID, timid, bimg%timea)
        CALL ERR_HDL(STATUS)
       END IF
C
C ... Les nombres de dimension par niveau, 1 si pas defini
C
       STATUS=NF_INQ_DIMID(NCID,cdim,dimdimid)
       IF (STATUS .EQ. NF_EBADDIM ) THEN
        PRINT *,' Only one dim assumed ...'
        bimg%ndim = 1
       ELSE
        STATUS=NF_INQ_DIM(NCID, dimdimid, clnam,bimg%ndim)
        CALL ERR_HDL(STATUS)
       END IF
       bimg%icod = 0
C
C ... defini x1mod y1mod etc... 1 1 ni nj
      IF (opt_ijgrid .EQ. 1 ) THEN
C ... Use ij coordinates
C JMM test de supression de opt_print=0 avec netcdf
C      opt_print = 0
       bimg%x1mod = 1
       bimg%y1mod = 1
       bimg%dx = 1
       bimg%dy = 1
       bimg%grid = 0
       bimg%mask = 0
       bimg%x2mod = bimg%nxfile
       bimg%y2mod = bimg%nyfile
      ELSE IF ( opt_clrgrid .NE. 0 ) THEN
C ... la grille sera lue plus tard dans un fichier
       bimg%grid = opt_clrgrid
      ELSE
C ... Read the geographical coordinates from cdf File
       CALL CdfXYgr (bimg,bimg%nxfile,bimg%nyfile)
        opt_print = 1
        do i=1,bimg%nxfile
        xygr(i,0,1)=2*xygr(i,1,1) - xygr(i,2,1)
        xygr(i,0,2)=2*xygr(i,1,2) - xygr(i,2,2)
        xygr(i,bimg%nyfile+1,1)=
     .    2*xygr(i,bimg%nyfile,1) - xygr(i,bimg%nyfile-1,1)
        xygr(i,bimg%nyfile+1,2)=
     .    2*xygr(i,bimg%nyfile,2) - xygr(i,bimg%nyfile-1,2)
        enddo
 
        do j=1,bimg%nyfile
        xygr(0,j,1)    =2*xygr(1,j,1) - xygr(2,j,1)
        xygr(0,j,2)    =2*xygr(1,j,2) - xygr(2,j,2)
        xygr(bimg%nxfile+1,j,1)=
     .    2*xygr(bimg%nxfile,j,1) - xygr(bimg%nxfile-1,j,1)
        xygr(bimg%nxfile+1,j,2)=
     .    2*xygr(bimg%nxfile,j,2) - xygr(bimg%nxfile-1,j,2)
        enddo                               
Dans ce cas (grille completement irreguliere), il faut determiner
c le min et le max en balayant toute la grille.
c
c initialise
      bimg%x1mod=99999.0
      bimg%y1mod=99999.0
      bimg%x2mod=-99999.0
      bimg%y2mod=-99999.0
c option -360 : ajoute 360 deg aux longitudes negatives
       if (opt_360 .EQ. 1 ) then
        PRINT *,' OPTION 360 ON'
        do i=1,bimg%nxfile
        do j=1,bimg%nyfile
          if ( xygr(i,j,1) .lt. 0 ) xygr(i,j,1) = xygr(i,j,1) + 360.
        enddo
        enddo
       endif
c calcule le min et le max
      do i=1,bimg%nxfile
      do j=1,bimg%nyfile
       bimg%x1mod=min(bimg%x1mod,xygr(i,j,1))
       bimg%y1mod=min(bimg%y1mod,xygr(i,j,2))
       bimg%x2mod=max(bimg%x2mod,xygr(i,j,1))
       bimg%y2mod=max(bimg%y2mod,xygr(i,j,2))
      enddo
      enddo
C . Verifie si la grille est vraiment irreguliere ..
      lflag=.true.
      nigr=bimg%nxfile
      njgr=bimg%nyfile
C JMM : si la grille est degeneree, c'est pas vraiement une grille irreguliere
C       et on a un debordement de tableau dans les lignes suivantes
      IF (nigr .NE. 1 .AND. njgr .NE. 1 ) THEN
         DO i=1,bimg%nxfile
          DO j=1,bimg%nyfile
           IF ( xygr(i,j-1,1) .NE. xygr(i,j,1) ) lflag = ( lflag .AND. .false. )
          END DO
         END DO

         DO j=1,bimg%nyfile
          DO i=1,bimg%nxfile
           IF ( xygr(i-1,j,2) .NE. xygr(i,j,2) ) lflag = ( lflag .AND. .false. )
          END DO
         END DO
      ENDIF

      IF (lflag) then 
       bimg%grid=1
      ELSE
       bimg%grid=3
      END IF
      print *,' BIMG.GRID = ', bimg%grid
      bimg%dx= (xygr(bimg%nxfile,1,1)-xygr(1,1,1)) / bimg%nxfile
c     bimg%dx = 1
      bimg%dy = 1
 
      if (yet_printed.eq.0) then
       yet_printed = 1
       print *,' LIMITES DE LA GRILLE IRREGULIERE:'
       print *,'   x1mod=',bimg%x1mod
       print *,'   x2mod=',bimg%x2mod
       print *,'   y1mod=',bimg%y1mod
       print *,'   y2mod=',bimg%y2mod
       endif                 
      END IF
C
      bimg%last_time  = 0
      bimg%last_layer = bimg%ncou
      bimg%last_dim   = bimg%ndim
      bimg%last_rect  = 10000000
      bimg%last_recr  = 0          

       CALL CdfInit
      RETURN
      END
CC
       SUBROUTINE CdfXYgr (bimg,kx,ky)
CCC --------------------------------------------------------------------------
CCC             SUBROUTINE CdfXYgr (bimg)
CCC             *************************
CCC
CCC     PURPOSE:
CCC     --------
CCC        Read the coordinates in the headerof CdfFiles
CCC
CC      AUTHOR:
CC      -------
CC        J.M. Molines October 2001
CC
CCC --------------------------------------------------------------------------
CC 0. Declarations
CC ---------------
CC
      IMPLICIT NONE
      INCLUDE 'netcdf.inc'
      INCLUDE 'common.h'
      INCLUDE 'cdfname.h'
      TYPE( bimgfile ) bimg
      INTEGER NCID, varid, STATUS, dimid(2)
      INTEGER dimlonid, dimlatid, lonpos, latpos
      INTEGER start(10),count(10)
      INTEGER ji,jj, ni,nj, kx,ky
C     REAL xyloc(bimg%nxfile, bimg%nyfile)
      REAL xyloc(kx,ky)
CC
CC 1. Initialisation
CC -----------------
CC
      PRINT *,' In CdfXYgr', bimg%modifier
       IF ( bimg%modifier .NE. 'none' ) CALL cmodif (bimg)
      NCID=bimg%ncid
C
C ... clonxy  --> xygr(,,1)
C 
      STATUS=NF_INQ_VARID(NCID,clonxy,varid)
      CALL ERR_HDL(STATUS)
      STATUS=NF_INQ_VARDIMID(NCID,varid,dimid)
      CALL ERR_HDL(STATUS)    
C
      STATUS=NF_INQ_DIMID(NCID,clon,dimlonid)
      CALL ERR_HDL(STATUS)
C
      STATUS=NF_INQ_DIMID(NCID,clat,dimlatid)
      CALL ERR_HDL(STATUS)  
CCC
      IF ( dimid(2) == 0 ) THEN
C   Dans ce cas, lon(ji) seulement
      lonpos=1
      latpos=2
       start (lonpos) = 1
       count (lonpos) = bimg%nxfile
       start (latpos) = 1
       count (latpos) = 1
      STATUS=NF_GET_VARA_REAL(NCID,varid,start,count,xyloc)
      CALL ERR_HDL(STATUS)
      DO ji=1,bimg%nxfile
       DO jj=1,bimg%nyfile
        xygr(ji,jj,1) = xyloc(ji,1)     
       END DO
      END DO
      bimg%dx= (xyloc(bimg%nxfile,1)-xyloc(1,1)) / bimg%nxfile
      else
C Cas general

      DO ji = 1,10
       start(ji) = 1
       count(ji) = 1
      END DO   
         DO ji = 1 , 2
          IF      (dimid(ji) .EQ. dimlonid ) THEN
            lonpos = ji
          ELSE IF (dimid(ji) .EQ. dimlatid ) THEN
            latpos = ji
          END IF
         END DO        
       start (lonpos) = 1
       count (lonpos) = bimg%nxfile
       start (latpos) = 1
       count (latpos) = bimg%nyfile                                           
      STATUS=NF_GET_VARA_REAL(NCID,varid,start,count,xyloc)
      CALL ERR_HDL(STATUS)
      DO ji=1,bimg%nxfile
       DO jj=1,bimg%nyfile
        xygr(ji,jj,1) = xyloc(ji,jj)     
       END DO
      END DO
      ENDIF
C
C ... clatxy  --> xygr(,,2)
C
      IF ( dimid(2) == 0 ) THEN
C   Dans ce cas, lat(jj) seulement
      lonpos=1
      latpos=2
       start (lonpos) = 1
       count (lonpos) = bimg%nyfile
       start (latpos) = 1
       count (latpos) = 1
       print *, start, count
      STATUS=NF_INQ_VARID(NCID,clatxy,varid)
      CALL ERR_HDL(STATUS)
      STATUS=NF_GET_VARA_REAL(NCID,varid,start,count,xyloc)
      CALL ERR_HDL(STATUS)
      DO ji=1,bimg%nxfile
       DO jj=1,bimg%nyfile
        xygr(ji,jj,2) = xyloc(jj,1)
       END DO
      END DO
      else
C Cas general

      STATUS=NF_INQ_VARID(NCID,clatxy,varid)
      CALL ERR_HDL(STATUS)
      STATUS=NF_GET_VARA_REAL(NCID,varid,start,count,xyloc)
      CALL ERR_HDL(STATUS)

      DO ji=1,bimg%nxfile
       DO jj=1,bimg%nyfile
        xygr(ji,jj,2) = xyloc(ji,jj)
       END DO
      END DO        
      ENDIF

      DO ji =1,bimg%nxfile
       bimg%d_xgrid(ji)= xygr(ji,1,1)
      END DO
C
      DO jj =1,bimg%nyfile
       bimg%d_ygrid(jj)= xygr(1,jj,2)
      END DO
C    
       CALL CdfInit
      RETURN
      END
CC
       SUBROUTINE CdfGetLayer (bimg, local_data,tstep,couche,dim,kx,ky)
CCC----------------------------------------------------------------------------
CCC                     SUBROUTINE CDFGETLAYER
CCC                     **********************
CCC
CCC      PURPOSE:
CCC      --------
CCC         Read the  required layer and update the bimg structure
CC
CC       AUTHOR:
CC       -------
CC           Original: J.M. Molines 10/2001
CCC-----------------------------------------------------------------------------        
CC 0. Declarations
CC ---------------
CC
      IMPLICIT NONE
      INCLUDE 'common.h'
      INCLUDE 'netcdf.inc'
      TYPE( bimgfile ) bimg
      INTEGER NCID,varid,STATUS
      INTEGER dimlonid, dimlatid, dimdepid,dimtimid,dimdimid
      INTEGER kx,ky
      REAL local_data(NXX,NYY)
C     REAL zwork(bimg%nxfile,bimg%nyfile)
      REAL zwork(kx,ky)
      INTEGER tstep, couche, dim
      INTEGER start(10),count(10),dimid(10)
      INTEGER ji, jj
      INTEGER lonpos,latpos,deppos,timpos,dimpos
C
      INCLUDE 'cdfname.h'
CC
CC 1. Get Layer
CC ------------
CC
      PRINT *,' In CdfGetLayer ...' , bimg%modifier
       IF ( bimg%modifier .NE. 'none' ) CALL cmodif (bimg)
      NCID  = bimg%ncid
      varid = bimg%varid
      DO ji = 1,10
       start(ji) = -10
       count(ji) = -10
      END DO
C
      STATUS=NF_INQ_DIMID(NCID,clon,dimlonid)
      CALL ERR_HDL(STATUS)    
C
      STATUS=NF_INQ_DIMID(NCID,clat,dimlatid)
      CALL ERR_HDL(STATUS)      
C
      STATUS=NF_INQ_DIMID(NCID,cdepth,dimdepid)
       IF (STATUS .EQ. NF_EBADDIM ) THEN
        dimdepid = -1
       ENDIF
C
      STATUS=NF_INQ_DIMID(NCID,ctime,dimtimid)
       IF (STATUS .EQ. NF_EBADDIM ) THEN
        dimtimid = -1
       END IF
C
      STATUS=NF_INQ_DIMID(NCID,cdim,dimdimid)
       IF (STATUS .EQ. NF_EBADDIM ) THEN
        dimdimid = -1
       END IF
C
      STATUS=NF_INQ_VARDIMID(NCID,varid,dimid)
      CALL ERR_HDL(STATUS)
         DO ji = 1 , bimg%ndimv
          IF      (dimid(ji) .EQ. dimlonid ) THEN
            lonpos = ji
          ELSE IF (dimid(ji) .EQ. dimlatid ) THEN
            latpos = ji
          ELSE IF (dimid(ji) .EQ. dimdepid ) THEN
            deppos = ji
          ELSE IF (dimid(ji) .EQ. dimtimid ) THEN
            timpos = ji
          ELSE IF (dimid(ji) .EQ. dimdimid ) THEN
            dimpos = ji
          END IF
         END DO  
          start (lonpos) = 1
          count (lonpos) = bimg%nxfile
          start (latpos) = 1
          count (latpos) = bimg%nyfile
C ... if some dimension missing they are assume to be 1
         IF ( deppos .GT. 0 ) THEN
          start (deppos) = couche
          count (deppos) = 1
         END IF
         IF ( timpos .GT. 0 ) THEN
          start (timpos) = tstep
          count (timpos) = 1
         END IF
         IF ( dimpos .GT. 0 ) THEN
          start (dimpos) = dim
          count (dimpos) = 1
         END IF                                                        
C
      STATUS=NF_GET_VARA_REAL(NCID,varid,start,count,zwork)
      CALL ERR_HDL(STATUS)          
      DO ji=1,bimg%nxfile
       DO jj=1,bimg%nyfile
        local_data(ji,jj) = zwork(ji,jj)
       END DO
      END DO
C
      bimg%time       = bimg%timea(tstep)
      bimg%last_time  = tstep
      bimg%last_layer = couche
      bimg%last_dim   = dim   
C
       CALL CdfInit
      RETURN
C
      END
CCC
      SUBROUTINE ERR_HDL(STATUS)
CCC ----------------------------------------------------------
CCC        SUBROUTINE ERR_HDL
CCC        ******************
CCC
CCC   PURPOSE:
CCC   --------
CCC    Error handle for NetCDF routine. Stop if STATUS indicates error
CCC   conditions.
CC
CC    METHOD:
CC    -------
CC       NetCDF 3.4 is used.
CC
CC    REMARK:
CC    ------
CC     Interpretation of the error is given in this routine by NF_STRERROR
CC
CC    AUTHOR
CC    ------
CC     Original: J.M. Molines (01/99)
CC
CCC -----------------------------------------------------------
        IMPLICIT NONE
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        IF (STATUS .NE. NF_NOERR ) THEN
           PRINT *, 'ERROR in NETCDF routine, status=',STATUS
           PRINT *, NF_STRERROR(STATUS)
           STOP
        END IF
        RETURN
        END
C
CC
         SUBROUTINE cmodif (bimg)
CCC --------------------------------------------------------------
CCC              SUBROUTINE CMODIF
CCC              *****************
CCC
CCC    PURPOSE:
CCC    --------
CCC      This routine check the modifier, and store new names in cdtmp
CC
CC     METHOD:
CC     -------
CC       It scan the standard name list to find the required modification,
CC     and save the new name. The modifier refer to non-standard name, it
CC     reports an error.
CC
CC     ARGUMENTS:
CC     ----------
CC       cda   = dummy argument corresponding to the standard name in the
CC               modifier
CC       cdb   = dummy argument corresponding to the new name in the
CC               modifier
CC       cdc   = dummy argument corresponding to the standard name list
CC       cdtmp = modified list of name (output)
CC       kn    = number of standard name
CC
CC     AUTHOR:
CC     -------
CC       Original: J.M. Molines 01/99
CC
CCC ----------------------------------------------------------------------
CC 0. Declarations
CC ----------------
         IMPLICIT NONE
      INCLUDE 'common.h'
      TYPE( bimgfile ) bimg
C ... arguments
         CHARACTER*80 ctmp,cur,ctmp2
         CHARACTER*80 cda, cdb
C ... local
         INTEGER ji,  ll, lc,ii, lnblnk,iii
         INTEGER nv, ipp,jj,jk
         INCLUDE 'cdfname.h'
         INTEGER ip(jpstand)
CC
CC 1. decode the modifier
CC ----------------------
C ... modifier is a string such as 'old1=new1,old2=new2'
C     Each change is separated from the former by a ,
C
      PRINT *,' In Cmodif'
      ctmp=bimg%modifier
      ii=1
      ll=lnblnk(ctmp)
      ctmp2=ctmp
      nv = 0
      ipp=0
      DO WHILE ( index(ctmp2,',') .NE. 0 )
        nv=nv+1
        ip(nv)= index(ctmp2,',')+ipp
        ipp=ip(nv)
        ctmp2= ctmp(ipp+1:)
      END DO
C
      ii=1
      DO ji=1,nv+1
       IF (ji .NE. nv+1) THEN
        jj=ip(ji)
        cur=ctmp(ii:jj-1)      
        ii=jj+1
       ELSE
        cur=ctmp(ii:)
       END IF
C ... Detect the '=' symbol
       ipp = index (cur,'=')
       IF ( ipp . EQ. 0 ) THEN
        PRINT *,' Erreur dans le modifier ',cur 
        STOP
       ELSE
        cda=cur(1:ipp-1)
        cdb=cur(ipp+1:)
       END IF
       PRINT *,cda(1:lnblnk(cda)),'---->', cdb(1:lnblnk(cdb))
CC
CC 2. Find the modifier and return once modif done
CC -----------------------------------------------
         DO jk = 1, jpstand
          IF (cda .EQ. cstandlist(jk) ) THEN
           cstandlist(jk) = cdb
          ENDIF
         END DO
C ... loop on next modifier
       END DO
CC
         RETURN
         END
C
        FUNCTION PrintBimgStructure (bimg)
CCC----------------------------------------------------------------------
CCC
CCC----------------------------------------------------------------------
        IMPLICIT NONE
        include 'common.h'
        INTEGER PrintBimgStructure
        INTEGER lnblnk
        TYPE( bimgfile ) bimg
	    print *,'str1 : ', bimg%str1 ( 1: lnblnk(bimg%str1))
	    print *,'str2 : ', bimg%str2 ( 1: lnblnk(bimg%str2))
	    print *,'str3 : ', bimg%str3 ( 1: lnblnk(bimg%str3))
	    print *,'str4 : ', bimg%str4 ( 1: lnblnk(bimg%str4))
        print *,'VER  : ', bimg%VER
        print *,'fname: ', bimg%fname(1:lnblnk(bimg%fname))
        print *,'nxfile :', bimg%nxfile
        print *,'nyfile :', bimg%nyfile
        print *,'nxdata :', bimg%nxdata
        print *,'nydata :', bimg%nydata
        print *,'ncou   :', bimg%ncou
        print *,'nt     :', bimg%nt  
        print *,'ndim   :', bimg%ndim  
        print *,'icod   :', bimg%icod  
        print *,'iversion:', bimg%iversion
        print *,'irecl  :', bimg%irecl  
        print *,'x1mod  :', bimg%x1mod  
        print *,'y1mod  :', bimg%y1mod  
        print *,'x2mod  :', bimg%x2mod  
        print *,'y2mod  :', bimg%y2mod  
        print *,'dx     :', bimg%dx
        print *,'dy     :', bimg%dy
        print *,'spval  :', bimg%spval
        print *,'spval0 :', bimg%spval0
        print *,'time   :', bimg%time
        print *,'f_unit :', bimg%f_unit
        print *,'last_layer :', bimg%last_layer
        print *,'last_dim :', bimg%last_dim
        print *,'last_time :', bimg%last_time
        print *,'last_rect :', bimg%last_rect
        print *,'last_recr :', bimg%last_recr
        print *,'mask      :', bimg%mask
        print *,'grid      :', bimg%grid
        print *,'shift     :', bimg%shift
C       print *,'d_xgrid   :', bimg%d_xgrid
C       print *,'d_ygrid   :', bimg%d_ygrid
        print *,'alphasup  :', bimg%alphasup
        PrintBimgStructure = 1
C
        RETURN
        END
