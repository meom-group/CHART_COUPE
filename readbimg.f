c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe, anim
c Fichier        :  readbimg%f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
c Mise a Jour    :  
c
c Description    :  Contient les fonctions de lecture d'un fichier de 
c                   donnees de format BIMG.
c 
c ---------------------------------------------------------------------


c ---------------------------------------------------------------------
c 
c Nom         :  BimgOpenFile
c 
c Parametre   :  filename - nom du fichier a ouvrir
c
c Description :  Cette verifie si un fichier BIMG existe, puis
c                l'ouvre en lui assignant un numero d'unite.
c
c Retour      :  BimgOpenFile retourne le numero d'unite.
c
c Note        :  bimg_unit a ete initialise par SetChartDefault ou
c                SetCoupeDefaults.
c 
c ---------------------------------------------------------------------

      integer function BimgOpenFile (bimg)

      implicit none

      character*(256) filename
      character*80 strtmp
      character*4 VER

      integer erreur
      integer FindFile
      integer lnblnk
      integer state
      integer ni,nj,irecl, iversion
      integer isdirect

      include 'common.h'   ! pour common /files/
      TYPE( bimgfile ) bimg
      

      integer bimg_unit
      common  /f_unit/ bimg_unit
      save   /f_unit/
      

      filename= bimg%fname
      erreur = FindFile(filename, 1, lnblnk(filename))      
      if (erreur.eq.1) then
         print *,'erreur : ce fichier n''existe pas : '
         print *,'         ',filename(1:lnblnk(filename))
         stop
      endif 
      bimg%irecl = isdirect(filename)
      irecl = bimg%irecl

      if (irecl .EQ. 0 ) THEN
C Ouverture de fichier sequentiel
C
       open(bimg_unit,file=filename,form='unformatted')
      else if (irecl .GT. 0 ) THEN
C Ouverture du fichier dimg
       open(bimg_unit,file=filename,form='unformatted',
     |     access='direct',recl=irecl)
      else if (irecl .EQ. -1 ) THEN
C Ouverture du fichier NetCdf
       call CdfOpen(filename,bimg)
      else
       PRINT *,' type de fichier non supporte.'
       STOP ' dans BimgOpenFile'
      endif


      BimgOpenFile  = bimg_unit
      bimg_unit = bimg_unit + 1

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  BimgReadHeader
c 
c Parametres  :  file      - structure bimgfile a remplir
c                file_unit - unite du fichier (deja ouvert)
c                h         - tableau d'epaisseur des couches
c
c Description :  Lit les premiers records d'un fichier BIMG et
c                met l'information dans une structure "bimgfile"
c 
c ---------------------------------------------------------------------


      subroutine BimgReadHeader (bimg)

      implicit none
      include 'common.h'

      integer k, it
      integer ier



      TYPE( bimgfile ) bimg


	if (bimg%irecl .EQ. 0 ) THEN
c**      print *,'Lecture du fichier de donnees'
      call flush(6)

c     RECORDS 1-4 -- strings ----------------------------

      read(bimg%f_unit) bimg%str1
      read(bimg%f_unit) bimg%str2
      read(bimg%f_unit) bimg%str3
      read(bimg%f_unit) bimg%str4


c     RECORD 5 ------------------------------------------

      read(bimg%f_unit) bimg%nxfile,bimg%nyfile,bimg%ncou,
     .                  bimg%nt,bimg%ndim,bimg%icod

      bimg%nxdata = bimg%nxfile
      bimg%nydata = bimg%nyfile


c     verifie si les tailles de tableau sont suffisantes

      ier=0
      if (bimg%nxfile.gt.NXX) then
         print *,'erreur: NXX insuffisant (Max : ',NXX,')'
         ier=1
      else if (bimg%nyfile.gt.NYY) then
         print *,'erreur: NYY insuffisant (Max : ',NYY,')'
         ier=1
      else if (bimg%ncou.gt.NA) then
         print *,'erreur: NA insuffisant (Max : ',NA,')'
         ier=1
      endif

      if (ier.eq.1) stop

c     RECORD 6 ------------------------------------------

      read(bimg%f_unit) bimg%x1mod,bimg%y1mod,
     .                  bimg%dx,bimg%dy,bimg%spval

      bimg%grid = 0
      bimg%mask = 0

      bimg%x2mod = bimg%x1mod + (float(bimg%nxfile-1)
     .                            * bimg%dx)
      bimg%y2mod = bimg%y1mod + (float(bimg%nyfile-1)
     .                            * bimg%dy)
       IF (opt_ijgrid .EQ. 1 ) THEN
C ... Use ij coordinates
        opt_print = 0
        bimg%x1mod = 1
        bimg%y1mod = 1
        bimg%dx = 1
        bimg%dy = 1
        bimg%grid = 0
        bimg%mask = 0
        bimg%x2mod = bimg%nxfile
        bimg%y2mod = bimg%nyfile
       ENDIF


      bimg%last_time  = 0
      bimg%last_layer = bimg%ncou
      bimg%last_dim   = bimg%ndim
      bimg%last_rect  = 10000000
      bimg%last_recr  = 0

c     RECORD 7 ------------------------------------------

      read(bimg%f_unit)(bimg%depth(k),k=1,bimg%ncou)
cJMM force les profondeurs a etre negatives ...
	do k=1,bimg%ncou
	 bimg%depth(k)=-1.*abs(bimg%depth(k))
	enddo

	else if (bimg%irecl .GT. 0 ) then
	print *,' Lecture fichiers direct'
C Version 1 La seule supportee (dimg clipper)
C----------
      read(bimg%f_unit,rec=1) bimg%VER,bimg%str1,bimg%irecl,
     |     bimg%nxfile,bimg%nyfile,bimg%ncou,
     |     bimg%nt,bimg%ndim,bimg%x1mod,bimg%y1mod,bimg%dx,bimg%dy,bimg%spval,
     |     (bimg%depth(k),k=1,bimg%ncou),(bimg%timea(it),it=1,bimg%nt)   
c
	print *,' Lecture du header '
      bimg%str2=' pas d''info pour ce champ'
      bimg%str3=' pas d''info pour ce champ'
      bimg%str4=' pas d''info pour ce champ'
      bimg%icod=0

      bimg%nxdata = bimg%nxfile
      bimg%nydata = bimg%nyfile



      bimg%grid = 0
      bimg%mask = 0

      bimg%x2mod = bimg%x1mod + (float(bimg%nxfile-1)
     .                            * bimg%dx)
      bimg%y2mod = bimg%y1mod + (float(bimg%nyfile-1)
     .                            * bimg%dy)


      bimg%last_time  = 0
      bimg%last_layer = bimg%ncou
      bimg%last_dim   = bimg%ndim

cJMM force les profondeurs a etre negatives ...
	do k=1,bimg%ncou
	 bimg%depth(k)=-1.*abs(bimg%depth(k))
	enddo
      else if (bimg%irecl .EQ. -1 ) then
       print *, 'Lecture entete du fichier NetCdf'
       call CdfHeader(bimg)
      else
C     On de doit jamais y passer le prog est stoppe avant
      endif
      if ( opt_spval.eq.1 ) bimg%spval=over_spval
	if (bimg%spval .eq. 0) then
          bimg%spval0 = .true.
        else
          bimg%spval0 = .false.
	endif
         bimg%shift = 0
      return
      end





c ---------------------------------------------------------------------
c 
c Nom         :  BimgReadData
c 
c Parametres  :  data_out      - tableau a remplir
c                couche        - la couche a lire
c                bimg          - structure d'information prealablement
c                                remplie par BimgReadHeader
c                coords        - 
c                dim           - composante a lire
c
c Description :  Lit la couche demandee dans le fichier de donnees et
c                transfert la partie specifiee dans le tableau data_out.
c 
c ---------------------------------------------------------------------


      subroutine BimgReadData (data_out, bimg,coords,tstep, couche,dim)
                           
      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimg

      integer couche
      real coords(4)
      integer dim, tstep
      character*10 strCouche, strTime, strDim

      real data_out(NXX,NYY)  ! tableau fourni par la fonction d'appel
      real local_data(NXX,NYY)
      real*8 zmean
      integer i,j, nmean

      integer imin,imax,jmin,jmax, ksup, kinf

      TYPE( bimgfile ) spembimg
      common /spem/ spembimg, imin,imax,jmin,jmax
      save   /spem/
      real zlat, zlon
      logical good_point

      integer lnblnk
      print '( 3(a,i10) )', 'Temps :',tstep,
     $              'Couche :', couche,
     $              'Dimension :', dim
      call flush(6)
      if (opt_dep.ne.1) then
	if (bimg%irecl .EQ. 0 ) then
      call BimgGetLayer   (bimg, local_data, tstep, couche, dim)
        else if (bimg%irecl .GT. 0 ) then
      call DirectGetLayer (bimg, local_data, tstep, couche, dim)
        else if (bimg%irecl .EQ. -1 ) then
      call CdfGetLayer    (bimg, local_data, tstep, couche, dim,bimg%nxfile,bimg%nyfile)
        else
C  On ne doit jamais passer par ici
	endif
       else
C interpolation des 2 niveaux pour avoir la hauteur exacte 
      ksup=couche
      kinf=couche + 1
	print *,'Interpollation entre couche ',ksup ,' et ',kinf 
        print *,' poids :',bimg%alphasup, 1.- bimg%alphasup
	print *,' depth :',bimg%depth(ksup), req_dep, bimg%depth(kinf)

C lecture du niveau superieur
        if (bimg%irecl .eq. 0 ) then
      call BimgGetLayer   (bimg, local_data, tstep, ksup, dim)
        else if (bimg%irecl .GT. 0 ) then  
      call DirectGetLayer (bimg, local_data, tstep, ksup, dim)
        else if (bimg%irecl .EQ. -1 ) then
      call CdfGetLayer    (bimg, local_data, tstep, ksup, dim,bimg%nxfile,bimg%nyfile)
        else
C  On ne doit jamais passer par ici    
        endif
C lecture du niveau inferieur
        if (bimg%irecl .eq. 0  ) then
      call BimgGetLayer   (bimg, data_out, tstep, kinf, dim)
        else if (bimg%irecl .GT. 0 ) then
      call DirectGetLayer (bimg, data_out, tstep, kinf, dim)
        else if (bimg%irecl .EQ. -1 ) then
      call CdfGetLayer    (bimg, data_out, tstep, kinf, dim,bimg%nxfile,bimg%nyfile)
        else
C  On ne doit jamais passer par ici      
        endif
C interpollation en prenant garde aux valeurs speciales
	do i=1,bimg%nxdata
	do j=1,bimg%nydata
	  if (data_out(i,j).ne.bimg%spval .AND. 
     .        local_data(i,j).ne.bimg%spval) then
             local_data(i,j)=bimg%alphasup*local_data(i,j) +
     .                       (1.-bimg%alphasup)*data_out(i,j)
          else
             local_data(i,j)=bimg%spval
          endif
         enddo
         enddo
        endif

C
C Verifie que spval n'est pas null --> pb de contourages.
C Si spval=0, alors, on passe a un spval de defspval. , avec
C le risque de flaguer des bonnes valeurs 0.
	if (bimg%spval0) then
	  bimg%spval=defspval
	   do i=1,bimg%nxfile
            do j=1,bimg%nyfile
              if (local_data(i,j).eq. 0.) local_data(i,j)=bimg%spval
            enddo
           enddo
         endif
 
C     si un masque a ete specifie, remplacer les zones a masquer
C     par spval dans le tableau de donnees.
C
      if (bimg%mask.eq.1) then
         print *, 'masking data'
         call BimgMaskData (bimg, local_data)
      endif 
      
C     option -scale
      if (opt_scale .eq. 1 ) then
         print *,' Scaling input data by ', zscale 
           do i=1,bimg%nxfile
            do j=1,bimg%nyfile
              if (local_data(i,j) .ne. bimg%spval ) local_data(i,j)=local_data(i,j)*zscale
            enddo
           enddo
      endif

C     option SHIFT
      if (opt_shift.eq.1) then
         call BimgShift (bimg, local_data, map_shift, coords)
        if (opt_clrgrid .ge.1) then
         call BimgShiftGrid (bimg, map_shift)
        endif
         bimg%x1mod = bimg%x1mod + map_shift
      endif 


c     si une grille a ete specifiee, on passe les donnees sur une 
c     grille reguliere de taille NXX, NYY, avec par default une
c     interpolation bi-lineaire.
        
      if (bimg%grid.eq.1) then
        if (bimg%nxfile.eq.1) then
         call BimgRegularGridy(local_data, bimg)
	 bimg%nxdata = 1
	 bimg%nydata = NYY
         bimg%dx = 0
         bimg%dy = (bimg%d_ygrid(bimg%nyfile) - bimg%d_ygrid(1))
     .             / float (NYY-1)
        elseif (bimg%nyfile.eq.1) then
         call BimgRegularGridx(local_data, bimg)
	 bimg%nxdata = NXX
	 bimg%nydata = 1
         bimg%dx = (bimg%d_xgrid(bimg%nxfile) - bimg%d_xgrid(1))
     .             / float (NXX-1)
         bimg%dy = 0
        else
         call BimgRegularGrid (local_data, bimg)
         bimg%nxdata  = NXX
         bimg%nydata  = NYY
         bimg%dx = (bimg%d_xgrid(bimg%nxfile) - bimg%d_xgrid(1))
     .             / float (NXX-1)
         bimg%dy = (bimg%d_ygrid(bimg%nyfile) - bimg%d_ygrid(1)) 
     .             / float (NYY-1)
        endif
      elseif  (bimg%grid.eq.2) then
         print *,'Grille irreguliere -option slow.. soyez patient'
         call BimgRegularGridSlow (local_data, bimg)
         bimg%nxdata  = NXX
         bimg%nydata  = NYY
         bimg%dx = (bimg%d_xgrid(bimg%nxfile) - bimg%d_xgrid(1))
     .             / float (NXX-1)
         bimg%dy = (bimg%d_ygrid(bimg%nyfile) - bimg%d_ygrid(1)) 
     .             / float (NYY-1)
      else
c     ce cas inclu les grilles completement irregulieres, car dans ce cas
c     aucune interpolation n'est faite. C'est au moment du trace, via cpmpxy que ce
c    sera fait.
         bimg%nxdata = bimg%nxfile
         bimg%nydata = bimg%nyfile
      endif 

c     calcule la partie du fichier qui nous interesse et la transfere
c     dans le tableau
c     Le tableau coords contient les coordonnees xmin xmax ymin ymax de la zone
c     d'interet, soit definie par les data, soit par le zoom.
c     Dans le cas de grille irreguliere (-grid ou -gridxy), les x1mod, x2mod 
c     y1mod et y2mod sont mis a jour correctement dans la routine de lecture 
c     de la grille (GetGridInfo), et passes dans coords dans OpenAndVerify.

      if (bimg%grid.ne.3) then
	if (bimg%dx.eq.0.) then
      imin=1
      imax=1
        else
      imin = nint((coords(1) - bimg%x1mod)/bimg%dx)+1
      imax = nint((coords(2) - bimg%x1mod)/bimg%dx)+1

        endif

        if (bimg%dy.eq.0.) then
      jmin=1
      jmax=1
        else
      jmin = nint((coords(3) - bimg%y1mod)/bimg%dy)+1
      jmax = nint((coords(4) - bimg%y1mod)/bimg%dy)+1
        endif
c     reajustement des coordonnees pour mieux correspondre a la grille
c     de donnees.

c     ATTENTION, ca va causer des pb le jour ou il y aura des donnees
c     avec des pas de grille differents......

      coords(1)=bimg%x1mod+bimg%dx*(imin-1)
      coords(2)=bimg%x1mod+bimg%dx*(imax-1)
      coords(3)=bimg%y1mod+bimg%dy*(jmin-1)
      coords(4)=bimg%y1mod+bimg%dy*(jmax-1)

      else
C cas d'une grille completement irreguliere ...
C  temporaire ...
	imin=1
	imax=bimg%nxdata
	jmin=1
	jmax=bimg%nydata
C Dans ce cas pour les zoom, il faut masquer les valeurs hors du champs
C car dans le cas contraire, les valeurs min et max sont calcules globalement.
	if (opt_zoom.eq.1 .and. opt_chart.eq.1) then
	do i=imin,imax
	 do j=jmin,jmax
	   zlat=xygr(i,j,2)
	   zlon=xygr(i,j,1)
	   good_point=.true.
	   good_point=(zlat.lt.coords(4))
	   good_point=good_point .AND. (zlat.gt.coords(3))
	   good_point=good_point .AND. (zlon.lt.coords(2))
	   good_point=good_point .AND. (zlon.gt.coords(1))
	   if (.not.good_point) local_data(i,j)=bimg%spval
         enddo
        enddo
        endif

      endif


c     verifie que les coordonnees sont bien dans ce fichier

      if ((imin.gt.bimg%nxdata).or.(imax.gt.bimg%nxdata).or.
     .    (jmin.gt.bimg%nydata).or.(jmax.gt.bimg%nydata)) then
         print *,'erreur : coordonnes hors du domaine'
         print *,coords(1),coords(2),coords(3),coords(4)
         print *,imin,bimg%nxdata
         print *,imax,bimg%nxdata
         print *,jmin,bimg%nydata
         print *,jmax,bimg%nydata
         stop
      endif 


c     Si les coordonnees sont inversees, le tableau de donnees est inverse.
c     Utilise pour afficher une coupe de gauche a droite du premier
c     au second point. Normalement, chart verifie que les points sont
c     dans le bon ordre.


c     cas normal (toujours, pour CHART)

      if ((imin.le.imax).and.(jmin.le.jmax)) then
         bimg%nxdata = imax - imin !  + 1
         bimg%nydata = jmax - jmin ! + 1
C     option -mean : change the mean value of local data set
       if (opt_mean .eq. 1 ) then
         print *, 'changing mean value of extracted data to ', zmean0
         print *, ' bimg%nxfile = ', bimg%nxfile
         print *, ' bimg%nxdata = ', bimg%nxdata
         nmean = 0
         zmean = 0.d0
         print *, imin, imax, jmin, jmax, bimg%spval
         DO i=imin, imax
          do j=jmin, jmax
            if ( local_data(i,j).ne. bimg%spval ) then
               nmean=nmean+1 
               zmean=zmean + local_data(i,j)
            endif
          enddo
         enddo
         print *, ' NMEAN = ', nmean
         zmean = zmean/nmean
         print *, ' ZMEAN = ', zmean
         
         DO i=imin, imax
          do j=jmin, jmax
            if ( local_data(i,j).ne. bimg%spval ) then
                local_data(i,j) = local_data(i,j) -zmean +zmean0
            endif
          enddo
         enddo
        end if

        IF ( opt_log == 1 .OR. ( opt_cntlog == 1 .AND. bimg%lcnt ) .OR. (opt_clrlog == 1 .AND. bimg%lclr) ) THEN
          WHERE(local_data(imin:imax,jmin:jmax) /=  bimg%spval .AND. 
     .          local_data(imin:imax,jmin:jmax) /= 0 ) 
              local_data(imin:imax,jmin:jmax) = log10( local_data(imin:imax,jmin:jmax))
          ELSEWHERE
              local_data(imin:imax,jmin:jmax) =  bimg%spval 
          ENDWHERE
        ENDIF
         
         do i=imin,imax
            do j=jmin,jmax
               data_out(i-imin+1,j-jmin+1) = local_data(i,j)
            enddo
         enddo

c     longitudes et latitudes inversees

      else if ((imax.lt.imin).and.(jmax.lt.jmin)) then
         bimg%nxdata = imin - imax !  + 1
         bimg%nydata = jmin - jmax !  + 1
         
         do i=1,imin-imax+1
            do j=1, jmin-jmax+1
               data_out(i,j) = local_data(imin-i+1,jmin-j+1)
            enddo
         enddo


c     longitudes inversees

      elseif ((imax.lt.imin).and.(jmin.le.jmax)) then
         bimg%nxdata = imin - imax !  + 1
         bimg%nydata = jmax - jmin !  + 1
         
         do i=1,imin-imax+1
            do j=jmin,jmax
               data_out(i,j-jmin+1) = local_data(imin-i+1,j)
            enddo
         enddo


c     latitudes inversees

      elseif ((imin.le.imax).and.(jmax.lt.jmin)) then
         bimg%nxdata = imax - imin !  + 1
         bimg%nydata = jmin - jmax !  + 1
         
         do i=imin,imax
            do j=1, jmin-jmax+1
               data_out(i-imin+1,j) = local_data(i,jmin-j+1)
            enddo
         enddo
      endif 

c     il est possible que imin = imax ou jmin = jmax lors d'une
c     coupe le long d'un meridien

      if (bimg%nxdata.eq.0) bimg%nxdata = 1
      if (bimg%nydata.eq.0) bimg%nydata = 1


      if (opt_shift.eq.1) then
         bimg%x1mod = bimg%x1mod - map_shift
      endif 
C restore original spval for next layer. 
C final change to spval will be done in the call to bimgreaddata
	if (bimg%spval0) bimg%spval = 0.

      return
      end






c ---------------------------------------------------------------------
c 
c Nom         :  BimgRegularGridSlow
c 
c Parametres  :  
c Description :  Cette fonction considere que les donnees sont reparties 
c                selon la grille lue par BimgGetGridInfo et utilise la 
c                fonction CPSPS2 pour interpoler les donnees sur une 
c                grille reguliere.
c 
c ---------------------------------------------------------------------

      subroutine BimgRegularGridSlow (data_in_out, bimg)


      implicit none

      include 'common.h'
      include 'workarray.h'

      real data_in_out (NXX,NYY)
        
      TYPE( bimgfile ) bimg

      integer i,j

c     voir Contouring and Mapping tutorial page 227

      real    rwrk (100 * ((NXX*NYY + 99)/100) + 3*NYY*NXX + NYY
     .              + 2*NXX)
      integer iwrk (100 * ((NXX*NYY + 99)/100))
      integer lrwk, liwk, lzrg
      integer nx, ny

      real rl,rr,rb,rt,ur,ul,ut,ub
      integer l

c     voir Contouring and Mapping tutorial page 227

      lrwk = 100 * ((NXX*NYY + 99)/100) + 3*NYY*NXX + NYY + 2*NXX
      liwk = 100 * ((NXX*NYY + 99)/100)
      lzrg = NXX*NYY

      call cpseti ('ZDS - Z data array Dimension Selector', 0)
      call cpseti ('ZD1 - Z data array Dimension 1', NXX)
      call cpseti ('ZDM - Z data array Dimension M', NXX)
      call cpseti ('ZDN - Z data array Dimension N', NYY)
      call cpsetr ('SPV - SPecial Value', bimg%spval)

      nx = bimg%nxfile
      ny = bimg%nyfile


c     getset et set sont necessaires, pcq CPSPS2 fait aussi un 
c     appel a set, ce qui cause des effets indesirables.

      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
      call cpsps2 (bimg%d_xgrid,bimg%d_ygrid, data_in_out,
     .             NXX, nx, ny, rwrk, lrwk,
     .             iwrk, liwk, work_array, lzrg)
      call set(rl,rr,rb,rt,ur,ul,ut,ub,l)

      do i=1,NXX
         do j=1,NYY
            data_in_out(i,j) = work_array(i,j)
         enddo 
      enddo 



      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  BimgRegularGrid
c 
c Parametres  :  
c Description :  Cette fonction considere que les donnees sont reparties 
c                selon la grille lue par BimgGetGridInfo et utilise une 
c                interpolation bi lineaire pour interpoler les donnees sur
c                une  grille reguliere. Beaucoup + rapide
c 
c ---------------------------------------------------------------------
      subroutine BimgRegularGrid (data_in_out, bimg)


      implicit none

      include 'common.h'
      include 'workarray.h'

      real data_in_out (NXX,NYY)
        
      TYPE( bimgfile ) bimg

      integer i,j, ifrst,ii, jj, nx, ny

      real facx(NXX), facy(NYY), dx, dy, P1, P2, P3, P4
      real xx, yy
      integer i0(NXX), j0(NYY)

       save facx,facy,i0,j0,ifrst

	data ifrst/0/

      nx = bimg%nxfile
      ny = bimg%nyfile

      if (ifrst.eq.0) then
       ifrst=1
c
c initialise les tableaux i0,j0, facx et facy
c
        dx=(bimg%d_xgrid(nx)-bimg%d_xgrid(1))/(NXX-1)
        dy=(bimg%d_ygrid(ny)-bimg%d_ygrid(1))/(NYY-1)
	 i0(1)=1
	 j0(1)=1
	 facx(1)=0
	 facy(1)=0
	 i0(NXX)=nx
	 j0(NYY)=ny
	 facx(NXX)=0
	 facy(NYY)=0
         i=1
         j=1
       do ii=2,NXX-1
         xx=(ii-1)*dx+bimg%d_xgrid(1)
           do while (xx.ge.bimg%d_xgrid(i))
             i=i+1
           enddo
	  i=i-1
         i0(ii)=i
         facx(ii)=(xx-bimg%d_xgrid(i))/
     |            (bimg%d_xgrid(i+1)-bimg%d_xgrid(i))
       enddo

       do jj=2,NYY-1
         yy=(jj-1)*dy+bimg%d_ygrid(1)
           do while (yy.ge.bimg%d_ygrid(j))
             j=j+1
           enddo
	  j=j-1
         j0(jj)=j
         facy(jj)=(yy-bimg%d_ygrid(j))/
     |            (bimg%d_ygrid(j+1)-bimg%d_ygrid(j))
       enddo

      endif
	work_array(1,1)=data_in_out(1,1)
	work_array(NXX,NYY)=data_in_out(nx,ny)

      do ii=2,NXX-1
      do jj=2,NYY-1
        P1=data_in_out(i0(ii),j0(jj))
        P2=data_in_out(i0(ii)+1,j0(jj))
        P3=data_in_out(i0(ii)+1,j0(jj)+1)
        P4=data_in_out(i0(ii),j0(jj)+1)
        if (P1.eq.bimg%spval.or.P2.eq.bimg%spval.or.
     |      P3.eq.bimg%spval.or.P4.eq.bimg%spval) then
        work_array(ii,jj)=bimg%spval
        else
        work_array(ii,jj)=
     |   (1-facx(ii))*(1-facy(jj))*P1 +
     |   facx(ii)*(1-facy(jj))*P2 +
     |   facx(ii)*facy(jj)*P3 +
     |   (1-facx(ii))*facy(jj)*P4
        endif
      enddo
      enddo
c
c traitement des bords du domaine. interpolation sur 2 pts seulements
c
	jj=1
      do ii=2,NXX-1
        P1=data_in_out(i0(ii),j0(jj))
        P2=data_in_out(i0(ii)+1,j0(jj))
        if (P1.eq.bimg%spval.or.P2.eq.bimg%spval) then
        work_array(ii,jj)=bimg%spval
        else
        work_array(ii,jj)=
     |   (1-facx(ii))*P1 +
     |   facx(ii)*P2 
        endif
      enddo
	jj=NYY
      do ii=2,NXX-1
        P1=data_in_out(i0(ii),j0(jj))
        P2=data_in_out(i0(ii)+1,j0(jj))
        if (P1.eq.bimg%spval.or.P2.eq.bimg%spval) then
        work_array(ii,jj)=bimg%spval
        else
        work_array(ii,jj)=
     |   (1-facx(ii))*P1 +
     |   facx(ii)*P2 
        endif
      enddo
c
	ii=1
      do jj=2,NYY-1
        P1=data_in_out(i0(ii),j0(jj))
        P4=data_in_out(i0(ii),j0(jj)+1)
        if (P1.eq.bimg%spval.or.P4.eq.bimg%spval) then
        work_array(ii,jj)=bimg%spval
        else
        work_array(ii,jj)=
     |   (1-facy(jj))*P1 +
     |   facy(jj)*P4
        endif
      enddo
	ii=NXX
      do jj=2,NYY-1
        P1=data_in_out(i0(ii),j0(jj))
        P4=data_in_out(i0(ii),j0(jj)+1)
        if (P1.eq.bimg%spval.or.P4.eq.bimg%spval) then
        work_array(ii,jj)=bimg%spval
        else
        work_array(ii,jj)=
     |   (1-facy(jj))*P1 +
     |   facy(jj)*P4
        endif
      enddo




      do i=1,NXX
         do j=1,NYY
            data_in_out(i,j) = work_array(i,j)
         enddo 
      enddo 
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  BimgRegularGridy
c 
c Parametres  :  
c Description :  Cette fonction considere que les donnees sont reparties 
c                selon la grille lue par BimgGetGridInfo et utilise une 
c                interpolation lineaire pour interpoler les donnees sur
c                une  grille reguliere (1,NYY). Beaucoup + rapide
c 
c ---------------------------------------------------------------------
      subroutine BimgRegularGridy (data_in_out, bimg)


      implicit none

      include 'common.h'
      include 'workarray.h'

      real data_in_out (NXX,NYY)
        
      TYPE( bimgfile ) bimg

      integer j, ifrst, jj,  ny

      real  facy(NYY), dy, P1, P4
      real  yy
      integer  j0(NYY)

       save facy,j0,ifrst

	data ifrst/0/

      ny = bimg%nyfile

      if (ifrst.eq.0) then
       ifrst=1
c
c initialise les tableaux i0,j0, facx et facy
c
        dy=(bimg%d_ygrid(ny)-bimg%d_ygrid(1))/(NYY-1)
	 j0(1)=1
	 facy(1)=0
	 j0(NYY)=ny
	 facy(NYY)=0
         j=1
 
       do jj=2,NYY-1
         yy=(jj-1)*dy+bimg%d_ygrid(1)
           do while (yy.ge.bimg%d_ygrid(j))
             j=j+1
           enddo
	  j=j-1
         j0(jj)=j
         facy(jj)=(yy-bimg%d_ygrid(j))/
     |            (bimg%d_ygrid(j+1)-bimg%d_ygrid(j))
       enddo

      endif
	work_array(1,1)=data_in_out(1,1)
	work_array(1,NYY)=data_in_out(1,ny)

      do jj=2,NYY-1
        P1=data_in_out(1,j0(jj))
        P4=data_in_out(1,j0(jj)+1)
        if (P1.eq.bimg%spval.or.P4.eq.bimg%spval) then
        work_array(1,jj)=bimg%spval
        else
        work_array(1,jj)= (1-facy(jj))*P1 + facy(jj)*P4
        endif
      enddo

         do j=1,NYY
            data_in_out(1,j) = work_array(1,j)
         enddo 
      return
      end



c ---------------------------------------------------------------------
c ---------------------------------------------------------------------
c 
c Nom         :  BimgRegularGridx
c 
c Parametres  :  
c Description :  Cette fonction considere que les donnees sont reparties 
c                selon la grille lue par BimgGetGridInfo et utilise une 
c                interpolation bi lineaire pour interpoler les donnees sur
c                une  grille reguliere. Beaucoup + rapide
c 
c ---------------------------------------------------------------------
      subroutine BimgRegularGridx (data_in_out, bimg)


      implicit none

      include 'common.h'
      include 'workarray.h'

      real data_in_out (NXX,NYY)
        
      TYPE( bimgfile ) bimg

      integer i, ifrst,ii,  nx

      real facx(NXX),  dx,  P1, P2
      real xx
      integer i0(NXX)

       save facx,i0,ifrst

	data ifrst/0/

      nx = bimg%nxfile

      if (ifrst.eq.0) then
       ifrst=1
c
c initialise les tableaux i0,j0, facx et facy
c
        dx=(bimg%d_xgrid(nx)-bimg%d_xgrid(1))/(NXX-1)
	 i0(1)=1
	 facx(1)=0
	 i0(NXX)=nx
	 facx(NXX)=0
         i=1
       do ii=2,NXX-1
         xx=(ii-1)*dx+bimg%d_xgrid(1)
           do while (xx.ge.bimg%d_xgrid(i))
             i=i+1
           enddo
	  i=i-1
         i0(ii)=i
         facx(ii)=(xx-bimg%d_xgrid(i))/
     |            (bimg%d_xgrid(i+1)-bimg%d_xgrid(i))
       enddo



      endif
	work_array(1,1)=data_in_out(1,1)
	work_array(NXX,1)=data_in_out(nx,1)

      do ii=2,NXX-1
        P1=data_in_out(i0(ii),1)
        P2=data_in_out(i0(ii)+1,1)
        if (P1.eq.bimg%spval.or.P2.eq.bimg%spval) then
        work_array(ii,1)=bimg%spval
        else
        work_array(ii,1)= (1-facx(ii))*P1 +facx(ii)*P2 
        endif
      enddo

      do i=1,NXX
            data_in_out(i,1) = work_array(i,1)
      enddo 
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  BimgGetLayer
c 
c Parametres  :  bimg    - structure decrivant le fichier
c                layer   - couche a lire
c                buf     - tableau a remplir
c                dim     - composante a lire
c 
c Description :  Initialise un tableau de donnees a spval et y
c                transfere le contenu de la couche demandee.
c
c        Note :  Le fichier reste ouvert entre chaque appel et la
c                fonction prend pour acquis que le pointeur n'a pas
c                change de place.
c 
c ---------------------------------------------------------------------

      subroutine BimgGetLayer0 (bimg, buf, tstep, layer, dim)

      implicit none

      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg
      integer dim, layer, tstep
      real buf (NXX,NYY)

      integer current_layer, current_dim, current_time

      integer i,j



c     if (dim.gt.3) then
c        print *, 'Vous ne pouvez avoir plus de trois dimensions'
c        stop
c     endif 

      if (bimg%last_time.eq.tstep) then

         if (bimg%last_layer.eq.layer) then
            do current_dim = bimg%last_dim+1,dim
               read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
            enddo 
         else       
            do current_dim = bimg%last_dim+1,bimg%ndim
               read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
            enddo 

            do current_layer=bimg%last_layer+1,layer-1
               do current_dim = 1,bimg%ndim
                  read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                            j=1,bimg%nyfile)
               enddo 
            enddo 

            do current_dim = 1,dim
               read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                         j=1,bimg%nyfile)
            enddo 
         endif 
      else 
         do current_dim = bimg%last_dim+1, bimg%ndim
            read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                         j=1,bimg%nyfile)
         enddo 

         do current_layer = bimg%last_layer+1, bimg%ncou
            do current_dim = 1,bimg%ndim
               read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                            j=1,bimg%nyfile)
            enddo 
         enddo 
         
         do current_time = bimg%last_time+1, tstep-1
             read(bimg%f_unit) bimg%time
            do current_layer = 1, bimg%ncou
                do current_dim = 1,bimg%ndim
                   read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                               j=1,bimg%nyfile)
               enddo 
            enddo 
         enddo 
         
         read(bimg%f_unit) bimg%time
         do current_layer = 1, layer-1
            do current_dim = 1,bimg%ndim
               read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
            enddo 
         enddo 

         do current_dim = 1,dim
            read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                j=1,bimg%nyfile)
         enddo 
      endif 
         
      bimg%last_time  = tstep
      bimg%last_layer = layer
      bimg%last_dim   = dim

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  BimgGetLayer
c 
c Parametres  :  bimg    - structure decrivant le fichier
c                layer   - couche a lire
c                buf     - tableau a remplir
c                dim     - composante a lire
c 
c Description :  Initialise un tableau de donnees a spval et y
c                transfere le contenu de la couche demandee.
c
c NOTE: Cette version suppose que le fichier reste ouvert, mais le
c       pointeur peut etre deplace, on se recale toujours /debut
c 
c ---------------------------------------------------------------------

      subroutine BimgGetLayer (bimg, buf, tstep, layer, dim)

      implicit none

      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg
      integer dim, layer, tstep
      real buf (NXX,NYY)


      integer i,j
      integer irect, irecr, irecdif, irecdift
      integer idstp 

	irect = 7 + (tstep-1)*(bimg%ncou*bimg%ndim + 1 )
	idstp = tstep - bimg%last_time
        irecdift = irect - bimg%last_rect

	irecr =     (layer-1)*bimg%ndim + (dim -1)
        irecdif = irecr - bimg%last_recr

	if (irecdift .eq. 0 .and. irecdif .gt. 0 ) then
	   do i=1,irecdif - 1
            read(bimg%f_unit)
           enddo          
            read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
        else if (irecdift.gt.0) then
c  Aller a la fin du bloc temps en cours:
           do i=1,bimg%ncou*bimg%ndim -(bimg%last_recr +1)
            read(bimg%f_unit)
           enddo          
c  Sauter les blocs temps inutiles
           do j=1,idstp -1 
	      do i=1, bimg%ncou*bimg%ndim + 1
               read(bimg%f_unit)
	      enddo
           enddo
c  Lire le temps ad hoc
	read(bimg%f_unit) bimg%time
c  Se positionner a irecr
        do i=1,irecr
        read(bimg%f_unit)
        enddo
c Lire les donnees:
        read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
        else
c Il y a un retour en arriere, on se synchronise sur le debut

	rewind(bimg%f_unit)

	do i=1,irect
	read(bimg%f_unit)
	enddo

	read(bimg%f_unit) bimg%time

	do i=1,irecr
	read(bimg%f_unit)
	enddo

        read(bimg%f_unit) ((buf(i,j),i=1,bimg%nxfile),
     .                             j=1,bimg%nyfile)
        endif
      bimg%last_time  = tstep
      bimg%last_layer = layer
      bimg%last_dim   = dim
      bimg%last_rect  = irect
      bimg%last_recr  = irecr

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  DirectGetLayer
c 
c Parametres  :  bimg    - structure decrivant le fichier
c                layer   - couche a lire
c                buf     - tableau a remplir
c                dim     - composante a lire
c 
c Description :  Initialise un tableau de donnees a spval et y
c                transfere le contenu de la couche demandee.
c                Idem BimgGetLayer mais acces direct.
c
c        Note :  Le fichier reste ouvert entre chaque appel et la
c                fonction prend pour acquis que le pointeur n'a pas
c                change de place.
c 
c ---------------------------------------------------------------------

      subroutine DirectGetLayer (bimg, buf, tstep, layer, dim)

      implicit none

      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg
      integer dim, layer, tstep
      real buf (NXX,NYY)


      integer i,j, irec


	irec=2 +(tstep-1)*bimg%ncou*bimg%ndim + (layer -1)*bimg%ndim +(dim -1)
	print *,' Lecture du record ',irec, tstep,layer,dim

            read(bimg%f_unit,rec=irec) ((buf(i,j),i=1,bimg%nxfile),
     .                j=1,bimg%nyfile)

      bimg%time = bimg%timea(tstep)        
      bimg%last_time  = tstep
      bimg%last_layer = layer
      bimg%last_dim   = dim

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  BimgGetGridInfo
c 
c Parametres  :  bimg   - structure d'information sur le fichier
c                grid   - structure d'information sur la grille
c                vector - indique si il s'agit d'une grille pour vecteurs
c 
c Description :  Lit un fichier contenant les coordonnees des points
c                d'une grille et remplit deux tableaux (x et y)
c 
c ---------------------------------------------------------------------

      subroutine BimgGetGridInfo (bimg,grid)

      implicit none

      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg, grid

      integer i,j, yet_printed
      real time_bidon
	data yet_printed /0/


c     Lecture de tout le fichier

      read(grid%f_unit) time_bidon
      read(grid%f_unit) ((work_array(i,j),i=1,bimg%nxfile),
     .                          j=1,bimg%nyfile)
	nigr=bimg%nxfile
	njgr=bimg%nyfile

	do i=1,bimg%nxfile
	 do j=1,bimg%nyfile
          xygr(i,j,1)=work_array(i,j)
	 enddo
        enddo
	
      do i=1, bimg%nxfile
         bimg%d_xgrid(i) = work_array(i,1)
      enddo 

c      read(grid%f_unit) time_bidon
      read(grid%f_unit) ((work_array(i,j),i=1,bimg%nxfile),
     .                          j=1,bimg%nyfile)

	do i=1,bimg%nxfile
	 do j=1,bimg%nyfile
          xygr(i,j,2)=work_array(i,j)
	 enddo
        enddo

      do j=1, bimg%nyfile
         bimg%d_ygrid(j) = work_array(1,j)
      enddo 
C
C choisi des valeurs particulieres bidon mais utiles pour detecter les
c problemes de bord: symetrie du premier pts dans le domaine, sert dans
c coupe seulement (CalculateCutPlane)
C
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

c
c     reajuste les coordonnees du fichier selon la grille
	bimg%grid  = opt_clrgrid

      bimg%x1mod = bimg%d_xgrid(1)
      bimg%x2mod = bimg%d_xgrid(bimg%nxdata)
      bimg%y1mod = bimg%d_ygrid(1)
      bimg%y2mod = bimg%d_ygrid(bimg%nydata)

      if (opt_clrgrid.eq.3) then
c Dans ce cas (grille completement irreguliere), il faut determiner
c le min et le max en balayant toute la grille.
c 
c initialise
	bimg%x1mod=99999.0
	bimg%y1mod=99999.0
	bimg%x2mod=-99999.0
	bimg%y2mod=-99999.0
c option -360 : ajoute 360 deg aux longitudes negatives
        PRINT *,' OPT_360 =', opt_360
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

	if (yet_printed.eq.0) then
	yet_printed = 1
	print *,' LIMITES DE LA GRILLE IRREGULIERE:'
	print *,'   x1mod=',bimg%x1mod
	print *,'   x2mod=',bimg%x2mod
	print *,'   y1mod=',bimg%y1mod
	print *,'   y2mod=',bimg%y2mod
	endif

      endif
      
C     if (bimg%nxfile.eq.1) then
C     bimg%dx=0.
C     else
C     bimg%dx = (bimg%d_xgrid(bimg%nxfile) - bimg%d_xgrid(1))
C    .             / float (NXX-1)
C     endif

C     if (bimg%nyfile.eq.1) then
C     bimg%dy=0.
C     else
C     bimg%dy = (bimg%d_ygrid(bimg%nyfile) - bimg%d_ygrid(1))
C    .             / float (NYY-1)
C     endif

      return
      end
      

c ---------------------------------------------------------------------
c 
c Nom         :  BimgMaskData
c 
c Parametres  :  bimg   - structure decrivant le fichier de donnees
c                tomask - donnees a masquer
c 
c Description :  Cette fonction lit un masque et remplace les zones
c                a masquer par spval dans le tableau tomask
c 
c ---------------------------------------------------------------------

      subroutine BimgMaskData (bimg, tomask)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimg
      real tomask (NXX,NYY)
      integer i,j
      
	if (opt_clrmask.eq.1) then
      do i=1,bimg%nxfile
         do j=1, bimg%nyfile
            if (bimg%d_mask (i,j).eq.1.0) then
               tomask(i,j) = bimg%spval
            endif 
         enddo  
      enddo 
	else if (opt_clrmask.eq.2) then
C mask de type opa
      do i=1,bimg%nxfile
         do j=1, bimg%nyfile
            if (bimg%d_mask (i,j).eq.0.0) then
               tomask(i,j) = bimg%spval
            endif    
         enddo   
      enddo
        endif
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  Shift
c
c                JMM - 15/03/94
c 
c ---------------------------------------------------------------------

      subroutine BimgShift (bimg, work, shift, coords)

      implicit none
      include 'common.h'

      TYPE( bimgfile ) bimg

      real tmp (NXX)
      real work (NXX, NYY)
      real shift
      real coords(4)
      
      integer i,j, i1
      
      
c     test de compatibilite:
c     ce programme n'a d'interet que si x2mod=x1mod a 360 deg pres
c     puisque l'on recopie la bande de gauche sur la droite.
      print *,' SHIFT: nxfile',bimg%nxfile, bimg%dx

      if ( (bimg%nxfile)*bimg%dx .ne.360) then
c        print *,'Shifter ce fichier n''a pas de sens !'
c        print *,(bimg%nxfile-1)*bimg%dx,bimg%dx,' degres seulement'
c        stop
      endif
      

      if (shift.gt.0) then
         i1=nint(shift/bimg%dx) + 1

         do j=1,bimg%nyfile

c     zone tampon de i=1 a i1-1
               
            do i=1,i1-1
               tmp(i)=work(i,j)
            enddo
               
c     copie de i1 -->bimg%nxfile dans 1--> i1-1
               
            do i=i1,bimg%nxfile
               work(i-i1+1,j)=work(i,j)
            enddo
 
c     copie de tmp dans i= bimg%nxfile-i1+2

            do i= bimg%nxfile-i1+2, bimg%nxfile
               work(i,j)=tmp(i+i1-1-bimg%nxfile)
            enddo

         enddo


c     SHIFT NEGATIF -------------------
      else if (shift.lt.0) then
         i1=bimg%nxfile+nint(shift/bimg%dx)

         do j=1,bimg%nyfile

c     zone tampon de i=1 a i1-1

            do i=i1+1,bimg%nxfile
               tmp(i-i1)=work(i,j)
            enddo

c     copie de i1 -->1 dans bimg%nxfile--> bimg%nxfile-i1+1
            
            do i=i1,1,-1
               work(bimg%nxfile-i1+i,j)=work(i,j)
            enddo

c     copie de tmp dans i= 1-->bimg%nxfile-i1

            do i= 1, bimg%nxfile-i1
               work(i,j)=tmp(i)
            enddo
         enddo
      endif


      print *,shift,  nint(shift/bimg%dx) * bimg%dx
      shift = nint(shift/bimg%dx) * bimg%dx

      
      return
      end

c ---------------------------------------------------------------------
c 
c Nom         :  BimgShiftGrid
c
c                JMM - 15/04/98
c 
c ---------------------------------------------------------------------

      subroutine BimgShiftGrid (bimg,shift)

      implicit none
      include 'common.h'

      TYPE( bimgfile ) bimg

      real shift
      
      integer i
      
C On suppose que les donnees on ete shiftee comme il faut. Donc on
C ne verifie pas pour la grille
C Par ailleurs, si la grille a ete deja shiftee une fois
C il ne faut plus le faire ! (nouveau champs bimg%shift)

        if (bimg%shift.ne.1) then
      print *,' SHIFT de la grille irreguliere '
           bimg%shift=1
	 do i=1,bimg%nxfile
          bimg%d_xgrid(i)=bimg%d_xgrid(i)+shift
         enddo
        endif
      return
      end
