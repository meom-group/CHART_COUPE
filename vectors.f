c --------------------------------------------------------------------
c 
c Programme      :  chart
c Fichier        :  vectors.c
c 
c Auteur         :  Eric Brown
c
c Description    :  Contient les fonction de dessin pour les vecteurs
c 
c ---------------------------------------------------------------------



c ---------------------------------------------------------------------
c 
c Nom         :  VectorPrepareData
c 
c Parametres  :  bimgvec   - structure decrivant la taille et le contenu
c                            du fichier de vecteurs.
c                bimgclr   - structure decrivant la taille et le contenu
c                            du fichier de scalaires.
c                clrdata  - donnees pour coloration
c                vectdata - tableau a remplir 
c                coords   - coordonnees de la carte a dessiner
c                couche   - couche a lire 
c
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine VectorPrepareData (bimgvec, bimgclr, clrdata, vectdata,
     .                              coords, tstep,couche)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimgvec(3), bimgclr
      real coords(4)
      integer tstep, couche, i,j

      real    vv_vrl, vv_vfr, vv_lwd, vv_vlc, vv_vhc
      real    vv_mns, vv_mnx, vv_mny, vv_mxs, vv_mxx, vv_mxy
      real    vv_vmd
      character*36 vv_mnt, vv_mxt
      integer vv_subx, vv_suby,vv_mnp,vv_mxp, vv_vst, vv_met, vv_vpo

      common  /vectors/ vv_subx, vv_suby, vv_vrl, vv_vfr, vv_lwd, 
     .                 vv_mns, vv_mnx, vv_mny, vv_mnp,
     .                 vv_mxs, vv_mxx, vv_mxy, vv_mxp, vv_met,vv_vpo,
     .                 vv_vst, vv_mnt, vv_mxt, vv_vlc, vv_vhc, vv_vmd
      save   /vectors/

      real vectdata(NXX,NYY,2), clrdata(NXX,NYY)


      do j=1,NYY
         do i=1,NXX
            vectdata(i,j,1) = 0.0
            vectdata(i,j,2) = 0.0
         enddo 
      enddo 

      if (opt_vect2D.eq.1) then
         call BimgReadVectorData (vectdata(1,1,1),bimgvec(1),
     .                            map_coord,tstep,couche,1)
         call BimgReadVectorData (vectdata(1,1,2),bimgvec(1),
     .                            map_coord,tstep,couche,2)
         if (opt_cgrid.eq.1) then
            print *,' Option -Cgrid avec vecdataxy ...'
	 endif
      else
         if (opt_vectX.eq.1) then
            PRINT *,' Read Vector Data X'
            call BimgReadVectorData (vectdata(1,1,1),bimgvec(1), 
     .                               map_coord,tstep,couche,1)

            if (opt_vecpsi.eq.1) then
               call VecCalcSpeed (vectdata, bimgvec(1), map_coord)
            endif 
         endif 
         if (opt_vectY.eq.1) then
            PRINT *,' Read Vector Data Y'
            call BimgReadVectorData (vectdata(1,1,2), bimgvec(2),
     .                               map_coord,tstep,couche,1)
         endif 
      endif 

         if (opt_cgrid.eq.1) then
C interpolle les 2 composantes sur le point T
            call PutVelOnTpoints(vectdata,bimgvec)
         endif

         if (opt_vecshade.eq.1) then
            bimgclr = bimgvec(1)
	        bimgclr%spval=defspval
            do i=1,bimgvec(1)%nxdata
             do j=1,bimgvec(1)%nydata
      if (vectdata(i,j,1) .ne. bimgvec(1)%spval) then
	   clrdata(i,j)=j
	  else
       clrdata(i,j)=defspval
	  endif
             enddo
            enddo
         endif

      if ((opt_vectmod.eq.1).or.(opt_clrmod.eq.1)) then
         bimgclr = bimgvec(1)
         call VectorCalcModule (bimgvec, bimgclr, vectdata, clrdata)         
      endif 

C   Modif de Laurent Debreu (JMM 29/09/2006)
C     call VectorSubSample (bimgvec, vectdata, vv_subx, vv_suby)

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine BimgReadVectorData(data_out, bimg,coords,tstep,
     .                              couche,dim)

      implicit none
      include 'common.h'

      TYPE( bimgfile ) bimg

      integer couche
      real coords(4)
      integer dim, tstep

      real data_out(NXX,NYY)  ! tableau fourni par la fonction d'appel
      real local_data(NXX,NYY)
      integer i,j


      integer imin,imax,jmin,jmax, kinf, ksup
      TYPE( bimgfile ) spembimg
      common /spem/ spembimg, imin,imax,jmin,jmax
      save   /spem/

      real zlat, zlon
      logical good_point


      print *,'Lecture de la couche :',couche
      call flush(6)

      bimg%nxdata = bimg%nxfile
      bimg%nydata = bimg%nyfile
      if(opt_dep.ne.1) then
        if (bimg%irecl .eq. 0 ) then
      call BimgGetLayer (bimg, local_data, tstep, couche, dim)
        else if (bimg%irecl .GT. 0 ) then
      call DirectGetLayer (bimg, local_data, tstep, couche, dim)
        else if (bimg%irecl .EQ. -1 ) then
         PRINT *,'Cdf Get Layer'
      call  CdfGetLayer    (bimg, local_data, tstep, couche, dim,bimg%nxfile,bimg%nyfile) 
        else
C      On ne doit jamais passer par la
        endif
      else
C interpolation des 2 niveaux pour avoir la hauteur exacte
      ksup=couche
      kinf=couche + 1
        print *,'Interpollation entre couche ',ksup ,' et ',kinf
        print *,' poids :',bimg%alphasup, 1.- bimg%alphasup
        print *,' depth :',bimg%depth(ksup), req_dep, bimg%depth(kinf)

C lecture du niveau superieur
        if (bimg%irecl .eq. 0  ) then
      call BimgGetLayer (bimg, local_data, tstep, ksup, dim)
        else if (bimg%irecl .GT. 0 ) then
      call DirectGetLayer (bimg, local_data, tstep, ksup, dim)
        else if (bimg%irecl .EQ. -1 ) then
      call  CdfGetLayer    (bimg, local_data, tstep, ksup, dim, bimg%nxfile,bimg%nyfile)
        else
C      On ne doit jamais passer par la 
        endif
C lecture du niveau inferieur
        if (bimg%irecl .eq. 0  ) then
      call BimgGetLayer (bimg, data_out, tstep, kinf, dim)
        else if (bimg%irecl .GT. 0 ) then
      call DirectGetLayer (bimg, data_out, tstep, kinf, dim)
        else if (bimg%irecl .EQ. -1 ) then
      call  CdfGetLayer    (bimg, data_out, tstep, kinf, dim, bimg%nxfile,bimg%nyfile)
        else
C      On ne doit jamais passer par la 
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

c     si un masque a ete specifie, remplacer les zones a masquer
c     par spval dans le tableau de donnees.

      if (bimg%mask.eq.1) then
         print *, 'masking data'
         call BimgMaskData (bimg, local_data)
      endif 


c     option SHIFT

      if (opt_shift.eq.1) then
         print *, 'Shifting by', map_shift
         call BimgShift (bimg, local_data, map_shift, coords)
        if (bimg%grid .ge.1) then
         call BimgShiftGrid (bimg, map_shift)
        endif
c JMM add 1 line 4/4/1995
         bimg%x1mod = bimg%x1mod + map_shift
      endif 



c     calcule la partie du fichier qui nous interesse et la transfere
c     dans le tableau

C     if (opt_vectgrid.eq.1) then
      if (bimg%grid.eq.1) then
         print *, 'Grille irreguliere'

         imin = 1
         do while ((abs(coords(1)-bimg%d_xgrid(imin+1)).lt.
     .              abs(coords(1)-bimg%d_xgrid(imin))).and.
     .              imin.lt.bimg%nxdata-1)
            imin = imin+1
         enddo 

         imax = 2
         do while ((abs(coords(2)-bimg%d_xgrid(imax)).lt.
     .              abs(coords(2)-bimg%d_xgrid(imax-1))).and.
     .              imax.lt.bimg%nxdata)
            imax = imax+1
         enddo 

         jmin = 1
         do while ((abs(coords(3)-bimg%d_ygrid(jmin+1)).lt.
     .              abs(coords(3)-bimg%d_ygrid(jmin))).and.
     .              jmin.lt.bimg%nydata-1)
            jmin = jmin+1
         enddo 

         jmax = 2
         do while ((abs(coords(4)-bimg%d_ygrid(jmax)).lt.
     .              abs(coords(4)-bimg%d_ygrid(jmax-1))).and.
     .              jmax.lt.bimg%nydata)
            jmax = jmax+1
         enddo 

C     else if (opt_vectgrid.eq.3) then
      else if (bimg%grid.eq.3) then
         imin = 1
	 imax = bimg%nxfile
         jmin = 1
	 jmax = bimg%nyfile

        if (opt_zoom.eq.1) then
        do i=imin,imax
         do j=jmin,jmax
           zlat=xygr(i,j,2)
           zlon=xygr(i,j,1)
           good_point=.true.
           good_point=(zlat.le.coords(4))
           good_point=good_point .AND. (zlat.ge.coords(3))
           good_point=good_point .AND. (zlon.le.coords(2))
           good_point=good_point .AND. (zlon.ge.coords(1))
           if (.not.good_point) local_data(i,j)=bimg%spval
         enddo
        enddo
        endif

      else
         imin = nint ((coords(1) - bimg%x1mod)/bimg%dx)+1
         imax = nint ((coords(2) - bimg%x1mod)/bimg%dx)+1
         jmin = nint ((coords(3) - bimg%y1mod)/bimg%dy)+1
         jmax = nint ((coords(4) - bimg%y1mod)/bimg%dy)+1
      endif 

c     verifie que les coordonnees sont bien dans ce fichier

      if ((imin.gt.bimg%nxfile).or.(imax.gt.bimg%nxfile).or.
     .    (jmin.gt.bimg%nyfile).or.(jmax.gt.bimg%nyfile)) then
         print *,'erreur : coordonnes hors du domaine'
         print *,coords(1),coords(2),coords(3),coords(4)
         print *,imin,bimg%nxdata
         print *,imax,bimg%nxdata
         print *,jmin,bimg%nydata
         print *,jmax,bimg%nydata
         stop
      endif 


c     cas normal (toujours, pour CHART)

      bimg%nxdata = imax - imin ! + 1
      bimg%nydata = jmax - jmin ! + 1
         
      do i=imin,imax
         do j=jmin,jmax
            data_out(i-imin+1,j-jmin+1) = local_data(i,j)
         enddo
      enddo
c 3 lines added JMM 4/4/1995
	if(opt_shift.eq.1) then
        bimg%x1mod = bimg%x1mod - map_shift
	endif

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  VectorTrace
c 
c Parametres  :  bimgvec   - structure decrivant la taille et le contenu
c                            du fichier de vecteurs.
c                bimgclr   - structure decrivant la taille et le contenu
c                            du fichier de scalaires.
c                clrdata   - donnees pour coloration
c                vectdata  - donnees a tracer
c                ncol      - nombre de couleurs dans la palette
c                coords    - coordonnees de la carte  
c                map       - indique a NCAR si les isocontours doivent
c                            etre projetes sur une carte.
c                            1 - oui,  0 - non
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine  VectorTrace (bimgvec,bimgclr, clrdata, vectdata, ncol,
     .                         coords)

      implicit none
      
      include 'common.h'
      include 'mapncar.h'
      include 'color.h'

      TYPE( bimgfile) bimgvec(3), bimgclr
      real vectdata(NXX,NYY,2), clrdata(NXX, NYY)
      real veclim(NBOXMAX+1)
      real coords(4)
      real,dimension(:),allocatable :: rdummy
      real idummy
      integer ncol, i, map_flag
      integer old_index, errind, PrintBimgStructure

      real vv_dmx, vv_tmp_vrl

      real    vv_vrl, vv_vfr, vv_lwd, vv_vlc, vv_vhc
      real    vv_mns, vv_mnx, vv_mny, vv_mxs, vv_mxx, vv_mxy
      real    vv_vmd
      character*36 vv_mnt, vv_mxt
      integer vv_subx, vv_suby,vv_mnp,vv_mxp, vv_vst, vv_met, vv_vpo

      common /vectors/ vv_subx, vv_suby, vv_vrl, vv_vfr, vv_lwd, 
     .                 vv_mns, vv_mnx, vv_mny, vv_mnp,
     .                 vv_mxs, vv_mxx, vv_mxy, vv_mxp, vv_met, vv_vpo,
     .                 vv_vst, vv_mnt, vv_mxt, vv_vlc, vv_vhc, vv_vmd
      save   /vectors/

      external vvumxy

      integer imin,imax,jmin,jmax
      TYPE( bimgfile ) spembimg
      common /spem/ spembimg, imin,imax,jmin,jmax
      save   /spem/


      integer l
      real    rl,rr,rb,rt, ur,ul,ut,ub
      real  vv_mnx1, vv_mny1, vv_mxx1, vv_mxy1

      print *, 'Calcul des vecteurs'


      idummy = 0

      map_flag = 1
      if (opt_noproj.eq.1) map_flag = 0
CJMM     if (opt_vectgrid.ge.1) then
      if (bimgvec(1)%grid.ge.1) then
         map_flag = 3
         spembimg = bimgvec(1)
      endif 

      call gflas1(7)

      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)


      call vvseti ('SET - do SET call flag         ', 0)
      call vvseti ('MAP - mapping flag             ', map_flag)

C   if (opt_vectgrid .ne. 3) then
      if (bimgvec(1)%grid .ne. 3) then
      call vvsetr ('XC1 - X coordinate at index 1  ', coords(1))
      call vvsetr ('XCM - X coordinate at index max', coords(2))
      call vvsetr ('YC1 - Y coordinate at index 1  ', coords(3))
      call vvsetr ('YCN - Y coordinate at index max', coords(4))
	else
      call vvsetr ('XC1 - X coordinate at index 1  ', float(imin))
      call vvsetr ('XCM - X coordinate at index max', float(imax))
      call vvsetr ('YC1 - Y coordinate at index 1  ', float(jmin))
      call vvsetr ('YCN - Y coordinate at index max', float(jmax))
	endif
      call vvsetr ('USV - U special value          ', bimgvec(1)%spval)
      call vvsetr ('VSV - V special value          ', bimgvec(1)%spval)
      call vvsetr ('PSV - P special value          ', bimgclr%spval)
      call vvseti ('SVF - special value flag       ', 3)
      call vvseti ('SPC - P Special Color          ', COLOR_SPVAL)
      call vvseti ('VST - Vector STatistics flag   ', vv_vst)
      call vvsetc ('ZFT - Zero Field Text string   ', ' ')

      if (opt_vecmin.eq.1) then
         print *,' VLC :', vv_vlc
         call vvsetr ('VLC - Vector Low Cutoff     ', vv_vlc)
      endif 
      if (opt_vecmax.eq.1) then
         call vvsetr ('VHC - Vector High Cutoff    ', vv_vhc)
      endif 

c     texte
c  MODIF JMM: variable locale vv_mnx1 etc pour ne pas ecraser vv_mnx etc
c dans le cas de plusieurs frames.
c
      vv_mnx1 = (vv_mnx-rl)/(rr-rl)    
      vv_mny1 = (vv_mny-rb)/(rt-rb)      
      vv_mxx1 = (vv_mxx-rl)/(rr-rl)    
      vv_mxy1 = (vv_mxy-rb)/(rt-rb)      

      call vvsetr ('MNS - MiN vector text block char Size', vv_mns)
      call vvsetr ('MNX - MiN vector text block X coord  ', vv_mnx1)
      call vvsetr ('MNY - MiN vector text block Y coord  ', vv_mny1)
      call vvseti ('MNP - MiN vector text block Pos. mode', vv_mnp)
      call vvsetc ('MNT - MiN vector Text string         ', vv_mnt)

      call vvsetr ('MXS - MaX vector text block char Size', vv_mxs)
      call vvsetr ('MXX - MaX vector text block X coord  ', vv_mxx1)
      call vvsetr ('MXY - MaX vector text block Y coord  ', vv_mxy1)
      call vvseti ('MXP - MaX vector text block Pos. mode', vv_mxp)
      call vvsetc ('MXT - MaX vector Text string         ', vv_mxt)


c      call vvsetr('LWD -- Vector Linewidth', 2.25)
      call vvseti ('VPO - Vector Position Method         ', vv_vpo)

      
      if ((opt_vectclr.eq.1).or.(opt_vectmod.eq.1)) then
         call vvseti ('CTV - Color Threshold Value       ', -2)
         call VecGetLimits (clrdata,bimgclr, veclim,ncol)
         if (opt_veclout.eq.1) then
            call WriteLimits (f_veclout, veclim, ncol+1)
         endif 

         if (opt_palbar.eq.1) then
            call AjoutePalette (veclim, ncol)
         endif 

         call vvseti ('NLV - Number of LeVels ',      ncol)

         do i=1,ncol
            call vvseti ('PAI - parameter array index',i)
            call vvseti ('CLR - GKS color index      ',i+COLOR_NRES-1)
            call vvsetr ('TVL', veclim(i+1))
         enddo 
      endif 

      call vvseti('XIN - X Axis Array Increment', vv_subx)
      call vvseti('YIN - Y Axis Array Increment', vv_suby)
      call vvsetr('VMD', vv_vmd)

      call gqplci (errind, old_index)
      call gsplci (COLOR_VECTOR)
C     errind=PrintBimgStructure (bimgvec(1))

      IF (vv_vmd .GT. 0. ) THEN
        allocate(rdummy(2*bimgvec(1)%nxdata * bimgvec(1)%nydata))
      ELSE
        allocate(rdummy(1))
      ENDIF
      rdummy = 0.0

      call vvinit (vectdata(1,1,1), NXX,
     .             vectdata(1,1,2), NXX,
     .             clrdata,         NXX,
     .             bimgvec(1)%nxdata, bimgvec(1)%nydata,
     .             rdummy, size(rdummy)) 


c     change default vector length (ref vectors, a vector field
c                                       plotting utility, p. 50)

      call vvgetr ('DMX - NDC Max vector size', vv_dmx)
      
      vv_tmp_vrl = vv_vrl * vv_dmx / (rr - rl)

      call vvsetr ('VRL - Vector Realized Length', vv_tmp_vrl)
      call vvsetr ('VFR -- Vector Fractional Minimum', vv_vfr)

      call vvectr (vectdata(1,1,1),vectdata(1,1,2),clrdata,
     .             iama, idummy,   rdummy)

      call set(rl,rr,rb,rt,ur,ul,ut,ub,l)
      
  
      call gsplci (old_index)
      call gflas2
      deallocate(rdummy)

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  VectorSubSample
c 
c Parametres  :  bimgvec  - structure d'information decrivant la taille
c                           et le contenu du tableau de donnees.
c                datavec  - tableau de donnees
c                Xsub     - sous-echantillonnage en X
c                Ysub     - sous-echantillonnage en Y
c 
c Description :  Ne conserve qu'une donnee sur Xsub et une sur Ysub et
c                remplace le reste par spval.
c 
c ---------------------------------------------------------------------

      subroutine VectorSubSample0  (bimgvec, datavec, Xsub, Ysub)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimgvec(3)
      real datavec (NXX,NYY,2)
      integer Xsub, Ysub
      real rX, rY
      real rXint, rYint
      
      integer i,j


c     if ((Xsub.ne.1.0).and.(Ysub.ne.1.0)) then
         do i=0,bimgvec(1)%nxdata-1
            rX    = float(i)/Xsub
            rXint = int(rX)
            
            do j=0,bimgvec(1)%nydata-1
               rY    = float(j)/Ysub
               rYint = int(rY)
               
               if (((rY-rYint).ne.0.0).or.((rX-rXint).ne.0.0)) then
                  datavec(i+1,j+1,1) = bimgvec(1)%spval
                  datavec(i+1,j+1,2) = bimgvec(1)%spval
               endif 
            enddo 
         enddo 
c     endif 

      return
      end

c ---------------------------------------------------------------------
c 
c Nom         :  VectorCalcModule
c 
c Parametres  :  bimgvec  - structure decrivant la taille et le contenu
c                           du fichier de vecteurs.
c                bimgclr  - structure decrivant la taille et le contenu
c                           du fichier de scalaires.
c                clrdata  - donnees pour coloration
c                vectdata - tableau a remplir 
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine VectorCalcModule (bimgvec, bimgclr, vectdata, clrdata)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimgvec(3), bimgclr

      real vectdata (NXX,NYY,2), clrdata (NXX,NYY)

      integer i,j
      integer nx,ny

      nx = bimgvec(1)%nxdata
      ny = bimgvec(1)%nydata

      do i=1,nx
         do j=1,ny
            if ((vectdata(i,j,1).ne.bimgvec(1)%spval).and.
     .          (vectdata(i,j,2).ne.bimgvec(1)%spval)) then            
               clrdata(i,j) = sqrt ((vectdata(i,j,1) ** 2.0) +
     .                              (vectdata(i,j,2) ** 2.0))
            else 
               clrdata(i,j) = bimgvec(1)%spval
            endif 
         enddo 
      enddo 

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  VecCalcSpeed
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine VecCalcSpeed (vectdata, bimgvec, coords)

      implicit none

      include 'common.h'
      real vectdata (NXX,NYY,2), work (NXX, NYY)
      real coords(4)
      
      TYPE( bimgfile ) bimgvec

      integer height, width
      real dx(NYY), dy, dyfile
      integer i, j
      real rad, spv, u, v

      dyfile = bimgvec%dy
      height = bimgvec%nxdata
      width  = bimgvec%nydata
      spv    = bimgvec%spval

      rad=4.*atan(1.)/180.
      dy=1852.*60*dyfile
      
      do j=1,bimgvec%nydata
         dx(j)   = dy*cos((coords(3)+float(j-1)*dyfile)*rad)
      enddo           
      

      print *,'Calcul de la vitesse'
      call flush(6)
        
      do j=2,height-1
         do i=2,width-1
            if ((vectdata(i,j,1).eq.spv).or.
     .          (vectdata(i+1,j,1).eq.spv).or.
     .          (vectdata(i-1,j,1).eq.spv).or.
     .          (vectdata(i,j+1,1).eq.spv).or.
     .          (vectdata(i,j-1,1).eq.spv)) then
               work(i,j) = spv
               vectdata(i,j,2) = spv
            else     
               v = (vectdata(i+1,j,1) - vectdata(i-1,j,1)) / (2*dx(j))
               u = (vectdata(i,j-1,1) - vectdata(i,j+1,1)) / (2*dy)

               work(i,j) = u
               vectdata(i,j,2) = v
            endif
         enddo
      enddo

      do i=1,width
         work (i,1)      = spv
         work (i,height) = spv
         vectdata (i,1,2)      = spv
         vectdata (i,height,2) = spv
      enddo
        
      do j=1,height
         work (1,j)   = spv
         work (width,j) = spv
         vectdata (1,j,2)   = spv
         vectdata (width,j,2) = spv
      enddo             

c     recopie le resultat dans le tableau initial
      
      do i=1,NXX
         do j=1,NYY
            vectdata(i,j,1) = work(i,j)
         enddo 
      enddo 




      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine vvumxy (x,y,u,v,uvm,xb,yb,xe,ye,ist)

      implicit none
      include 'common.h'

C
C The mapping common block: made available to user mapping routines
C
      real*4 x,y,u,v,uvm,xb,yb,xe,ye
      integer ist, icount

      real dx,dy, xx, yy, zx, zy, zlat, zlon
      integer i1, i2, j1, j2

      integer imin,imax,jmin,jmax
      TYPE( bimgfile ) spembimg
      common /spem/ spembimg, imin,imax,jmin,jmax
      save   /spem/

      integer i,j
	data icount/0/
	icount = icount +1

	xb = 0.
	yb = 0.
	xe = 0.
	ye = 0.

      dx = (map_coord(2) - map_coord(1)) / float (imax-imin-1)
      dy = (map_coord(4) - map_coord(3)) / float (jmax-jmin-1)

      i = nint ((x - spembimg%d_xgrid(imin))/ dx) 
      j = nint ((y - spembimg%d_ygrid(jmin))/ dy) 
         
	if (spembimg%grid .ne. 3) then
      x = spembimg%d_xgrid(imin+i)
      y = spembimg%d_ygrid(jmin+j)
	else
         yy = y
         xx = x

         i1 = int(xx)
         i2 = i1 +1
         j1 = int (yy)
         j2 = j1 + 1
         zx = x - i1
         zy = yy - j1
          if ((i1.gt.NXX).or.(i1.lt.1)) print *,' i1 =',i1,NXX
          if ((i2.gt.NXX).or.(i2.lt.1)) print *,' i2 =',i2,NXX
          if ((j1.gt.NYY).or.(j1.lt.1)) print *,' j1 =',j1,NYY
          if ((j2.gt.NYY).or.(j2.lt.1)) print *,' j2 =',j2,NYY
c
         zlat = xygr(i1,j1,2) * (1. - zx ) * (1. - zy ) +
     &       xygr(i1,j2,2) * (1. - zx ) * zy +
     &       xygr(i2,j1,2) * (     zx ) * ( 1. -zy ) +
     &       xygr(i2,j2,2) * zx * zy
         zlon = xygr(i1,j1,1) * (1. - zx ) * (1. - zy ) +
     &        xygr(i1,j2,1) * (1. - zx ) * zy +
     &       xygr(i2,j1,1) * (     zx ) * ( 1. -zy ) +
     &       xygr(i2,j2,1) * zx * zy

	x=zlon
	y=zlat

	endif

C Dans vvmuxy on ne fait que changer x et y, on laisse le boulot de
C calculer xb etc a vvmpxy que l'on rappelle alors avec IMAP = 1
C
      call vvseti ('MAP - mapping flag             ', 1)
      call vvmpxy (x,y,u,v,uvm,xb,yb,xe,ye,ist)
      call vvseti ('MAP - mapping flag             ', 3)

      return

      end


c ---------------------------------------------------------------------
c           call PutVelOnTpoints(vectdata,bimgvec)
c 
c Nom         :  PutVelOnTpoints
c 
c Parametres  :  bimgvec  - structure decrivant la taille et le contenu
c                           du fichier de vecteurs.
c                vectdata - tableau a modifier. 
c                          en entree il contient U,V sur C-grid
c                          en sortie il contient U,V sur A-grid.
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine PutVelOnTpoints (vectdata, bimgvec)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimgvec(3)

      real vectdata (NXX,NYY,2)

      real ua(NXX,NYY),va(NXX,NYY)

      integer i,j
      integer nxu,nyu
      integer nxv,nyv
      real spvalu, spvalv

      print *,' Passage de C-grid en A-grid.'
      nxu = bimgvec(1)%nxdata
      nyu = bimgvec(1)%nydata
      spvalu = bimgvec(1)%spval
      spvalv = bimgvec(2)%spval

      nxv = bimgvec(2)%nxdata
      nyv = bimgvec(2)%nydata
C Met les spval a 0: Facilite l'interpolation sur le masque.
C-----------------------------------------------------------
      do i=1,nxu
        do j=1,nyu
	   if (vectdata(i,j,1).eq.spvalu) then
        vectdata(i,j,1)=0.
       endif
        enddo
      enddo
C     bimgvec(1)%spval = 0.
      spvalu = 0.

      do i=1,nxv
	do j=1,nyv
	  if (vectdata(i,j,2).eq.spvalv) vectdata(i,j,2)=0.
        enddo
      enddo
C     bimgvec(2)%spval = 0.
      spvalv = 0.

C Composante U:
C---------------
      do j=1,nyu
          ua(1,j)=spvalu
         do i=2,nxu
	  ua(i,j)=0.5*(vectdata(i-1,j,1) + vectdata(i,j,1))
         enddo 
      enddo 
C Composante V:
C---------------
      do i=1,nxv
	  va(i,1)=spvalv
         do j=2,nyv
	  va(i,j)=0.5*(vectdata(i,j-1,2) + vectdata(i,j,2))
         enddo 
      enddo 
C Tranfere ua, va dans vectdata
C ------------------------------
      do i=1,nxu
	do j=1,nyu
	  vectdata(i,j,1)=ua(i,j)
	enddo
      enddo

      do i=1,nxv
	do j=1,nyv
	  vectdata(i,j,2)=va(i,j)
	enddo
      enddo

      return
      end



