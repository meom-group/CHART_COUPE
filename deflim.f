	
c ---------------------------------------------------------------------
c 
c Nom         :  deflim
c Creation    :  JMM 17/09/92 version CRAY-2
c                update 30/06/93 Eric Brown
c
c Parametres  :  limit  - tableau des limites
c                map_in    - tableau du champ 2D a representer
c                nbox   - nombre de boites dans la palette
c
c Description :  ce sous programme permet de definir les limites de 
c                la palette de couleur pour chacune des "boites"
c                
c                method :  1 - lineaire entre fl1 et fl2
c                          2 - contraste maximum (equalized)
c                          3 - limites lues dans un fichier
c ---------------------------------------------------------------------

      subroutine deflim(map_in,bimg,limit,nbox,method,xmini,xmaxi)
      
      implicit none

      integer  nbox

      integer NDISCR
      integer nx,ny
c     parameter (NDISCR=5000)
      parameter (NDISCR=2000)
      real xmini, xmaxi

      include 'common.h'

      TYPE( bimgfile ) bimg

      integer   method


      integer i,j,n, n0, n1, n2
      real    dxbox, dv, dnva
      real    limit(nbox+1),map_in(NXX,NYY)

      integer nv(NDISCR+1),nva(NDISCR+1)
      real v(NDISCR+1)
      real spv

      integer numlim

      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval
	
      print *,'  VMAX= ', xmaxi
      print *,'  VMIN= ', xmini
      if (xmini.eq.xmaxi) then
      print *,'  Valeurs constantes, -clrmet 1 impose'
      method=1
      endif



c     METHOD 1 - INTERPOLATION LINEAIRE ------------------------------ 

      if (method.eq.1) then
c**         print *,'Calcul des limites '
         call flush(6)

         dxbox=(xmaxi-xmini)/nbox
         do i=1,nbox+1
            limit(i)=xmini+(i-1)*dxbox
         enddo

         do j=1,ny
            do i=1,nx
               if ((map_in(i,j).ne.spv).and.
     .             (map_in(i,j).le.limit(nbox+1)).and.
     .             (map_in(i,j).ge.limit(1))) then
               continue
               else
                  if (opt_msk .eq. 1 ) then
	          map_in(i,j)=spv
                  endif
               endif
            enddo
         enddo

         

c     METHOD 2 - EQUALIZE --------------------------------------------

      else if (method.eq.2) then
c**         print *,'Calcul des limites '
         call flush(6)

         dv=(xmaxi-xmini)/NDISCR
         do i=1,NDISCR
            nv(i)=0
         enddo
         
         do i=1,NDISCR+1
            nva(i)=0
            v(i)=(i-1)*dv+xmini
         enddo
         
c     histogramme
         
         do j=1,ny
            do i=1,nx
               if ((map_in(i,j).ne.spv).and.
     .             (map_in(i,j).le.xmaxi).and.
     .             (map_in(i,j).gt.xmini)) then
                  n=ifix((map_in(i,j)-xmini)/dv)+1
                  n=min(n,NDISCR)
                  nv(n)=nv(n)+1
               else
                  if (opt_msk .eq. 1 ) then
	          map_in(i,j)=spv
                  endif
               endif
            enddo
         enddo

c        if (nv(NDISCR).eq.0) nv(NDISCR)=1


c     fonction de repartition

         do i=2,NDISCR+1
            nva(i) = nva(i-1)+nv(i-1)
         enddo


c     retour aux variables map_in
           
         dnva=float(nva(NDISCR+1))/nbox
         n1=0
         n0=1

         limit(1)=xmini
         limit(nbox+1)=xmaxi
         do i=1,nbox-1
            n1=n1+dnva
            do n=n0,NDISCR+1
               if(nva(n).ge.n1) then
                  n0=n-1
c                 n0=n
                  goto20
               endif 
            enddo
 20         continue
            
c     interpollation lineaire

            n2=n0+1
c    print *,'>>> deflim n2,n0',n2,n0
 21         if (nva(n2).eq.nva(n0)) then
               if (n2.lt.NDISCR+1) then
                  n2=n2+1
                  goto 21
               else
                  stop
               endif
            endif

            limit(i+1) = v(n0)+(n1-nva(n0))*(v(n2)-v(n0))/
     .                   (nva(n2)-nva(n0))
         enddo
         

c     METHOD 3 - FORCED LIMITS --------------------------------------

      else if (method.eq.3) then
c**         print *,'Lecture des limites '
         call flush(6)
         
         numlim = 0

         open(88,file=f_clrlim,form='formatted')
         do i=1,nbox+1
            read(88, *, end=22) limit(i)
         enddo
 22      continue
         close(88)
	 numlim=i - 1
         
         do j=1,ny
            do i=1,nx
               if ((map_in(i,j).ne.spv).and.
     .             (map_in(i,j).le.limit(nbox+1)).and.
     .             (map_in(i,j).ge.limit(1))) then
               continue
               else
                  if (opt_msk .eq. 1 ) then
	          map_in(i,j)=spv
                  endif
               endif
            enddo
         enddo


         if (numlim.lt.nbox+1) then
            print *,'erreur : moins de limites que de couleurs,'
            print *,'Pour l''instant, veuillez donner une palette'
            print *,'comportant nlim-1 couleurs.'
            stop
c            nbox = i-1
         endif 


      else
         print *,'erreur dans deflim: methode non existante'
         stop 'programme stoppe'
      endif
        
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  WriteLimits
c 
c Parametres  :  filename - nom du fichier a creer
c                limit    - tableau contenant les limites
c                nlim     - nombre de limites
c 
c Description :  Cette fonction ecrit les limites calculees par chart
c                dans les fichier specifie par l'utilisateur
c 
c ---------------------------------------------------------------------

      subroutine WriteLimits (filename, limit, nlim)

      implicit none

      character*256 filename
      real          limit(*)
      integer       nlim, i

      open(88,file=filename,form='formatted')
      do i=1,nlim
         write(88,'(e15.5)')limit(i)
      enddo
      close(88)
      
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  ClrGetMark
c 
c Parametres  :  
c 
c Description :  Lis le tableau des clrmark dans le fichier clrmark
c               determine le nombre de marques a faire sur la palette
c 
c ---------------------------------------------------------------------
        subroutine ClrGetMark()
        implicit none
        include "common.h"
        open(88,file=f_clrmark,form='formatted')
        nmark = 1
        do while ( 1 .NE. 2 )
         read(88,*,end=22) clrmark(nmark)
         nmark = nmark+1
         if ( nmark .GT. NCLRMARK ) then
          print *, 'Erreur: Trop de marques !'
          print *, 'Augmenter NCLRMARK '
          stop 'erreur dans ClrGetMark'
         endif
        end do
   22   continue
        close(88)
        nmark = nmark -1
        if (nmark.LT.2 ) then
         print *, 'Erreur in ClrGetMark: il faut au moins 2 valeurs'
         print *,'  dans le fichier clrmark'
         stop 'ClrGetMark'
        endif
        return
        end
                 
c ---------------------------------------------------------------------
c 
c Nom         :  ClrGetLimits 
c 
c Parametres  :  
c 
c Description :  calcul des limites pour le bloc couleurs
c 
c ---------------------------------------------------------------------

      subroutine ClrGetLimits (clrdata,bimgclr,limit,ncol)

      implicit none

      include 'common.h'
      include 'color.h'

      real    clrdata (NXX,NYY)
      real    limit(NBOXMAX+1)
      TYPE( bimgfile ) bimgclr
      integer ncol

      real    cl_min, cl_max
      integer cl_met, cl_exp
      common  /color/cl_min, cl_max, cl_met, cl_exp
      save    /color/

      real vmax, vmin

      call FindMinMax (bimgclr, clrdata, vmin, vmax)


      if (opt_min.eq.0) cl_min = vmin         
      if (opt_max.eq.0) cl_max = vmax

      if (opt_clrmark .EQ. 1 ) THEN
        cl_min = clrmark(1)
        cl_max = clrmark(nmark)
      endif


c     initialisation des variables "affichables"
      
c     call SetClrMin (cl_min)
c     call SetClrMax (cl_max)
      call SetClrMin (vmin)
      call SetClrMax (vmax)


      call deflim (clrdata,bimgclr,limit,ncol,cl_met,cl_min, cl_max)

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  VecGetLimits 
c 
c Parametres  :  
c 
c Description :  calcul des limites pour le bloc couleurs
c 
c ---------------------------------------------------------------------

      subroutine VecGetLimits (vecdata,bimgvec,limit,ncol)

      implicit none

      include 'common.h'
      include 'color.h'

      real    vecdata (NXX,NYY)
      real    limit(NBOXMAX+1)
      TYPE( bimgfile ) bimgvec
      integer ncol

      real    vv_vrl, vv_vfr, vv_lwd, vv_vlc, vv_vhc
      real    vv_vmd
      real    vv_mns, vv_mnx, vv_mny, vv_mxs, vv_mxx, vv_mxy
      character*36 vv_mnt, vv_mxt
      integer vv_subx, vv_suby,vv_mnp,vv_mxp, vv_vst, vv_met, vv_vpo

      common /vectors/ vv_subx, vv_suby, vv_vrl, vv_vfr, vv_lwd, 
     .                 vv_mns, vv_mnx, vv_mny, vv_mnp,
     .                 vv_mxs, vv_mxx, vv_mxy, vv_mxp, vv_met, vv_vpo,
     .                 vv_vst, vv_mnt, vv_mxt, vv_vlc, vv_vhc, vv_vmd
      save   /vectors/

      real    cl_min, cl_max
      integer cl_met, cl_exp
      common  /color/cl_min, cl_max, cl_met, cl_exp
      save    /color/


      real vmax, vmin

      call FindMinMax (bimgvec, vecdata, vmin, vmax)

      if (opt_min.eq.0) cl_min = vmin         
      if (opt_max.eq.0) cl_max = vmax

      if (opt_clrmark .EQ. 1 ) THEN
        cl_min = clrmark(1)
        cl_max = clrmark(nmark)
      endif
      vmin=cl_min
      vmax=cl_max


c     initialisation des variables "affichables"
      
      if (vv_vlc .EQ. 0 ) vv_vlc = vmin
      if (vv_vhc .EQ. 0 ) vv_vhc = vmax
C     if (vv_vlc.eq.0.0) then
         call SetVecMin (vmin)
C     else 
C        call SetVecMin (vv_vlc)
C     endif 
C        
C     if (vv_vhc.eq.0.0) then
         call SetVecMax (vmax)
C     else 
C        call SetVecMax (vv_vhc)
C     endif 

      call deflim (vecdata,bimgvec,limit,ncol,vv_met,vv_vlc,vv_vhc)

      return
      end


