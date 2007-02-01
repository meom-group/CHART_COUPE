c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe, anim
c Fichier        :  calcul.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
c Mise a Jour    :  
c
c Description    :  ce fichier contient les fonctions de traitement
c                   d'images et de calculs sur les donnees.
c                   ex. calculs de vorticite et de vitesse
c 
c ---------------------------------------------------------------------





c ---------------------------------------------------------------------
c 
c Nom              :  interp
c Date de creation :
c Parametres       :
c Description      :
c 
c ---------------------------------------------------------------------

      subroutine interp(map_out, map_in, bimg)
        
      implicit none

      include 'common.h'
      TYPE( bimgfile ) bimg

      real map_in(NXX,NYY), map_out(NXX,NYY)
      integer nx, ny, oldnx, oldny
      real cof, spv

      real    xij, yij, x, y, xfrac, x1frac, yfrac, y1frac
      real    a,b,c,d
      integer i,j,il,jl


      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval

c**      print *,'Interpolation des points de grille...'
      call flush(6)
	
      cof = amin1 ((float(NXX-1)/float(nx-1)),
     .             (float(NYY-1)/float(ny-1)))

      oldnx = nx
      oldny = ny

c      nx = int((float(nx)*cof)-cof)
c      ny = int((float(ny)*cof)-cof)

      nx = int((float(nx-1)*cof)) + 1
      ny = int((float(ny-1)*cof)) + 1

c      do il=2,nx-1
      do il=1,nx
         x=1. + float(il-1)/cof
         i = int(x)
         xij = float(i)
         
c         do jl=2,ny-1
         do jl=1,ny
            y=1. + float(jl-1)/cof
            j = int(y)
            yij = float (j)
              

c     on calcule le pixel seulement si les points
c     entre lesquels il se trouve sont tous differents
c     de spval. Si on est sur la frontiere, on ne
c     tient pas compte de i+1 ou j+1 selon le cas
c     car ces pixels seront hors du tableau

               if ((map_in(i,j).eq.spv).or.
     .            ((map_in(i+1,j).eq.spv).and.
     .                (i.ne.oldnx)).or.
     .            ((map_in(i,j+1).eq.spv).and.
     .                (j.ne.oldny)).or.
     .            ((map_in(i+1,j+1).eq.spv).and.
     .                (j.ne.oldny).and.
     .                (i.ne.oldnx))) then
                  map_out(il,jl) = spv
               else

                  xfrac  = x-xij
                  x1frac = 1.-xfrac                 
                  yfrac  = y-yij
                  y1frac = 1.-yfrac
                  
                  a = (xfrac  * yfrac) 
                  b = (x1frac * yfrac) 
                  c = (x1frac * y1frac)
                  d = (xfrac  * y1frac)
                 
                  map_out(il,jl) = a*map_in(i+1,j+1) +
     .                               b*map_in(i,j+1)   +
     .                               c*map_in(i,j)     +
     .                               d*map_in(i+1,j)


c
c     pour l'option noint, centre les carres sur le point
c     de grille
c
c                  if (xfrac.lt.0.5) then
c                     if (yfrac.lt.0.5) then
c                        map_out (il,jl) = map_in(i,j)
c                     else 
c                        map_out (il,jl) = map_in(i,j+1)
c                     endif 
c                  else 
c                     if (yfrac.lt.0.5) then
c                        map_out (il,jl) = map_in(i+1,j)
c                     else 
c                        map_out (il,jl) = map_in(i+1,j+1)
c                     endif 
c                  endif 




               endif 
c            endif
         enddo
      enddo

      
      bimg%nxdata = nx
      bimg%nydata = ny

      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  CalcDxDy
c Date de creation :  6 juillet 93
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------


      subroutine CalcDxDy (dx, dy, dxdy, dyfile, y1mod, size)

      implicit none
      real dx(*), dy, dxdy(*), dyfile, y1mod
      integer j,size
      real rad

      rad=4.*atan(1.)/180.
      dy=1852.*60*dyfile
      
      do j=1,size
         dx(j)   = dy*cos((y1mod+float(j-1)*dyfile)*rad)
         dxdy(j) = dy * dx(j)
      enddo           
      
      return 
      end







c ---------------------------------------------------------------------
c 
c Nom         :  CalcVorticite
c Creation    :  01/07/93
c
c Parametres  :  map     - tableau contenant width * height donnees
c                width   - largeur du tableau
c                height  - hauteur du tableau
c
c Description :  calcule la vorticite a partir de psi
c 
c ---------------------------------------------------------------------

      subroutine CalcVorticite (map_in, bimg, coords)
                                 
      implicit none

      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg

      real    map_in (NXX,NYY), dxdy (NYY)
      integer width, height
      real    coords(4), dyfile, dy
      integer i,j
      real    spv

      real rad


      print *,'Calcul de la vorticite'
      call flush(6)

      width = bimg%nxdata
      height = bimg%nydata
      spv = bimg%spval
      dyfile = bimg%dy



      rad=4.*atan(1.)/180.
      dy=1852.*60*dyfile
      
      do j=1,height
         dxdy(j) = dy * dy*cos((coords(3)+float(j-1)*dyfile)*rad)
      enddo           


      do j=2,height-1
         do i=2,width-1
            if ((map_in(i+1,j).eq.spv).or.
     .          (map_in(i-1,j).eq.spv).or.
     .          (map_in(i,j+1).eq.spv).or.
     .          (map_in(i,j-1).eq.spv)) then
               work_array(i,j) = spv
            else
               work_array(i,j) = (map_in(i+1,j) + map_in(i-1,j)
     .                        + map_in(i,j+1) + map_in(i,j-1)
     .                        - 4*map_in(i,j)) / dxdy(j)

            endif
         enddo
      enddo



      do i=1,width
         work_array (i,1)      = spv
         work_array (i,height) = spv
      enddo
        
      do j=1,height
         work_array (1,j)     = spv
         work_array (width,j) = spv
      enddo             


c     recopie le resultat dans le tableau initial

      do i=1,NXX
         do j=1,NYY
            map_in(i,j) = work_array(i,j)
         enddo 
      enddo 

      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  CalcVitesse
c Creation    :  01/07/93
c
c Parametres  :  map_in  - tableau contenant width * height donnees
c                width   - largeur du tableau
c                height  - hauteur du tableau
c
c Description :  calcule la vorticite a partir de psi
c 
c ---------------------------------------------------------------------

      subroutine CalcVitesse (map_in,width, height, dx, dy,spv)
                               
      implicit none

      include 'common.h'
      include 'workarray.h'

      real    map_in (NXX,NYY), dx(NYY), dy
      real    u,v
      real    spv
      integer width, height
      integer i,j

      print *,'Calcul de la vitesse'
      call flush(6)
        
      do j=2,height-1
         do i=2,width-1
            if ((map_in(i+1,j).eq.spv).or.
     .                (map_in(i-1,j).eq.spv).or.
     .                (map_in(i,j+1).eq.spv).or.
     .                (map_in(i,j-1).eq.spv)) then
               work_array(i,j) = spv
            else     
               v = (map_in(i+1,j) - map_in(i-1,j)) / (2*dx(j))
               u = (map_in(i,j-1) - map_in(i,j+1)) / (2*dy)

               work_array(i,j) = sqrt ((u**2) + (v**2))
            endif
         enddo
      enddo

      do i=1,width
         work_array (i,1)   = spv
         work_array (i,height) = spv
      enddo
        
      do j=1,height
         work_array (1,j)   = spv
         work_array (width,j) = spv
      enddo             

c     recopie le resultat dans le tableau initial
      
      do i=1,NXX
         do j=1,NYY
            map_in(i,j) = work_array(i,j)
         enddo 
      enddo 
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  CalcConvolution
c Date de creation :  5 juillet 93
c
c Parametres       :  map_in  - tableau a convoluer
c                     matrix  - matrice de convolution
c                     iter    - nombre de convolutions a effectuer
c
c Description      :  la matrice de convolution est un array de 9
c                     elements qui decrivent une matrice 3x3
c
c                     | conv(1)  conv(2)  conv(3) |
c                     | conv(4)  conv(5)  conv(6) |
c                     | conv(7)  conv(8)  conv(9) |
c 
c ---------------------------------------------------------------------

      subroutine CalcConvolution (map_in, bimg, matrix, iter)
                                    

      implicit none
      include 'common.h'
      include 'workarray.h'

      TYPE( bimgfile ) bimg

      real    map_in(NXX,NYY), matrix(9)
      integer iter, imax,jmax
      integer i,j,l, ip1,im1,jp1,jm1
      real    spv

      print *,'Convolution'
      call flush(6)
      
      imax = bimg%nxfile
      jmax = bimg%nyfile
      spv  = bimg%spval

      do l=1,iter
         do i=1,NXX
            do j=1,NYY
               if (l.gt.1) then
                  map_in(i,j)=work_array(i,j)
               endif 
               work_array(i,j) = spv
            enddo
         enddo

         do j=1,bimg%nydata
            jp1 = j+1
            jm1 = j-1

            do i=1,bimg%nxdata
               ip1 = i+1
               im1 = i-1

              if((i.eq.1).or.(j.eq.1).or.(i.eq.imax).or.(j.eq.jmax))then
                  work_array(i,j) = map_in(i,j)
               else if ((map_in(i  ,j).eq.spv).or.

     .                  (map_in(im1,jm1).eq.spv).or.
     .                  (map_in(im1,jp1).eq.spv).or.
     .                  (map_in(ip1,jp1).eq.spv).or.
     .                  (map_in(ip1,jm1).eq.spv).or.

     .                  (map_in(ip1,j).eq.spv).or.
     .                  (map_in(im1,j).eq.spv).or.
     .                  (map_in(i,jp1).eq.spv).or.
     .                  (map_in(i,jm1).eq.spv)) then
                  work_array(i,j) = map_in(i,j)
               else
                  work_array(i,j) = matrix(1) * map_in(im1,jm1)
     .                          + matrix(2) * map_in(i  ,jm1)
     .                          + matrix(3) * map_in(ip1,jm1)
     .                          + matrix(4) * map_in(im1,j  )
     .                          + matrix(5) * map_in(i  ,j  )
     .                          + matrix(6) * map_in(ip1,j  )
     .                          + matrix(7) * map_in(im1,jp1)
     .                          + matrix(8) * map_in(i  ,jp1)
     .                          + matrix(9) * map_in(ip1,jp1)
               endif
            enddo
         enddo
      enddo 

c     recopie le resultat dans le tableau initial

      do i=1,NXX
         do j=1,NYY
            map_in(i,j) = work_array(i,j)
         enddo 
      enddo 

      return
      end
c--------------------------------------------------------------
c                Subroutine CalculateCutHeading
c  Objet : Calcule le cap geographique (mis dans la variable
c          angled du common /cut_properties/), en degres.
c
c  Utilisation: Le tableau map_coord est passe en argument
c
c  Auteur : Jean-Marc Molines 1996
c-------------------------------------------------------------
	subroutine CalculateCutHeading(map_coord)

c---------------
c determine la direction /Nord d'un segment precise par map_coord
c Le resultat en degres est passe dans angled du common /cut/
c JMM 29/07/96
c---------------
	implicit none
	real map_coord(4), angled, pi,cut_dist
	real xa,xb,ya,yb

	common /cut_properties/ angled,cut_dist

	pi=acos(-1.)
	
	xa=map_coord(1)*pi/180.
	xb=map_coord(2)*pi/180.

	ya=-alog(tand(45-map_coord(3)/2.))
	yb=-alog(tand(45-map_coord(4)/2.))

	angled=atan2((xb-xa),(yb-ya))
	angled=angled*180./pi
	if (angled.lt.0) angled=angled+360.
	print '(a,f5.1)',' La coupe est orientee a N',angled
	return
	end
c
c-----------------------------------------------------------------
c                 Subroutine CalculateCutDistance
c  Objet:  calcule la longueur de la coupe le long de la ligne de
c          coupe. Cette distance est la somme des distances
c          orthodromiques entre les pts interpolles
c
c  Utilisation: le tableau map_coord contient les x1,x2,y1,y2 
c           limitant la coupe. Cette routine est utilisee pour
c           les options d'echelle en km
c 
c  Auteur: Jean-Marc Molines 1996 
c------------------------------------------------------------------ 
	subroutine CalculateCutDistance(map_coord,nx)
	implicit none
	real*4 map_coord(4) 
	real*8 dist, x0, x1, y0, y1, dxd, dyd
	integer nx, i
	real*4 cut_dist,  angled

        common /cut_properties/ angled,cut_dist

	cut_dist=0.

	dxd=dble((map_coord(2)-map_coord(1)))/(nx-1)
	dyd=dble((map_coord(4)-map_coord(3)))/(nx-1)
	print *,' Calcul de la distance ...'

	x0=map_coord(1)
	y0=map_coord(3)

	do i=2,nx
	x1=map_coord(1) + (i-1) * dxd
	y1=map_coord(3) + (i-1) * dyd
	cut_dist=cut_dist + dist(x0,x1,y0,y1)
	x0=x1
	y0=y1
	enddo
	
	print '(a,f7.1,a)',' La coupe fait ',cut_dist,' km de long'
	return
	end

c-----------------------------------------------------------------
c                 FOnction dist:
c  Objet:  calcule la distance orthodromique entre les points A et B
c          de position geographique A(lona,lata) B(lonb,latb)
c
c  Utilisation: utilisee dans CalculateCutDistance par sommation
c               entre les points d'un arc de coupe.
c  Remarque: Double precision necessaire.
c
c  Auteur: Jean-Marc Molines 1996
c------------------------------------------------------------------

        real*8 function dist(lona,lonb,lata,latb)
c
c Cette fonction retourne la distance en km entre les pts
c A (lona, lata) et B(lonb, latb) le long d'une orthodromie
 	implicit none
        real*8 lata, lona, pi
        real*8 latb, lonb, R
	real*8 latar, latbr, lonar, lonbr
	real*8 pds

	real*8 ux,uy,uz,vx,vy,vz,conv
        pi=dacos(-1.d0)
        conv=pi/180.
c Rayon de la terre
        R=(6378.137+6356.7523)/2.0 ! km

        latar=lata*conv
        latbr=latb*conv

        lonar=lona*conv
        lonbr=lonb*conv
	if (lonar.ne.lonbr) then
c
        ux=dcos(lonar)*dcos(latar)
        uy=dsin(lonar)*dcos(latar)
        uz=dsin(latar)
c
        vx=dcos(lonbr)*dcos(latbr)
        vy=dsin(lonbr)*dcos(latbr)
        vz=dsin(latbr)
      pds=ux*vx+uy*vy+uz*vz
      if (pds.gt.1.d0) then
         dist=0.
      else
         dist=R*dacos(pds)
      endif
	else
	dist=R*(latbr-latar)
	endif
	
        return
        end



