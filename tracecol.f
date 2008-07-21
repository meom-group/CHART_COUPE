c --------------------------------------------------------------------
c 
c Programme      :  chart
c Fichier        :  tracecol.f
c 
c Auteur         :  Eric Brown
c
c Description    :  Contient la plupart des fonctions qui dessinent des
c                   elements de la carte.
c 
c ---------------------------------------------------------------------




c ---------------------------------------------------------------------
c 
c Nom         :  InitMap
c 
c Description :  Initialise les parametres de la carte
c                (utile meme si les continents ne sont pas dessines)
c                A appeler absoluement avant les autres fonctions
c                de dessin.
c                Contient entre autres le premier "call set", qui
c                influence tous les autres appels a "set".
c 
c ---------------------------------------------------------------------

      subroutine  InitMap ()

      implicit none
      include 'common.h'
      include 'mapncar.h'

      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep, kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw


      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm

      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat

      real corner1(2),corner2(2),corner3(2),corner4(2)
      real ang

      real PI

      integer iasf(13)
      data iasf / 13*1 /

      real xperim(5),yperim(5)
      data xperim /0.0, 1.0, 1.0, 0.0, 0.0/
      data yperim /0.0, 0.0, 1.0, 1.0, 0.0/


c**      print *,'Initialisation des parametres de la carte'
c**      call  flush(6)

      call gsclip(0)
      call gsasf (iasf)

c     couleur et qualite du texte de toute la carte

      call pcsetc ('FC','|')       ! set control character to |
      call pcsetc ('FN', c_font)   ! font
      call pcseti ('CC(0)',1)      ! character color index
      call gaseti ('LTY',1)        ! use plcchq to draw labels

      call gsplci(1)               ! set polyline color index
      call gsfais(1)               ! set polyline fill interior style

      call arinam(iama,sz_iama)

      if (opt_noproj.eq.0) then
       if (opt_rlat.eq.0) then
      	map_rlat = (map_marg(4)-map_marg(3))/2. + map_marg(3)
       endif 

       if (opt_rlon.eq.0) then
      	map_rlon = (map_marg(2)-map_marg(1))/2. + map_marg(1)
       endif 

       call mappos (x1pos,x2pos,y1pos,y2pos) ! position the map 
       call mpsetc ('OU' ,'CO') ! continental outlines
       call mpseti ('PE',1)
       call mpseti ('LA' ,0)  ! no labels for meridians
 
       corner1(1) = map_marg(4)     ! limites de la projection
       corner1(2) = map_marg(1)
       corner2(1) = map_marg(4)
       corner2(2) = map_marg(2)
       corner3(1) = map_marg(3)
       corner3(2) = map_marg(2)
       corner4(1) = map_marg(3)
       corner4(2) = map_marg(1) 
 
c     Dans le cas d'une projection Orthographique, les limites
c     de la projection sont plutot definies par LI, pour permettre
c     une projection telle que dans la figure 3, p 90 de Contouring
c     and Mapping Tutorial. Details page 91, formule utilisee :
c     Orthographic, MP 1.5 page 71.
c     Pourrait etre etendu a d'autres projections.

       if (map_proj.eq.'OR') then
      	if (map_zone.ne.'MA') then
      	   map_zone = 'LI'
	   PI = 3.14159265358979323846
	   corner1(1) = map_marg(1) - map_rlon
	   corner2(1) = map_marg(2) - map_rlon
	   corner3(1) = map_marg(3) - map_rlat
	   corner4(1) = map_marg(4) - map_rlat
	   
c     inutile de traiter plus qu'un hemisphere...

	   if (corner1(1).lt.-90) corner1(1) = -90
	   if (corner2(1).gt.90)  corner2(1) = 90
	   
	   corner1(1) = sin ((corner1(1) * PI)/180.0)
	   corner2(1) = sin ((corner2(1) * PI)/180.0)
	   corner3(1) = sin ((corner3(1) * PI)/180.0)
	   corner4(1) = sin ((corner4(1) * PI)/180.0)
	   
      	endif 
	
       elseif (map_proj.eq.'CE'.or.map_proj.eq.'ME') then
      	map_rlat = 0.
       elseif (map_proj.eq.'MO'.or.map_proj.eq.'RO') then
        map_zone='GR'
C       print *,'corner1', corner1, map_marg(1), map_rlon
C       print *,'corner2', corner2, map_marg(2), map_rlon
C       print *,'corner3', corner4, map_marg(3), map_rlat
C       print *,'corner4', corner4, map_marg(4), map_rlat
        corner1(1)=map_marg(3)
        corner2(1)=map_marg(1)
        corner3(1)=map_marg(4)
        corner4(1)=map_marg(2)
        call mpseti ('PE',1)
       call mpseti ('C4', 22 )
        call mpseti('EL',1)   !  elliptic perimeter
       endif 
 
 
       if (map_proj.eq.'CE') then      
      	call mpsetr ('GR',0.)
       else
      	call mpsetr ('GR',xstep)
       endif 
 
 
       call maproj (map_proj,map_rlat,map_rlon,0.)
 
 
       call mapset (map_zone,corner1,corner2,corner3,corner4)
C JMM
      IF ( map_proj .eq. 'OR' ) then 
        
        CALL MAPSTI ('EL',1)
        IF ( map_coord(4) .LE. 0 ) then
C        south hemisphere
         print *,'ANG', ang
         ang=90.+ map_coord(4)+2
        ELSE
C        north hemisphere
         ang=90. -map_coord(3)+2
        ENDIF
        CALL MAPSET ('AN',ang,ang,ang,ang)
      ENDIF
       call mapint  
C choose the standard resolution ezmap data base or the higher one
       if (opt_high .eq. 0 ) then
       call mapbla(iama)      ! add boundary lines to area map
       else
       call mplnam('Earth..1',1,iama)      ! add boundary lines to area map
       endif

      else 
C ... opt_noproj = 1 (ie pas de projection)
       print *,'X1POS...', x1pos, x2pos, y1pos, y2pos
       print *, map_coord
       print *, map_marg
       call set   (x1pos,x2pos,y1pos,y2pos,
     .               map_coord(1), map_coord(2),
     .               map_coord(3), map_coord(4),1)
C        call set   (x1pos,x2pos,y1pos,y2pos,
C    .               map_marg(1), map_marg(2),
C    .               map_marg(3), map_marg(4),1)

      call  aredam (iama,xperim,yperim,5,15,30,31)            
      endif 

c     Fichier de coordonnees pour SIMGRAPH ----------------------------

         if (opt_coords.eq.1) then
          call CreateCoordsFile (map_marg, f_coords)
         endif

         return 
         end

c ---------------------------------------------------------------------
c 
c Nom         :  ColorMap
c 
c Parametres  :  
c Description :  Associe une couleur a chaque point de grille.
c                procede par isocontours si l'option -print est
c                utilisee.
c 
c ---------------------------------------------------------------------

         subroutine ColorMap (fld, bimg, limit, ncol)

         implicit none

         include 'common.h'
         include 'color.h'
         include 'mapncar.h'

         real     limit(NBOXMAX+1)
         real     fld(NXX, NYY)
         integer  nx,ny, ncol

         TYPE( bimgfile ) bimg

         integer i,j, l
         real x,y,rlat,rlon
         real dx,dy, ptr
         integer icra, ncls, iusr, jusr
c        dimension icra(1500,1500)
         dimension icra(1615,1615)
         data ncls /300/
         integer offset,index, map_flag
         real   rl,rr,rb,rt, ur,ul,ut,ub

         real cfux, cfuy
         external colcontours

         real x1pos,x2pos,y1pos,y2pos
         real x1bat,x2bat,y1bat,y2bat
         real x1pal,x2pal,y1pal,y2pal
         real ylon,ylat,ykm

         common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat

      real xperim(5),yperim(5)
      data xperim /0.0, 1.0, 1.0, 0.0, 0.0/
      data yperim /0.0, 0.0, 1.0, 1.0, 0.0/

      real spv

      if (opt_color.eq.0) return

      call gflas1(2)

      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval

      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)

      dx = real(nx)/(real(map_coord(2)-map_coord(1)))
      dy = real(ny)/(real(map_coord(4)-map_coord(3)))

      if (opt_print.eq.0) then
       if (opt_noproj.eq.0) then
c**         print *,'Coloration des pixels'
    	call  flush(6)

	  do i=1,ncls
	   x=cfux(x1pos+(x2pos-x1pos)*(real(i-1)+.5)/real(ncls))

	   do j=1,ncls                                           
		 y=cfuy(y1pos+(y2pos-y1pos)*(real(j-1)+.5)/real(ncls))

		 call maptri (x,y,rlat,rlon)                          


c     comme maptri retourne des valeurs comprises entre -180 et 180
c     il faut verifier que rlon est quand meme dans le fichier si nos
c     donnees sont reparties de 0 a 360.

c                  if ((rlon.lt.0).and.
c     .                (map_coord(2).gt.180).and.
c     .                (rlon.ge.(map_coord(2)-540))) then
	   if ((rlon.lt.map_coord(1)).and.
     .                (rlon+360.le.map_coord(2))) then
			rlon = rlon + 360.
		  endif 

c     il faut aussi faire la verification si la carte debute avant -180.

		  if ((rlon.gt.map_coord(2)).and.
     .                (rlon.ge.map_coord(1))) then
			 rlon = rlon - 360.
		  endif  
		  
		  
		  if ((rlat.ne.1.e12).and.
     .                (rlat.ge.map_coord(3)).and.
     .                (rlat.le.map_coord(4)).and.
     .                (rlon.ge.map_coord(1)).and.
     .                (rlon.le.map_coord(2)))then

			 iusr = int(real(rlon-map_coord(1))*dx)+1
			 jusr = int(real(rlat-map_coord(3))*dy)+1

			 if (fld(iusr,jusr).eq.spv) then 
				icra(i,j) = COLOR_SPVAL
			 else if (fld(iusr,jusr).le.limit(1)) then
				icra(i,j) = COLOR_NRES
			 else if (fld(iusr,jusr).ge.limit(ncol)) then
				icra(i,j) = ncol + COLOR_NRES -1
			 else
				offset = 0
				ptr = float(ncol)
				
				do while (ptr.ne.1.0)
				   ptr = ptr / 2.0

				   if (ptr.le.1.0) ptr = 1.0
				   index = nint(ptr)+offset
				   
				   if (fld(iusr,jusr).ge.limit(index)) then
					  if (fld(iusr,jusr).lt.limit(index+1))then
						 ptr = 1.0
					  endif 
					  offset = index
				   endif                   
				enddo 
			 
				icra(i,j) = offset + COLOR_NRES - 1
			 endif 
		  else                         
			 if (rlat.eq.1.e12) then
				icra(i,j)=COLOR_BACKGROUND
			 else 
				icra(i,j)=COLOR_OCEAN
			 endif 
		  end if             
	   
	   enddo 
    	enddo 

C           call gca (cfux(x1pos),cfuy(y1pos),cfux(x2pos),cfuy(y2pos),
C    .                1000,1000, 1,1,ncls,ncls,icra) 
c  call gca (cfux(x1pos),cfuy(y1pos),cfux(x2pos),cfuy(y2pos),
c    .                1500,1500, 1,1,ncls,ncls,icra) 
	  call gca (cfux(x1pos),cfuy(y1pos),cfux(x2pos),cfuy(y2pos),
     .                1615,1615, 1,1,ncls,ncls,icra) 
      else
    	do i=1,nx
	   do j=1,ny
		  if (fld(i,j).eq.spv) then 
			 icra(i,j) = COLOR_SPVAL
		  else if (fld(i,j).lt.limit(1)) then
			 icra(i,j) = COLOR_NRES
		  else if (fld(i,j).ge.limit(ncol)) then
			 icra(i,j) = ncol + COLOR_NRES-1
		  else
			 offset = 0
			 ptr = float(ncol)
			 
			 do while (ptr.ne.1.0)
				ptr = ptr / 2.0
				index = nint(ptr)+offset
				if (fld(i,j).ge.limit(index)) then
				   if (fld(i,j).lt.limit(index+1)) then
					  ptr = 1.0
				   endif 
				   offset = index
				endif                   
			 enddo 
			 
			 icra(i,j) = offset + COLOR_NRES-1
			 
		  endif 
	    enddo
    	enddo

    	call set(x1pos,x2pos,y1pos,y2pos,
     .               1.,float(nx),1.,float(ny),1)

c    call gca(1.,1.,float(nx),float(ny),1500,1500,1,1,nx,ny,icra)
	    call gca(1.,1.,float(nx),float(ny),1615,1615,1,1,nx,ny,icra)

        endif 
      else 
c**            print *,'Coloration des pixels par isocontours'
      call  flush(6)

       if (bimg%grid .eq. 3 ) then
        map_flag = 6
       else 
        map_flag = 1
       endif

       if (opt_noproj .eq. 1) map_flag = 0
       call ColorContour (fld, bimg, limit, map_coord, map_flag,ncol)
       call set   (rl,rr,rb,rt,0.0,1.0,0.0,1.0,1)
       call  aredam (iama,xperim,yperim,5,15,30,31)            
       call arscam(iama,xcra,ycra,sz_cra,iaia,igia,10,colcontours)
       endif             

       call set   (rl,rr,rb,rt,ur,ul,ut,ub,1)

       call gflas2

c      print *,'Space used :' ,sz_iama - (iama(6) - iama(5) - 1)

       return
       end



c ---------------------------------------------------------------------
c 
c Nom         :  CreateCoordsFile
c 
c Parametres  :  coordonnees de la fenetre en longitude, latitude
c                (evite d'inclure common.h)
c                nom du fichier dans lequel sauver les coordonnees.
c
c Description :  cree un fichier de coordonnees pour
c                permettre le reperage par souris dans simgraph
c                option CYLINDRICAL EQUIDISTANT (CE) seulement
c 
c ---------------------------------------------------------------------

      subroutine CreateCoordsFile (coords, filecoords)

      implicit none

      real rl, rr, rb, rt, coords(4)
      real ur,ul,ut,ub
      integer l
      character*256 filecoords

      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)


      open(88,file=filecoords,form='formatted')
      write(88,*)'PLOTFRAME'
      write(88,'(e15.5)')rl
      write(88,'(e15.5)')coords(1)
      write(88,'(e15.5)')rr
      write(88,'(e15.5)')coords(2)
      write(88,'(e15.5)')rb
      write(88,'(e15.5)')coords(3)
      write(88,'(e15.5)')rt
      write(88,'(e15.5)')coords(4)
      close(88)

      return
      end
c ---------------------------------------------------------------------
c 
c Nom              :  colcontours
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine colcontours (xcs,ycs,ncs,iai,iag,nai)

      implicit none

      real    xcs(*),ycs(*)
      integer iai(*),iag(*), ncs, nai
      integer i, iai1, iaiperim


      iai1=-1
      do 101 i=1,nai
         if (iag(i).eq.3) then 
      	  iai1=iai(i)
         elseif (iag(i).eq.15) then
      	  iaiperim = iai(i)
         endif
101    continue


      if ((iai1.gt.0).and.(iaiperim.ne.31)) then
         call gsfaci(iai1)
         call gfa(ncs-1,xcs,ycs)               
      endif

      return
      end

c ---------------------------------------------------------------------
c 
c Nom         :  AjouteTexte
c 
c Parametres  :  
c
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine AjouteTexte ()

      implicit none

      include 'common.h'
      include 'color.h'
      include 'val_table.h'

      character*256 strOut


      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      integer iclip, ierr
      real zrec(4)
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep,kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm

      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat

      integer lnblnk
      integer i,  len

      real    rl,rr,rb,rt, ur,ul,ut,ub
      integer l

      character*24 fdate, cdat
      call gqclip(ierr,iclip,zrec)
      call gsclip(0)
      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
      call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
c     -----------------------------------------------------------
c     option -dat
      if ( opt_dat .EQ.  1 ) then
      cdat=fdate()
C      call plchhq( 0.98,0.01,cdat,-0.35,0.,1.)
       call plchhq( 0.01,0.01,cdat,-0.35,0.,-1.)
      endif
c
c     -----------------------------------------------------------
c     option -team
      if ( opt_team .EQ. 1 ) then
      cdat='LEGI-MEOM'
      call plchhq( 0.01,0.02,cdat,-0.35,0.,-1.)
      endif              
c     -----------------------------------------------------------
c     option -string

      if (strcount.ne.0) then
       do i = 1, strcount
      	call ParseString (text(i)%str, strOut)
      	len = lnblnk(strOut)

        if ( text(i)%ypos .LT.  0. ) text(i)%ypos = rt + 0.03
      	call plchhq (text(i)%xpos, text(i)%ypos,
     .                   strOut(1:len), -(text(i)%csize),0.,
     .                   text(i)%align)
       enddo 
      endif 

c     -----------------------------------------------------------
c     option -stringr

      if (strcountr.ne.0) then
       do i = 1, strcountr
      	call ParseString (textr(i)%str, strOut)
      	len = lnblnk(strOut)

      	call plchhq (textr(i)%xpos, textr(i)%ypos,
     .                   strOut(1:len), -(textr(i)%csize),
     .                   textr(i)%angle,
     .                   textr(i)%align)
        enddo 
       endif 

       call set(rl,rr,rb,rt,ur,ul,ut,ub,l)
       call gsclip (iclip)

       return
       end
c ---------------------------------------------------------------------
c 
c Nom         :  AjoutePalette
c 
c Parametres  :  
c
c Description :  Cette fonction dessine une barre de palette dans
c                un rectangle defini par l'option -xybar
c
c                Les coordonnees du rectangle dans lequel sera dessinee
c                la palette sont donnees par x1bat, x2bat, y1bat,y2bat.
c                La fonction set est appelee pour que tout le plot frame
c                de NCAR soit accessible. 
c                La fonction remet le plot frame a son etat initial.
c 
c ---------------------------------------------------------------------

      subroutine AjoutePalette (limit, ncol)

      implicit none

      include 'common.h'
      include 'val_table.h'
      include 'color.h'

      real    limit(NBOXMAX+1)
      integer ncol
      integer int_format, lnblnk, len
      
      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep, kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw

      real    coef, coeflab, ydeb,yfin, xdeb,xfin
      character*20 llbs(NBOXMAX+1)

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(146)


      real    cl_min, cl_max
      integer cl_met, cl_exp

      common  /color/cl_min, cl_max, cl_met, cl_exp
      save    /color/


      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat

      integer i, jmark
      integer lfin(NBOXMAX+1)

      real    rl,rr,rb,rt, ur,ul,ut,ub
      real x1per, y1per, x2per, y2per
      integer l
      integer IDformat



      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
      call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

c     ------------------------------------------------------------
c     calcul des indices de couleurs et des labels
c     pour la palette
c     les boites correspondent aux indices definis dans le fichier
c     de palette.

      coef = 10.**cl_exp

      int_format = IDformat (int_table(ICLR_PAL)%format)
        
      do i=1,marks_count+1
         lfin(i) = i

         if (int_format.eq.1) then
            write(llbs(i),int_table(ICLR_PAL)%format)
     .              nint (limit(marks_index(i)-COLOR_NRES+1)/(coef))
            call FilterBlanks (llbs(i))
         else 
            write(llbs(i),int_table(ICLR_PAL)%format)
     .                  (limit(marks_index(i)-COLOR_NRES+1)/(coef))
            call FilterBlanks (llbs(i))
         endif 
        IF (opt_clrmark.EQ. 1 ) llbs(i)=' '
      enddo
      

      call gsfais (1)
C Pour memoire les params possible pour lbsetr sont
C  CBL - Integer Color of box lines def -1
C  CFL - Integer Color of fill lines def -1
C  CLB - Integer Color of labels  def -1
C  WBL - Real  width of box lines def 0
C  WFL - Real  width of fill lines def 0
C  WLB - Real  width of label lines def 0
      call lbsetr ('WBL', 0.)
	if (opt_vertpal .eq. 0 ) then
      call DrawPalette (0,      ! horizontal bar
     .          x1pal,          ! left plotter frame coord
     .          x2pal,          ! right plotter frame coord
     .          y1pal,          ! bottom plotter frame coord
     .          y2pal,          ! top plotter frame coord
     .    nobox*marks_count,    ! number of color boxes
     .          1.0,            ! fill all color box width
     .          1./4.,          ! fill only part of box height
     .          lfin,           ! color index array
     .          1,              ! 
     .          llbs,           ! label list
     .          marks_count+1,  ! label centered in boxes
     .          lbpos)          ! position of label 1 below 2 above 3 both
C                               ! 0 nolabel
        x1per = x1pal
        x2per = x2pal
      IF (lbpos .EQ. 0 .OR. lbpos .EQ. 3 ) THEN
C  la division par 8 est liee a /4/2, 4 etant la valeur 1./4. ...
        y1per = y1pal + (1. - 1/4.) /2. * (y2pal - y1pal)
        y2per = y2pal - (1. - 1/4.) /2. * (y2pal - y1pal)
      ELSE IF (lbpos .EQ. 2 ) THEN
        y1per=y1pal
        y2per = y1pal + (y2pal - y1pal)/4.
      ELSE IF (lbpos .EQ. 1 ) THEN
        y1per= y2pal - (y2pal - y1pal)/4.
        y2per = y2pal
      END IF
	else
      call DrawPalette (1,      ! vertical bar
     .          x1pal,          ! left plotter frame coord
     .          x2pal,          ! right plotter frame coord
     .          y1pal,          ! bottom plotter frame coord
     .          y2pal,          ! top plotter frame coord
     .    nobox*marks_count,    ! number of color boxes
     .          1.0/4.,         ! fill all color box width
C    .          0.95,           ! fill only part of box height
     .          1.00,           ! fill only part of box height
     .          lfin,           ! color index array
     .          1,              ! 
     .          llbs,           ! label list
     .          marks_count+1,  ! label centered in boxes
     .          lbpos)          ! 1 right, 2 left, 3 both 0 nolabel
        y1per = y1pal
        y2per = y2pal
      IF (lbpos .EQ. 0 .OR. lbpos .EQ. 3 ) THEN
C  la division par 4 est liee a /2/2, 2 etant la valeur 1.0/2.0 ...
C       x1per = x1pal + (x2pal - x1pal)/8.
C       x2per = x2pal - (x2pal - x1pal)/8.
        x1per = x1pal + (1. - 1/4.) /2. * (x2pal - x1pal)
        x2per = x2pal - (1. - 1/4.) /2. * (x2pal - x1pal)
      ELSE IF (lbpos .EQ. 1 ) THEN
        x1per=x1pal
        x2per = x1pal + (x2pal - x1pal)/4.
      ELSE IF (lbpos .EQ. 2 ) THEN
        x1per= x2pal - (x2pal - x1pal)/4.
        x2per = x2pal
      END IF
	endif
      IF (opt_clrmark .EQ. 1 ) THEN
       DO i=1,nmark
         if (int_format.eq.1) then
            write(llbs(i),int_table(ICLR_PAL)%format)
     .              nint (clrmark(i)/(coef))
            call FilterBlanks (llbs(i))
         else
            write(llbs(i),int_table(ICLR_PAL)%format)
     .                  (clrmark(i)/(coef))
            call FilterBlanks (llbs(i))
         endif
      enddo
       ENDIF

      IF ( nobox .EQ. -1 ) THEN
       CALL plotif(0.,0.,2)
       CALL frstpt(x1per,y1per)
       CALL vector( x1per,y1per)
       CALL vector( x2per,y1per)
       CALL vector( x2per,y2per)
       CALL vector( x1per,y2per)
       CALL vector( x1per,y1per)
       CALL plotif(0.,0.,2)
       if ( opt_vertpal .EQ. 0 ) then
        coeflab = (x2per - x1per)/(cl_max-cl_min)
        do jmark=1,nmark
         xmark(jmark) = x1per + coeflab*(clrmark(jmark)-cl_min)
        end do
C pour faire un 'tick' qui depasse la pallette
        IF (lbpos .EQ. 1 .OR. lbpos .EQ. 3) THEN
         ydeb=y1per
         yfin=y1per-0.15*(y2per-y1per)
         DO jmark=1,nmark
          len=lnblnk(llbs(jmark))
C         PRINT *,llbs(jmark),xmark(jmark),yfin
          call plotif(0.,0.,2)
          call frstpt(xmark(jmark),ydeb)
          call vector(xmark(jmark),yfin)
          call plchhq(xmark(jmark),yfin-0.02,llbs(jmark)(1:len),rlbsc*0.012,0.,0.)
         END DO
         call plotif(0.,0.,2)
        ENDIF
        IF (lbpos .EQ. 2 .OR. lbpos .EQ. 3) THEN
         ydeb=y2per
         yfin=y2per+0.15*(y2per-y1per)
         DO jmark=1,nmark
          len=lnblnk(llbs(jmark))
C         PRINT *,llbs(jmark),xmark(jmark),yfin
          call plotif(0.,0.,2)
          call frstpt(xmark(jmark),ydeb)
          call vector(xmark(jmark),yfin)
          call plchhq(xmark(jmark),yfin+0.02,llbs(jmark)(1:len),rlbsc*0.012,0.,0.)
         END DO
         call plotif(0.,0.,2)
        ENDIF
C
C palette verticale
       else
        coeflab = (y2per - y1per)/(cl_max-cl_min)
        do jmark=1,nmark
         xmark(jmark) = y1per + coeflab*(clrmark(jmark)-cl_min)
        end do
        IF (lbpos .EQ. 1 .OR. lbpos .EQ. 3) THEN
         xdeb=x2per
         xfin=x2per+0.10*(x2per-x1per)
         DO jmark=1,nmark
          len=lnblnk(llbs(jmark))
          call plotif(0.,0.,2)
          call frstpt(xdeb,xmark(jmark))
          call vector(xfin,xmark(jmark))
          call plchhq(xfin+0.02,xmark(jmark),llbs(jmark)(1:len),rlbsc*0.012,0.,-1.)
         END DO
         call plotif(0.,0.,2)
        END IF
        IF (lbpos .EQ. 2 .OR. lbpos .EQ. 3) then
         xdeb=x1per
         xfin=x1per-0.10*(x2per-x1per)
         DO jmark=1,nmark
          len=lnblnk(llbs(jmark))
          call plotif(0.,0.,2)
          call frstpt(xdeb,xmark(jmark))
          call vector(xfin,xmark(jmark))
          call plchhq(xfin-0.02,xmark(jmark),llbs(jmark)(1:len),rlbsc*0.012,0.,1.)
         END DO
         call plotif(0.,0.,2)
        ENDIF
       endif
      END IF

      call set(rl,rr,rb,rt,ur,ul,ut,ub,l)
     
      return
      end

c ---------------------------------------------------------------------
c 
c Nom         :  DessineContinents
c 
c Parametres  :  segment_num - identification du segment cree. 
c                              (indice utilise par GFLAS3)
c 
c Description :  Cette fonction cree un segment de metacode contenant
c                les continents. Il n'est dessine qu'une seule fois 
c                au debut et est rappele avec GFLAS3 pour chacune des 
c                couches.
c 
c ---------------------------------------------------------------------

      subroutine DessineContinents(segment_num)

      implicit none
      include 'common.h'
      include 'mapncar.h'
      include 'color.h'

      integer segment_num

      external colcont

      call mpseti ('C5 - continental outlines color',
     .              COLOR_CONTINENT_PERIM)

      call gflas1(segment_num)

      if (opt_mapfill.eq.1) then
         call arscam (iama,xcra,ycra,sz_cra,iaia,igia,10,colcont)
      endif 
      
      if (opt_high .eq. 0 ) then
      call maplot
      else
      call mplndr('Earth..1',1)
      endif


      call gflas2
      

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  DessineOceans
c 
c Parametres  :  segment_num - identification du segment cree. 
c                              (indice utilise par GFLAS3)
c 
c Description :  Cette fonction cree un segment de metacode contenant
c                les oceans. Il n'est dessine qu'une seule fois 
c                au debut et est rappele avec GFLAS3 pour chacune des 
c                couches. (seulement pour l'option noproj)
c 
c ---------------------------------------------------------------------

      subroutine DessineOceans(segment_num)

      implicit none
      include 'common.h'
      include 'mapncar.h'

      integer segment_num

      external colocean


      call gflas1(segment_num)

cJMM Next if uncommented October 6, 95

       if ((opt_color.eq.1).and.(opt_noproj.eq.0)) then   

      call arscam (iama,xcra,ycra,sz_cra,iaia,igia,10,colocean)

       endif

      call gflas2
      

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  DessineMeridiens
c 
c Parametres  :  segment_num - identification du segment cree. 
c                              (indice utilise par GFLAS3)
c 
c Description :  Cette fonction cree un segment de metacode contenant
c                les meridiens. Il n'est dessine qu'une seule fois 
c                au debut et est rappele avec GFLAS3 pour chacune des 
c                couches.
c 
c ---------------------------------------------------------------------

      subroutine DessineMeridiens (segment_num)

      implicit none

      include 'common.h'
      include 'val_table.h'
      include 'mapncar.h'

      integer segment_num

      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep, kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw

      real    rl,rr,rb,rt, ur,ul,ut,ub
      common /special/ rl,rr,rb,rt, ur,ul,ut,ub
      integer l
      integer ixtick, iytick
      integer ixlb, iylb
      
      external TraceMeridiens
       external TraceMeridian

      call gflas1(segment_num)

      call pcsetr ('SA',cs_scale)      ! change character size
      call getset(rl,rr,rb,rt,ul,ur,ub,ut,l)
C     if ((map_proj.eq.'ME').and.(opt_grid.eq.1)) then
      if ((map_proj.eq.'ME')) then
         call mapgrm (iama,xcra,ycra,sz_cra,iaia,igia,10,
     .                TraceMeridian)
      else if ((map_proj.ne.'CE').and.(opt_grid.eq.1)) then
         call mapgrm (iama,xcra,ycra,sz_cra,iaia,igia,10,
     .                TraceMeridiens)
      endif 

      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
      call set   (rl,rr,rb,rt,
     .            map_marg(1),map_marg(2),map_marg(3),map_marg(4),1)

      call labmod (int_table(ICLR_XAXIS)%format,
     .             int_table(ICLR_YAXIS)%format,
     .             0,0,1,1,10,10,0)

      if ((opt_labx.eq.0).or.(map_proj.ne.'CE')) then
         ixlb = 0
      else 
         ixlb = 1
      endif 

      if ((opt_laby.eq.0).or.(map_proj.ne.'CE')) then
         iylb = 0
      else 
         iylb = 1
      endif 

      if (opt_xstep.eq.0) xstep = abs(map_marg(1)-map_marg(2))/4.0
      if (opt_ystep.eq.0) ystep = abs(map_marg(3)-map_marg(4))/4.0


      if ((opt_grad.eq.0).or.(map_proj.ne.'CE')) then
         ixtick = 0
         iytick = 0
      else 
         ixtick = nint (abs(map_marg(1)-map_marg(2))/xstep)         
         iytick = nint (abs(map_marg(3)-map_marg(4))/ystep)
      endif


      if (opt_perim.eq.1) then
         if (map_proj.eq.'CE') then                        
            if (opt_grid.eq.1) then 
               if (opt_xgrid.eq.1) then
                  call gasetr ('WMJ - major tick line width',xlw)
                  call gridal (ixtick,xsub,0,0,ixlb,iylb,1,0.,0.)
                  call gasetr ('WMJ - major tick line width',1.0)
               endif 
               if (opt_ygrid.eq.1) then
                  call gasetr ('WMJ - major tick line width',ylw)
                  call gridal (0,0,iytick,ysub,ixlb,iylb,4,0.,0.)
                  call gasetr ('WMJ - major tick line width',1.0)
               endif 
            else 
               call gridal (ixtick,xsub,iytick,ysub,ixlb,iylb,5,0.,0.)
            endif 
         else 
            call gridal (0,0,0,0,ixlb,iylb,5,0.,0.)
         endif 
      endif 
 
      call pcsetr ('SA',0.88888888888) ! reset character size


c     remet les coordonnees a leur etat initial

      call set(rl,rr,rb,rt,ur,ul,ut,ub,l)

      call gflas2

      return
      end



c ---------------------------------------------------------------------
c 
c Nom          :  TraceMeridiens
c
c Description  :  Fonction appelee par MAPGRM (ncar) pour tracer
c                 des meridiens masques par les continents.
c
c Reference    :  Contouring and Mapping Tutorial
c                 Mp 4.6,  page 128
c 
c ---------------------------------------------------------------------

        subroutine TraceMeridiens (xc, yc, mcs, areaid, grpid, idsize)

        implicit none

        integer mcs, idsize, i
        integer areaid(idsize), grpid(idsize), id
        real xc(mcs), yc(mcs)
        integer mapaci, mpisci

        include 'common.h'

        do i=1,idsize
           if (grpid(i).eq.1) id = areaid(i)
        enddo 

        if (opt_map.eq.1) then
         if (opt_high .eq. 0 ) then
           if ((mapaci(id).eq.1).and.(mcs.ge.2)) then
              call curved (xc,yc,mcs)
           endif 
         else
           if ((mpisci(id).eq.1).and.(mcs.ge.2)) then
              call curved (xc,yc,mcs)
           endif 
         endif
        else 
           call curved (xc,yc,mcs)
        endif 

        return 
        end
c
         subroutine TraceMeridian(xc,yc,mcs,areaid,grpid,idsize)
c Draw a curve along the the coordinates (xc,yc)(i=1,mcs). By default 
c the line is dotted. You can define other dash patterns with the 
c parameter dashid. Refer to the NCAR manual for valid parameters dashid. 
c The parameter maskid determinates wether the curves are drawn through 
c the whole area (maskid=0, default), over the ocean areas only (maskid=2) 
c or over land (maskid=1). Each curve is labelled by it's longitude or 
c latitude value. It is assumed that a map is to be labelled. Coordinate 
c values therefore must be included in the intervals -360.0<x<360.0 and
c -90.0<y<90.0, respectively. If these conditions are not met the line 
c is not labelled. The actual label is a string in the ranges '180W' to 
c '180E' or '90S' to '90N', depending on the average slope of the drawn 
c curve. For slopes in the interval [-1.0,1.0] labels of the form '90S' to 
c '90N' are used. Otherwise a meridian ('180W' to '180E') is labelled.
c The label is placed near the intersection of the curves with the perimeter 
c of the map. The character size may be controlled by cs_label (default 0.8).
c Ch. Dieterich Wed Nov 15 10:42:43 MET 1995
c based on subroutine TraceMeridiens (xc, yc, mcs, areaid, grpid, idsize)
        implicit none
        integer mcs, idsize, i
        integer areaid(idsize), grpid(idsize), id
        real xc(mcs), yc(mcs)
        integer mapaci, llab, lnblnk
        include 'common.h'
        include 'val_table.h'
        real x1w,x2w,x1u,x2u,rlon,rlon0,rlon1,rlon2,rxstep,
     *  y1w,y2w,y1u,y2u,rlat,rlat0,rlat1,rlat2,rystep,range
        integer ixstep,nxstep,newx,oldx,
     *  iystep,nystep,newy,oldy,ilab,nlab,ll,solid
        character*80 crlab
        character*80 clab
        character*80 cfmt
        real cs_label
	real dummy
      real    rl,rr,rb,rt, ur,ul,ut,ub
      common /special/ rl,rr,rb,rt, ur,ul,ut,ub

        integer dashid,maskid
        common /CTrace/ cs_label,dashid,maskid
c       data solid/65535/,range/720.0/,dashid/52428/
        data solid/65535/,range/720.0/,dashid/43690/
        save ixstep,nxstep,rxstep,rlon0,oldx,
     *  iystep,nystep,rystep,rlat0,oldy
	cs_label=0.8
c Retrieve the group id.
        do i=1,idsize
           if (grpid(i).eq.1) id = areaid(i)
        enddo
c Set up the dash pattern.
         call dashdb(dashid)
c        call dashdb(solid)
c Draw the curve.
        if (opt_grid .eq. 1 ) then
        if (opt_map.eq.1) then
           if (maskid.eq.0) then
              call curved (xc,yc,mcs)
           else if (mapaci(id).eq.maskid.and.(mcs.ge.2)) then
              call curved (xc,yc,mcs)
           endif
        else
           call curved (xc,yc,mcs)
        endif
        endif
c Reset to solid lines.
        call dashdb(solid)
c Get the perimeter of the map in world coordinates. 
c The calling routine MAPGRM has aligned world and user coordinates.
        call getset(x1w,x2w,y1w,y2w,x1u,x2u,y1u,y2u,ll)
c Compute the average slope of the curve...

        rlon=abs(xc(mcs)-xc(1))
        rlat=abs(yc(mcs)-yc(1))
        if(rlon.eq.0.0.and.rlat.eq.0.0)return
c Find the geographical coordinate of the points xc,yc
c ... get the coordinate in u/v plan
c ...   ul ... rl ... have been passed by common /special/
	x1u=ul + (xc(1) - rl)/(rr - rl)*(ur -ul)
	y1u=ub + (yc(1) - rb)/(rt - rb)*(ut -ub)
	x2u=ul + (xc(mcs) - rl)/(rr - rl)*(ur -ul)
	y2u=ub + (yc(mcs) - rb)/(rt - rb)*(ut -ub)
c ... perform inverse projection
	call maptri(x1u,y1u,rlat1,rlon1)
	call maptri(x2u,y2u,rlat2,rlon2)
      IF (rlat1 .eq. 1.e12 ) return
      IF (rlat2 .eq. 1.e12 ) return
      IF (rlon1 .eq. 1.e12 ) return
      IF (rlon2 .eq. 1.e12 ) return
c Disable clipping.
        call gsclip(0)
c ...and decide wether it is a meridian or not.
        if(rlon.eq.0.0)go to 100
        if(rlat/rlon.gt.1.0)go to 100
c It's not a meridian. 
c Compute a number proportional to the actual y-world coordinate.
c It should be an integer number to make sure that the statement 
c if(newy.eq.oldy)return works properly.
c The number of different latitudes recogniced and labelled by 
c this routine is int((y2w-y1w)*range).
        newy=nint(yc(1)*range)
c For the first call, get the grid spacing, the starting latitude
c and a positive or negative increment for latitudinal steps.
        if(nystep.eq.0)then
          call mpgetr('GR',rystep)
          if(yc(1).lt.0.5*(y2w-y1w)+y1w)then
            iystep=1
            rlat0=rlat1
          else
            iystep=-1
            rlat0=rlat2
          end if
          oldy=newy*10
        end if
c This y-position has already been labelled.
        if(newy.eq.oldy)return
c Step ahead.
        oldy=newy
        rlat=rlat0+nystep*rystep
        nystep=nystep+iystep
c This is not a valid latitude value.
        if(abs(rlat).gt.90.0)return
c Labels for the northern or for the southern hemisphere?
        if(rlat.ge.0.0)then
          clab(4:4)='N'
        else
          clab(4:4)='S'
        end if
c Set up the labels.
c Set up the labels.
        ilab=nint(rlat)
!        cfmt='( '//int_table(ICLR_YAXIS)%format//',1x,a)'
         cfmt='( '//int_table(ICLR_YAXIS)%format//',a)'
C   look for integer of real format
        if ( index (cfmt,'I') .EQ. 0 .AND. index (cfmt,'i') .EQ. 0 ) then
        write(crlab,cfmt) rlat, clab(4:4)
        else
        write(crlab,cfmt) nint(rlat), clab(4:4)
        endif
        call FilterBlanks(crlab)
        llab = lnblnk(crlab)
 
C       clab=crlab(1:lnblnk(crlab))
c Plot the characters.
        if (opt_laby .ne. 0 ) then
        call plchhq(x1w-0.01,yc(1),crlab(1:llab),-abs(cs_label),0.0,1.0)
       if (opt_grid .eq. 0) call line(x1w,yc(1),x1w+0.01,yc(1))
        
        endif
        return
c It's a meridian. 
c Compute a number proportional to the actual x-world coordinate.
c It should be an integer number to make sure that the statement 
c if(newx.eq.oldx)return works properly.
c The number of different longitudes recogniced and labelled by 
c this routine is int((x2w-x1w)*range).
100     newx=int(xc(1)*range)
c For the first call, get the grid spacing, the starting longitude
c and a positive or negative increment for longitudinal steps.
        if(nxstep.eq.0)then
          call mpgetr('GR',rxstep)
          if(xc(1).lt.0.5*(x2w-x1w)+x1w)then
            ixstep=1
            rlon0=rlon1
          else
            ixstep=-1
            rlon0=rlon2
          end if
          oldx=newx*10
        end if
c This x-position has already been labelled.
        if(newx.eq.oldx)return
c Step ahead.
        oldx=newx
        rlon=rlon0+nxstep*rxstep
        nxstep=nxstep+ixstep
c This is not a valid longitude value.
        if(abs(rlon).gt.360.0)return
        rlon=rlon+360.0
        rlon=amod(rlon,360.0)
c Labels for 'West of Greenwich' or for 'East of Greenwich'?
        if(nint(rlon).ne.nint(amod(rlon,180.0)))then
          rlon=180.0-amod(rlon,180.0)
          clab(4:4)='W'
        else
          clab(4:4)='E'
        end if
c Set up the labels.
        ilab=nint(rlon)
!        cfmt='( '//int_table(ICLR_XAXIS)%format//',1x,a)'
        cfmt='( '//int_table(ICLR_XAXIS)%format//',a)'
C   look for integer of real format
        if ( index (cfmt,'I') .EQ. 0 .AND. index (cfmt,'i') .EQ. 0 ) then
        write(crlab,cfmt) rlon, clab(4:4)
        else
        write(crlab,cfmt) nint(rlon), clab(4:4)
        endif
        call FilterBlanks(crlab)
        llab = lnblnk(crlab)
        
C       clab=crlab(1:lnblnk(crlab))
c Plot the characters.
	if (opt_labx .ne. 0 ) then
        call plchhq(xc(1),y1w-0.02,crlab(1:llab),-abs(cs_label),0.0,0.0)
       if (opt_grid .eq. 0) call line(xc(1),y1w,xc(1),y1w+0.01)
        endif
        return
        end


c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine mark (x, y, m, n, istp)

      implicit none

      
      integer m,n,istp
      real x(m), y(n)
      integer i, j
      real x_map, y_map
      
      call gsmksc(.3)

      do  i=1,m,istp
         do  j=1,n,istp
            call maptra (y(j), x(i), x_map, y_map)
            if (x_map.ne.1.e12) then
               call points (x_map, y_map, 1, -4, 0)
            endif 
         enddo 
      enddo 

      return
      end
