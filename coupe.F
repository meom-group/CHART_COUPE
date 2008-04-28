c --------------------------------------------------------------------
c 
c Programme      :  COUPE
c Fichier        :  coupe.f
c 
c Auteur         :  Eric Brown pour la version initiale
c                :  Jean-Marc Molines pour les remaniement ulterieurs
c
c Description    :  Fichier principal du programme COUPE.
c                   Contient les differentes etapes de creation d'un 
c                   dessin.
c 
c ---------------------------------------------------------------------

      program coupe

      implicit none

c     declaration des variables ---------------------------------------

      include 'common.h'
      include 'val_table.h'
      include 'color.h'
      include 'mapncar.h'

      integer couche(NA),couche_max, tstep(Ntime), tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/

      integer ncol

      integer iargc, lnblnk
      integer narg
      integer t
      integer map_flag

      real    cntdata (NXX,NYY)
      real    clrdata (NXX,NYY)

      real    limit(NBOXMAX+1)
      real xover(NOVERPTS), yover(NOVERPTS)
      real xoverm(NOVERPTS), yoverm(NOVERPTS)
      integer nover, noverm

      real    plot_coord(4)
      real    rr1,rr2

      TYPE( bimgfile ) bimgclr, bimgcnt, bimgvec(3)
      TYPE( bimgfile ) bimgzlevel
      
C     save /str/
C     data strcount/0/

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      character*80 cmd        
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save  /layout/
	integer ifrst
	data ifrst /0/


c     declaration des variables -- fin ---------------------------------

      
	include 'coupe_version.h'
      strcount = 0

      plot_coord(1) = 1.
      plot_coord(2) = float(NXX)
      plot_coord(3) = 1.
      plot_coord(4) = float(NYY)


c     Nombre d'arguments sur la ligne de commande.
c     Si aucun argument n'est donne, imprimer les options possibles
c     et sortir.

      narg = iargc()

      if (narg.eq.0) then
         call PrintOptions ()
         stop
      endif

	opt_chart = 0
	opt_coupe = 1
c     configuration par defaut et lecture des arguments de la ligne de
c     commande

      call InitValTable()
      call SetDefaults()
      call SetCoupeDefaults()
C     call CdfInit

c     Si il n'y a qu'un seul argument, le considerer comme un fichier
c     de donnees et chercher une configuration par defaut.

      if (narg.eq.1) then
         call getarg (1,f_icoddata)
         call ReadIcodConfig()
      else 
         call ReadArgs (narg)
      endif 

      if (opt_lev.eq.0) then
         call SetLevels ()
      endif 


      call OpenNCAR_GKS()

      call OpenAndVerifyFiles (bimgclr, bimgzlevel, bimgcnt, bimgvec)
      call CheckOptions (bimgclr, bimgcnt, bimgvec)
      call SetValTable (bimgclr, bimgcnt, bimgvec)

      if (opt_zoom.eq.0) then
         print *,'erreur : veuillez specifier les coordonnees;'
         print *,'         de la coupe (-pts)'
         stop
      endif 

      if (opt_pal.eq.1) then
         call ReadPaletteFile (filepal,ncol, opt_reverse)
      else
         call DefaultPalette (ncol, opt_reverse,opt_pal)
      endif

      call CoupeInitNCAR(bimgclr,bimgcnt)

      if (opt_map.eq.1) then
         call gflas1(6)
         call CreateBathy ()
         call gflas2
      endif 

            if (opt_overdata.eq.1) then
               print *,'Lecture des points Overlay...'
               call OverReadData(xover,yover,nover)
               print *,'      Done: ',nover,' points lus.'
            endif

            if (opt_overmark.eq.1) then
               print *,'Lecture des points Overmark...'
               call OverMarkReadData(xoverm,yoverm,noverm)
               print *,'      Done: ',noverm,' points lus.'
            endif


c boucle sur les "time frame"
      do t = 1, tstep_max

         if (tstep(t).eq.1) then
C     call arinam(iama,sz_iama)
C     call aredam (iama,xperim,yperim,5,15,30,31)

c     Calcul des couleurs

            if (opt_clrdata.eq.1) then
	     print *,' CLRDATA...'
c             if (opt_clrgrid .ne. 0 ) print *,'      Grille irreguliere type:', opt_clrgrid
              if (bimgclr%grid .ne. 0 ) print *,'      Grille irreguliere type:', bimgclr%grid
             bimgclr%lclr=.true. ; bimgclr%lcnt=.false.
             call CalculateCutPlane (clrdata, bimgclr,bimgzlevel, t, cl_dim)
	    endif

	    if (opt_color.eq.1) then
                  if (opt_clrmark .EQ. 1 ) THEN
                   call ClrGetMark()
                  endif
               call ClrGetLimits (clrdata(1,1),bimgclr,limit,ncol)
               
               if (opt_clrlout.eq.1) then
                  call WriteLimits (f_clrlout, limit, ncol+1)
               endif 
         
               call ColorCoupe (clrdata, bimgclr, ncol, limit ) !,ncol)
               call SetClrCNESday (bimgclr%time)
            endif 

c     Calcul des isocontours

	    if (opt_sigma.eq.0) then
	       map_flag=0
	    else
	       map_flag=5  ! dans ce cas on utilisera le cpmpxy inspires de SPEM 
	    endif


            if (opt_contours.eq.1) then
	     print *,' CNTDATA...'
c             if (opt_contgrid .ne. 0 ) print *,'      Grille irreguliere type:', opt_contgrid
              if (bimgcnt%grid .ne. 0 ) print *,'      Grille irreguliere type:', bimgcnt%grid
             bimgcnt%lcnt=.true. ; bimgcnt%lclr=.false.
             call CalculateCutPlane (cntdata, bimgcnt,bimgzlevel, t, ct_dim)

      if(opt_marg.eq.0) then
          call set(x1pos,x2pos,y1pos,y2pos,1.,float(NXX),1.,float(NYY),1)
      else 
          if(map_coord(3).eq.map_coord(4)) then 
c coupe zonale 
        rr2=1+(map_marg(2)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(NXX-1)
        rr1=1+(map_marg(1)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(NXX-1)
         elseif (map_coord(1).eq.map_coord(2)) then         
c coupe meridienne 
        rr2=1+(map_marg(4)-map_coord(3))/ 
     |   (map_coord(4)-map_coord(3))*(NXX-1) 
        rr1=1+(map_marg(3)-map_coord(3))/ 
     |   (map_coord(4)-map_coord(3))*(NXX-1) 
         else 
         print *,' ERROR: l''option -marg incompatible avec coupes obliques'
	 stop
         endif 
      call set(x1pos,x2pos,y1pos,y2pos,rr1,rr2,1.,float(NYY),1)
        endif

               call TraceContours (cntdata,bimgcnt,plot_coord,map_flag)
C     call gsclip (0)
C              call gflas3(3)
            endif 

            call UpdateValTable (tstep, 1,1,1, bimgclr, bimgcnt, bimgvec)


            call AjouteTexte ()
C Bug fix 09/10/2002 JMM
             if (opt_contours .EQ. 1 ) call gflas3(3)
c           call AjouteGrille ()

            if (opt_sigmatr.eq.1) then
	       if(ifrst.eq.0) then
	        ifrst=1
	        call gflas1(89)
	        call PlotSigmaLevel(bimgzlevel%ncou)
	        call gflas2
	       endif
            endif
      
            if (opt_map.eq.1) then
               call gflas3(6)
            endif 

            call AjouteGrille ()

	    if (opt_sigmatr.eq.1) then
	       call gflas3(89)
	    endif
         

           if (opt_overdata.eq.1) then
              call OverTrace(xover,yover,nover)
           endif
           if (opt_overmark.eq.1) then
              call OverMarkTrace(xoverm,yoverm,noverm)
           endif
            if (opt_overdata.eq.1) call gflas3(55)
            if (opt_overmark.eq.1) call gflas3(56)
            call frame

         endif 
      enddo 

      call gclwk(9)
      call clsgks
#ifdef linux
c
c     LINUX ONLY:
c ---------------

c     nettoyage des fichiers intermediaires

      call system ('/bin/rm -f *.tmpclrnk')
      call system ('/bin/rm -f *.tmpcntlnk')                         
#endif

C      call system ('/bin/rm GNFB*')

      print *,'Programme coupe termine normalement.'
      call  flush(6)
      
      end


c ---------------------------------------------------------------------
c 
c Nom         :  CoupeInitNCAR
c 
c Description :  Initialise differents parametres de NCAR
c 
c ---------------------------------------------------------------------

      subroutine CoupeInitNCAR(bimgclr,bimgcnt)

      implicit none

      include 'common.h'
      include 'mapncar.h'

      integer i,j
      TYPE( bimgfile ) bimgclr, bimgcnt
      integer ip,kp, imin, jmin, iquadran
      real dx, dy
        real xn,yn, xx,yy, x(3), y(3), dM(3)
        real xtmp,ytmp,dtmp
        integer iitmp, jjtmp
      real aire2
      real an,a1,a2
      real bn,b1,b2
      real cn,c1,c2

      integer ii0(3,NXX), jj0(3,NXX)
      real alfan(NXX), alfa1(NXX), alfa2(NXX)
      common /tordu/ alfan,alfa1,alfa2,ii0,jj0

      real xperim(5),yperim(5)
      data xperim /0.0, 1.0, 1.0, 0.0, 0.0/
      data yperim /0.0, 0.0, 1.0, 1.0, 0.0/

      integer iasf(13)
      data iasf / 13*1 /

      call gsclip(1)
      call gsasf (iasf)


c     couleur et qualite du texte de toute la carte

      call pcsetc ('FC','|')       ! set control character to |
      call pcsetc ('FN', c_font)   ! font
      call pcseti ('CC(0)',1)      ! character color index
      call gaseti ('LTY',1)        ! use plcchq to draw labels
     
      call gsplci(1)               ! set polyline color index
      call gsfais(1)               ! set polyline fill interior style

      call arinam(iama,sz_iama)
      call aredam (iama,xperim,yperim,5,15,30,31)            
c
c    Prepare les donnees d'interpolation de la coupe dans le cas gridxy
c
cif (opt_clrgrid.eq.3) then
      if (bimgclr%grid .eq. 3 .or. bimgcnt%grid .eq. 3 ) then

c   Point de depart de la coupe
	  imin=0
	  jmin=0
	  call NearestPoint(map_coord(1),map_coord(3),imin,jmin,iquadran)
C contrairement a l'option classique, meme le 1er pt est interpolle.
c On part donc exactement des coordonnees qui ont ete specifiees.
c
        dx=(map_coord(2) - map_coord(1))/(NXX-1)
        dy=(map_coord(4) - map_coord(3))/(NXX-1)  ! NXX oui oui oui !!!

	  do i=1,NXX 
        xx=map_coord(1) + (i -1)*dx
        yy=map_coord(3) + (i -1)*dy

	  call NearestPoint(xx,yy,imin,jmin,iquadran)
	xn=xygr(imin,jmin,1)
	yn=xygr(imin,jmin,2)

        if (iquadran.eq.1) then
        x(1)=xygr(imin+1, jmin,1)
        y(1)=xygr(imin+1, jmin,2)
	ii0(1,i) = imin+1
	jj0(1,i) = jmin

        x(2)=xygr(imin, jmin+1,1)
        y(2)=xygr(imin, jmin+1,2)
	ii0(2,i) = imin
	jj0(2,i) = jmin +1
        endif

        if (iquadran.eq.2) then
        x(1)=xygr(imin, jmin+1,1)
        y(1)=xygr(imin, jmin+1,2)
	ii0(1,i) = imin
	jj0(1,i) = jmin + 1

        x(2)=xygr(imin-1, jmin+1,1)
        y(2)=xygr(imin-1, jmin+1,2)
	ii0(2,i) = imin -1
	jj0(2,i) = jmin +1
 
        endif

        if (iquadran.eq.3) then
        x(1)=xygr(imin-1, jmin+1,1)
        y(1)=xygr(imin-1, jmin+1,2)
        ii0(1,i)=imin -1
        jj0(1,i)=jmin +1

        x(2)=xygr(imin-1, jmin,1)
        y(2)=xygr(imin-1, jmin,2)
        ii0(2,i) = imin -1
        jj0(2,i) = jmin
        endif
 
        if (iquadran.eq.4) then
        x(1)=xygr(imin-1, jmin,1)
        y(1)=xygr(imin-1, jmin,2)
	ii0(1,i) = imin -1
	jj0(1,i) = jmin
 
        x(2)=xygr(imin, jmin-1,1)
        y(2)=xygr(imin, jmin-1,2)
	ii0(2,i) = imin
        jj0(2,i) = jmin-1
        endif

        if (iquadran.eq.5) then
        x(1)=xygr(imin, jmin-1,1)
        y(1)=xygr(imin, jmin-1,2)
        ii0(1,i) = imin 
        jj0(1,i) = jmin -1

        x(2)=xygr(imin+1, jmin-1,1)
        y(2)=xygr(imin+1, jmin-1,2)
        ii0(2,i) = imin+1
        jj0(2,i) = jmin-1
        endif
 
        if (iquadran.eq.6) then
        x(1)=xygr(imin+1, jmin-1,1)
        y(1)=xygr(imin+1, jmin-1,2)
	ii0(1,i)=imin+1
	jj0(1,i)=jmin-1
 
        x(2)=xygr(imin+1, jmin,1)
        y(2)=xygr(imin+1, jmin,2)
	ii0(2,i)=imin+1
	jj0(2,i)=jmin 
        endif
 
C       dM(1) = sqrt( (xx-x(1))**2 + (yy-y(1))**2)
C       dM(2) = sqrt( (xx-x(2))**2 + (yy-y(2))**2)
C       dM(3) = sqrt( (xx-x(3))**2 + (yy-y(3))**2)
C       print *, dM(1), dM(2), dM(3)
c
c  Trie des distance en ordre croissant pour ne retenir QUE 
c  les 2 points les plus proches

C        kp=1
C10      kp = 3*kp +1
C        if (kp.le.3) goto 10
C20      kp = kp/3
C        if (kp.ge.1) then
C          do 50 ip=kp+1,3
C           dtmp = dM(ip)
C           xtmp = x(ip)
C           ytmp = y(ip)
C	   iitmp = ii0(ip,i)
C	   jjtmp = jj0(ip,i)
C           j=ip
C30         if (dM(j-kp).le.dtmp) goto 40
C           dM(j)=dM(j-kp)
C           x(j) = x(j-kp)
C           y(j) = y(j-kp)
C	   ii0(j,i)=ii0(j-kp,i)
C	   jj0(j,i)=jj0(j-kp,i)
C           j= j-kp
C           if (j.gt.kp) goto 30
C40        dM(j)=dtmp
C          x(j) = xtmp
C          y(j) = ytmp
C	  ii0(j,i) = iitmp
C          jj0(j,i) = jjtmp
C50        continue
C        goto 20
C        endif
C une fois trie on garde imin, jmin dans ii0(3), jj0(3) 
	  ii0(3,i)=imin
	  jj0(3,i)=jmin

C   l'interpolation se fera avec
C   vv = alfan.vn + alfa1.v1 + alfa2.v2 (vn est la valeur la plus proche)
c
C       PRINT '(a,I2,8f10.2)', ' N, 1 , 2, P ', iquadran, xn,yn, x(1), y(1), x(2),y(2), xx,yy
C       PRINT '(5f10.2)',xygr(imin-1,jmin,1),xygr(imin,jmin,1), xygr(imin+1,jmin,1),xn, xx
C       PRINT '(5f10.2)',xygr(imin,jmin-1,2),xygr(imin,jmin,2), xygr(imin,jmin+1,2),yn, yy
        aire2=xn*(y(1)-y(2)) + x(1)*(y(2)-yn) + x(2)*(yn-y(1))
 
        an=(x(1)*y(2)-x(2)*y(1))
        a1=(x(2)*yn-xn*y(2))
        a2=(xn*y(1)-x(1)*yn)
 
        bn=(y(1)-y(2))
        b1=(y(2)-yn)
        b2=(yn-y(1))
 
        cn=-(x(1)-x(2))
        c1=-(x(2)-xn)
        c2=-(xn-x(1))
 
C       alfan(i)=abs(( an + bn*xx + cn*yy )/aire2)
C       alfa1(i)=abs(( a1 + b1*xx + c1*yy )/aire2)
C       alfa2(i)=abs(( a2 + b2*xx + c2*yy )/aire2)

C       alfan(i)=(( an + bn*xx + cn*yy )/aire2)
C       alfa1(i)=(( a1 + b1*xx + c1*yy )/aire2)
C       alfa2(i)=(( a2 + b2*xx + c2*yy )/aire2)
        alfa1(i)=( xx*(y(2)- yn  ) + x(2)*(yn   - yy ) + xn  * (yy   - y(2)) ) /aire2
        alfa2(i)=( xx*(yn  - y(1)) + x(1)*(yy   - yn ) + xn  * (y(1) - yy  ) ) /aire2
        alfan(i)=( xx*(y(1)-y(2) ) + x(1)*(y(2) - yy ) + x(2)* (yy   - y(1)) ) /aire2
C       print *, alfan(i), alfa1(i), alfa2(i), alfan(i)+ alfa1(i)+ alfa2(i)
C       STOP

	if (iquadran.eq.7) then
         PRINT *,' Iquadran = 7 pour i=',i
	alfan(i)=1.
	alfa1(i)=0.
	alfa2(i)=0.
        endif

        enddo  ! fin de la boucle i 2,NXX
      endif ! gridxy

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  CalculateCutPlane
c 
c Parametres  :  cut_plane - le tableau a remplir
c                bimginfo  - la structure d'information sur le fichier
c                            bimg.
c
c Description :  Cette fonction lit chaque plan du fichier et interpole
c                entre les points de grille pour obtenir NXX points de
c                resolution horizontale, puis elle interpole 
c                verticalement, pour obtenir NYY points sur la 
c                verticale.
c
c                utilise les variables globales map_marg(1 a 4)
c 
c ---------------------------------------------------------------------

      subroutine CalculateCutPlane 
     &            (cut_plane, bimginfo, bimgzlevel,t_step,tmpdim)

      implicit none

      include 'common.h'
      include 'sigma.h'

      real cut_plane(NXX,NYY)
      integer t_step,tmpdim,ifrst
      TYPE( bimgfile ) bimginfo, bimgzlevel

      integer couche(NA),couche_max, tstep(Ntime), tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/

      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist
      save   /cut/

      real    x_interp(NXX,NA) 
      real    work(NXX,NYY)
      real    layer(NA)

      integer i,j,k,p, layer_cnt
      integer isrc,jsrc
	logical good_point

      integer ii0(3,NXX), jj0(3,NXX)
      real alfan(NXX), alfa1(NXX), alfa2(NXX)
      common /tordu/ alfan,alfa1,alfa2,ii0,jj0

      real    prof, delta_prof  ! , h(NA)
      real    dx, dy
      real    xinc, yinc
      real    fracx, fracy
      real    fracz
      real    spv, wgt
	data ifrst /0/

c     initialisation des tableaux a spval

c**      print *,'initialisation des tableaux'
	if (bimginfo%spval0) then
      spv = defspval
        else
      spv = bimginfo%spval
	endif

      do k=1,bimginfo%ncou
         do i=1,NXX
            x_interp(i,k) = spv
         enddo
      enddo

      do j=1,NYY
         do i=1,NXX
            cut_plane(i,j) = spv
         enddo
      enddo


c     calcul des points sur chaque couche -----------------------------

      layer_cnt = 0

      do k = 1, bimginfo%ncou
         if (couche(k).eq.1) then
            layer_cnt = layer_cnt + 1

c     Tableau des profondeurs. 
            layer(layer_cnt) = -bimginfo%depth(k)
            call BimgReadData (work,bimginfo,map_coord,t_step,k,tmpdim)
	     if (bimginfo%spval0) bimginfo%spval=defspval

	   if (bimginfo%grid .ne. 3 ) then 
            dx = float(bimginfo%nxdata-1) / float(NXX)
            dy = float(bimginfo%nydata-1) / float(NXX) ! oui, oui, NXX
            xinc = 0.0
            yinc = 0.0

            do i = 1, NXX
               fracx = xinc - aint(xinc)
               fracy = yinc - aint(yinc)
            
               isrc = 1 + int(xinc)
               jsrc = 1 + int(yinc)

	    if (fracx.eq.0) then
 
               if ((work(isrc,jsrc).eq.spv).or.
     .             (work(isrc,jsrc+1).eq.spv)) then
                  x_interp(i,layer_cnt) = spv
               else
                  x_interp(i,layer_cnt) = 
     .                work (isrc,jsrc+1) * fracy +
     .                work (isrc,jsrc) *  (1.-fracy)
 
               endif
	    else if (fracy.eq.0) then
 
               if ((work(isrc,jsrc).eq.spv).or.
     .             (work(isrc+1,jsrc).eq.spv)) then
                  x_interp(i,layer_cnt) = spv
               else
                  x_interp(i,layer_cnt) =
     .                work (isrc+1,jsrc) * fracx  +
     .                work (isrc,jsrc) * (1.-fracx) 
 
               endif
	   else

               if ((work(isrc,jsrc).eq.spv).or.
     .                   (work(isrc+1,jsrc).eq.spv).or.
     .                   (work(isrc,jsrc+1).eq.spv).or.
     .                   (work(isrc+1,jsrc+1).eq.spv)) then
                  x_interp(i,layer_cnt) = spv
               else
                  x_interp(i,layer_cnt) =
     .                work (isrc+1,jsrc+1) * fracx * fracy +
     .                work (isrc,jsrc+1) * (1.-fracx) * fracy +
     .                work (isrc+1,jsrc) * fracx * (1.-fracy) +
     .                work (isrc,jsrc) * (1.-fracx) * (1.-fracy)

               endif 
	   endif
            
               xinc = xinc + dx
               yinc = yinc + dy
            enddo 

	  else
c Cas de la grille gridxy, les coeff d'interpolation sont calcules dans CoupeInitNCAR
c Attention aux valeurs speciales pour l'interpolation a ameliorer pour approcher le bord
c 
	do i=1,NXX
	good_point=.true.
	good_point=good_point .AND. (work(ii0(3,i),jj0(3,i)) .NE. bimginfo%spval)
	good_point=good_point .AND. (work(ii0(1,i),jj0(1,i)) .NE. bimginfo%spval)
	good_point=good_point .AND. (work(ii0(2,i),jj0(2,i)) .NE. bimginfo%spval)

	if (good_point) then
C   wgt=alfan(i) + alfa1(i)+alfa2(i)
 	   x_interp(i,layer_cnt) = ( alfan(i)*work(ii0(3,i),jj0(3,i)) +
     .                             alfa1(i)*work(ii0(1,i),jj0(1,i)) +
     .                             alfa2(i)*work(ii0(2,i),jj0(2,i)) ) 
C        if ( k .EQ. 10 ) print *, jj0(3,i), jj0(1,i), jj0(2,i),  x_interp(i,layer_cnt)
C        if ( k .EQ. 10 ) print *, ii0(3,i), ii0(1,i), ii0(2,i),  x_interp(i,layer_cnt)
	else
	   x_interp(i,layer_cnt) = bimginfo%spval
	endif

	enddo

	  endif  ! gridxy
         endif   ! layer
C        If find a layer with all data = spval, then skip remaining layers
         IF ( SUM(x_interp(:,layer_cnt)) == NXX*bimginfo%spval ) EXIT
      enddo 

c     Interpolation verticale -----------------------------------------

c      if (opt_pmin.eq.0) prof_min = layer(1)
c      if (opt_pmax.eq.0) prof_max = layer(layer_cnt)

	if (opt_sigma.eq.0) then
      delta_prof = (prof_max - prof_min) / float(NYY)
      prof = prof_min 
	else
      delta_prof = (layer(layer_cnt) - layer(1)) / float(NYY)
      prof = layer(1) 
	endif
      p = 1
      
      do k = NYY, 1,-1

         if ((prof.lt.layer(1)).or.(prof.gt.layer(layer_cnt))) then
            do i = 1,NXX
               cut_plane (i,k) = spv
            enddo 
         
         else 
            p = 1
            do while ((prof.lt.layer(p)).or.(prof.gt.layer(p+1)).and.
     .                (p.lt.layer_cnt))
               p = p+1
            enddo 

            if (p.lt.layer_cnt) then
               fracz = (prof - layer(p)) / (layer(p+1) - layer(p))
            endif 
 
            do i = 1,NXX
               if ((x_interp(i,p).eq.spv).or.
     .             (x_interp(i,p+1).eq.spv).or.
     .             (p.eq.layer_cnt)) then
                  cut_plane (i,k) = spv
               else 
                  cut_plane (i,k) = (1.0-fracz) * x_interp(i,p)
     .                            + fracz * x_interp (i,p+1)
               endif 
            enddo 
         endif 

         prof = prof + delta_prof
      enddo 

      bimginfo%nxdata = NXX
      bimginfo%nydata = NYY

	if (opt_sigma.eq.1.and.ifrst.eq.0) then
c     calcul des  zlevel  sur chaque couche -----------------------------
	ifrst=1
	spv=bimgzlevel%spval
       layer_cnt = 0
         print *,'ZLEVEL...'

      do k = 1, bimgzlevel%ncou
         if (couche(k).eq.1) then
            layer_cnt = layer_cnt + 1

c     Tableau des profondeurs. 

            layer(layer_cnt) = -bimgzlevel%depth(k)

            bimgzlevel%lclr=.false. ; bimgzlevel%lcnt=.false.
            call BimgReadData (work,bimgzlevel,map_coord,1,k,1)

	   if (bimgzlevel%grid .ne. 3 ) then
            dx = float(bimgzlevel%nxdata-1) / float(NXX)
            dy = float(bimgzlevel%nydata-1) / float(NXX) ! oui, oui, NXX
            xinc = 0.0
            yinc = 0.0

            do i = 1, NXX
               fracx = xinc - aint(xinc)
               fracy = yinc - aint(yinc)
            
               isrc = 1 + int(xinc)
               jsrc = 1 + int(yinc)
	    if (fracx.eq.0) then

               if ((work(isrc,jsrc).eq.spv).or.
     .             (work(isrc,jsrc+1).eq.spv)) then
                  zs(i,layer_cnt) = spv
               else
                  zs(i,layer_cnt) =
     .                work (isrc,jsrc+1) *  fracy +
     .                work (isrc,jsrc) * (1.-fracy)

               endif 
	     elseif (fracy.eq.0) then

               if ((work(isrc,jsrc).eq.spv).or.
     .             (work(isrc+1,jsrc).eq.spv)) then
                  zs(i,layer_cnt) = spv
               else
                  zs(i,layer_cnt) =
     .                work (isrc+1,jsrc) * fracx  +
     .                work (isrc,jsrc) * (1.-fracx) 

               endif 
	     else

               if ((work(isrc,jsrc).eq.spv).or.
     .                   (work(isrc+1,jsrc).eq.spv).or.
     .                   (work(isrc,jsrc+1).eq.spv).or.
     .                   (work(isrc+1,jsrc+1).eq.spv)) then
                  zs(i,layer_cnt) = spv
               else
                  zs(i,layer_cnt) =
     .                work (isrc+1,jsrc+1) * fracx * fracy +
     .                work (isrc,jsrc+1) * (1.-fracx) * fracy +
     .                work (isrc+1,jsrc) * fracx * (1.-fracy) +
     .                work (isrc,jsrc) * (1.-fracx) * (1.-fracy)

               endif 
	    endif
            
               xinc = xinc + dx
               yinc = yinc + dy
            enddo 

         else

c Cas de la grille gridxy, les coeff d'interpolation sont calcules dans CoupeInitNCAR
c Attention aux valeurs speciales pour l'interpolation a ameliorer pour approcher le bord
c 
	do i=1,NXX
	good_point=.true.
	good_point=good_point .AND. (work(ii0(3,i),jj0(3,i)) .NE. bimginfo%spval)
	good_point=good_point .AND. (work(ii0(1,i),jj0(1,i)) .NE. bimginfo%spval)
	good_point=good_point .AND. (work(ii0(2,i),jj0(2,i)) .NE. bimginfo%spval)

	if (good_point) then
 	   zs(i,layer_cnt) = alfan(i)*work(ii0(3,i),jj0(3,i)) +
     .                             alfa1(i)*work(ii0(1,i),jj0(1,i)) +
     .                             alfa2(i)*work(ii0(2,i),jj0(2,i)) 
	else
	   zs(i,layer_cnt) = spv
	endif

	enddo

	  endif  ! gridxy
         endif 
      enddo 
c     Interpolation verticale -----------------------------------------

      delta_prof = (layer(layer_cnt) - layer(1)) / float(NYY)
      prof = layer(1) 

      p = 1
      
      do k = NYY, 1,-1

         if ((prof.lt.layer(1)).or.(prof.gt.layer(layer_cnt))) then
            do i = 1,NXX
               ys (i,k) = spv
            enddo 
         
         else 
            p = 1
            do while ((prof.lt.layer(p)).or.(prof.gt.layer(p+1)).and.
     .                (p.lt.layer_cnt))
               p = p+1
            enddo 

            if (p.lt.layer_cnt) then
               fracz = (prof - layer(p)) / (layer(p+1) - layer(p))
            endif 
 
            do i = 1,NXX
               if ((zs(i,p).eq.spv).or.
     .             (zs(i,p+1).eq.spv).or.
     .             (p.eq.layer_cnt)) then
                  ys (i,k) = spv
               else 
                  ys (i,k) = (1.0-fracz) * zs(i,p)
     .                            + fracz * zs (i,p+1)
               endif 
            enddo 
         endif 
	       ys(NXX+1,k)=ys(NXX,k)

         prof = prof + delta_prof
      enddo 

	endif

      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  CreateBathy
c Date de creation :  2 decembre 1993
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine CreateBathy ()

      implicit none

      include 'common.h'
      include 'mapncar.h'

      TYPE( bimgfile ) bathyinfo

      real    bathy(NXX,NYY)
      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist
      save   /cut/

      real    dx,dy,xinc,yinc,fracx,fracy
      integer isrc,jsrc,i,k,j
      integer value
      integer*4 state
      integer iytick

      real bat_x(NXX+3), bat_y(NXX+3)
      real spv

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      real rr1, rr2
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save  /layout/

      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw

      common  /trace/xstep, ystep, zstep, kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw
      save    /trace/

      integer ii0(3,NXX), jj0(3,NXX)
      real alfan(NXX), alfa1(NXX), alfa2(NXX)
      common /tordu/ alfan,alfa1,alfa2,ii0,jj0
	logical good_point

      real    rl, rr, rb, rt, ul, ur, ub, ut
      integer l

      external colbathy
      external drwbathy
      integer  BimgOpenFile 

      PRINT *, 'Reading bathy file for coupe'

      open (20,file=filebat,iostat=state,status='old')
      if (state.eq.0) then
         close (20)
         value = 0
      else
         call getenv('BIMG_BATHY', filebat)

         open (20,file=filebat,iostat=state,status='old')
         if (state.eq.0) then
            close (20)
            value = 0
         else
c**            print *, 'pas de fichier de bathymetrie specifie'
            value = 1
         endif 
      endif 

      
      if (value.eq.0) then
	 bathyinfo%fname=filebat
         PRINT *,' bimgOpenfile'
         bathyinfo%modifier='none'
         bathyinfo%varname='Bathymetry'
         bathyinfo%f_unit = BimgOpenFile (bathyinfo)
         PRINT *,' bimgReafHeader'
         call BimgReadHeader (bathyinfo)

c     option grille

         if (opt_batgrid.ne.0) then
            call ReadGrid (bathyinfo, f_batgrid)
         endif 


	 print *,'BATHYMETRY'
         bathyinfo%lclr=.false. ; bathyinfo%lcnt=.false.
         call BimgReadData (bathy,bathyinfo,map_coord,1,1,1)
         close (20)
c
c Force les bathy a etre negatives
c
	do i=1,bathyinfo%nxdata
	do j=1,bathyinfo%nydata
	 if ( bathy(i,j).ne.bathyinfo%spval) then
           bathy(i,j)=-abs(bathy(i,j))
         endif
        enddo
        enddo

         spv = bathyinfo%spval

	if (bathyinfo%grid .ne. 3 ) then
         dx = float(bathyinfo%nxdata-1) / float(NXX)
         dy = float(bathyinfo%nydata-1) / float(NXX) ! oui, oui, NXX


            
         xinc = 0.0
         yinc = 0.0
         k = 1
         
         do i = 1, NXX
            fracx = xinc - aint(xinc)
            fracy = yinc - aint(yinc)
         
            isrc = 1 + int(xinc)
            jsrc = 1 + int(yinc)

            bat_x(i) = i

c     on calcule le pixel seulement si les points
c     entre lesquels il se trouve sont tous differents
c     de spvalx. Si on est sur la frontiere, on ne
c     tient pas compte de i+1 ou j+1 selon le cas
c     car ces pixels seront hors du tableau
	  if  (fracx.eq.0) then

            if ((bathy(isrc,jsrc).eq.spv).or.
     .         ((bathy(isrc,jsrc+1).eq.spv).and.
     .          (jsrc.ne.NYY))) then
               bat_y(i) = spv
            else
               bat_y(i) = -(bathy (isrc,jsrc+1) * fracy +
     .                   bathy (isrc,jsrc) * (1.-fracy))
            endif 

	 else if (fracy.eq.0) then
            if ((bathy(isrc,jsrc).eq.spv).or.
     .         ((bathy(isrc+1,jsrc).eq.spv).and.
     .          (isrc.ne.NXX))) then
               bat_y(i) = spv
            else
               bat_y(i) = -(bathy (isrc+1,jsrc) * fracx  +
     .                   bathy (isrc,jsrc) * (1.-fracx))
            endif 
	 else
            if ((bathy(isrc,jsrc).eq.spv).or.
     .         ((bathy(isrc+1,jsrc).eq.spv).and.
     .          (isrc.ne.NXX)).or.
     .         ((bathy(isrc,jsrc+1).eq.spv).and.
     .          (jsrc.ne.NYY)).or.
     .         ((bathy(isrc+1,jsrc+1).eq.spv).and.
     .          (jsrc.ne.NYY).and.
     .          (isrc.ne.NXX))) then
               bat_y(i) = spv
            else
               bat_y(i) = -(bathy (isrc+1,jsrc+1) * fracx * fracy +
     .                   bathy (isrc,jsrc+1) * (1.-fracx) * fracy +
     .                   bathy (isrc+1,jsrc) * fracx * (1.-fracy) +
     .                   bathy (isrc,jsrc) * (1.-fracx) * (1.-fracy))
            endif 
         endif

            xinc = xinc + dx
            yinc = yinc + dy
         enddo 
	else  
C cas gridxy
c alfan alfa etc deja calcules.
        do i=1,NXX
        good_point=.true.
        good_point=good_point .AND. (bathy(ii0(3,i),jj0(3,i)) .NE. bathyinfo%spval)
        good_point=good_point .AND. (bathy(ii0(1,i),jj0(1,i)) .NE. bathyinfo%spval)
        good_point=good_point .AND. (bathy(ii0(2,i),jj0(2,i)) .NE. bathyinfo%spval)
	bat_x(i) = i
        if (good_point) then
           bat_y(i) = -(alfan(i)*bathy(ii0(3,i),jj0(3,i)) +
     .                alfa1(i)*bathy(ii0(1,i),jj0(1,i)) +
     .                alfa2(i)*bathy(ii0(2,i),jj0(2,i)) )
        else
           bat_y(i) = spv
        endif

        enddo
	endif ! gridxy
      
         do i = 1,NXX
            if (bat_y(i).ge.prof_max) then
               bat_y(i) = 1
            else if (bat_y(i).le.prof_min) then
               bat_y(i) = NYY
            else 
               bat_y(i) = 1 + NYY -
     .                   int (((bat_y(i)-prof_min)/(prof_max-prof_min))*
     .                   float(NYY))
            endif 
         enddo 


c     on ferme le polygone

         bat_x(NXX+1) = bat_x(NXX)
         bat_y(NXX+1) = 0
         
         bat_x(NXX+2) = bat_x(1)
         bat_y(NXX+2) = 0
         
         bat_x(NXX+3) = bat_x(1)
         bat_y(NXX+3) = bat_y(1)
         value = 0
      endif 


      if (opt_xybat.eq.0) then
         x1bat = x1pos
         x2bat = x2pos
         y1bat = y1pos
         y2bat = y2pos
      endif 


      call getset (rl,rr,rb,rt,ul,ur,ub,ut,l)

      if(opt_marg.eq.0) then
      call set(x1bat,x2bat,y1bat,y2bat,1.,float(NXX),1.,float(NYY),1)
      else
          if(map_coord(3).eq.map_coord(4)) then
c coupe zonale
        rr2=1+(map_marg(2)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(NXX-1)
        rr1=1+(map_marg(1)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(NXX-1)
         elseif (map_coord(1).eq.map_coord(2)) then
c coupe meridienne
        rr2=1+(map_marg(4)-map_coord(3))/
     |   (map_coord(4)-map_coord(3))*(NXX-1)
        rr1=1+(map_marg(3)-map_coord(3))/
     |   (map_coord(4)-map_coord(3))*(NXX-1)
         else
         print *,' ERROR: l''option -marg incompatible avec coupes obliques'
         stop
         endif
      call set(x1bat,x2bat,y1bat,y2bat,rr1,rr2,1.,float(NYY),1)
        endif


c     call set(x1bat,x2bat,y1bat,y2bat,
c    .          1.,float(NXX),1.,float(NYY),1)
            
      call  aredam (iama,bat_x,bat_y,NXX+3,15,5,3)
      
      if (opt_mapfill.eq.1) then
         call arscam(iama,xcra,ycra,sz_cra,iaia,igia,10,colbathy)    
      endif 

      call ardrln (iama,bat_x,bat_y,NXX+3,
     .             xcra,ycra,sz_cra,iaia, igia,10,drwbathy)
      
      call pcsetr ('SA',cs_scale)      ! change character size
c     iytick = nint (abs(prof_min-prof_max)/zstep)
c     call gridal (0,0,iytick,0,0,0,5,0.,0.)
      call  pcsetr ('SA',0.88888888888) ! reset character size      

      call set (rl,rr,rb,rt,ul,ur,ub,ut,l)


c      call set(x1pos,x2pos,y1pos,y2pos,
c     .          1.,float(NXX),1.,float(NYY),1)
      
      
      return
      end
      


c ---------------------------------------------------------------------
c 
c Nom              :  colbathy
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

        subroutine colbathy (xcs,ycs,ncs,iai,iag,nai)

        implicit none
        include 'color.h'

        real    xcs(*),ycs(*)
        integer iai(*),iag(*), ncs, nai
        integer i, iai1

	iai1=-1
	do 101 i=1,nai
           if (iag(i).eq.15) then 
              iai1=iai(i)
           endif
 101    continue


	if (iai1.gt.0) then
           if (iai1.eq.3) then
              call gsfaci(COLOR_CONTINENT)
              call gsplci(COLOR_CONTINENT_PERIM)
              call gfa(ncs-1,xcs,ycs)               
              call gsplci(1)    ! set polyline color index
           endif
        endif
        return
        end


c ---------------------------------------------------------------------
c 
c Nom              :  drwbathy
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

        subroutine drwbathy (xcs,ycs,ncs,iai,iag,nai)

        implicit none
        include 'color.h'

        real    xcs(*),ycs(*)
        integer iai(*),iag(*), ncs, nai
        integer i, iai1

	iai1=-1
	do 101 i=1,nai
           if (iag(i).eq.15) then 
              call gsplci(COLOR_CONTINENT_PERIM)
              call curved (xcs,ycs,ncs)
              call gsplci(1)    ! set polyline color index
           endif
 101    continue

        return
        end



c ---------------------------------------------------------------------
c 
c Nom              :  InvertBuffer
c Date de creation :  
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

        subroutine InvertBuffer (buffer,nx,ny)
        
        include 'common.h'

        real buffer(NXX,NYY)
        integer  nx,ny,i,j

        real line(NXX)

c**        print *,'invert buffer'

        do j = 1,ny
           do i = 1,nx
              line (nx+1-i) = buffer (i,j)
           enddo 

           do i = 1,nx
              buffer (i,j) = line(i)
           enddo 
        enddo 

        return
        end


c ---------------------------------------------------------------------
c 
c Nom         :  ColorCoupe
c 
c Parametres  :  
c
c Description :  Assigne une couleur a chaque pixel de la coupe,
c                selon les limites. Procede par isocontours si 
c                l'option -print est choisie.
c 
c ---------------------------------------------------------------------

      subroutine ColorCoupe (fld, bimg, ncol,limit)

      implicit none

      include 'common.h'
      include 'color.h'
      include 'mapncar.h'

      real fld(NXX, NYY)
      real limit(NBOXMAX+1)
      real plot_coord(4)
      real spv
      integer ncol
      integer nx,ny
      integer map_flag

      integer clria(NXX,NYY)

      real ptr
      integer i,j, index, offset

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      real rr1, rr2
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save   /layout/

      TYPE( bimgfile ) bimg

      external colcontours


      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval

      if (opt_sigma.eq.1) then
       map_flag=5
      else
       map_flag=0
      endif

      if(opt_marg.eq.0) then
      call set(x1pos,x2pos,y1pos,y2pos,1.,float(nx),1.,float(ny),1)
      else
	  if(map_coord(3).eq.map_coord(4)) then
c coupe zonale
	rr2=1+(map_marg(2)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(nx-1)
	rr1=1+(map_marg(1)-map_coord(1))/
     |   (map_coord(2)-map_coord(1))*(nx-1)
         elseif (map_coord(1).eq.map_coord(2)) then
c coupe meridienne
        rr2=1+(map_marg(4)-map_coord(3))/
     |   (map_coord(4)-map_coord(3))*(nx-1)
        rr1=1+(map_marg(3)-map_coord(3))/
     |   (map_coord(4)-map_coord(3))*(nx-1)
	 else
         print *,' ERROR: l''option -marg incompatible avec coupes obliques'
	 stop
	 endif
      call set(x1pos,x2pos,y1pos,y2pos,rr1,rr2,1.,float(ny),1)
	endif

      if (opt_color.eq.1) then      
         if (opt_print.eq.0) then
c**            print *,'Coloration des pixels'
            call  flush(6)

            do i=1,nx
               do j=1,ny
                  if (fld(i,j).eq.spv) then 
                     clria(i,j) = COLOR_SPVAL
                  else if (fld(i,j).lt.limit(1)) then
                     clria(i,j) = COLOR_NRES+1
                  else if (fld(i,j).ge.limit(ncol)) then
                     clria(i,j) = ncol + COLOR_NRES - 1
                  else
                     offset = 0
                     ptr = float(ncol)
                     
                     do while (ptr.ne.1.0)
                        ptr = ptr / 2.0

                        if (ptr.le.1.0) ptr = 1.0
                        index = nint(ptr)+offset
                        
                        if (fld(i,j).ge.limit(index)) then
                           if (fld(i,j).lt.limit(index+1)) then
                              ptr = 1.0
                           endif 
                           offset = index
                        endif                   
                     enddo 
                     
                     clria(i,j) = offset + COLOR_NRES - 1
                  endif 
               enddo
            enddo

            call gca(1.,1.,float(nx),float(ny),NXX,NYY,1,1,nx,ny,clria)
         else 
            plot_coord(1) = 1.
            plot_coord(2) = float(NXX)
            plot_coord(3) = 1.
            plot_coord(4) = float(NYY)

            call ColorContour (fld, bimg, limit, plot_coord, map_flag,ncol)
            call arscam(iama,xcra,ycra,sz_cra,iaia,igia,10,colcontours)  
         endif 
      endif 

      if ((opt_palbar.eq.1).and.(opt_color.eq.1)) then
         call AjoutePalette (limit, ncol)
      endif 

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  AjouteGrille
c 
c Description :  Dessine des lignes horizontales pour la profondeur
c                et verticales pour les meridiens.
c                Il est possible de choisir quelles lignes seront
c                dessinees : -xgrid  : longitudes
c                            -ygrid  : latitudes
c                            -zgrid  : profondeur
c
c ---------------------------------------------------------------------

      subroutine AjouteGrille ()

      implicit none

      include 'common.h'
      include 'val_table.h'

      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist
      save   /cut/

      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw

      common  /trace/xstep, ystep, zstep, kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw
      save    /trace/

      integer ixtick, iytick
      real    rl, rr, rb, rt, ul, ur, ub, ut
      integer l
      character*20 label
      integer ixlb, iylb, izlb
      integer lnblnk

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      real angled, cut_dist, cut_dist_adjusted
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      real rr1, rr2, rr2bis
      common /cut_properties/ angled, cut_dist
      save   /layout/


      call pcsetr ('SA',cs_scale)      ! change character size

      call getset (rl,rr,rb,rt,ul,ur,ub,ut,l)

      call set (rl, rr, rb, rt, rl, rr,
     .          -prof_max, -prof_min,1)


      if (opt_labx.eq.0) then
         ixlb = 0
      else 
         ixlb = 1
      endif 

      if (opt_laby.eq.0) then
         iylb = 0
      else 
         iylb = 1
      endif 

      if (opt_labz.eq.0) then
         izlb = 0
      else 
         izlb = 1
      endif 

      if (opt_xstep.eq.0) xstep = abs(map_marg(1)-map_marg(2))/4.0
      if (opt_ystep.eq.0) ystep = abs(map_marg(3)-map_marg(4))/4.0
      if (opt_zstep.eq.0) zstep = abs(prof_min-prof_max)/4.0
      if (opt_kmstep.eq.0) kmstep = cut_dist/4.




      call labmod ('(i10)', int_table(ICLR_ZAXIS)%format,0,0,1,1,0,5,0)

      if (opt_grad.eq.1) then
         iytick = nint (abs(prof_min-prof_max)/zstep)
      else 
         iytick = 0
      endif 

      if (opt_zgrid.eq.0) then
C        call gridal (0,0,iytick,0,0,izlb,5,0.,0.)
         call gridal (0,0,iytick,zsub,0,izlb,5,0.,0.)
      else
         call gasetr ('WMJ - major tick line width',zlw)
         call gridal (0,0,iytick,zsub,0,izlb,0,0.,0.)
         call gasetr ('WMJ - major tick line width',1.0)
      endif 

      if (opt_xgrid.eq.1) then
         call gasetr ('WMJ - major tick line width',xlw)
         ixtick = nint (abs(map_marg(1)-map_marg(2))/xstep)
         call gridal (ixtick,xsub,0,0,0,0,0,0.,0.)
         call gasetr ('WMJ - major tick line width',1.0)
      endif 

      if (opt_ygrid.eq.1) then
         call gasetr ('WMJ - major tick line width',ylw)
         ixtick = nint (abs(map_marg(3)-map_marg(4))/ystep)
         call gridal (ixtick,ysub,0,0,0,0,0,0.,0.)
         call gasetr ('WMJ - major tick line width',1.0)
      endif 

c     longitude et latitude



      if (opt_nolon.eq.0) then
         call labmod (int_table(ICLR_XAXIS)%format,'(i1)',0,0,1,1,0,2,0)

         call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
         write(label,200)xaxist(1:lnblnk(xaxist))
         call plchhq (x1pos-0.02,ylon,label(1:lnblnk(label)),-1.,0.,1.)

         if (map_marg(1).ne.map_marg(2)) then
            ixtick = nint (abs(map_marg(1)-map_marg(2))/xstep)

            call set (rl,rr,0.,1.,map_marg(1),map_marg(2),0.,1.,1)
            call halfax (ixtick,xsub,0,0,0,ylon,ixlb,-1)
         else 
            write(label,201) map_marg(1)
            call plchhq (x1pos,ylon,label,-1.,0.,-1.)
         endif 
      endif 


      if (opt_nolat.eq.0) then
         call labmod (int_table(ICLR_YAXIS)%format,'(i1)',0,0,1,1,0,2,0)

         call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
         write(label,200)yaxist(1:lnblnk(yaxist))
         call plchhq (x1pos-0.02,ylat,label(1:lnblnk(label)),-1.,0.,1.)

         if (map_marg(3).ne.map_marg(4)) then
            ixtick = nint (abs(map_marg(3)-map_marg(4))/ystep)
            
            call set (rl,rr,0.,1.,map_marg(3),map_marg(4),0.,1.,1)
            call halfax (ixtick,ysub,0,0,0,ylat,iylb,-1)
         else 
            write(label,201) map_marg(3)
            call plchhq (x1pos,ylat,label,-1.,0.,-1.)
         endif 
      endif 
c
c option echelle en km
c
	if (opt_km.eq.1) then

         call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
         write(label,200)kmaxist(1:lnblnk(kmaxist))
         call plchhq(x1pos-0.02,ykm,label(1:lnblnk(label)),-1.,0.,1.)

	    cut_dist_adjusted=(int(cut_dist/kmstep) +1 )* kmstep

	  if (opt_marg.eq.1) then
	  if (map_marg(2).ne.map_marg(1)) then
c  coupe zonale
	    rr1=rl + (rr-rl)*(map_coord(1)-map_marg(1))/
     |          (map_marg(2)-map_marg(1))
	    rr2=rl + (rr-rl)*(map_coord(2)-map_marg(1))/
     |          (map_marg(2)-map_marg(1))
	  elseif (map_marg(4).ne.map_marg(3)) then
c coupe meridienne
	    rr1=rl + (rr-rl)*(map_coord(3)-map_marg(3))/
     |          (map_marg(4)-map_marg(3))
	    rr2=rl + (rr-rl)*(map_coord(4)-map_marg(3))/
     |          (map_marg(4)-map_marg(3))
          endif
	 else ! opt_marg.eq.0
            rr1=rl
	    rr2=rr
	 endif
	    rr2bis= (rr2-rr1)/cut_dist*cut_dist_adjusted +rr1
	  if (rr2bis.ge.1) then
	    print *,' rr2bis trop grand ...', rr2bis
	    rr2bis=rr2
            cut_dist_adjusted=cut_dist ! tant pis !
	  endif

            call set (rr1,rr2bis,0.,1.,0.,cut_dist_adjusted,0.,1.,1)

	    ixtick=nint(cut_dist_adjusted/kmstep)
            call halfax (ixtick,kmsub,0,0,0,ykm,ixlb,-1)

	endif

      call pcsetr ('SA',0.88888888888) ! reset character size


 200  format(a)
 201  format(f6.2)

      call set (rl,rr,rb,rt,ul,ur,ub,ut,l)

      return
      end


	subroutine NearestPoint(zlon,zlat,imin,jmin,iquadran)

	implicit none
	include 'parameter.h'
	real*4 zlon, zlat
	integer imin, jmin, iquadran
	integer i0,i1, j0,j1
        real xygr(0:NXX+1,0:NYY+1,2)
        real,dimension(:,:), allocatable :: zdis
        integer nigr,njgr, mimi(2)
        common /gridxy/xygr,nigr,njgr

	real alpha(0:6), x(0:6), y(0:6)
        real dx, dy, dalph
	real  pi
	integer i,j,ierr
	logical bord_west, bord_est, bord_sud, bord_nord
	

	pi=acos(-1.)
	iquadran = 0
C On cherche les points dans le voisinage du precedent (rayon de 10, a voir)
C Pour chercher toute la grille on demare avec imin=jmin=0 
C
	if (imin.gt.0 .and. jmin .gt. 0 ) then
	 i0=imin -10
	 i1=imin +10
	 j0=jmin -10
	 j1=jmin +10
          if (i0.le.0) i0= 1
          if (i1.gt.nigr) i1= nigr
          if (j0.le.0) j0= 1
          if (j1.gt.njgr) j1= njgr
	else
	 i0=1
	 i1=nigr 
	 j0=1
	 j1=njgr
	endif

          IF (ALLOCATED(zdis) .AND. size(zdis,1) == i1-i0+1  .AND. size(zdis,2) == j1-j0+1 ) THEN
C         no need to reallocate zdis
          ELSE
          IF (allocated(zdis))  DEALLOCATE(zdis)
          ALLOCATE(zdis(i1-i0+1,j1-j0+1) )
          ENDIF
        zdis(:,:)=( zlat - xygr(i0:i1,j0:j1,2))*( zlat - xygr(i0:i1,j0:j1,2)) + 
     .                    (zlon - xygr(i0:i1,j0:j1,1))*(zlon - xygr(i0:i1,j0:j1,1))
        mimi=minloc(zdis)
        imin=mimi(1) +i0-1
        jmin=mimi(2) +j0-1


	bord_west = .false.
	bord_est  = .false.
	bord_sud  = .false.
	bord_nord = .false.

	if (imin.eq.1   ) bord_west = .true.
	if (imin.eq.nigr ) bord_est  = .true.
	if (jmin.eq.1   ) bord_sud  = .true.
	if (jmin.eq.njgr ) bord_nord = .true.

	x(0)=xygr(imin,jmin,1)
	y(0)=xygr(imin,jmin,2)

	x(1)=xygr(imin+1,jmin,1)
	y(1)=xygr(imin+1,jmin,2)

	x(2)=xygr(imin,jmin+1,1)
	y(2)=xygr(imin,jmin+1,2)

	x(3)=xygr(imin-1,jmin+1,1)
	y(3)=xygr(imin-1,jmin+1,2)

	x(4)=xygr(imin-1,jmin,1)
	y(4)=xygr(imin-1,jmin,2)

	x(5)=xygr(imin,jmin-1,1)
	y(5)=xygr(imin,jmin-1,2)

	x(6)=xygr(imin+1,jmin-1,1)
	y(6)=xygr(imin+1,jmin-1,2)

c                       (3)         (2)
c                          \         |  	  
c                            \       |           
c                              \     |   x     
c                       (4) ------- (0)_________ (1)
c                                    \
c                                     \
c                                      \          (6)
c                                      (5)
c
C il faut maintenant determiner les pts qui limitent la cellule ou se trouve
C NEW (2006) on defini 6 quadran pour toujours avoir les meme triangles
C le pt de donnee:Calcul de l'angle . 

	dy = (zlat - y(0) )
	dx = (zlon - x(0) )
	if (dx .eq. 0. .and. dy .eq. 0.) then
	alpha(0)=0.
	iquadran=7
	else
	alpha(0)=180./pi* atan2(dy,dx)
	endif

	if (alpha(0).lt.0) alpha(0) = 360. + alpha(0)
	
	do i=1,6
	dy = (y(i) - y(0) )
	dx = (x(i) - x(0) )
	alpha(i)=180./pi* atan2(dy,dx)
	if (alpha(i).lt.0) alpha(i) = 360. + alpha(i)
	enddo

	dalph = alpha(1)
	do i=0,6
	alpha(i)=alpha(i) - dalph
	if (alpha(i).lt.0) alpha(i) = 360. + alpha(i)
	enddo
	

c	print *, (alpha(i),i=0,4)

	dalph=alpha(0)
	if (iquadran .ne. 7 ) then
	if (dalph .ge. alpha(6) ) then
c 6eme quadran. 
	iquadran=6
	else if (dalph .ge. alpha(5) ) then
c 5eme quadran
	iquadran=5
	else if (dalph .ge. alpha(4) ) then
c 4eme quadran
	iquadran=4
	else if (dalph .ge. alpha(3) ) then
c 3eme quadran
	iquadran=3
	else if (dalph .ge. alpha(2) ) then
c 2eme quadran
	iquadran=2
	else if (dalph .ge. 0. ) then
c 1eme quadran
	iquadran=1
	else
	print *,'   ',dalph, (alpha(i),i=1,6)
	STOP ' pb '
	endif
	endif
	ierr=0
	if (bord_west .and. (iquadran.EQ.2 .OR. iquadran.eq.3) ) then
	 print *, 'Point a l''Ouest du domaine'
	 ierr=ierr+1
	endif
	if (bord_est .and. (iquadran.EQ.1 .OR. iquadran.eq.4) ) then
	 print *, 'Point a l''Est du domaine'
         print *, bord_est, iquadran
	ierr=ierr+1
	endif
	if (bord_nord .and. (iquadran.EQ.1 .OR. iquadran.eq.2) ) then
	 print *, 'Point au nord du domaine'
	 ierr=ierr+1
	endif
	if (bord_sud .and. (iquadran.EQ.3 .OR. iquadran.eq.4) ) then
	 print *, 'Point au sud du domaine'
	 ierr=ierr+1
	endif

	if (ierr.ne. 0 ) then
	  stop ' dans NearestPoint (module coupe.F)'
	endif
	return

	end
