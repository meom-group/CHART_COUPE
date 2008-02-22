c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  checkfiles.f
c 
c Auteur         :  Eric Brown
c
c Description    :  contient les fonctions de verification sur les 
c                   fichiers donnes en arguments.
c 
c ---------------------------------------------------------------------
      
      subroutine OpenAndVerifyFiles (color, zlevel, contours, vectors)

      implicit none

      include 'common.h'

      integer   couche(NA),couche_max, tstep(Ntime), tstep_max
      integer  cl_dim, ct_dim
      real    conv(9)
      common  /calc/conv,couche,couche_max, tstep, tstep_max,
     .              cl_dim, ct_dim
      save    /calc/

      TYPE( bimgfile) color
      TYPE( bimgfile) zlevel
      TYPE( bimgfile) contours
      TYPE( bimgfile) vectors(3)

      integer zoom_set
      integer nx,ny,i
      integer kinf, ksup, knear, k
      integer BimgOpenFile
      integer lnblnk

      character*255 cmd

      zoom_set = 0
C ... initialize some stuff for NetCdf 
C
      color%varname      = v_clr
      contours%varname   = v_cnt
      vectors(1)%varname = v_vecx
      vectors(2)%varname = v_vecy
C
      color%modifier     = modif_clr
      contours%modifier  = modif_cnt
      vectors(1)%modifier= modif_vecx
      vectors(2)%modifier= modif_vecy
C
      if (opt_dep.ne.0 ) then
       do k=1,NA
        couche(k) = 0
       enddo
      endif

      if (opt_sigma.eq.1) then
       zlevel%fname = f_zlevel
       zlevel%f_unit = BimgOpenFile(zlevel)
       call BimgReadHeader(zlevel)
       if (opt_clrgrid.ne.0) then 
        call ReadGrid(zlevel,f_clrgrid)
       endif
      endif
      
      if (opt_single .eq. 1) then
        couche_max = 1
        couche(1)  = 1
      endif
     
c     COULEUR ----------------------------------------------------
      
      if (opt_clrdata.eq.1) then
#ifdef linux
C Linux adaptation : On cree un lien du fichier f_clrdata avec extension
C .tmpclr
        call system('basename '//f_clrdata//' > zzzbasenam') 
        open(3,file='zzzbasenam')
        read(3,'(a)') cmd
        close(3)
        call system('/bin/rm zzzbasenam')
        cmd=cmd(1:lnblnk(cmd))//'.tmpclrnk'
        call system(' ln -s '//f_clrdata(1:lnblnk(f_clrdata))//' '//cmd)
       f_clrdata=cmd
C fin de modif linux
#endif
         color%fname = f_clrdata
         color%f_unit = BimgOpenFile (color)
         call BimgReadHeader(color)
         if (color%spval .eq. 0 ) print *,' WARNING  CLRDATA :  spval = 0'

         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
      call LevelOfDepth(color,req_dep,ksup,kinf,knear)        
      kdessus=ksup
      couche(knear)=1
      couche_max=knear
          endif
      

c     temps 

         if (tstep_max.eq.99999) then
            tstep_max = color%nt
         endif 

         if (color%nt.lt.tstep_max) then
            call PrintMessage (1, 9, f_clrdata(1:lnblnk(f_clrdata)))
         endif 

c     couche

         if (couche_max.eq.99999) then
            couche_max = color%ncou
         endif 

         if (color%ncou.lt.couche_max) then
            call PrintMessage (1, 1, f_clrdata(1:lnblnk(f_clrdata)))
         endif 

c     option masque

         if (opt_clrmask.ne.0) then
            call ReadMask (color, f_clrmask)
         endif 
         
c     option grille
         
         if (opt_clrgrid.ge.1) then
            call ReadGrid (color, f_clrgrid)
         endif 
         
c     coordonnees
         
         if (opt_zoom.eq.1) then
C           if (((color%x1mod+map_shift).gt.map_coord(1)).or.
C    .          ((color%x2mod+map_shift).lt.map_coord(2)).or.
C    .          (color%y1mod.gt.map_coord(3)).or.
C    .          (color%y2mod.lt.map_coord(4))) then
C              call PrintMessage (1, 2, ' ')
C           endif 
            if ( (color%x1mod+map_shift).gt.map_coord(1)) then
             PRINT *, 'Warning ... Zoom lon min < x1 --> mis a x1 +0.01'
             map_coord(1) = color%x1mod+map_shift +0.01
            endif
            if ( (color%x2mod+map_shift).lt.map_coord(2)) then
             PRINT *, 'Warning ... Zoom lon max > x2 --> mis a x2 -0.01'
             map_coord(2) = color%x2mod+map_shift -0.01
            endif
            if ( (color%y1mod).gt.map_coord(3)) then
             PRINT *, 'Warning ... Zoom lat min < y1  --> mis a y1 +0.01'
             map_coord(3) = color%y1mod + 0.01
            endif
            if ( (color%y2mod).lt.map_coord(4)) then
             PRINT *, 'Warning ... Zoom lat max > y2  --> mis a y2 -0.01'
             map_coord(4) = color%y2mod - 0.01
            endif
         else 
            map_coord(1) = color%x1mod 
            map_coord(2) = color%x2mod 
            map_coord(3) = color%y1mod
            map_coord(4) = color%y2mod
            zoom_set = 1
         endif             
      endif 


c     CONTOURS ----------------------------------------------------

      if (opt_contdata.eq.1) then
#ifdef linux
C Linux adaptation : On cree un lien du fichier f_cntdata avec extension
C .tmpcnt
        call system('basename '//f_cntdata//' > zzzbasenam')
        open(3,file='zzzbasenam')
        read(3,'(a)') cmd
        close(3)
        call system('/bin/rm zzzbasenam')
        cmd=cmd(1:lnblnk(cmd))//'.tmpcntlnk'
        call system(' ln -s '//f_cntdata(1:lnblnk(f_cntdata))//' '//cmd)
       f_cntdata=cmd
C fin de modif linux                      
#endif
         contours%fname = f_cntdata
         contours%f_unit = BimgOpenFile (contours)
         call BimgReadHeader(contours)
         if (contours%spval .eq. 0 ) print *,' WARNING  CNTDATA :  spval = 0'

         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
	call LevelOfDepth(contours,req_dep,ksup,kinf,knear)        
	kdessus=ksup
        couche(knear)=1
        couche_max=knear
         endif

c     temps

         if (tstep_max.eq.99999) then
            tstep_max = contours%nt
         endif 

         if (contours%nt.lt.tstep_max) then
            call PrintMessage (1, 9, f_cntdata(1:lnblnk(f_cntdata)))
         endif 

c     couche

         if (couche_max.eq.99999) then
            couche_max = contours%ncou
         endif 

         if (contours%ncou.lt.couche_max) then
            call PrintMessage (1, 1, f_cntdata(1:lnblnk(f_cntdata)))
         endif 


c     option masque

         if (opt_contmask.ne.0) then
            call ReadMask (contours, f_cntmask)
         endif 

c     option grille

         if (opt_contgrid.ge.1) then
            call ReadGrid (contours, f_cntgrid)
         endif 

c     coordonnees

         if (opt_zoom.eq.1) then
C           if (((contours%x1mod+map_shift).gt.map_coord(1)).or.
C    .          ((contours%x2mod+map_shift).lt.map_coord(2)).or.
C    .          (contours%y1mod.gt.map_coord(3)).or.
C    .          (contours%y2mod.lt.map_coord(4))) then
C              call PrintMessage (1, 2, ' ')
C              stop
C           endif 
            if ( (contours%x1mod+map_shift).gt.map_coord(1)) then
             PRINT *, 'Warning ... Zoom lon min < x1 --> mis a x1 '
             map_coord(1) = contours%x1mod+map_shift
            endif
            if ( (contours%x2mod+map_shift).lt.map_coord(2)) then
             PRINT *, 'Warning ... Zoom lon max > x2 --> mis a x2 '
             map_coord(2) = contours%x2mod+map_shift
            endif
            if ( (contours%y1mod).gt.map_coord(3)) then
             PRINT *, 'Warning ... Zoom lat min < y1  --> mis a y1 '
             map_coord(3) = contours%y1mod
            endif
            if ( (contours%y2mod).lt.map_coord(4)) then
             PRINT *, 'Warning ... Zoom lat max > y2  --> mis a y2 '
             map_coord(4) = contours%y2mod
            endif
         else 
            if (zoom_set.eq.1) then
               if (contours%x1mod.gt.map_coord(1))
     .                   map_coord(1) = contours%x1mod
               if (contours%x2mod.lt.map_coord(1))
     .                   map_coord(2) = contours%x2mod
               if (contours%y1mod.gt.map_coord(3))
     .                   map_coord(3) = contours%y1mod
               if (contours%y2mod.lt.map_coord(4))
     .                   map_coord(4) = contours%y2mod
            else 
               map_coord(1) = contours%x1mod
               map_coord(2) = contours%x2mod
               map_coord(3) = contours%y1mod
               map_coord(4) = contours%y2mod
               zoom_set = 1
            endif 
         endif             
      endif 


c     VECTEURS ----------------------------------------------------

      if (opt_vectors.eq.1) then
         if ((opt_vect3D.eq.1).or.
     .       (opt_vect2D.eq.1)) then
            vectors(1)%fname = f_vecdata1
            vectors(1)%f_unit = BimgOpenFile (vectors(1))
            call BimgReadHeader(vectors(1))
         if (vectors(1)%spval .eq. 0 ) print *,' WARNING  VECTORS UV :  spval = 0'
	    vectors(2)=vectors(1)

         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
	call LevelOfDepth(vectors(1),req_dep,ksup,kinf,knear)        
	kdessus=ksup
        couche(knear)=1
        couche_max=knear
         endif

         else

            if (opt_vectX.eq.1) then
            vectors(1)%fname = f_vecdata1
               vectors(1)%f_unit = BimgOpenFile (vectors(1))
               call BimgReadHeader(vectors(1))
         if (vectors(1)%spval .eq. 0 ) print *,' WARNING  VECTORS U :  spval = 0'
         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
	call LevelOfDepth(vectors(1),req_dep,ksup,kinf,knear)        
	kdessus=ksup
        couche(knear)=1
        couche_max=knear
         endif
            endif 

            if (opt_vectY.eq.1) then
            vectors(2)%fname = f_vecdata2
               vectors(2)%f_unit = BimgOpenFile (vectors(2))
               call BimgReadHeader(vectors(2))
         if (vectors(2)%spval .eq. 0 ) print *,' WARNING  VECTORS V :  spval = 0'
         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
	call LevelOfDepth(vectors(2),req_dep,ksup,kinf,knear)        
	kdessus=ksup
        couche(knear)=1
        couche_max=knear
         endif
            endif 

            if (opt_vectZ.eq.1) then
            vectors(3)%fname = f_vecdata3
               vectors(3)%f_unit = BimgOpenFile (f_vecdata3)
               call BimgReadHeader(vectors(3))
         if (vectors(3)%spval .eq. 0 ) print *,' WARNING  VECTORS W:  spval = 0'
         if (opt_dep.ne.0) then
C cherche les profondeurs correspondantes a req_dep
C	
	call LevelOfDepth(vectors(3),req_dep,ksup,kinf,knear)        
	kdessus=ksup
        couche(knear)=1
        couche_max=knear
         endif
            endif 

            if ((opt_vectX.eq.1).and.(opt_vectY.eq.1)) then
               if ((vectors(1)%nxfile.ne.vectors(2)%nxfile).or.
     .             (vectors(1)%nyfile.ne.vectors(2)%nyfile).or.
     .             (vectors(1)%spval .ne.vectors(2)%spval )) then
                  PRINT *,' Cas 1'
                  call PrintMessage (1, 3, ' ')
               endif
C patches pour l'option Cgrid qui travaille avec des fichiers
C n'ayant pas necessairement le meme layout...
C
               if (((vectors(1)%x1mod .ne.vectors(2)%x1mod ).or.
     .             (vectors(1)%dx    .ne.vectors(2)%dx    ).or.
     .             (vectors(1)%dy    .ne.vectors(2)%dy    ).or.
     .             (vectors(1)%y1mod .ne.vectors(2)%y1mod )).and.
     .             (opt_cgrid.eq.0)) then
                  PRINT *,' Cas 2'
                  call PrintMessage (1, 3, ' ')
               else
                 vectors(1)%x1mod = vectors(2)%x1mod
                 vectors(1)%y1mod = vectors(2)%y1mod
                 vectors(1)%x2mod = vectors(2)%x2mod
                 vectors(1)%y2mod = vectors(2)%y2mod
                 vectors(1)%dx    = vectors(2)%dx
                 vectors(1)%dy    = vectors(2)%dy
               endif
            endif

            if ((opt_vectX.eq.1).and.(opt_vectZ.eq.1)) then
               if ((vectors(1)%nxfile.ne.vectors(3)%nxfile).or.
     .             (vectors(1)%nyfile.ne.vectors(3)%nyfile).or.
     .             (vectors(1)%dx    .ne.vectors(3)%dx    ).or.
     .             (vectors(1)%dy    .ne.vectors(3)%dy    ).or.
     .             (vectors(1)%x1mod .ne.vectors(3)%x1mod ).or.
     .             (vectors(1)%y1mod .ne.vectors(3)%y1mod ).or.
     .             (vectors(1)%spval .ne.vectors(3)%spval )) then
                  PRINT *,' Cas 3'
                  call PrintMessage (1, 3, ' ')
               endif 
            endif 

            if ((opt_vectZ.eq.1).and.(opt_vectY.eq.1)) then
               if ((vectors(3)%nxfile.ne.vectors(2)%nxfile).or.
     .             (vectors(3)%nyfile.ne.vectors(2)%nyfile).or.
     .             (vectors(3)%dx    .ne.vectors(2)%dx    ).or.
     .             (vectors(3)%dy    .ne.vectors(2)%dy    ).or.
     .             (vectors(3)%x1mod .ne.vectors(2)%x1mod ).or.
     .             (vectors(3)%y1mod .ne.vectors(2)%y1mod ).or.
     .             (vectors(3)%spval .ne.vectors(2)%spval )) then
                  PRINT *,' Cas 4'
                  call PrintMessage (1, 3, ' ')
               endif 
            endif 
         endif 

c     temps

         if (tstep_max.eq.99999) then
            tstep_max = vectors(1)%nt
         endif 

         if (vectors(1)%nt.lt.tstep_max) then
            call PrintMessage (1, 9, f_clrdata(1:lnblnk(f_vecdata1)))
         endif 

c     couche

         if (couche_max.eq.99999) then
            couche_max = vectors(1)%ncou
         endif 

         if (vectors(1)%ncou.lt.couche_max) then
            call PrintMessage (1, 1, f_clrdata(1:lnblnk(f_vecdata1)))
         endif 

         nx = vectors(1)%nxfile
         ny = vectors(1)%nyfile


c     option masque

         if (opt_vectmask.ne.0) then
            call ReadMask (vectors(1), f_vecmask)
         endif 

c     option grille

         if (opt_vectgrid.ge.1) then
            vectors(2)%x1mod = vectors(1)%x1mod
            vectors(2)%x2mod = vectors(1)%x2mod
            vectors(2)%y1mod = vectors(1)%y1mod
            vectors(2)%y2mod = vectors(1)%y2mod
            vectors(2)%dx    = vectors(1)%dx
            vectors(2)%dy    = vectors(1)%dy

            call ReadGrid (vectors(1), f_vecgrid)
            vectors(2)%grid = vectors(1)%grid
            do i = 1,nx
               vectors(2)%d_xgrid(i) = vectors(1)%d_xgrid(i)
            enddo
            do i = 1,ny
               vectors(2)%d_ygrid(i) = vectors(1)%d_ygrid(i)
            enddo

            if (opt_zoom.eq.0) then
               map_coord(1) = vectors(1)%x1mod
               map_coord(2) = vectors(1)%x2mod
               map_coord(3) = vectors(1)%y1mod
               map_coord(4) = vectors(1)%y2mod
            endif 

            vectors(1)%x1mod = vectors(2)%x1mod
            vectors(1)%x2mod = vectors(2)%x2mod
            vectors(1)%y1mod = vectors(2)%y1mod
            vectors(1)%y2mod = vectors(2)%y2mod
            vectors(1)%dx    = vectors(2)%dx
            vectors(1)%dy    = vectors(2)%dy
         endif 


c     coordonnees

         if (opt_zoom.eq.1) then
            if (opt_vectgrid.eq.0) then
               if (((vectors(1)%x1mod+map_shift).gt.map_coord(1)).or.
     .             ((vectors(1)%x2mod+map_shift).lt.map_coord(2)).or.
     .             (vectors(1)%y1mod.gt.map_coord(3)).or.
     .             (vectors(1)%y2mod.lt.map_coord(4))) then
                  call PrintMessage (1, 2, ' ')
                  stop
               endif 
            else if ((vectors(1)%d_xgrid(1).gt.map_coord(1)).or.
     .               (vectors(1)%d_xgrid(nx).lt.map_coord(2)).or.
     .               (vectors(1)%d_ygrid(1).gt.map_coord(3)).or.
     .               (vectors(1)%d_ygrid(ny).lt.map_coord(4))) then
               call PrintMessage (1, 2, ' ')
c               stop
            endif 
         else 
            if (opt_vectgrid.eq.0) then
               if (zoom_set.eq.1) then
                  if (vectors(1)%x1mod.gt.map_coord(1))
     .                   map_coord(1) = vectors(1)%x1mod
                  if (vectors(1)%x2mod.lt.map_coord(1))
     .                   map_coord(2) = vectors(1)%x2mod
                  if (vectors(1)%y1mod.gt.map_coord(3))
     .                   map_coord(3) = vectors(1)%y1mod
                  if (vectors(1)%y2mod.lt.map_coord(4))
     .                   map_coord(4) = vectors(1)%y2mod
               else 
                  map_coord(1) = vectors(1)%x1mod
                  map_coord(2) = vectors(1)%x2mod
                  map_coord(3) = vectors(1)%y1mod
                  map_coord(4) = vectors(1)%y2mod
               endif 
            endif 
         endif             
      endif 

      if (opt_zoom.eq.0) then
         map_coord(1) = map_coord(1) + map_shift
         map_coord(2) = map_coord(2) + map_shift
         opt_zoom = 1
      endif 


      if (opt_marg.eq.0) then
         map_marg(1) = map_coord(1)
         map_marg(2) = map_coord(2)
         map_marg(3) = map_coord(3)
         map_marg(4) = map_coord(4)
      endif 

      return
      end





c ---------------------------------------------------------------------
c 
c Nom         :  ReadMask
c 
c Parametres  :  
c 
c Description :  Lit le fichier de masque et verifie que ses dimensions
c                correspondent au fichier de donnees auquel il se
c                rattache.
c 
c                Remplace spval par 1 et le reste par 0 dans le tableau
c                interne.
c
c ---------------------------------------------------------------------

      subroutine ReadMask (bimg, filename)

      implicit none

      include 'common.h'
      
      TYPE( bimgfile) mask, bimg
      character*256 filename
      integer BimgOpenFile

      

      mask%fname = filename
      mask%f_unit = BimgOpenFile (mask)
      
      call BimgReadHeader(mask)
      
      if ((mask%nxfile.ne.bimg%nxfile).or.
     .    (mask%nyfile.ne.bimg%nyfile)) then
         print *,'erreur : la taille du masque ne correspond pas'
         print *,'         au fichier de donnees.'
         stop
      endif 
      
      bimg%mask = 1
      call BimgGetLayer  (mask,bimg%d_mask,1,1,1)
      close (mask%f_unit)

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  ReadGrid
c 
c Parametres  :  
c                vector - indique si il s'agit d'une grille pour vecteurs
c 
c Description :  Lit un fichier de grille et verifie que sa taille
c                correspond bien au fichier de donnees auquel il se
c                rattache.
c 
c ---------------------------------------------------------------------

      subroutine ReadGrid (bimg, filename)

      implicit none

      include 'common.h'

      TYPE( bimgfile) grid, bimg
      character*256 filename
      integer BimgOpenFile

      grid%fname = filename
      grid%f_unit = BimgOpenFile (grid)

      call BimgReadHeader(grid)

      if ((grid%nxfile.ne.bimg%nxfile).or.
     .    (grid%nyfile.ne.bimg%nyfile)) then
         print *,'erreur : la taille de la grille ne correspond pas'
         print *,'         au fichier de donnees.'
         stop
      endif 
      
      call BimgGetGridInfo (bimg,grid)

      close (grid%f_unit)
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  CheckOptions
c 
c Parametres  :  
c 
c Description :  Cherche des conflits dans les choix d'options.
c 
c ---------------------------------------------------------------------
      
      subroutine CheckOptions(bimgclr, bimgcnt, bimgvec)
      
      implicit none
      
      include 'common.h'
      
      TYPE( bimgfile) bimgclr, bimgcnt, bimgvec(3)
      integer   couche(NA),couche_max, tstep(Ntime), tstep_max
      integer  cl_dim, ct_dim, cl_met, cl_exp
      real    conv(9), cl_max, cl_min
      common  /calc/conv,couche,couche_max, tstep, tstep_max,
     .              cl_dim, ct_dim
      common  /color/cl_min, cl_max, cl_met, cl_exp

c Dans le cas de chart, l'utilisation d'une grille gridxy impose
c l'option opt_print = 1 pour passer dans cpmpxy. Pour coupe, ce n'est
c pas necessaire:
	if (opt_chart .eq. 1 .and. opt_clrgrid .eq. 3 ) then
         opt_print = 1
	endif
c
c dans le cas cntshade, il faut que cl_dim = ct_dim
c
	if (opt_shade .eq. 1 ) then
         cl_dim = ct_dim
	endif
c
c dans le cas vecshade, si une grille irreguliere est specifiee
c elle doit l'etre par -gridxy
	if (opt_vecshade .eq. 1 .AND. opt_clrgrid.ne.0) then
	  if (opt_clrgrid.ne.3) then
c on force l'option gridxy ...
	  print *,' Avec vecshade, utiliser l''option gridxy SVP.'
	  stop ' Arret dans checkoptions'
	 endif
        endif
	
c
c dans le cas de l'option vecclr, en cas de grille irreguliere, il faut utiliser
c l'option -gridxy.
c
	if (opt_vectclr .eq. 1) then
        if (opt_clrgrid. eq. 1 .or. opt_clrgrid.eq.2) then
          print *,' Pour colorer les vecteurs avec une grille irreguliere,'
          print *,' utiliser l''option -gridxy. Merci.'
          stop 
        endif
        endif
c
c     si les vecteurs sont colores par un champ de scalaires,
c     verifier que les fichiers sont de meme dimension.

      if ((opt_vectclr.eq.1).and.(opt_clrdata.eq.1)) then
         if ((bimgclr%nxfile.ne.bimgvec(1)%nxfile).or.
     .       (bimgclr%nyfile.ne.bimgvec(1)%nyfile).or.
     .       (bimgclr%dx.ne.bimgvec(1)%dx).or.
     .       (bimgclr%dy.ne.bimgvec(1)%dy)) then
c        print *, 'warning : les donnees de couleur doivent etre de meme'
c        print *, '         dimensions que les donnees vectorielles.'
c        stop
         endif 
      endif 

      if ((opt_vectclr.eq.1).and.(opt_clrdata.eq.0)) then
      print *,'erreur : veuillez specifier un fichier de scalaires pour'
      print *,'         l''opiton -vecclr. (utilisez -clrdata)'
      stop
      endif 
      
      if ((opt_vectclr.eq.1).and.(opt_vectmod.eq.1)) then
      print *, 'erreur : on ne peut colorer selon le module et selon'
      print *, '         un scalaire en meme temps...'
      stop
      endif 
      
      
c     si on choisi de colorer les vecteurs avec un scalaire, enlever
c     l'option color, sinon on ne verra pas les vecteurs.
      
      if ((opt_vectclr.eq.1).and.(opt_clrdata.eq.1)) then
         opt_color = 0
      endif 
c
c     L'option clrmark impose un calcul lineaire de la repartition des couleur
c     c'est a dire cl_met = 1
      if (opt_clrmark .EQ. 1 .AND. cl_met. NE. 1 ) THEN
        cl_met = 1
        PRINT *, ' clrmet 1 impose par l''option clrmark qui prime!'
      endif
      if (opt_clrmark .EQ. 1 ) nobox = -1

      return
      end

C
        INTEGER FUNCTION isdirect(clname)
CCC -------------------------------------------------------------------------
CCC                     FUNCTION ISDIRECT
CCC                     *****************
CCC
CCC    PURPOSE : This integer function returns the record length if clname
CCC              is a valid dimg file, it returns 0 either.
CCC
CCC    METHOD : Open the file and look for the key characters (@!01) for
CCC             identification.
CCC
CCC    AUTHOR : Jean-Marc Molines (Apr. 1998)
CCC -------------------------------------------------------------------------
CC 1.0 Declarations:
CC -----------------
      IMPLICIT NONE
      CHARACTER*(*) clname
      CHARACTER*4 VER
      CHARACTER*80 clheader
C
      INTEGER irecl
CC
CC 2.0 Look for VER:
CC ----------------
CC
      OPEN(110,FILE=clname,
     .         FORM   ='UNFORMATTED',
     .         ACCESS ='DIRECT',
     .         RECL   =88)
       READ(110,REC=1) VER,clheader,irecl
	print *,'VER',VER
      CLOSE(110)
C
      IF (VER .EQ. '@!01' ) THEN
       isdirect=irecl
      ELSEIF (VER(1:3) .EQ. 'CDF' ) THEN
       isdirect = -1
C      PRINT *,' NetCDF supported soon ... but not yet :( !'
C      STOP
      ELSE
       isdirect=0
      END IF
C
      RETURN
      END

