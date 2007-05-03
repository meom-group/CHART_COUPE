c --------------------------------------------------------------------
c 
c Programme      :  CHART
c Fichier        :  chart.f
c 
c Auteur         :  Eric Brown
c
c Description    :  Fichier principal du programme CHART.
c                   Contient les differentes etapes de creation d'un 
c                   dessin.
c 
c ---------------------------------------------------------------------


      program chart

      implicit none


c     declaration des variables ---------------------------------------

      include 'common.h'
      include 'color.h'
      include 'mapncar.h'

      integer   couche(NA), couche_max, tstep(Ntime), tstep_max, cl_dim
      integer ct_dim
      real    conv(9)
      common  /calc/conv,couche, couche_max, tstep, tstep_max,
     .              cl_dim, ct_dim
      save    /calc/


      integer k,t, lnblnk
      integer ncol

      integer iargc
      integer narg
      integer mark_step
      integer map_flag
      integer ier,PrintBimgStructure
	common /markc/ mark_step

      real    vecdata (NXX,NYY,2)
      real    cntdata (NXX,NYY)
      real    clrdata (NXX,NYY)
      real    intdata (NXX,NYY)
      character*80 cmd
      
      real    limit(NBOXMAX+1)
      real xover(NOVERPTS), yover(NOVERPTS)
      real xoverm(NOVERPTS), yoverm(NOVERPTS)
      integer nover, noverm

      TYPE( bimgfile) bimgclr, bimgcnt, bimgvec(3), bimgint
      TYPE( bimgfile) bimgzlevel

C     save /str/
C     data strcount/0/
C     save /strr/
C     data strcountr/0/

c     declaration des variables -- fin ---------------------------------

	include 'chart_version.h'

c     Nombre d'arguments sur la ligne de commande.
c     Si aucun argument n'est donne, imprimer les options possibles
c     et sortir.

      strcount = 0
      strcountr = 0
      narg = iargc()

      if (narg.eq.0) then
         call PrintOptions ()
         stop
      endif
	opt_chart = 1
	opt_coupe = 0

c     configuration par defaut et lecture des arguments de la ligne de
c     commande

      call InitValTable()
      call SetDefaults()
C     call CdfInit

c     Si il n'y a qu'un seul argument, le considerer comme un fichier
c     de donnees et chercher une configuration par defaut.

      if (narg.eq.1) then
         call getarg (1,f_icoddata)
         call ReadIcodConfig()
      else 
         call ReadArgs (narg)
      endif 


      call OpenNCAR_GKS()

      call OpenAndVerifyFiles (bimgclr, bimgzlevel, bimgcnt, bimgvec)
      call CheckOptions (bimgclr, bimgcnt, bimgvec)
      call SetValTable (bimgclr, bimgcnt, bimgvec)

      if (opt_pal.eq.1) then
         call ReadPaletteFile (filepal,ncol, opt_reverse)
      else
         call DefaultPalette (ncol, opt_reverse,opt_pal)
      endif

      


c     Initialisation de la carte --------------------------------------

      call InitMap()

c     Creation des elements fixes de la carte

      call DessineMeridiens (4)
      if (opt_ocean.eq.1) call DessineOceans (1)
      if (opt_map.eq.1)    call DessineContinents (5)
      if (opt_showgrid.eq.1) call ShowGrid (f_showgrid, 57)

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
      
  
c     on boucle sur les pas de temps --------------------------------

      do t = 1, tstep_max

         if (tstep(t).eq.1) then
            
c     on boucle sur les couches -------------------------------------

      do k=1,couche_max
C JMM: La ligne suivante est ajoutee en correctif du probleme des
C      chart sur plusieurs niveaux dans un seul plot, ou auparavant,
C      les "label-box" aparaissaient sur les niveaux profonds ...
C
        call InitMap()

         if (couche(k).eq.1) then
	if (opt_single.eq.0) then
	  kclr = k
	  kcnt = k
	  kvec = k
	endif
	 if (opt_dep.eq.1) then
          kclr=kdessus
          kcnt=kdessus
          kvec=kdessus
         endif

c     preparation des donnees 

            if (opt_clrdata.eq.1) then
               bimgclr%lclr=.true. ; bimgclr%lcnt=.false.
               call BimgReadData (clrdata,bimgclr,map_coord,t,kclr,cl_dim)
	        if (bimgclr%spval0 ) bimgclr%spval = defspval
               if (opt_vorticity.eq.1) then
                  call  CalcVorticite (clrdata, bimgclr, map_coord)
               endif 
               call SetClrCNESday (bimgclr%time)
            endif 
C         ier=PrintBimgStructure(bimgclr)
            
            if (opt_contours.eq.1) then
               bimgcnt%lcnt=.true. ; bimgcnt%lclr=.false.
               call BimgReadData (cntdata,bimgcnt,map_coord,t,kcnt,ct_dim)
               call SetClrCNESday (bimgcnt%time)
	        if (bimgcnt%spval0) bimgcnt%spval = defspval
               if (opt_vorticity.eq.1) then
                  call  CalcVorticite (cntdata, bimgcnt, map_coord)
               endif 
            endif 
            
            if (opt_vectors.eq.1) then
               PRINT *,'Vectors are being prepared ..'
               call VectorPrepareData (bimgvec, bimgclr, clrdata, 
     .                                 vecdata,map_coord, t,kvec)
            endif 
	

            call UpdateValTable (t, kclr, kcnt, kvec, bimgclr, bimgcnt, bimgvec)

c        creation du dessin

            if (opt_color.eq.1) then
                  if (opt_clrmark .EQ. 1 ) THEN
                   call ClrGetMark()
                  endif
               if (opt_noint.eq.0) then
                  bimgint = bimgclr
                  call interp (intdata, clrdata, bimgint)

                  call ClrGetLimits (intdata,bimgint,limit,ncol)
                  if (opt_clrlout.eq.1) then
                     call WriteLimits (f_clrlout, limit, ncol+1)
                  endif 

                  call ColorMap (intdata, bimgint, limit, ncol)
               else 
                  call ClrGetLimits (clrdata,bimgclr,limit,ncol)
                  if (opt_clrlout.eq.1) then
                     call WriteLimits (f_clrlout, limit, ncol+1)
                  endif 
                  call ColorMap (clrdata, bimgclr, limit, ncol)
               endif 
               
            endif 

            if (opt_contours.eq.1) then
	       if (opt_noproj.eq.1) then
                 map_flag=0
	       else
                map_flag=1
	       endif

c       if (opt_contgrid.eq.3) then
           if (bimgcnt%grid.eq.3) then
	        map_flag=6
	       endif

               call TraceContours (cntdata,bimgcnt,map_coord,map_flag)
            endif 

            if (opt_vecnotr.eq.0.and.opt_vectors.eq.1) then
               call VectorTrace (bimgvec, bimgclr, clrdata, 
     .                           vecdata, ncol, map_coord)
            endif 
	   


	   if (opt_overdata.eq.1) then
              call OverTrace(xover,yover,nover)
	   endif
	   if (opt_overmark.eq.1) then
	      call OverMarkTrace(xoverm,yoverm,noverm)
	   endif
	   
            call gflas1(6)
            call AjouteTexte ()
            
            if ((opt_palbar.eq.1).and.(opt_color.eq.1)) then
               call AjoutePalette (limit, ncol)
            endif 

c     CECI EST UNE HORRIBLE PATCHE (pour Bernard)

            if (opt_marks.eq.1) then
               call mark (bimgclr%d_xgrid, bimgclr%d_ygrid,
     $                    bimgclr%nxfile, bimgclr%nyfile,mark_step)
            endif 
            call gflas2
         

c     assemblage des differentes parties du dessin

            if (opt_ocean .eq. 1)  call gflas3(1)
            if (opt_color.eq.1.and.opt_overclr.eq.0)   call gflas3(2)
            if (opt_contours.eq.1) call gflas3(3)
            if ((opt_vecnotr.eq.0).and.( opt_vectors.eq.1))  call gflas3(7)
            if (opt_map.eq.1)      call gflas3(5)
	    if (opt_overdata.eq.1) call gflas3(55)
	    if (opt_overmark.eq.1) call gflas3(56)
            if (opt_showgrid.eq.1) call gflas3(57)
            call gflas3(4)
            call gflas3(6)
            call frame
         endif 
	if (bimgclr%spval0) bimgclr%spval = 0
	if (bimgcnt%spval0) bimgcnt%spval = 0
      enddo    ! couches
      endif 
      enddo    ! pas de temps

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

C     call system ('/bin/rm GNFB*')

      print *,'Programme chart termine normalement.'
      print *,' '
      print *,' '
      call flush(6)
      
      end


