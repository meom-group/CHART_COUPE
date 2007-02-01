c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe, anim
c Fichier        :  readpal.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
C Refonte complete : Jean-Marc Molines 16/01/2002
c Mise a Jour    :  
c
c Description    :  fonction de lecture d'un fichier de palette
c 
c ---------------------------------------------------------------------

      subroutine ReadPaletteFile(filpal,ncol, rev)

      implicit none
      include 'common.h'
      include 'color.h'

      character*256 filpal,line
      integer ncol
      integer*2 rev

      real    wk1(3),wk2(3), fore(3)
      integer i, j, k, icol1, icol2, ierr
      real    incr(3), r,g,b
      integer state, length
      integer isteps
      integer lnblnk
      integer index, oldindex
      integer icol0,  icolstp

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(152)
      save   /marks/

      open (50,file=filpal,iostat=state,status='old') 
      
      i = lnblnk (filpal)
c**      print *,'Lecture de la palette : ',filpal(1:i)
      call  flush(6)


      index = -1
      fore (1) = 0.0
      fore (2) = 0.0
      fore (3) = 0.0
C
C ... Look for index 1
      DO WHILE ( index .LT. 1 )
	  READ(50,'(a)',end=6) line
      if (line(1:1) .NE. '#' )  THEN
      length = lnblnk(line)
       if ( length .NE. 0 )   THEN
         read (line,*) index,(wk1(k),k=1,3)
       endif
      endif
      END DO
      fore(1) = wk1(1)
      fore(2) = wk1(2)
      fore(3) = wk1(3)
6     continue
      REWIND(50)
C
C ... initialize all colors to fore
C
      DO i = 0,  COLOR_NRES
       call gscr(1,i, fore(1), fore(2), fore(3))
      END DO

c     lecture de la palette
      
C
C ... read and affect colors to the index in the palette
C ... set and update the marks_index table
C 
      index = -1
      marks_count = 0
      ncol = 0
C
      DO WHILE ( index .LE. NBOXMAX )
	  READ(50,'(a)',end=7) line
      if (line(1:1) .NE. '#' )  THEN
      length = lnblnk(line)
       if ( length .NE. 0 )   THEN
         read (line,*) index,(wk1(k),k=1,3)
         call gscr(1,index, wk1(1), wk1(2), wk1(3))
         if (index .GE. COLOR_NRES ) THEN
          ncol = ncol +1
          marks_count = marks_count + 1
          marks_index(marks_count) = index
         end if
       endif
      endif
      END DO
7     continue
      close(50)
C ... fill colors between index by interpolatted color
      DO i = 1, marks_count -1
       icol1 = marks_index(i)
       icol2 = marks_index(i+1)
       isteps = icol2-icol1
       IF ( isteps .NE. 1 ) THEN
        call gqcr(1,icol1,0,ierr,wk1(1), wk1(2), wk1(3))
        call gqcr(1,icol2,0,ierr,wk2(1), wk2(2), wk2(3))
         incr(1)= (wk2(1) - wk1(1) ) / isteps
         incr(2)= (wk2(2) - wk1(2) ) / isteps
         incr(3)= (wk2(3) - wk1(3) ) / isteps
        DO j=icol1+1, icol2-1
         wk2(1)=wk1(1) + (j -icol1)* incr(1)
         wk2(2)=wk1(2) + (j -icol1)* incr(2)
         wk2(3)=wk1(3) + (j -icol1)* incr(3)
         ncol = ncol +1
         call gscr(1,j,wk2(1),wk2(2),wk2(3) )
        END DO
       END IF
      END DO
C
C ... if -rev option swap foreground (1) and background (0) color
      IF ( rev .EQ. 1 ) THEN
        call gqcr(1,1,0,ierr,wk1(1), wk1(2), wk1(3))
        call gqcr(1,0,0,ierr,wk2(1), wk2(2), wk2(3))
C
        call gscr(1,0,wk1(1), wk1(2), wk1(3))
        call gscr(1,1,wk2(1), wk2(2), wk2(3))
      END IF
     
      marks_index(marks_count+1) = ncol+COLOR_NRES
C     PRINT *,' READPAL : MARKS_COUNT NCOL == ', marks_count,ncol
      if (opt_palout.eq.1) then
       open(99,file='used_palette')
       write(99,'(a)')'# fichier palette genere automatiquement par -palout'
       write(99,'(a,i4,a)')'# les ',COLOR_NRES,' premieres couleurs sont reservees'
       do i=0,COLOR_NRES-1
        call gqcr(1,i,0,ierr,r,g,b)
        write(99,'(i3,2x,3f10.4)')i,r,g,b
       enddo
        write(99,'(a)')'# Les couleurs suivantes sont les couleurs pour les donnees' 
C       do i=1,ncol
C        write(99,'(i3,2x,3f10.4)'),i-1+COLOR_NRES,rgbw(1,i),  rgbw(2,i), rgbw(3,i)
C       enddo
        DO i=COLOR_NRES, COLOR_NRES+ncol-1
        call gqcr(1,i,0,ierr,r,g,b)
        write(99,'(i3,2x,3f10.4)')i,r,g,b
        END DO
       close(99)
      endif         

      
      return
      end       

c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe, anim
c Fichier        :  readpal.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
c Mise a Jour    :  
c
c Description    :  fonction de lecture d'un fichier de palette
c 
c ---------------------------------------------------------------------

      subroutine ReadPaletteFile0(filpal,ncol, rev)

      implicit none
      include 'color.h'

      character*256 filpal,line
      integer ncol
      integer*2 rev

      real    wk1(3),wk2(3), fore(3)
      integer i, j, k, icol1, icol2
      real    incr(3)
      integer state, length
      integer isteps
      integer lnblnk
      integer index, oldindex
      integer icol0,  icolstp

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(152)
      save   /marks/

      open (50,file=filpal,iostat=state,status='old') 
      
      i = lnblnk (filpal)
c**      print *,'Lecture de la palette : ',filpal(1:i)
      call  flush(6)


      fore (1) = 0.0
      fore (2) = 0.0
      fore (3) = 0.0


c     lecture de la palette
      
      ncol     = 1
      icol1    = 1
      oldindex = -1

c     Boucle sur les couleurs reservees et comble les index
c     indefinis avec du noir.
      
 20   read (50,'(a)', end=10)line
         if (line(1:1).eq.'#') goto 20 ! skip comments
      
         length = lnblnk(line)
         if (length.eq.0) goto 20 ! skip empty lines
      
         read (line,*,end=10) index,(wk1(k),k=1,3)
         
         if (index.eq.1) then
            fore(1) = wk1(1)
            fore(2) = wk1(2)
            fore(3) = wk1(3)
         endif 

         do while ((oldindex.ne.index-1).and.(oldindex.lt.COLOR_NRES))
            oldindex = oldindex + 1

            call gscr(1,oldindex, fore(1), fore(2), fore(3))
         enddo 

         if ((rev.eq.1).and.(index.eq.0)) then
            call gscr(1,1,wk1(1),wk1(2),wk1(3))
         elseif ((rev.eq.1).and.(index.eq.1)) then
            call gscr(1,0,wk1(1),wk1(2),wk1(3))
         else 
            call gscr(1,index,wk1(1),wk1(2),wk1(3))
         endif 

         oldindex = index

         if (index.lt.COLOR_NRES) then
            goto 20
         endif 

 10   continue




c     initialise le premier index de la table contenant
c     les index de chaque couleur definie dans le fichier

      icol1 = index
      marks_index(1) = index
      marks_count = 1
      i = 2
      
      
      
 60   read (50,'(a)', end=50)line
      if (line(1:1).eq.'#') goto 60 ! skip comments
      
      length = lnblnk(line)
      if (length.eq.0) goto 60      ! skip empty lines
      
      read (line,*,end=50) icol2,(wk2(k),k=1,3)
      
      
      marks(i) = 1
      marks_count = marks_count + 1
      marks_index(marks_count) = icol2
      
      
      if (icol1.ne.icol2) then
         ncol = ncol + 1
         
         isteps = icol2 - icol1
         
         if (isteps.gt.1) then            
            incr(1) = (wk2(1) - wk1(1))/float(isteps)
            incr(2) = (wk2(2) - wk1(2))/float(isteps)
            incr(3) = (wk2(3) - wk1(3))/float(isteps)
            
            do j = 1, isteps-1
               ncol = ncol + 1
               
               wk1(1) = wk1(1) + incr(1)
               wk1(2) = wk1(2) + incr(2)
               wk1(3) = wk1(3) + incr(3)
               
               call gscr(1,(icol1 + j), wk1(1),wk1(2),wk1(3))
            enddo 
               
            call gscr(1,icol2,wk2(1),wk2(2),wk2(3))            
         else 
            call gscr(1,icol2,wk2(1),wk2(2),wk2(3))
         endif 
         
         icol1 = icol2
         wk1(1) = wk2(1)
         wk1(2) = wk2(2)
         wk1(3) = wk2(3)
         
         i = i + 1
      endif 
      goto 60
      
      
      
 50   continue
      
      marks_index(marks_count+1) = ncol+COLOR_NRES
C     PRINT *,' READPAL : MARKS_COUNT NCOL == ', marks_count,ncol

      close(50)

      
      return
      end       


c ---------------------------------------------------------------------
c 
c Nom              :  DefaultPalette
c
c Parametres       :  ncol = nombre de couleurs dans la palette (output)
c                     rev  = si rev = 1 on inverse le backgrd et foregrd
c                     kpal = indice de palette.
c                       - kpal = 0 : palette standard par default 14 coul
c			  kpal = 1 : palette lue sur fichier on ne passe 
c                                    pas ici
c                         kpal = 2 : palette de gris par default
c                         kpal = 3 : palette pour l'option cntshade
c                         kpal = 4 : palette pour option vecshade
c                         kpal = 5 : palette de type jetpal avec ncol
c Description      :  Defini la table de couleur utilisee dans chart et coupe.
c                     Il y a COLOR_NRES reservee pour le systeme
c                     les index a partir de COLOR_NRES + 1 sont pour l'utilisateur
c                     les index reserves sont: (definis dans color.h)
c
c       	(COLOR_BACKGROUND=0)
c       	(COLOR_FOREGROUND=1)
c       	(COLOR_CONTINENT=2)
c       	(COLOR_SPVAL=3)
c       	(COLOR_OCEAN=4)
c       	(COLOR_ISOCONTOUR=5)
c      	 	(COLOR_VECTOR=6)
c       	(COLOR_CONTINENT_PERIM=7)
     
c 
c ---------------------------------------------------------------------

      subroutine DefaultPalette (ncol,rev,kpal)

      implicit none
      
      include 'common.h'
      include 'color.h'
      integer ncol, i, j, ierr
      integer*2 kpal

c     145 couleurs pour le moment + 5 premiers index
c     meme palette par defaut que edpal
      
      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(152)
      save   /marks/
    
      integer icol0,icol1,icolstp

      integer*2 rev

      real rgbw(3,NBOXMAX)
      real rgbv(3,14)
      real rgbg(3,11)
      real rgbsh(3,2)
      real rgbvec(3,2)
      real r,g,b

C
C Palette de couleur par defaut, jetpal14 (kpal=0)
C
      data rgbv / 
     +  0,    0, 0.9,
     +  0,    0.4500,    1.0000,
     +  0,    0.6000,    1.0000,
     +  0,    0.7500,    1.0000,
     +  0,    0.9000,    1.0000,
     +  0.2500,    1.0000,    1.0000,
     +  0.5000,    1.0000,    0.7500,
     +  0.7500,    1.0000,    0.5000,
     +  0.9000,    1.0000,    0.4500,
     +  1.0000,    1.0000,         0,
     +  1.0000,    0.7500,         0,
     +  1.0000,    0.5000,         0,
     +  1.0000,    0.2500,         0,
     +  0.9000,         0,         0/
C
C Palette de gris par defaut si option -greyscale (kpal=2)
C
      data rgbg /
     + 0.142857 , 0.142857 , 0.142857,
     + 0.222222 , 0.222222 , 0.222222,
     + 0.301587 , 0.301587 , 0.301587,
     + 0.380952 , 0.380952 , 0.380952,
     + 0.460317 , 0.460317 , 0.460317,
     + 0.539683 , 0.539683 , 0.539683,
     + 0.619048 , 0.619048 , 0.619048,
     + 0.698413 , 0.698413 , 0.698413,
     + 0.777778 , 0.777778 , 0.777778,
     + 0.857143 , 0.857143 , 0.857143,
     + 0.936508 , 0.936508 , 0.936508/

C
C Palette pour l'option shade (kpal=3)
C
      data rgbsh /
     +  0.9 , 0.9, 0.9 ,
     +  1,    1, 1/
C
C Palette pour l'option vecshade (kpal=4)
C la couleur est blanche, on utilise seulement la couleur ocean(4)
C
      data rgbvec /
     + 1., 1., 1.,
     + 1., 1., 1. /


c**      print *,'palette par defaut'


      if (rev.eq.0) then
         call gscr(1,COLOR_BACKGROUND,1.,1.,1.) ! background 
         call gscr(1,COLOR_FOREGROUND,0.,0.,0.) ! foreground
      else 
         call gscr(1,COLOR_BACKGROUND,0.,0.,0.) ! background 
         call gscr(1,COLOR_FOREGROUND,1.,1.,1.) ! foreground
      endif 

c     continents: index 2
c-------------------------

      if (kpal.lt.2) then
      call gscr(1,COLOR_CONTINENT,0.5,0.5,0.5)
      else
      call gscr(1,COLOR_CONTINENT,0.502,0.502,0.502)
      endif

c     spval: index 3
c-------------------

      if (kpal .NE. 3 ) then
      call gscr(1,COLOR_SPVAL,0.9,0.9,0.9)
       else
      call gscr(1,COLOR_SPVAL,1.,1.,1.)
      endif

c     ocean: index 4
c-------------------

      if (kpal.ne.4) then
      call gscr(1,COLOR_OCEAN,1.,1.,1.)
      else
      call gscr(1,COLOR_OCEAN,0.9,0.9,0.9)
      endif

c     isocontours 1: index 5
c --------------------------
      call gscr(1,COLOR_ISOCONTOUR,0.,0.,0.)

c     vecteurs : index 6
c ----------------------
      call gscr(1,COLOR_VECTOR,0.,0.,0.)

c     perimetre continents : index 7
c ----------------------
      call gscr(1,COLOR_CONTINENT_PERIM,0.,0.,0.)

c     index libres de 8 a COLOR_NRES - 1
c---------------------------

      do i=8,COLOR_NRES-1
         call gscr (1,i,0.,0.,0.)
      enddo

	print *,' Palette interne utilisee # :',kpal
c  Tranfert des couleurs voulues dans les index utiles
c-----------------------------------------------------
	if (opt_prev.eq.0) then
	 icol0=0
	 icolstp= 1
        else
         icol0=ncol+1
         icolstp = -1
        endif
C
	if (kpal.eq.0) then
         ncol=14
        if (opt_prev.eq.1) icol0=ncol+1
         marks_count = 14
           do i=1,ncol
            do j=1,3
             rgbw(j,i)=rgbv(j,icol0+icolstp*i)
            enddo
           enddo
        else if (kpal.eq.2 ) then
         ncol=11
        if (opt_prev.eq.1) icol0=ncol+1
         marks_count = 11
           do i=1,ncol
            do j=1,3
             rgbw(j,i)=rgbg(j,icol0+icolstp*i)
            enddo
           enddo
        else if (kpal.eq.3 ) then
         ncol =2
        if (opt_prev.eq.1) icol0=ncol+1
         marks_count = 2
           do i=1,ncol
            do j=1,3
             rgbw(j,i)=rgbsh(j,icol0+icolstp*i)
            enddo
           enddo
        else if (kpal.eq.4 ) then
         ncol =2
        if (opt_prev.eq.1) icol0=ncol+1
         marks_count = 2
           do i=1,ncol
            do j=1,3 
             rgbw(j,i)=rgbvec(j,icol0+icolstp*i)
            enddo
           enddo
        else if (kpal.eq.5 ) then
	call JetPal(rgbw,ncol_pal)
	ncol=ncol_pal
	marks_count = ncol_pal

        else if (kpal.eq.6 ) then
	call JetPal (rgbw,ncol_pal)
	ncol=ncol_pal
	marks_count = ncol_pal
	  do i =2, ncol_pal,2
	   rgbw(1,i)=0.
	   rgbw(2,i)=0.
	   rgbw(3,i)=0.
          enddo
        else
         print *,'Palette par defaut ', kpal,' inconnue !'
	 stop
	endif

c     indice 0 dedouble

      do i=1,ncol
         call gscr(1,i+COLOR_NRES-1, rgbw(1,i),  rgbw(2,i), rgbw(3,i))
         marks_index(i) = i+COLOR_NRES-1
      enddo 

      marks_index(marks_count+1) = ncol+COLOR_NRES

      if (opt_palout.eq.1) then
	open(99,file='used_palette')
	write(99,'(a)')'# fichier palette genere automatiquement par -palout'
        write(99,'(a,i4,a)')'# les ',COLOR_NRES,' premieres couleurs sont reservees'
	do i=0,COLOR_NRES-1
	call gqcr(1,i,0,ierr,r,g,b)
	write(99,'(i3,2x,3f10.4)')i,r,g,b
	enddo
	write(99,'(a)')'# Les couleurs suivantes sont les couleurs pour les donnees'
	
	do i=1,ncol
	write(99,'(i3,2x,3f10.4)')i-1+COLOR_NRES,rgbw(1,i),  rgbw(2,i), rgbw(3,i)
	enddo
	close(99)
      endif

      return
      end


	subroutine JetPal(rgb,m)
c---------------------------------------------
c Cette routine cree une palette de type JetPal (matlab)
c avec ncol_pal couleurs.
c JMM 6/02/98
c----------------------------------------------
	implicit none
	include 'color.h'
	integer m, i, k, n, n2
	real rgb(3,NBOXMAX), x(NBOXMAX),y(NBOXMAX),e(NBOXMAX)
	
C
	if (m.gt.NBOXMAX) then
	 print *,'Probleme dans Jetpal : trop de couleurs, maxi possible:',NBOXMAX
	 stop
	endif
	n=nint(m/4.)
cif (mod(n,2).ne.0) n=n+1
	n2=n/2
C initialisation des tableaux de travail
	do i=1,n
	x(i)=float(i)/n
	enddo
C
	do i=1,n2
	y(i)=(float(i+n2-1))/n
	enddo
c
	do i=1,n
	e(i)=1.
	enddo
C
c Remplissage des RED
c
	do i=1,n2+n
	rgb(1,i)=0.
	enddo
c
	k=0
	do i=n2+n+1,n2+2*n
	k=k+1
	rgb(1,i)=x(k)
	enddo
c
        k=0
        do i= n2+2*n+1,n2+3*n
        k=k+1
        rgb(1,i)=e(k)
        enddo

        k=n2
        do i= n2+3*n +1, m
        rgb(1,i)=y(k)
        k=k-1
        enddo
c
c Remplissage des GREEN
c
        do i=1,n2
        rgb(2,i)=0.
        enddo
        k=0  
        do i=n2+1, n2+n
        k=k+1
        rgb(2,i)=x(k)
        enddo

        k=0
        do i=n2+n+1, n2+2*n
        k=k+1
        rgb(2,i)=e(k)
        enddo

        k=n+1
        do i=n2+2*n+1, n2+3*n
        k=k-1
        rgb(2,i)=x(k)
        enddo

        k=0
        do i=n2+3*n+1, m
        rgb(2,i)=0.
        enddo
c
c Remplissage des BLUES
c

        do i=1,n2
        rgb(3,i)=y(i)
        enddo
        k=0  
        do i=n2+1, n2+n
        k=k+1
        rgb(3,i)=e(k)
        enddo
        k=n+1
        do i=n2+n+1, n2+2*n
        k=k-1
        rgb(3,i)=x(k)
        enddo
        k=0  
        do i=n2+2*n+1,m
        rgb(3,i)=0.
        enddo

	return
	end





	
