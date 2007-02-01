c module overlay.f:
c  Ensemble des sous-programmes utilises en cas d'overlay de courbes 
c     sur chart/coupe.
c Auteur: J.M. Molines
c          15/07/96
c --------------------------------------------------------------------
c ---------------------------------------------------------------------
c 
c Nom         :  OverReadData
c
c Parametres : xover : coordonnees x des points a tracer
c              yover : coordonnees y des points a tracer
c 
c
c Description :  
c              Lit le fichier de donnee f_overdata
c 
c ---------------------------------------------------------------------

      subroutine OverReadData(xover,yover,nover)

      implicit none

      include 'common.h'
      real*4 xover(*),yover(*)
      character*80 line

      integer i, nover

      open(55,file=f_overdata)
        nover=0
      do while(.true.)
        read(55,'(a)',end=100) line
         if (line(1:1) .ne. '#' ) then
          nover = nover +1
          read(line,*) xover(nover),yover(nover)
         end if
      enddo
100     continue
      close(55)
      return
      end

c---------------------------------------------------------------------

	subroutine OverTrace(xover,yover,nover)
      implicit none
 
      include 'common.h'
      real*4 xover(*),yover(*),lwidth
      real zco(4)
      real over_lw
      integer over_ic
      common /overl/ over_lw, over_ic
      integer errid, iclip

      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      real rr1, rr2

      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat

      real rl,rr,rb,rt,ul,ur,ub,ut
      real uval,vval, cufx, cufy
      integer l
 
      integer  nover,n, ncoul, ncoul0
        IF ( opt_overout .EQ. 1 ) OPEN(88, FILE=f_overout)
        call gqclip( errid,iclip,zco)
        call gsclip(1)
        call gqlwsc(errid,lwidth)
        call gqplci(errid,ncoul0)
        call gsplci(over_ic)

 	call gslwsc(over_lw)
      print *,' OVER TRACE : opt_noproj =',opt_noproj
       IF (opt_coupe .EQ. 1 ) THEN
          print *,' OVERLAY non prevu pour coupe'
          print *,' en chantier ...'
          opt_overdata = 0
          return
       
         call getset (rl,rr,rb,rt,ul,ur,ub,ut,l)
         call set  (rl,rr,rb,rt,
     .      1.,float(NXX),-prof_max,-prof_min,l)
          print *,'OVERLAY :', rl,rr,rb,rt,
     .      map_marg, prof_min,prof_max,l
       ENDIF
        ncoul=20
        call gflas1(55)
C       call gsplci(ncoul)
       IF (opt_noproj .EQ. 0 ) THEN
        call mapiq
        call mapit(yover(1),xover(1),0)
        n=0
        do while (n.lt.nover)
         n=n+1
         if (yover(n).ge.9999) then
C       ncoul=ncoul+1
Cif (ncoul .gt. ncol_pal) ncoul=20
C       call gsplci(ncoul)
          call mapiq
          n=n+1
          call mapit(yover(n),xover(n),0)
         endif
         call mapit(yover(n),xover(n),1)
         IF (opt_overout .EQ. 1 ) THEN
          CALL MAPTRN(yover(n),xover(n), uval,vval)
          print *,
     |  'OVEROUT ',n,xover(n),yover(n), uval,vval
          write(88, '(a,i3,1x,2G10.5,2f10.7)')
     |  'OVEROUT ',n,xover(n),yover(n), uval,vval
         END IF
        enddo
        call mapiq
       ELSE
        call frstpt (xover(1),yover(1))
        n=0
        DO WHILE ( n .LT. nover )
         n = n + 1
         if (yover(n).ge.9999) then
          call plotif(0.,0.,2)
          n = n + 1
          call  frstpt (xover(n),yover(n))
         endif
         call vector (xover(n),yover(n))
         IF (opt_overout .EQ. 1 ) THEN
          print *,
     |  'OVEROUT ',n,xover(n),yover(n), CUFX(xover(n)),CUFY(yover(n))
          WRITE(88,'(a,i3,1x,2G10.5,2f10.7)')
     | 'OVEROUT ',n,xover(n),yover(n), CUFX(xover(n)),CUFY(yover(n))
         ENDIF
        END DO
         call plotif(0.,0.,2)
       END IF
        call gslwsc(lwidth)
        call gsclip(iclip)
        call gsplci(ncoul0)
        call gflas2
       IF (opt_coupe .EQ. 1 ) THEN
        call set (rl,rr,rb,rt,ul,ur,ub,ut,l)
       ENDIF
C
       IF (opt_overout .EQ. 1 ) CLOSE (88)
      return
      end
c ---------------------------------------------------------------------
c
c Nom         :  OverMarkReadData
c
c Parametres : xoverm : coordonnees x des points a tracer
c              yoverm : coordonnees y des points a tracer
c              noverm : nombre de pts mark
c
c
c Description :
c              Lit le fichier de donnee f_overmark
c
c ---------------------------------------------------------------------

      subroutine OverMarkReadData(xoverm,yoverm,noverm)

      implicit none
      include 'common.h'
      real*4 xoverm(*),yoverm(*)
      character*80 line


      integer i, noverm

      open(56,file=f_overmark)
        noverm=0
      do while(.true.)
        read(56,'(a)',end=100) line
         if (line(1:1) .ne. '#' ) then
          noverm = noverm +1
          read(line,*) xoverm(noverm),yoverm(noverm)
         end if
      enddo
100   continue
      close(56)
      return
      end
c---------------------------------------------------------------------

        subroutine OverMarkTrace(xoverm,yoverm,noverm)
      implicit none
 
      include 'common.h'
      real*4 xoverm(*),yoverm(*)
      integer over_mk, mtype, over_mkic
      real mszsf,over_mksc, uval,vval
      common /overm/ over_mk, over_mksc, over_mkic
      integer errid
 
      integer i, noverm
        call gflas1(56)
        call gqmk(errid,mtype)
        call gqmksc(errid,mszsf)
 
        call gsmk(over_mk)
	call gsmksc(over_mksc)
        call gspmci(over_mkic)
	do i=1,noverm
         IF (xoverm(i) .lt. 9999 ) THEN
          IF ( opt_noproj .EQ. 1 ) THEN
           uval = xoverm(i)
           vval = yoverm(i)
          ELSE
 	  call maptrn(yoverm(i),xoverm(i),uval,vval)
          END IF
 	  call GPM(1,uval,vval)
         END IF
	enddo

        call gsmk(mtype)
	call gsmksc(mszsf)
        call gflas2
        return
        end
c
        subroutine ShowGrid(showfile,sid)
C Objet: Cette routine permet de tracer une grille en surimpression sur la
C carte. 
C Parametres: showfile = fichier bimg contenant la grille a visualiser
c             sid = segment id: entier utilise pour les gflash
c
	implicit none
	include 'parameter.h'
        include 'workarray.h'
        character*8 map_proj, map_zone
        real map_rlat,map_rlon
        real map_coord(4), map_marg(4), map_shift

        common /map/map_coord, map_marg, map_shift,
     .              map_zone, map_rlat,map_rlon,map_proj

	character*(*) showfile
	integer sid
C local
	real*4 grx(NXX,NYY),q,gspval,tag,tmpx,tmpy
	real*4 gry(NXX,NYY)
	equivalence (work_array,gry)
        real*4 px(NYY),py(NYY),x1,x2,y1,y2
        integer nxg,nyg,nzg,ntg,ndg,icod, i, j, k
	integer n, ii, jj
	character*80 dum

	call gflas1(sid)
	x1 =  map_coord(1)
	x2 =  map_coord(2)
	y1 =  map_coord(3)
	y2 =  map_coord(4)

	open(10,file=showfile, form='unformatted')
        read(10) dum
        read(10) dum
        read(10) dum
        read(10) dum
        read(10) nxg,nyg,nzg,ntg,ndg,icod
        read(10) q,q,q,q,gspval
        read(10) (q,k=1,nzg)
        read(10) tag
        read(10)((grx(i,j),i=1,nxg),j=1,nyg)
        read(10)((gry(i,j),i=1,nxg),j=1,nyg)
        close(10)
        print *,' Grille lue.'

C trace des lignes a j constant:
        do j=1,nyg
        ii=0
        do i=1,nxg
        tmpx=grx(i,j)
        tmpy=gry(i,j)
        if ( x1 .lt. tmpx .AND. tmpx .lt. x2) then
          if ( y1 .lt. tmpy .AND. tmpy  .lt. y2) then
           ii=ii+1
            px(ii)=tmpx
            py(ii)=tmpy
        endif
        endif
 
        enddo
        call mapiq
        call mapit(py(1),px(1),0)
        n=0
        do while (n.lt.ii)
        n=n+1
        if (py(n).ge.999) then
         call mapiq
         n=n+1
         call mapit(py(n),px(n),0)
        endif
        call mapit(py(n),px(n),1)
        enddo
        call mapiq
        enddo
C trace des lignes a i constant:
        do i=1,nxg
        jj=0
 
        do j=1,nyg
        tmpx=grx(i,j)
        tmpy=gry(i,j)
        if ( x1 .lt. tmpx .AND. tmpx .lt. x2) then
          if ( y1 .lt. tmpy .AND. tmpy  .lt. y2) then
           jj=jj+1
            px(jj)=tmpx
            py(jj)=tmpy
        endif
        endif
        enddo
         call mapiq
        call mapit(py(1),px(1),0)
        n=0
        do while (n.lt.jj)
        n=n+1
        if (py(n).ge.999) then
         call mapiq
         n=n+1    
         call mapit(py(n),px(n),0)
        endif
        call mapit(py(n),px(n),1)
        enddo
        call mapiq
        enddo

	call gflas2
	return
	end








