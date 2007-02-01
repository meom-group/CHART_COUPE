c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  isocontours.f
c 
c Auteur         :  Eric Brown
c
c Description    :  Contient toutes les fonctions de dessin
c                   d'isocontours.
c 
c ---------------------------------------------------------------------




c ---------------------------------------------------------------------
c 
c Nom         :  TraceContours
c
c Parametres  :  fld     - tableau de donnees
c                nx,ny   - tailles en x et y du tableau de donnees
c                coords  - coordonnees de la zone 
c                map     - indique a NCAR si les isocontours doivent
c                          etre projetes sur une carte.
c                          1 - oui,  0 - non
c
c Description :  
c 
c ---------------------------------------------------------------------


      subroutine TraceContours (fld, bimg, coords,map_flag)

      implicit none

      include 'common.h'
      include 'color.h'
      include 'mapncar.h'

      TYPE( bimgfile ) bimg

      real    coords(4)
      integer nx, ny
      real    fld(NXX, NYY)
      integer n_contours
      real    contour(200)
      integer lnblnk

      external drawcl
      external cpchil

      integer ccolor

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(146)
      save   /marks/

      integer l
      real    rl,rr,rb,rt, ur,ul,ut,ub

      real    check_lbl

      integer iclu, iclv,icln
      integer*4 state
      integer   map_flag
      real spv
      real vmin, vmax
      real cntval
      integer count, length
      integer errind

      character*256 strIn, strOut

      integer imin, imax, jmin, jmax
      TYPE( bimgfile ) spembimg
      common /spem/ spembimg, imin,imax,jmin,jmax
      save   /spem/




      print *, 'Calcul des isocontours'


      call gflas1(3)

      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval


      call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
      


      call FindMinMax (bimg, fld, vmin, vmax)

      if (opt_minc.eq.0) ct_cmn = vmin
      if (opt_maxc.eq.0) ct_cmx = vmax

      if (((opt_minc.eq.1).or.(opt_maxc.eq.1)).and.
     .     (opt_contint.eq.0)) then
         ct_cis = (ct_cmx - ct_cmn) / float(ct_cls)
      endif 

C     call SetCntMin (ct_cmn)
C     call SetCntMax (ct_cmx)
      call SetCntMin (vmin)
      call SetCntMax (vmax)
     
      call gqtxci (errind, ccolor)
      call gstxci (ct_llc)


      call cpseti ('SET - do SET call flag             ', 0)
      call cpseti ('MAP - mapping flag                 ', map_flag)

	if (bimg%grid .eq.3 .and. opt_chart .eq. 1) then
      call cpsetr ('XC1 - X coordinate at index 1      ', float(imin))
      call cpsetr ('XCM - X coordinate at index max    ', float(imax))
      call cpsetr ('YC1 - Y coordinate at index 1      ', float(jmin))
      call cpsetr ('YCN - Y coordinate at index max    ', float(jmax))
	else
      call cpsetr ('XC1 - X coordinate at index 1      ', coords(1))
      call cpsetr ('XCM - X coordinate at index max    ', coords(2))
      call cpsetr ('YC1 - Y coordinate at index 1      ', coords(3))
      call cpsetr ('YCN - Y coordinate at index max    ', coords(4))
	endif

      call cpsetc ('HLT - high low label text          ', ' ')
      call cpsetr ('SPV - special value                ', spv)
      call cpsetr ('SFS - scale factor                 ', 10.**ct_sfs)

      call cpseti ('CLS - contour level selection      ', ct_cls)
      call cpsetr ('CIS - contour interval             ', ct_cis)

      call cpsetr ('CMN - contour minimum              ', ct_cmn)
      call cpsetr ('CMX - contour maximum              ', ct_cmx)
      call cpsetr ('CWM - character width multip.      ', ct_cwm)
      call cpseti ('NOF - Numeric Omission Flag        ', 7)

      call cpseti ('LLP - Line Label Positioning       ', ct_llp)
      call cpseti ('LLB - Line Label Box Flag          ', ct_llb)
      call cpsetr ('LLS - Line Label Size              ', ct_lls)
      call cpseti ('LIS - Label Interval Specifier     ', ct_lis)
      call cpseti ('LBC - Label Box Color index        ', ct_lbc)

      call cpseti ('ILB - Information Label Box flag   ', ct_ilb)
      call cpseti ('ILC - Information Label Color index', ct_ilc)
      call cpsetr ('ILL - Information Label Line width ', ct_ill)
      call cpsetr ('ILS - Information Label Size       ', ct_ils)
      call cpseti ('ILP - Information Label Positioning', ct_ilp)

      call cpsetr ('RC1 -                              ', ct_rc1)
      call cpsetr ('RC2 -                              ', ct_rc2)

 
c     support des variables dans le texte d'information

      call FilterQuotes (ct_ilt, strIn)
      call ParseString (strIn, strOut)
      length = lnblnk (strOut)
      if (length.gt.100) length = 100
      ct_ilt = strOut(1:length)
      

      call cpsetc ('ILT - information label text       ', ct_ilt)

      call cpsetr ('ILX - information label X coord    ',(ct_ilx-rl)/
     .                                                    (rr-rl))    
      call cpsetr ('ILY - information label Y coord    ',(ct_ily-rb)/
     .                                                   (rt-rb))      
      call cpsetr ('ORV - Out-of-Range Value           ', 1.e12)

      call cprect (fld, NXX, nx, ny, rwrk, SZ_RWRK, iwrk, SZ_IWRK)


c     option : contours tires d'un fichier

      if (opt_contlim.eq.1) then
         n_contours = 0

         open (20,file=f_cntlim,iostat=state,status='old')
         if (state.ne.0) then
            l = lnblnk (f_cntlim)
       print *,'erreur : ouverture du fichier impossible:',f_cntlim(1:l)
            stop
         endif
 
c     lecture du fichier de contours

 20      continue
         read(20, *, end=10) contour(n_contours+1)
         n_contours = n_contours + 1
         goto 20

 10      continue
         close (20)


         call cpseti ('CLS - Contour Level Selection ', 0)
         call cpseti ('NCL - Number of Contour Levels', n_contours)

         do iclv = 1, n_contours
            call cpseti ('PAI - parameter array index  ', iclv)
            call cpsetr ('CLV - contour level     ', contour(iclv))
            call cpseti ('CLC - contour line color', COLOR_ISOCONTOUR)
            call cpseti ('LLC - Line Label Color Index ', ct_llc)

            check_lbl = float(iclv-1) / float(ct_lis)

c            if ((check_lbl - aint(check_lbl)).eq.0.0) then
            if (amod( float(iclv-1),float(ct_lis)).eq.0.0) then
               print *,'line label for:',contour(iclv)

               call cpsetr ('CLL - contour line line width',ct_cll2)
               call cpseti ('CLU - contour level use', 3)
            else 
               call cpsetr ('CLL - contour line line width',ct_cll1)
               call cpseti ('CLU - contour level use', 1)
            endif              
         enddo 
 

c     contours automatiques

      else
         count = 1

         call cppkcl (fld, rwrk, iwrk)
         call cppklb (fld, rwrk, iwrk)
         call cpgeti ('NCL - number of contour levels', icln)
         
         do iclv = 1, icln
            call cpseti ('PAI - parameter array index', iclv)

            if (opt_contint.eq.0) then
               if (count.eq.1) then
                  call cpseti ('CLU - contour level use', 3)
                  count = count + 1
               else 
                  if (count.eq.ct_lis) then
                     count = 1
                  else 
                     count = count + 1
                  endif 
               endif 
            endif 

            call cpgeti ('CLU - contour level use', iclu)
            
            call cpseti ('CLC - contour line color',COLOR_ISOCONTOUR)
            call cpseti ('LLC - Line Label Color Index ', ct_llc)
   
            if (iclu.eq.3) then
               call cpsetr ('CLL - contour line line width',ct_cll2)
            else 
               call cpsetr ('CLL - contour line line width',ct_cll1)
            endif 
         enddo 

         if (opt_cntlout.eq.1) then
            open(88,file=f_cntlout,form='formatted')

            do iclv = 1, icln                 
               call cpseti ('PAI - parameter array index', iclv)
               call cpgetr ('CLV - contour level value  ', cntval)
               write(88,'(e15.5)') cntval
            enddo 
            
            close(88)               
         endif 
      endif 
         

      if (ct_llp.gt.1) then
         call cplbam (fld, rwrk, iwrk, iama)
      endif 

      call cplbdr (fld, rwrk, iwrk)        
      call cpcldm (fld, rwrk, iwrk, iama, drawcl)

      call gstxci (ccolor)
      call set(rl,rr,rb,rt,ur,ul,ut,ub,l)

c      call cpgeti ('IWU', count)
c      print *,'IWRK size :',count
c      call cpgeti ('RWU', count)
c      print *,'RWRK size :',count

c      print *,'Space used :' ,sz_iama - (iama(6) - iama(5) - 1)

      call gflas2

      return 
      end


c ---------------------------------------------------------------------
c 
c Nom         :  cpchil
c 
c Parametres  :  flag
c 
c Description :  Cette routine est appelee par NCAR juste avant et juste
c                apres le dessin du "information label".
c                flag = 3 est la valeur qui nous interesse, c'est au
c                moment ou plchhq s'apprete a etre appelee.
c                Ref : Conpack, a contouring package
c                      NCAR Graphics Programmer Document
c                      page 27, 3.5
c 
c ---------------------------------------------------------------------

      subroutine cpchil (flag)

      implicit none

      integer flag
      integer  old_color
      save    old_color
      include 'common.h'

      if (flag.eq.3) then
         call pcgeti ('CC', old_color)
         call pcseti ('CC', ct_ilc)
      elseif  (flag.eq.-3) then
         call pcseti ('CC', old_color)
      endif 
         
      return
      end




        
c ---------------------------------------------------------------------
c 
c Nom         :  ColorContour
c 
c Parametres  :  
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine ColorContour (fld, bimg, limit, coords, map,ncol)


      implicit none

      include 'common.h'
      include 'color.h'
      include 'mapncar.h'

      TYPE( bimgfile ) bimg

      integer nx, ny,ncol
      real    fld(NXX, NYY)
      real    limit(NBOXMAX+1)
      integer map
       real    coords(4)

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(146)
      save   /marks/

      integer imin, imax, jmin, jmax 
      TYPE( bimgfile ) spembimg 
      common /spem/ spembimg, imin,imax,jmin,jmax 
      save   /spem/
 
      integer iclv

      real spv

      nx  = bimg%nxdata
      ny  = bimg%nydata
      spv = bimg%spval

      call cpseti ('SET - do SET call flag         ', 0)
      call cpseti ('MAP - mapping flag             ', map)

	if (bimg%grid .eq.3 .and. opt_chart .eq. 1) then
      call cpsetr ('XC1 - X coordinate at index 1      ', float(imin))
      call cpsetr ('XCM - X coordinate at index max    ', float(imax))
      call cpsetr ('YC1 - Y coordinate at index 1      ', float(jmin))
      call cpsetr ('YCN - Y coordinate at index max    ', float(jmax))
	else
      call cpsetr ('XC1 - X coordinate at index 1      ', coords(1))
      call cpsetr ('XCM - X coordinate at index max    ', coords(2))
      call cpsetr ('YC1 - Y coordinate at index 1      ', coords(3))
      call cpsetr ('YCN - Y coordinate at index max    ', coords(4))
	endif

      call cpsetr ('SPV - special value            ', spv)

      call cpsetc ('HLT - high low label text      ', ' ')
      call cpsetc ('ILT - information label text   ', ' ')

      call cprect (fld, NXX, nx, ny, rwrk, SZ_RWRK, iwrk, SZ_IWRK)

      call cppkcl (fld, rwrk, iwrk)

C JMM En test pour les palettes avec plusieurs couleurs par index
C     avec ncol ca marche, mais alors ca plante avec les palettes 'normales'
       call cpseti ('NCL - number of contour levels',marks_count-1)
C      call cpseti ('NCL - number of contour levels',ncol+1)
   
       do iclv = 1, marks_count-1
C      do iclv = 1, ncol+1
         call cpseti ('PAI - parameter array index', iclv)
C        call cpsetr ('CLV - contour level',limit(iclv) ) 
         call cpsetr ('CLV - contour level', limit(marks_index(iclv)-COLOR_NRES+2))
         call cpseti ('CLU - contour level use', 1)
         call cpseti ('AIB - area identifier below', iclv+COLOR_NRES-1)
         call cpseti ('AIA - area identifier above', iclv+COLOR_NRES)

      enddo 

c Les lignes suivantes (6) sont commentees sur le conseil de C. Dietrich
c


c      call cpseti ('PAI - parameter array index', -1)
c      call cpseti ('AIA - area identifier below', 0)
      
c      call cpseti ('PAI - parameter array index', -2)
c      call cpseti ('AIA - area identifier below', 0)
      
c      call cpseti ('PAI - parameter array index', -3)
c      call cpseti ('AIA - area identifier below', 0)
      

      call cpclam (fld, rwrk, iwrk, iama)

c      call cpgeti ('IWU', count)
c      print *,'IWRK size :',count
c      call cpgeti ('RWU', count)
c      print *,'RWRK size :',count

c      print *,'Space used :' ,sz_iama - (iama(6) - iama(5) - 1)
      return
      end



c ---------------------------------------------------------------------
c 
c Nom              :  
c
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

        subroutine drawcl (xcs, ycs, ncs, iai, iag, nai)

        implicit none
	include 'common.h'
      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist

        real xcs(*), ycs(*)
	real map_coef_x, map_coef_y, map_coef_z
        real  xc0, yc0, zc0, lala, lolo, dep
	real rl, rr, rb, rt, ur, ul, ut, ub
	real contour_value
        integer iai(*), iag(*)
        integer ncs, nai, idr, i, l

	
        idr = 1


        do i=1,nai
           if (iai(i).lt.0) idr = 0
        enddo 

	if (opt_dash.eq.1) then
	call CPGETR('CLV - contour level',contour_value )
	if (contour_value.lt.0.) then
	call dashdb(52428)
	else
	call dashdb(65535)
	endif
	endif
           
        if (idr.ne.0) call curved (xcs, ycs, ncs)

	
	if (opt_cntsav.eq.1) then
        if ( opt_chart .EQ. 1 ) THEN
        call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
Ccall CPGETR('CLV - contour level',contour_value )
	map_coef_x=(map_marg(2)-map_marg(1))/(rr - rl)
	map_coef_y=(map_marg(4)-map_marg(3))/(rt - rb)
	xc0=map_marg(1) - map_coef_x * rl
	yc0=map_marg(3) - map_coef_y * rb
	open(95, file=f_cntsav)
	write(95,'(a)')'# Longitude, Latitude'
Cwrite(95,*)ncs
        write(95,*) 9999., 9999.
	do i=1,ncs
	lolo=xcs(i)*map_coef_x + xc0
	lala=ycs(i)*map_coef_y + yc0
	write(95,*) lolo, lala  ! contour_value
	enddo
        ELSE if (opt_coupe .EQ. 1 ) THEN
C       print *,'cntsav dans coupe'
        call getset(rl,rr,rb,rt,ur,ul,ut,ub,l)
C       call CPGETR('CLV - contour level',contour_value )
        map_coef_x=(map_marg(2)-map_marg(1))/(rr - rl)
	map_coef_y=(map_marg(4)-map_marg(3))/(rr - rl)
        map_coef_z=(prof_min - prof_max)/(rt - rb )
        xc0=map_marg(1) - map_coef_x * rl
	yc0=map_marg(3) - map_coef_y * rl
        zc0=prof_max -  map_coef_y * rb
        open(95, file=f_cntsav)
        write(95,'(a)')'# Abcisses, Profondeur'
Cwrite(95,*)ncs
        write(95,*) 9999., 9999.
        do i=1,ncs
        lolo=xcs(i)*map_coef_x + xc0
        lala=xcs(i)*map_coef_y + yc0
        dep=ycs(i)*map_coef_z + zc0
        write(95,*) lolo, lala, dep
        enddo
        ENDIF
	endif

        return
        end



c ---------------------------------------------------------------------
c 
c Nom         :  FilterQuotes (strIn, strOut)
c 
c ---------------------------------------------------------------------

      subroutine FilterQuotes (strIn, strOut)

      implicit none
      character*100 strIn
      character*256 strOut

      integer chr1, chrend
      integer lnblnk

      
      chr1   = 1
      chrend = lnblnk (strIn)

c     filtrage des caracteres " et ' au debut et a la fin de la
c     chaine

      if ((strIn(chr1:chr1).eq.'''').or.
     .    (strIn(chr1:chr1).eq.'"')) then
         chr1 = 2
      endif 

      if ((strIn(chrend:chrend).eq.'''').or.
     .    (strIn(chrend:chrend).eq.'"')) then
         chrend = chrend-1
      endif 

      strOut = strIn(chr1:chrend)

      return
      end
