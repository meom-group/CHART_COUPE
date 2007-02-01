c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  drawpalette.f
c 
c Auteur         :  Eric Brown
c
c Description    :  Contient les fonctions de dessin pour la barre de
c                   palette. Adaptation du code original de NCAR pour
c                   la fonction LBLBAR.
c 
c ---------------------------------------------------------------------



c ---------------------------------------------------------------------
c 
c Nom         :  DrawPalette
c 
c Parametres  :  Voir NCAR Fundamentals, page 191
c
c Description :  Cette fonction est une copie de la fonction LBLBAR
c                de NCAR. (d'ailleurs cree a partir de leur code source)
c                La seule difference avec la fonction originale est que
c                la taille maximale des caracteres est limitee.
c 
c ---------------------------------------------------------------------

      subroutine DrawPalette (ihov,xleb,xreb,ybeb,yteb,nbox,wsfb,
     .                        hsfb,lfin,iftp,llbs,nlbs,lbab)

      implicit none


      dimension lfin(*)
      character*(*) llbs(*)
      integer ihov, nbox, lfin, iftp, nlbs, lbab
      real    xleb,xreb,ybeb,yteb,wsfb,hsfb


C Declare the common block where internal parameters are stored.

c        COMMON /LBCOMN/ ICBL,ICFL,ICLB,WOBL,WOFL,WOLB
c        SAVE   /LBCOMN/

c     remplace le common de NCAR

      integer icbl,icfl,iclb
      real    wobl,wofl,wolb
        

      real xlvp,xrvp,ybvp,ytvp,xlwd,xrwd,ybwd,ytwd
      real wsob,winc,hsob,hinc,xlb1,xrb1,ybb1,ytb1
      real stlw,wmax,hmax
      real dstl,dstr,dstb,dstt,hola,wchr,wola

      integer lnlg,ierr,isfc,ispc,i,itex,nclb,iscl,isct


C Define local arrays to hold X and Y coordinates of boxes.

      real xcra(5), ycra(5)

C Define local arrays for use as work arrays by the routine SFSGFA.

      real rwrk(6),iwrk(8)


c     recupere les parametres du common NCAR, pour maintenir 
c     la compatibilite avec les fonction LPSETI et LBSETR.

      call lbgeti ('CBL',icbl)
      call lbgeti ('CFL',icfl)
      call lbgeti ('CLB',iclb)
      call lbgetr ('WBL',wobl)
      call lbgetr ('WFL',wofl)
      call lbgetr ('WLB',wolb)



C Save the current SET parameters and arrange for the use of normalized
C device coordinates.

        call getset (xlvp,xrvp,ybvp,ytvp,xlwd,xrwd,ybwd,ytwd,lnlg)
        call    set (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)

C Compute the width and height of each section of the bar and the
C coordinates of the edges of the first solid-filled box.

        if (ihov.eq.0) then
           wsob=(xreb-xleb)/real(abs(nbox))
           winc=wsob
           hsob=yteb-ybeb
           hinc=0.
           xlb1=xleb+.5*(1.-wsfb)*wsob
           xrb1=xlb1+wsfb*wsob
           if (lbab.eq.1) then
              ybb1=yteb-hsfb*hsob
              ytb1=yteb
           else if (lbab.eq.2) then
              ybb1=ybeb
              ytb1=ybeb+hsfb*hsob
           else
              ybb1=ybeb+.5*(1.-hsfb)*hsob
              ytb1=yteb-.5*(1.-hsfb)*hsob
           end if
        else
           wsob=xreb-xleb
           winc=0.
           hsob=(yteb-ybeb)/real(abs(nbox))
           hinc=hsob
           if (lbab.eq.1) then
              xlb1=xleb
              xrb1=xleb+wsfb*wsob
           else if (lbab.eq.2) then
              xlb1=xreb-wsfb*wsob
              xrb1=xreb
           else
              xlb1=xleb+.5*(1.-wsfb)*wsob
              xrb1=xreb-.5*(1.-wsfb)*wsob
           end if
           ybb1=ybeb+.5*(1.-hsfb)*hsob
           ytb1=ybb1+hsfb*hsob
        end if

C Draw the bar by filling all of the individual boxes.

        call gqfaci (ierr,isfc)
        if (ierr.ne.0) then
          call seter ('lblbar - error exit from gqfaci',1,2)
          stop
        end if

        if (icfl.ge.0) then
          call gqplci (ierr,ispc)
          if (ierr.ne.0) then
            call seter ('lblbar - error exit from gqplci',2,2)
            stop
          end if
          call gsplci (icfl)
        end if

        if (wofl.gt.0.) then
          call gqlwsc (ierr,stlw)
          if (ierr.ne.0) then
            call seter ('lblbar - error exit from gqlwsc',3,2)
            stop
          end if
          call gslwsc (wofl)
        end if

        do 101 i=1,abs(nbox)
          xcra(1)=xlb1+real(i-1)*winc
          ycra(1)=ybb1+real(i-1)*hinc
          xcra(2)=xrb1+real(i-1)*winc
          ycra(2)=ycra(1)
          xcra(3)=xcra(2)
          ycra(3)=ytb1+real(i-1)*hinc
          xcra(4)=xcra(1)
          ycra(4)=ycra(3)
          xcra(5)=xcra(1)
          ycra(5)=ycra(1)
          if (iftp.eq.0) then
            call sfsgfa (xcra,ycra,4,rwrk,6,iwrk,8,lfin(i))
          else
            call lbfill (iftp,xcra,ycra,5,lfin(i))
          end if
  101   continue

        call gsfaci (isfc)
        if (icfl.ge.0) call gsplci (ispc)
        if (wofl.gt.0.) call gslwsc (stlw)

C If it is to be done, outline the boxes now.

        if (nbox.gt.0) then

          if (icbl.ge.0) then
            call gqplci (ierr,ispc)
            if (ierr.ne.0) then
              call seter ('lblbar - error exit from gqplci',4,2)
              stop
            end if
            call gsplci (icbl)
          end if

          if (wobl.gt.0.) then
            call gqlwsc (ierr,stlw)
            if (ierr.ne.0) then
              call seter ('lblbar - error exit from gqlwsc',5,2)
              stop
            end if
            call gslwsc (wobl)
          end if

          do 102 i=1,abs(nbox)
            xcra(1)=xlb1+real(i-1)*winc
            ycra(1)=ybb1+real(i-1)*hinc
            xcra(2)=xrb1+real(i-1)*winc
            ycra(2)=ycra(1)
            xcra(3)=xcra(2)
            ycra(3)=ytb1+real(i-1)*hinc
            xcra(4)=xcra(1)
            ycra(4)=ycra(3)
            xcra(5)=xcra(1)
            ycra(5)=ycra(1)
            if (ihov.eq.0) then
              if (i.eq.1.or.wsfb.ne.1.) then
                call gpl (5,xcra,ycra)
              else
                call gpl (4,xcra,ycra)
              end if
            else
              if (i.eq.1.or.hsfb.ne.1.) then
                call gpl (5,xcra,ycra)
              else
                call gpl (4,xcra(2),ycra(2))
              end if
            end if
  102     continue

          if (icbl.ge.0) call gsplci (ispc)
          if (wobl.gt.0.) call gslwsc (stlw)

        end if

C If labelling is to be done at all ...

        if (lbab.ne.0) then

C ... save the current setting of the PLOTCHAR "text extent" parameter
C and reset it to force computation of "text extent" quantities.

          call pcgeti ('te - text extent flag',itex)
          call pcseti ('te - text extent flag',1)

C Find the dimensions of the largest label in the list of labels.

          wmax=0.
          hmax=0.

          do 104 i=1,nlbs
            nclb=len(llbs(i))
  103       if (llbs(i)(nclb:nclb).eq.' ') then
              nclb=nclb-1
              if (nclb.ne.0) go to 103
            end if
            if (nclb.ne.0) then
              call plchhq (.5,.5,llbs(i)(1:nclb),.01,360.,0.)
              call pcgetr ('dl - distance to left edge'  ,dstl)
              call pcgetr ('dr - distance to right edge' ,dstr)
              call pcgetr ('db - distance to top edge'   ,dstb)
              call pcgetr ('dt - distance to bottom edge',dstt)
              wmax=max(wmax,dstl+dstr+.02)
              hmax=max(hmax,dstb+dstt+.02)
            end if
  104     continue

C If the maximum height and width are undefined, quit.

          if (wmax.le..02.or.hmax.le..02) go to 107

C Determine the character width to be used and the resulting offset
C distance to the bottom or top of the label.




          if (ihov.eq.0) then
            hola=(1.-hsfb)*hsob
            if (lbab.ge.3) hola=hola/2.
            wchr=.01*min(wsob/wmax,hola/hmax)

c     Voici la ligne pour laquelle on a fait tout ce travail !
            if (wchr.gt.0.012) wchr = 0.012

            dstb=(dstb+.01)*(wchr/.01)
            dstt=(dstt+.01)*(wchr/.01)
          else
            wola=(1.-wsfb)*wsob
            if (lbab.ge.3) wola=wola/2.
            wchr=.01*min(wola/wmax,hsob/hmax)

c     Voici la ligne pour laquelle on a fait tout ce travail !
            if (wchr.gt.0.012) wchr = 0.012

          end if


C Draw the labels.

          call gqplci (ierr,iscl)
          if (ierr.ne.0) then
            call seter ('lblbar - error exit from gqplci',6,2)
            stop
          end if
          call gqtxci (ierr,isct)
          if (ierr.ne.0) then
            call seter ('lblbar - error exit from gqtxci',7,2)
            stop
          end if
          if (iclb.lt.0) then
            call gsplci (isct)
          else
            call gsplci (iclb)
            call gstxci (iclb)
          end if
          if (wolb.gt.0.) then
            call gqlwsc (ierr,stlw)
            if (ierr.ne.0) then
              call seter ('lblbar - error exit from gqlwsc',8,2)
              stop
            end if
            call gslwsc (wolb)
          end if

          if (nlbs.lt.abs(nbox)) then
            xlb1=xlb1+winc
            ybb1=ybb1+hinc
          else if (nlbs.eq.abs(nbox)) then
            xlb1=xlb1+wsfb*winc/2.
            ybb1=ybb1+hsfb*hinc/2.
          end if

          do 106 i=1,nlbs
            nclb=len(llbs(i))
  105       if (llbs(i)(nclb:nclb).eq.' ') then
              nclb=nclb-1
              if (nclb.ne.0) go to 105
            end if
            if (nclb.ne.0) then
              if (ihov.eq.0) then
                if (lbab.eq.1.or.lbab.ge.3)
     +            call plchhq (xlb1+real(i-1)*wsob,ybb1-dstt,
     +                            llbs(i)(1:nclb),wchr,0.,0.)
                if (lbab.eq.2.or.lbab.ge.3)
     +            call plchhq (xlb1+real(i-1)*wsob,ytb1+dstb,
     +                            llbs(i)(1:nclb),wchr,0.,0.)
              else
                if (lbab.eq.1.or.lbab.ge.3)
     +            call plchhq (xrb1+wchr,ybb1+real(i-1)*hsob,
     +                            llbs(i)(1:nclb),wchr,0.,-1.)
                if (lbab.eq.2.or.lbab.ge.3)
     +            call plchhq (xlb1-wchr,ybb1+real(i-1)*hsob,
     +                            llbs(i)(1:nclb),wchr,0.,+1.)
              end if
            end if
  106     continue

          call gsplci (iscl)
          if (iclb.ge.0) call gstxci (isct)
          if (wolb.gt.0.) call gslwsc (stlw)

C Restore the original setting of the PLOTCHAR text extent flag.

  107     call pcseti ('te - text extent flag',itex)

        end if

C Restore the original SET parameters.

        call set (xlvp,xrvp,ybvp,ytvp,xlwd,xrwd,ybwd,ytwd,lnlg)

c done.

        return

      end




c ---------------------------------------------------------------------
c 
c Nom              :  LBFILL
c Date de creation :  4 mai 1993
c Parametres       :  
c Description      :  remplace la fonction par defaut appelee
c                     par LBLBAR, pour remplir les boites selon
c                     ma propre technique...
c 
c ---------------------------------------------------------------------

      subroutine lbfill (iftp,xcra, ycra, ncra, indx)
      
      implicit none
      
      include 'common.h'
      integer iftp, indx, ncra
      real    xcra(*), ycra(*)
      real    xincr,yincr
      integer i, color_num, color_index

      integer        marks,     marks_count, marks_index
      common /marks/ marks(145),marks_count, marks_index(146)
      save   /marks/

c     indx is an index in the array marks_index, not a color index   
      
      color_index = marks_index(indx) 
      color_num   = marks_index(indx+1) - marks_index(indx)

C      PRINT *,'#### COLOR_NUM ####=',color_num
      if (color_num.eq.0) then
         color_num = 1.0
      endif 
      
      if (opt_vertpal .EQ. 0) then
      xincr = (xcra(2) - xcra(1))/(float(color_num))


      xcra(2) = xcra(1) + xincr
      xcra(3) = xcra(4) + xincr
      
      do i=0,color_num-1
         call gsfaci (color_index+i)

         call gfa    (ncra-1, xcra, ycra)
         xcra(1) = xcra(2)
         xcra(2) = xcra(2) + xincr
         xcra(4) = xcra(3)
         xcra(3) = xcra(3) + xincr
      enddo
      else
      yincr = (ycra(4) - ycra(1))/(float(color_num))


      ycra(3) = ycra(2) + yincr
      ycra(4) = ycra(1) + yincr
      
      do i=0,color_num-1
         call gsfaci (color_index+i)

         call gfa    (ncra-1, xcra, ycra)
         ycra(1) = ycra(4)
         ycra(2) = ycra(3)
         ycra(3) = ycra(2) + yincr
         ycra(4) = ycra(1) + yincr
      enddo
      endif


      return
      end

