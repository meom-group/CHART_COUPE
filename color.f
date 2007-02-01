c --------------------------------------------------------------------
c 
c Programme      :  chart
c Fichier        :  colors.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
c Mise a Jour    :  
c
c Description    :  ce fichier contient les fonctions qui associent
c                   des couleurs aux differentes regions d'une carte
c 
c ---------------------------------------------------------------------





c ---------------------------------------------------------------------
c 
c Nom              :  colcont
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

        subroutine colcont (xcs,ycs,ncs,iai,iag,nai)

        implicit none
        
        include 'common.h'
        include 'color.h'

        real    xcs(*),ycs(*)
        integer iai(*),iag(*), ncs, nai
        integer i, iai1, iairef
        if (opt_high .eq. 0 ) then
         iairef = 2
        else
         iairef = 1032
        endif

	iai1=-1
	do 101 i=1,nai
           if (iag(i).eq.1) then 
              iai1=iai(i)
           endif

 101    continue


c iai1= 2 = l'ocean 
c iai1= index area du pays, si positive voir DOC NCAR V2.00 p 271-286

        if (iai1.gt.0) then
           if (iai1.ne.iairef) then
              call gsfaci(COLOR_CONTINENT)
              call gfa(ncs-1,xcs,ycs)
           endif
        endif

        return
        end



c ---------------------------------------------------------------------
c 
c Nom              :  colocean 
c Parametres       :  
c Description      :  donne une couleur a l'ocean differente de
c                     la couleur de background.
c 
c ---------------------------------------------------------------------

        subroutine colocean (xcs,ycs,ncs,iai,iag,nai)

        implicit none
        include 'common.h'
        include 'color.h'

        real    xcs(*),ycs(*)
        integer iai(*),iag(*), ncs, nai
        integer i, iai1, iairef

        if (opt_high .eq. 0 ) then
         iairef = 2
        else
         iairef = 1032
        endif

	iai1=-1
	do 101 i=1,nai
           if (iag(i).eq.1) then 
              iai1=iai(i)
           endif
 101    continue

c iai1= 2 = l'ocean 

        if (iai1.eq.iairef) then
          call gsfaci(COLOR_OCEAN)
          call gfa(ncs-1,xcs,ycs)
        endif

        return
        end
