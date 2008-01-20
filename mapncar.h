c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  mapdata.h
c 
c Auteur         :  Eric Brown
c
c Description    :  Ce fichier contient le common ou se trouvent les
c                   parametres NCAR initialises par mapint
c 
c --------------------------------------------------------------------


      integer  sz_iama, sz_cra
      parameter (sz_iama = 250000000)
      parameter (sz_cra =  10000000)


      integer  iama(sz_iama), iaia(10), igia(10)
      real     xcra(sz_cra),ycra(sz_cra)

      common /mapncar/iama,iaia,igia,xcra,ycra


      integer SZ_IWRK, SZ_RWRK
      parameter (SZ_IWRK=190000)
      parameter (SZ_RWRK=190000)

      integer iwrk (SZ_IWRK)
      real    rwrk (SZ_RWRK)
      common /contwork/ iwrk, rwrk
