c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  global_data.h
c 
c Auteur         :  Eric Brown
c
c Description    :  common contenant les donnees de masques 
c                   et de grilles.
c 
c                   Utilise par : CheckFiles (checkfiles.f)
c                                 
c
c ---------------------------------------------------------------------

      real d_clrmask(NXX,NYY),d_contmask(NXX,NYY),d_vectmask(NXX,NYY)
      real d_clrgrid_x(NXX),  d_clrgrid_y(NYY)
      real d_contgrid_x(NXX), d_contgrid_y(NYY)
      real d_vectgrid_x(NXX), d_vectgrid_y(NYY)

      common  /gridmask/d_clrmask,d_contmask,d_vectmask,
     .                  d_clrgrid_x, d_clrgrid_y,
     .                  d_contgrid_x,d_contgrid_y,
     .                  d_vectgrid_x,d_vectgrid_y
      save    /gridmask/
