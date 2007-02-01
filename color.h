c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  color.h
c 
c Auteur         :  Eric Brown
c
c Description    :  Contient les definitions de couleurs
c 
c ---------------------------------------------------------------------



        integer NBOXMAX
        parameter (NBOXMAX=256)

        integer COLOR_NRES      ! number of reserved colors
        parameter (COLOR_NRES=20)

        integer COLOR_BACKGROUND, COLOR_FOREGROUND, COLOR_SPVAL
        integer COLOR_OCEAN, COLOR_CONTINENT, COLOR_ISOCONTOUR
        integer COLOR_VECTOR, COLOR_CONTINENT_PERIM

        parameter (COLOR_BACKGROUND=0)
        parameter (COLOR_FOREGROUND=1)
        parameter (COLOR_CONTINENT=2)
        parameter (COLOR_SPVAL=3)
        parameter (COLOR_OCEAN=4)
        parameter (COLOR_ISOCONTOUR=5)
        parameter (COLOR_VECTOR=6)
        parameter (COLOR_CONTINENT_PERIM=7)
     
