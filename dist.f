c###
        function dist(lona,lonb,lata,latb)
c
c Cette fonction retourne la distance en km entre les pts
c A (lona, lata) et B(lonb, latb) le long d'une orthodromie
c	implicit none
        real*4 lata, lona, pi
        real*4 latb, lonb, R
	real*4 latar, latbr, lonar, lonbr
        pi=acos(-1.)
        conv=pi/180.
c Rayon de la terre
        R=(6378.137+6356.7523)/2.0 ! km

        latar=lata*conv
        latbr=latb*conv

        lonar=lona*conv
        lonbr=lonb*conv
c
        ux=cos(lonar)*cos(latar)
        uy=sin(lonar)*cos(latar)
        uz=sin(latar)
c
        vx=cos(lonbr)*cos(latbr)
        vy=sin(lonbr)*cos(latbr)
        vz=sin(latbr)
      pds=ux*vx+uy*vy+uz*vz
      if (pds.ge.1.) then
         dist=0.
      else
         dist=R*acos(pds)
      endif
        return
        end

