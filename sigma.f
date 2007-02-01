
      subroutine cpmpxy(imap,x,y,fx,fy)
	implicit none
      integer    imap, i1, i2, j1, j2
      real x, y, fx, fy, yy, xx
      real zlat, zlon
      real zx, zy
	real    prof_min, prof_max, xaxist, yaxist
	common /cut/prof_max,prof_min,xaxist, yaxist
	real z
	include 'parameter.h'
       real xygr(0:NXX+1,0:NYY+1,2)
       integer nigr,njgr
        common /gridxy/xygr,nigr,njgr
	include 'sigma.h'
       if ( imap .eq. 1) then
         call maptra(y,x,fx,fy)

       else if ( imap .eq. 5) then
         yy = y 
c        fx = xs(int(x)) + (xs(int(x)+1)-xs(int(x))) * (x-aint(x))
         fx = x
	 i1 = int(x)
         i2 = i1 +1
         j1 = int (yy)
         j2 = j1 + 1
	 zx = x - i1
         zy = yy - j1
c
	 z = ys(i1,j1) * (1. - zx ) * (1. - zy ) +
     &       ys(i1,j2) * (1. - zx ) * zy +
     &       ys(i2,j1) * (     zx ) * ( 1. -zy ) +
     &       ys(i2,j2) * zx * zy
	  z=-z
	 fy = 1 + (z-prof_max)/(prof_min-prof_max)*(NYY -1)

        else if ( imap .eq. 6) then
         yy = y 
         xx = x

	 i1 = int(xx)
         i2 = i1 +1
         j1 = int (yy)
         j2 = j1 + 1
	 zx = x - i1
         zy = yy - j1
C  if ((i1.gt.NXX).or.(i1.lt.1)) print *,' i1 =',i1,NXX
C  if ((i2.gt.NXX).or.(i2.lt.1)) print *,' i2 =',i2,NXX
C  if ((j1.gt.NYY).or.(j1.lt.1)) print *,' j1 =',j1,NYY
C  if ((j2.gt.NYY).or.(j2.lt.1)) print *,' j2 =',j2,NYY
c
	 zlat = xygr(i1,j1,2) * (1. - zx ) * (1. - zy ) +
     &       xygr(i1,j2,2) * (1. - zx ) * zy +
     &       xygr(i2,j1,2) * (     zx ) * ( 1. -zy ) +
     &       xygr(i2,j2,2) * zx * zy
	 zlon = xygr(i1,j1,1) * (1. - zx ) * (1. - zy ) +
     &        xygr(i1,j2,1) * (1. - zx ) * zy +
     &       xygr(i2,j1,1) * (     zx ) * ( 1. -zy ) +
     &       xygr(i2,j2,1) * zx * zy

       call maptra(zlat, zlon, fx, fy)
      else
         print *,'Unknown mapping in cpmpxy ',imap
      end if
      return
      end
c###
	subroutine PlotSigmaLevel(nsigma)
	implicit none
	include 'parameter.h'
	include 'sigma.h'
	integer nsigma, k, i
	real xtmp(NXX),ytmp(NXX)
	real ul, ur, ub, ut, wl, wr, wb, wt
	integer l
        real    prof_min, prof_max, xaxist, yaxist
        common /cut/prof_max,prof_min,xaxist, yaxist

	call getset(ul, ur, ub, ut, wl, wr, wb, wt, l)
	call set (ul, ur, ub, ut, float(1), float(NXX),prof_max, prof_min,l)
	do i=1,NXX
	xtmp(i)=float(i)
	enddo
	do k=1,nsigma
	  do i=1,NXX
	  ytmp(i)=-zs(i,k)
	  enddo
	 call curve(xtmp,ytmp,NXX)
	enddo
	call set (ul, ur, ub, ut, wl, wr, wb, wt, l)
	return
	end


