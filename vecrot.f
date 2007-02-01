	parameter (nimax=360,njmax=360,nkmax=30,ndimax=1)
	character*80 line1, line2, line3, line4
	character*80 fil1,fil2,line,filgrid,cmd
	real u(nimax,njmax), h1d(nkmax)
	real v(nimax,njmax)
	real urot(nimax,njmax)
	real vrot(nimax,njmax)
	pi=acos(-1.)
	narg=iargc()
	if (narg.lt.2) then
	 print *,'USAGE: vecrot <angle>  vecxyfile'
	 print *,' sortie sur vecrot.bimg>'
	stop
	endif
c
c
	call getarg(1,fil1)
	read(fil1,*)angled
	angle=angled*pi/180.
	alfa=angle-pi/2.
	
	ifich=2
	call getarg(ifich,fil1)
	open(10,file=fil1,form='unformatted')
120	format(a,i3.3)
	print *,' Sortie sur vecrot.bimg'
	open(11,file='vecrot.bimg',form='unformatted')
	read(10) line1
	read(10) line2
	read(10) line3
	read(10) line4

	write(line4,120)'Rotation des axes de ',angled
	write(11) line1
	write(11) line2
	write(11) line3
	write(11) line4
c
c
	read(10) ni,nj,nk,nt,ndim,icod
	read(10) x1,y1,dx,dy,spval
	write(11) ni,nj,nk,nt,ndim,icod
	write(11) x1,y1,dx,dy,spval
c
c
	read(10) (h1d(k),k=1,nk)
	read(10) time_tag1
	write(11) (h1d(k),k=1,nk)
	write(11) time_tag1
	 do k=1,nk
	   read(10)((u(i,j),i=1,ni),j=1,nj)
	   read(10)((v(i,j),i=1,ni),j=1,nj)

		do i=1,ni
	         do j=1,nj
                  if (u(i,j).ne.spval.and.v(i,j).ne.spval) then
              urot(i,j)=u(i,j)*cos(alfa)-v(i,j)*sin(alfa)
	      vrot(i,j)=u(i,j)*sin(alfa)+v(i,j)*cos(alfa)
                  else
              urot(i,j)=spval
	      vrot(i,j)=spval
	          endif
	         enddo
                enddo
	   write(11)((urot(i,j),i=1,ni),j=1,nj)
	   write(11)((vrot(i,j),i=1,ni),j=1,nj)
         enddo
	close(10)
	close(11)
	end

        function lnblnk(string)
        character string*(*)
        ll= len(string)
        ii= index(string,' ')
        if (ll.eq.0.or.ii.eq.1) then
          lnblnk= 1
          return
        else if (ii.eq.0) then
          lnblnk= ll
          return
        end if
        do 10 k= ll, ii-1, -1
          if (string(k:k).ne.' ') go to 20
10      continue
20      lnblnk= k
        return
        end
