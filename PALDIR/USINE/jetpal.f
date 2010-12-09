	parameter (ncol=300)
	character*80 line
	real*4 r(ncol), g(ncol), b(ncol)
	real*4 x(ncol)
	real*4 y(ncol)
	real*4 e(ncol)
	call getarg(1,line)
	read(line,*) m
	n=nint(m/4.)
	print *,'n=',n, n/2
	do i=1,n
	x(i)=float(i)/n
	enddo

	do j=1,n/2
	y(j)=(float(j-1+n/2))/n
	enddo

	do i=1,n
	e(i)=1.
	enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	do i=1,n/2+n
	r(i)=0.
	enddo

	k=0
	do i=n/2+n+1,n/2+2*n
	k=k+1
	r(i)=x(k)
	enddo

	k=0
	do i= n/2+2*n+1,n/2+3*n
	k=k+1
	r(i)=e(k)
	enddo

	k=n/2
	do i= n/2+3*n +1, m
	r(i)=y(k)
	k=k-1
	enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	do i=1,n/2
	g(i)=0.
	enddo
	k=0
	do i=n/2+1, n/2+n
	k=k+1
	g(i)=x(k)
	enddo

	k=0
	do i=n/2+n+1, n/2+2*n
	k=k+1
	g(i)=e(k)
	enddo

	k=n+1
	do i=n/2+2*n+1, n/2+3*n
	k=k-1
	g(i)=x(k)
	enddo

	k=0
	do i=n/2+3*n+1, m
	g(i)=0.
	enddo

ccccccccccccccccccccccccccccccccccccccc

	do i=1,n/2
	b(i)=y(i)
	enddo
	k=0
	do i=n/2+1, n/2+n
	k=k+1
	b(i)=e(k)
	enddo
	k=n+1
	do i=n/2+n+1, n/2+2*n
	k=k-1
	b(i)=x(k)
	enddo
	k=0
	do i=n/2+2*n+1,m
	b(i)=0.
	enddo

	do i=1,m
	print '(3f8.4)',r(i), g(i), b(i)
	enddo

	end


