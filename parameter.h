        integer NXX,NYY,NA,NOVERPTS,Ntime
        integer NCLRMARK
	real*4 defspval
!      parameter (NXX=750,NYY=500,NA=100,Ntime=700)
! Standard
!      parameter (NXX=773,NYY=1296,NA=150,Ntime=700)
!      parameter (NXX=800,NYY=1296,NA=100,Ntime=700)
! for ORCA025
!      parameter (NXX=1442,NYY=1021,NA=190,Ntime=700)
      parameter (NXX=1500,NYY=1500,NA=100,Ntime=700)
! for etopo2
!     parameter (NXX=1711,NYY=601,NA=100,Ntime=700)
! for POP10
!     parameter (NXX=3600,NYY=2400,NA=40,Ntime=10)
!     parameter (NXX=5000,NYY=1400,NA=220,Ntime=700)
!     parameter (NXX=1376,NYY=1031,NA=100,Ntime=700)
!     parameter (NXX=1400,NYY=1400,NA=100,Ntime=700)
!     parameter (NXX=100,NYY=100,NA=100,Ntime=700)
!     parameter (NXX=200,NYY=118,NA=100,Ntime=700)
!parameter (NXX=800,NYY=700,NA=100,Ntime=700)
!parameter (NXX=750,NYY=750,NA=100,Ntime=100)
!parameter (NXX=1900,NYY=300,NA=100,Ntime=100)
!     parameter (NXX=800,NYY=1400,NA=100,Ntime=700)
	parameter (NOVERPTS=500000)
	parameter (defspval=999999999.)
        parameter (NCLRMARK=256)
