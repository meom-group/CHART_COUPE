MODULE vectors
  !!======================================================================
  !!                     ***  MODULE  vectors  ***
  !! All routines dedicated to vector representation on a map
  !!=====================================================================
  !! History :  1.0   : 06/1993      : E. Brown     : original code
  !!          2 -> 6  : 1994 -> 2010 : J.M. Molines : improvements
  !!            7/0   : 12/2010      : J.M. Molines : F90 and Doctor norm
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!  VectorPrepareData 
  !!  BimgReadVectorData
  !!  VectorTrace 
  !!  VectorCalcModule 
  !!  VecCalcSpeed
  !!  PutVelOnTpoints 
  !!----------------------------------------------------------------------
  USE modcom
  USE modmapncar
  USE modcolor
  USE modeflim
  USE readbimg
  USE cdf
  USE tracecol

  IMPLICIT NONE

  PRIVATE

  PUBLIC  ::  VectorTrace            ! called from chart
  PUBLIC  ::  VectorPrepareData      ! called from chart

  PRIVATE ::  BimgReadVectorData     ! local call
  PRIVATE ::  VectorCalcModule       ! local call
  PRIVATE ::  VecCalcSpeed           ! local call
  PRIVATE ::  PutVelOnTpoints        ! local call

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

CONTAINS


  SUBROUTINE VectorPrepareData (bdimgvec, bdimgclr, pclrdata, pvectdata, pcoords, ktstp, klev)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE VectorPrepareData  ***
    !!
    !! ** Purpose :   Read vector data in 1 or 2 files, interpolate components on A-Grid
    !!                if necessary, set clrdata in case of vecshade option, and compute 
    !!                vector module if vecclrmod in use. 
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),        INTENT(inout) :: bdimgvec(3)
    TYPE( bimgfile ),        INTENT(inout) :: bdimgclr
    REAL(KIND=4), DIMENSION(:), INTENT(in) :: pcoords   ! 4
    INTEGER(KIND=4),            INTENT(in) :: ktstp
    INTEGER(KIND=4),            INTENT(in) :: klev
    REAL(KIND=4), DIMENSION(:,:,:), INTENT(out) :: pvectdata ! (NXX,NYY,2)
    REAL(KIND=4), DIMENSION(:,:)  , INTENT(out) :: pclrdata  ! (NXX,NYY)

    INTEGER(KIND=4)                 :: inx, iny
    REAL(KIND=4)                    :: zspval
    !!----------------------------------------------------------------------
    pvectdata(:,:,:) = 0.0

    IF (opt_vect2D == 1) THEN
       CALL BimgReadVectorData (pvectdata(:,:,1), bdimgvec(1), rmap_coord, ktstp, klev, 1)
       CALL BimgReadVectorData (pvectdata(:,:,2), bdimgvec(1), rmap_coord, ktstp, klev, 2)
       IF (opt_cgrid == 1) THEN
          PRINT *,' Option -Cgrid with  -vecdataxy ...'
       ENDIF
    ELSE
       IF (opt_vectX == 1) THEN
          PRINT *,' Read Vector Data X'
          CALL BimgReadVectorData (pvectdata(:,:,1), bdimgvec(1), rmap_coord, ktstp, klev, 1)

          IF (opt_vecpsi == 1) THEN
             CALL VecCalcSpeed (pvectdata, bdimgvec(1), rmap_coord)
          ENDIF
       ENDIF
       IF (opt_vectY == 1) THEN
          PRINT *,' Read Vector Data Y'
          CALL BimgReadVectorData (pvectdata(:,:,2), bdimgvec(2), rmap_coord,ktstp,klev,1)
       ENDIF
    ENDIF

    ! Interpol C-grid velocities on A-grid
    IF (opt_cgrid == 1)         CALL PutVelOnTpoints(pvectdata, bdimgvec)

    IF (opt_vecshade == 1) THEN
       bdimgclr       =  bdimgvec(1)
       bdimgclr%spval = rp_defspval

       inx    = bdimgvec(1)%nxdata 
       iny    = bdimgvec(1)%nydata 
       zspval = bdimgvec(1)%spval

       WHERE ( pvectdata(1:inx, 1:iny, 1) /= zspval )
          pclrdata( 1:inx, 1:iny ) = 1.
       ELSE WHERE
          pclrdata( 1:inx, 1:iny ) = rp_defspval
       END WHERE
    ENDIF

    IF ( (opt_vectmod == 1) .OR. (opt_clrmod == 1) ) THEN
       bdimgclr = bdimgvec(1)
       CALL VectorCalcModule (bdimgvec, pvectdata, pclrdata)         
    ENDIF


  END SUBROUTINE VectorPrepareData


  SUBROUTINE BimgReadVectorData(pdata_out, bdimg, pcoords, ktstp, klev, kdim )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BimgReadVectorData  ***
    !!
    !! ** Purpose : Get one component of vector data, perform vertical 
    !!              interpolation (if any) and extract the area corresponding
    !!              to the specified geographical window
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(out)   :: pdata_out ! (NXX,NYY) 
    TYPE( bimgfile ),             INTENT(inout) :: bdimg
    REAL(KIND=4), DIMENSION(:),   INTENT(inout) :: pcoords   ! 4
    INTEGER(KIND=4),                 INTENT(in) :: ktstp
    INTEGER(KIND=4),                 INTENT(in) :: klev
    INTEGER(KIND=4),                 INTENT(in) :: kdim

    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zlocal_data  ! (NXX,NYY)
    INTEGER(KIND=4)                 :: ji,jj
    INTEGER(KIND=4)                 :: inx, iny
    INTEGER(KIND=4)                 :: iksup, ikinf
    REAL(KIND=4)                    :: zlat, zlon, zspval, zalpha, zalpha1
    LOGICAL                         :: ll_good_point
    !!----------------------------------------------------------------------
    IF (lo_debug ) PRINT *,'#BRVD  BimgReadVectorData : Reading Level :',klev
    CALL flush(6)
    ALLOCATE ( zlocal_data (NXX,NYY) )

    bdimg%nxdata = bdimg%nxfile
    bdimg%nydata = bdimg%nyfile

    ! Always get level klev

    IF (bdimg%nrecl  ==  0 ) THEN
       CALL BimgGetLayer   (bdimg, zlocal_data, ktstp, klev, kdim)
    ELSE IF (bdimg%nrecl  >  0 ) THEN
       CALL DirectGetLayer (bdimg, zlocal_data, ktstp, klev, kdim)
    ELSE IF (bdimg%nrecl  ==  -1 ) THEN
       CALL CdfGetLayer    (bdimg, zlocal_data, ktstp, klev, kdim) 
    ELSE
          !      Not possible 
    ENDIF

    IF ( opt_dep == 1 ) THEN
       ! interpolation  between 2 levels
       iksup = klev      ! already read above
       ikinf = klev + 1

       PRINT *,'  Interpolation between levels ',iksup ,' and ',ikinf
       PRINT *,'     weight :',bdimg%alphasup, 1.- bdimg%alphasup
       PRINT *,'      depth :',bdimg%depth(iksup), req_dep, bdimg%depth(ikinf)

       ! Read lower level
       IF (bdimg%nrecl  ==  0  ) THEN
          CALL BimgGetLayer   (bdimg, pdata_out, ktstp, ikinf, kdim)
       ELSE IF (bdimg%nrecl  >  0 ) THEN
          CALL DirectGetLayer (bdimg, pdata_out, ktstp, ikinf, kdim)
       ELSE IF (bdimg%nrecl  ==  -1 ) THEN
          CALL CdfGetLayer    (bdimg, pdata_out, ktstp, ikinf, kdim)
       ELSE
          !     Not possible 
       ENDIF

       ! Interpolation taking care of spvals
       inx     = bdimg%nxdata
       iny     = bdimg%nydata
       zspval  = bdimg%spval
       zalpha  = bdimg%alphasup
       zalpha1 = 1. - zalpha
       
       WHERE ( (pdata_out(1:inx,1:iny) /= zspval) .AND. (zlocal_data(1:inx,1:iny) /= zspval) )
         zlocal_data(1:inx,1:iny) = zalpha * zlocal_data(1:inx,1:iny) + zalpha1 * pdata_out(1:inx,1:iny)
       ELSE WHERE
         zlocal_data(1:inx,1:iny) = zspval
       END WHERE

    ENDIF

    ! mask data if required
    IF ( bdimg%mask == 1)    CALL BimgMaskData (bdimg, zlocal_data)

    ! -shift  option
    IF ( opt_shift == 1) THEN
       CALL BimgShift (bdimg, zlocal_data, shift_map, pcoords)
       IF (bdimg%ngrid >= 1) CALL BimgShiftGrid (bdimg, shift_map)
       bdimg%x1mod = bdimg%x1mod + shift_map
    ENDIF

    ! Extract the required domain from the data and transfert to data out
    SELECT CASE  ( bdimg%ngrid )
      CASE(  1  )  ! Irregular grid
       nimin = 1
       DO WHILE ( (ABS(pcoords(1)-bdimg%d_xgrid(nimin+1)) <       &
          &        ABS(pcoords(1)-bdimg%d_xgrid(nimin  )) ) .AND. &
          &        nimin < bdimg%nxdata-1 )
          nimin = nimin+1
       ENDDO

       nimax = 2
       DO WHILE ( (ABS(pcoords(2)-bdimg%d_xgrid(nimax  )) <       &
          &        ABS(pcoords(2)-bdimg%d_xgrid(nimax-1)) ) .AND. &
          &        nimax < bdimg%nxdata)
          nimax = nimax+1
       ENDDO

       njmin = 1
       DO WHILE ( (ABS(pcoords(3)-bdimg%d_ygrid(njmin+1)) <       &
          &        ABS(pcoords(3)-bdimg%d_ygrid(njmin  )) ) .AND. &
          &        njmin < bdimg%nydata-1)
          njmin = njmin+1
       ENDDO

       njmax = 2
       DO WHILE ( (ABS(pcoords(4)-bdimg%d_ygrid(njmax  )) <       &
          &        ABS(pcoords(4)-bdimg%d_ygrid(njmax-1)) ) .AND. &
          &        njmax < bdimg%nydata)
          njmax = njmax+1
       ENDDO

      CASE (   3  )  ! completely irregular grid
       nimin = 1
       nimax = bdimg%nxfile
       njmin = 1
       njmax = bdimg%nyfile

       IF (opt_zoom == 1) THEN
          DO ji=nimin,nimax
             DO jj=njmin,njmax
                zlat = xygr(ji,jj,2)
                zlon = xygr(ji,jj,1)
                ll_good_point = .TRUE.
                ll_good_point = (zlat <= pcoords(4))
                ll_good_point = ll_good_point .AND. (zlat >= pcoords(3))
                ll_good_point = ll_good_point .AND. (zlon <= pcoords(2))
                ll_good_point = ll_good_point .AND. (zlon >= pcoords(1))

                IF (.NOT.ll_good_point) zlocal_data(ji,jj) = bdimg%spval
             ENDDO
          ENDDO
       ENDIF

      CASE DEFAULT
       nimin = NINT ((pcoords(1) - bdimg%x1mod)/bdimg%dx)+1
       nimax = NINT ((pcoords(2) - bdimg%x1mod)/bdimg%dx)+1
       njmin = NINT ((pcoords(3) - bdimg%y1mod)/bdimg%dy)+1
       njmax = NINT ((pcoords(4) - bdimg%y1mod)/bdimg%dy)+1
    END SELECT

    ! check that the required windoaw is within the domain
    IF ( (nimin > bdimg%nxfile) .OR. (nimax > bdimg%nxfile) .OR. &
      &  (njmin > bdimg%nyfile) .OR. (njmax > bdimg%nyfile) ) THEN
       PRINT *,'ERROR : required windows out of the data domain '
       PRINT *,pcoords(1),pcoords(2),pcoords(3),pcoords(4)
       PRINT *,nimin,bdimg%nxdata
       PRINT *,nimax,bdimg%nxdata
       PRINT *,njmin,bdimg%nydata
       PRINT *,njmax,bdimg%nydata
       STOP
    ENDIF

    bdimg%nxdata = nimax - nimin ! + 1
    bdimg%nydata = njmax - njmin ! + 1
    
    pdata_out(1:bdimg%nxdata+1,1:bdimg%nydata+1) = zlocal_data(nimin:nimax,njmin:njmax)

    IF ( opt_shift == 1 )  bdimg%x1mod = bdimg%x1mod - shift_map

    DEALLOCATE ( zlocal_data  )

  END SUBROUTINE BimgReadVectorData


  SUBROUTINE  VectorTrace (bdimgvec, bdimgclr, pclrdata, pvectdata, kncol, pcoords)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE VectorTrace  ***
    !!
    !! ** Purpose :  Draw vectors on the map according to specifications
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile),                 INTENT(in) :: bdimgvec(3)
    TYPE( bimgfile),                 INTENT(in) :: bdimgclr
    REAL(KIND=4), DIMENSION(:,:,:),  INTENT(in) :: pvectdata  ! (NXX,NYY,2)
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: pclrdata   ! (NXX, NYY)
    INTEGER(KIND=4),                 INTENT(in) :: kncol
    REAL(KIND=4), DIMENSION(:),      INTENT(in) :: pcoords    ! ( 4 )

    REAL(KIND=4), DIMENSION(NBOXMAX+1)    :: zveclim
    REAL(KIND=4),DIMENSION(:),ALLOCATABLE :: zdummy
    INTEGER(KIND=4)                       :: idummy
    INTEGER(KIND=4)                       :: ji
    INTEGER(KIND=4)                       :: imap_flag
    INTEGER(KIND=4)                       :: icprev, ierr 
    REAL(KIND=4)                          :: zvv_dmx, zvv_tmp_vrl
    INTEGER(KIND=4)                       :: ilog, iclip
    REAL(KIND=4)                          :: zrl, zrr, zrb, zrt
    REAL(KIND=4)                          :: zur,zul,zut, zub
    REAL(KIND=4)                          :: zvv_mnx1, zvv_mny1
    REAL(KIND=4)                          :: zvv_mxx1, zvv_mxy1
    REAL(KIND=4), DIMENSION(4)            :: zdum
    !!----------------------------------------------------------------------
    CALL BimgAlloc (bimgwk)
    idummy = 0

    imap_flag = 1
    IF (opt_noproj == 1) imap_flag = 0

    IF (bdimgvec(1)%ngrid >= 1) THEN
       imap_flag = 3
       bimgwk    = bdimgvec(1)
    ENDIF

    CALL gflas1( jp_ib_vectors )
    CALL gqclip ( ierr, iclip, zdum)
    CALL gsclip ( 1 )
    CALL getset(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)

    CALL vvseti ('SET - do SET call flag            ', 0           )
    CALL vvseti ('MAP - mapping flag                ', imap_flag   )

    IF (bdimgvec(1)%ngrid  /=  3) THEN
       CALL vvsetr ('XC1 - X coordinate at index 1  ', pcoords(1)  )
       CALL vvsetr ('XCM - X coordinate at index max', pcoords(2)  )
       CALL vvsetr ('YC1 - Y coordinate at index 1  ', pcoords(3)  )
       CALL vvsetr ('YCN - Y coordinate at index max', pcoords(4)  )
    ELSE
       CALL vvsetr ('XC1 - X coordinate at index 1  ', float(nimin))
       CALL vvsetr ('XCM - X coordinate at index max', float(nimax))
       CALL vvsetr ('YC1 - Y coordinate at index 1  ', float(njmin))
       CALL vvsetr ('YCN - Y coordinate at index max', float(njmax))
    ENDIF

    CALL vvsetr ('USV - U special value          ', bdimgvec(1)%spval)
    CALL vvsetr ('VSV - V special value          ', bdimgvec(1)%spval)
    CALL vvsetr ('PSV - P special value          ', bdimgclr%spval   )
    CALL vvseti ('SVF - special value flag       ', 3                )
    CALL vvseti ('SPC - P Special Color          ', COLOR_SPVAL      )
    CALL vvseti ('VST - Vector STatistics flag   ', nvv_vst          )
    CALL vvsetc ('ZFT - Zero Field Text string   ', ' '              )

    IF (opt_vecmin == 1)  CALL vvsetr ('VLC - Vector Low Cutoff     ', rvv_vlc)
    IF (opt_vecmax == 1)  CALL vvsetr ('VHC - Vector High Cutoff    ', rvv_vhc)
    IF (opt_vecvrm == 1)  CALL vvsetr ('VRM - Vector Ref. Magnitude ', rvv_vrm)

    ! text
    zvv_mnx1 = (rvv_mnx - zrl) / (zrr -zrl)    
    zvv_mny1 = (rvv_mny - zrb) / (zrt -zrb)      
    zvv_mxx1 = (rvv_mxx - zrl) / (zrr -zrl)    
    zvv_mxy1 = (rvv_mxy - zrb) / (zrt -zrb)      

    CALL vvsetr ('MNS - MiN vector text block char Size', rvv_mns )
    CALL vvsetr ('MNX - MiN vector text block X coord  ', zvv_mnx1)
    CALL vvsetr ('MNY - MiN vector text block Y coord  ', zvv_mny1)
    CALL vvseti ('MNP - MiN vector text block Pos. mode', nvv_mnp )
    CALL vvsetc ('MNT - MiN vector Text string         ', cvv_mnt )

    CALL vvsetr ('MXS - MaX vector text block char Size', rvv_mxs )
    CALL vvsetr ('MXX - MaX vector text block X coord  ', zvv_mxx1)
    CALL vvsetr ('MXY - MaX vector text block Y coord  ', zvv_mxy1)
    CALL vvseti ('MXP - MaX vector text block Pos. mode', nvv_mxp )
    CALL vvsetc ('MXT - MaX vector Text string         ', cvv_mxt )

! TEST of differnt arrows
    IF ( opt_filled_arrow == 1 ) THEN
       CALL vvseti ('AST - Arrow STyle', 1 )
       CALL vvseti ('CTV - Color Threshold Value', -1  )  ! -1 : color vector based on magnitude
       CALL vvseti ('NLV - Number of LeVels     ', 1   )
       CALL vvseti ('ACM - Arrow Color Mode     ', -2  )  ! control filling and edge of arrow
       CALL vvseti ('PAI - Parameter Array Index', 1   )
       CALL vvseti ('CLR - GKS Color Index      ', 1   )  ! 1 = foreground
       CALL vvsetr ('TVL - Array of Threshold Val',10.  ) ! 10 m/s arbitrary
       CALL vvsetr ('AFO - Arrow Interior Reference', 1 )
       CALL vvsetr ('LWD - Vector Linewidth', 1.1 )
       CALL vvsetr ('AWF - Arrow Width Fractional Minimum', 0.95 )
       CALL vvsetr ('AWR', 0.05 )
       CALL vvsetr ('AXF', 0. )
       CALL vvsetr ('AYF', 0. )
    ENDIF



    !      call vvsetr('LWD -- Vector Linewidth', 2.25)
    CALL vvseti ('VPO - Vector Position Method         ', nvv_vpo)

    IF ( (opt_vectclr == 1) .OR. (opt_vectmod == 1) ) THEN
       CALL vvseti ('CTV - Color Threshold Value       ', -2      )
       CALL VecGetLimits (pclrdata, bdimgclr, zveclim, kncol)
       IF (opt_veclout == 1 )  CALL WriteLimits (cf_veclout, zveclim, kncol+1)
       IF (opt_palbar == 1  )  CALL AddPalette (zveclim, kncol)

       CALL vvseti ('NLV - Number of LeVels            ', kncol   )

       DO ji=1,kncol
          CALL vvseti ('PAI - Parameter Array Index    ', ji             )
          CALL vvseti ('CLR - GKS Color Index          ', ji+COLOR_NRES-1)
          CALL vvsetr ('TVL -                          ', zveclim(ji+1)  )
       ENDDO
    ENDIF

    CALL vvseti('XIN - X Axis Array Increment          ', nvv_subx       )
    CALL vvseti('YIN - Y Axis Array Increment          ', nvv_suby       )
    CALL vvsetr('VMD -                                 ', rvv_vmd        )

    CALL gqplci (ierr, icprev)
    CALL gsplci (COLOR_VECTOR)

    IF ( lo_debug2 ) ierr = PrintBimgStructure (bdimgvec(1))

    IF (rvv_vmd  >  0. ) THEN
       ALLOCATE(zdummy(2*bdimgvec(1)%nxdata * bdimgvec(1)%nydata))
    ELSE
       ALLOCATE(zdummy(1))
    ENDIF

    zdummy = 0.0

    CALL vvinit (pvectdata(1,1,1), NXX,                  &
    &            pvectdata(1,1,2), NXX,                  &
    &            pclrdata,         NXX,                  &
    &            bdimgvec(1)%nxdata, bdimgvec(1)%nydata, &
    &            zdummy, SIZE(zdummy) ) 

    ! change default vector length  
    CALL vvgetr ('DMX - NDC Max vector size        ', zvv_dmx    )

    zvv_tmp_vrl = rvv_vrl * zvv_dmx / (zrr - zrl)

    CALL vvsetr ('VRL - Vector Realized Length     ', zvv_tmp_vrl)
    CALL vvsetr ('VFR - Vector Fractional Minimum  ', rvv_vfr    )

    CALL vvectr (pvectdata(:,:,1), pvectdata(:,:,2), pclrdata, niama, idummy, zdummy)

    CALL set(zrl, zrr, zrb, zrt, zur, zul, zut, zub, ilog)

    CALL gsplci (icprev)
    CALL gflas2
    CALL gsclip (iclip )

    DEALLOCATE(zdummy)
    CALL BimgDeAlloc(bimgwk)

  END SUBROUTINE VectorTrace


  SUBROUTINE VectorCalcModule (bdimgvec, pvectdata, pclrdata)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE VectorCalcModule  ***
    !!
    !! ** Purpose :  Compute the modulus of the vector
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),               INTENT(in) :: bdimgvec(3)
    REAL(KIND=4), DIMENSION(:,:,:), INTENT(in) :: pvectdata  ! (NXX,NYY,2)
    REAL(KIND=4), DIMENSION(:,:),  INTENT(out) :: pclrdata   ! (NXX,NYY)

    REAL(KIND=4)                  :: zspval
    INTEGER(KIND=4)               :: inx, iny
    !!----------------------------------------------------------------------

    inx    = bdimgvec(1)%nxdata
    iny    = bdimgvec(1)%nydata
    zspval = bdimgvec(1)%spval

    WHERE ( (pvectdata(1:inx,1:iny,1) /= zspval) .AND. (pvectdata(1:inx,1:iny,2) /= zspval) )
       pclrdata(1:inx,1:iny) = SQRT ((pvectdata(1:inx,1:iny,1) ** 2) + (pvectdata(1:inx,1:iny,2) ** 2) )
    ELSE WHERE
       pclrdata(1:inx,1:iny) = zspval
    END WHERE

  END SUBROUTINE VectorCalcModule


  SUBROUTINE VecCalcSpeed (pvectdata, bdimgvec, pcoords)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE <routine>  ***
    !!
    !! ** Purpose :   Compute velocity field from BSF . Old QG story
    !!
    !! ** Method  :  Derive the psi field, assuming a regular grid 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:,:), INTENT(inout) :: pvectdata  !(NXX,NYY,2)
    TYPE( bimgfile ),                  INTENT(in) :: bdimgvec
    REAL(KIND=4), DIMENSION(:),        INTENT(in) :: pcoords        ! 4

    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: work
    REAL(KIND=4), DIMENSION(NYY)              :: dx
    REAL(KIND=4)                              :: dy, zdyfile
    REAL(KIND=4)                              :: zrad, zspv, zu, zv
    INTEGER(KIND=4)                           :: inx, iny
    INTEGER(KIND=4)                           :: ji, jj
    !!----------------------------------------------------------------------
    ALLOCATE ( work(NXX,NYY) )

    zdyfile = bdimgvec%dy
    inx     = bdimgvec%nxdata
    iny     = bdimgvec%nydata
    zspv    = bdimgvec%spval

    zrad=4.*ATAN(1.)/180.
    dy=1852.*60*zdyfile

    DO jj=1,inx
       dx(jj) = dy*COS((pcoords(3)+float(jj-1)*zdyfile)*zrad)
    ENDDO

    PRINT *,'compute velocity '
    CALL flush(6)

    DO jj=2,inx-1
       DO ji=2,iny-1
          IF (  (pvectdata(ji  ,jj  ,1) == zspv) .OR. &
            &   (pvectdata(ji+1,jj  ,1) == zspv) .OR. &
            &   (pvectdata(ji-1,jj  ,1) == zspv) .OR. &
            &   (pvectdata(ji  ,jj+1,1) == zspv) .OR. &
            &   (pvectdata(ji  ,jj-1,1) == zspv)) THEN
             work(ji,jj)        = zspv
             pvectdata(ji,jj,2) = zspv
          ELSE     
             zv = (pvectdata(ji+1,jj  ,1) - pvectdata(ji-1,jj  ,1)) / (2*dx(jj))
             zu = (pvectdata(ji  ,jj-1,1) - pvectdata(ji  ,jj+1,1)) / (2*dy    )

             work     (ji,jj)   = zu
             pvectdata(ji,jj,2) = zv
          ENDIF
       ENDDO
    ENDDO

    DO ji=1,iny
       work (ji,1  )        = zspv
       work (ji,inx)        = zspv
       pvectdata (ji,1  ,2) = zspv
       pvectdata (ji,inx,2) = zspv
    ENDDO

    DO jj=1,inx
       work (1   , jj)      = zspv
       work (iny , jj)      = zspv
       pvectdata (1  ,jj,2) = zspv
       pvectdata (iny,jj,2) = zspv
    ENDDO


    pvectdata(:,:,1) = work(:,:)

    DEALLOCATE (work )

  END SUBROUTINE VecCalcSpeed


  SUBROUTINE PutVelOnTpoints (pvectdata, bdimgvec)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE PutVelOnTpoints  ***
    !!
    !! ** Purpose :  Interpolate velocity components from C-grid Location to
    !!               T point ( A-Grid)
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:,:), INTENT(inout) :: pvectdata !  (NXX,NYY,2)
    TYPE( bimgfile ),                  INTENT(in) :: bdimgvec(3)

    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zua, zva     !  (NXX,NYY)
    INTEGER(KIND=4)  :: ji,jj
    INTEGER(KIND=4)  :: inxu, inyu
    INTEGER(KIND=4)  :: inxv,inyv
    REAL(KIND=4)     :: spvalu, spvalv
    !!----------------------------------------------------------------------
    ALLOCATE (zua (NXX,NYY), zva(NXX,NYY) )

    inxu   = bdimgvec(1)%nxdata
    inyu   = bdimgvec(1)%nydata
    spvalu = bdimgvec(1)%spval
    spvalv = bdimgvec(2)%spval

    inxv = bdimgvec(2)%nxdata
    inyv = bdimgvec(2)%nydata

    ! set spval to 0 to ease interpolation
    WHERE ( pvectdata(1:inxu,1:inyu,1) == spvalu )  pvectdata(1:inxu,1:inyu,1) = 0. 
    WHERE ( pvectdata(1:inxv,1:inyv,2) == spvalv )  pvectdata(1:inxv,1:inyv,2) = 0. 
    spvalu = 0.
    spvalv = 0.

    !  U component
    DO jj=1,inyu
       zua(1,jj)=spvalu
       DO ji=2,inxu
          zua(ji,jj)=0.5*(pvectdata(ji-1,jj,1) + pvectdata(ji,jj,1))
       ENDDO
    ENDDO
 
   !   V component
    DO ji=1,inxv
       zva(ji,1)=spvalv
       DO jj=2,inyv
          zva(ji,jj)=0.5*(pvectdata(ji,jj-1,2) + pvectdata(ji,jj,2))
       ENDDO
    ENDDO
    
    pvectdata(1:inxu,1:inyu,1) = zua(1:inxu,1:inyu)
    pvectdata(1:inxv,1:inyv,2) = zva(1:inxv,1:inyv)

    DEALLOCATE (zua, zva )

  END SUBROUTINE PutVelOnTpoints

END MODULE VECTORS
