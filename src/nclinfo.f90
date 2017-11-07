MODULE nclinfo
  !!======================================================================
  !!                     ***  MODULE  nclinfo  ***
  !! Implementation of some NCL functions to get parameters and show them
  !! used for debugging purposed, debug2 level.
  !!=====================================================================
  !! History : 7.0  !  11/2010  J.M. Molines : Original code
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   showcp_param
  !!   showar_param
  !!   showmp_param
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: showcp_param   ! conpack parameters
  PUBLIC :: showar_param   ! area parameters
  PUBLIC :: showmp_param   ! mapping parameters
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

  CONTAINS 

  SUBROUTINE showcp_param (cdfrom)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE showcp_param  ***
    !!
    !! ** Purpose :   Shows conpack parameters
    !!
    !! References :   man conpack_params (NCL)
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdfrom

    INTEGER(KIND=4)   :: ji, ival, incl
    REAL(KIND=4)      :: zval
    CHARACTER(LEN=80) :: cval
    !!----------------------------------------------------------------------
    PRINT *, '#SCP ConPack parameters as seen in ',TRIM(cdfrom)
    CALL cpgeti ('NCL', incl) 
    PRINT *, ' NCL', incl 

    DO ji=  -3, incl
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI',ji) 
      CALL cpgeti ('AIA', ival)
      PRINT *, ' AIA ( ',ji,' )=', ival
    ENDDO

    DO ji=  1, incl
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI',ji) 
      CALL cpgeti ('AIB', ival)
      PRINT *, ' AIB ( ',ji,' )=', ival
    ENDDO

    
    CALL cpgeti ('CAF', ival) 
    PRINT *, 'CAF = ', ival   

    CALL cpgetr ('CFA', zval) 
    PRINT *, 'CFA = ', zval   

    CALL cpgeti ('CFB', ival) 
    PRINT *, 'CFB = ', ival   

    CALL cpgeti ('CFC', ival) 
    PRINT *, 'CFC = ', ival   

    CALL cpgeti ('CFF', ival) 
    PRINT *, 'CFF = ', ival   

    CALL cpgetr ('CFL', zval) 
    PRINT *, 'CFL = ', zval   

    CALL cpgeti ('CFP', ival) 
    PRINT *, 'CFP', ival 

    CALL cpgetr ('CFS', zval) 
    PRINT *, 'CFS', zval 

    CALL cpgetc ('CFT', cval) 
    PRINT *, 'CFT', cval 

    CALL cpgetr ('CFW', zval) 
    PRINT *, 'CFW', zval 

    CALL cpgetr ('CFX', zval) 
    PRINT *, 'CFX', zval 

    CALL cpgetr ('CFY', zval) 
    PRINT *, 'CFY', zval 

    CALL cpgetr ('CIS', zval) 
    PRINT *, 'CIS', zval 

    DO ji=1,10
      CALL cpseti ('PAI', ji )
      CALL cpgetr ('CIT', zval )
      PRINT *, 'CIT(', ji,') ', zval 
    ENDDO

    CALL cpgetr ('CIU', zval) 
    PRINT *, 'CIU', zval 

    DO ji= -3, incl  ! max = see CLV
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI', ji )
      CALL cpgeti ('CLC', ival )
      PRINT *, 'CLC(',ji,') ', ival 
    ENDDO

    DO ji=-3, incl  !
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI', ji )
      CALL cpgetc ('CLD', cval)
      PRINT *, 'CLD(',ji,') ', cval
    ENDDO
   
    DO ji = -3, incl  ! max = see CLV
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI', ji )
      CALL cpgetr ('CLL', zval )
      PRINT *, 'CLL(',ji,') ', zval 
    ENDDO

    CALL cpgeti ('CLS', ival) 
    PRINT *, 'CLS', ival 

    DO ji = -3, incl  ! max = see CLV
      IF (ji == 0 )  CYCLE 
      CALL cpseti ('PAI', ji )
      CALL cpgeti ('CLU', ival )
      PRINT *, 'CLU(',ji,') ', ival 
    ENDDO

    DO ji = 1, incl  ! max = see CLV NCL
      CALL cpseti ('PAI', ji )
      CALL cpgeti ('CLV', ival )
      PRINT *, 'CLV(',ji,') ', ival 
    ENDDO

    CALL cpgetr ('CMN', zval) 
    PRINT *, 'CMN', zval 

    CALL cpgetr ('CMX', zval) 
    PRINT *, 'CMX', zval 

    CALL cpgetc ('CTM', cval)
    PRINT *, 'CTM', cval

    CALL cpgetr ('CWM', zval) 
    PRINT *, 'CWM', zval 

    CALL cpgetr ('DPS', zval) 
    PRINT *, 'DPS', zval 

    CALL cpgeti ('DPU', ival) 
    PRINT *, 'DPU', ival 

    CALL cpgetr ('DPV', zval) 
    PRINT *, 'DPV', zval 

    CALL cpgeti ('GIC', ival) 
    PRINT *, 'GIC', ival 

    CALL cpgeti ('GIL', ival) 
    PRINT *, 'GIL', ival 

    CALL cpgeti ('GIS', ival) 
    PRINT *, 'GIS', ival 

    CALL cpgetr ('HCL', zval) 
    PRINT *, 'HCL', zval 

    CALL cpgetr ('HCS', zval) 
    PRINT *, 'HCS', zval 

    CALL cpgeti ('HCF', ival) 
    PRINT *, 'HCF', ival 

    CALL cpgeti ('HIC', ival) 
    PRINT *, 'HIC', ival 

    CALL cpgetc ('HIT', cval)
    PRINT *, 'HIT', cval

    CALL cpgetr ('HLA', zval) 
    PRINT *, 'HLA', zval 

    CALL cpgeti ('HLB', ival) 
    PRINT *, 'HLB', ival 

    CALL cpgeti ('HLC', ival) 
    PRINT *, 'HLC', ival 

    CALL cpgeti ('HLE', ival) 
    PRINT *, 'HLE', ival 

    CALL cpgetr ('HLL', zval) 
    PRINT *, 'HLL', zval 

    CALL cpgeti ('HLO', ival) 
    PRINT *, 'HLO', ival 

    CALL cpgetr ('HLS', zval) 
    PRINT *, 'HLS', zval 

    CALL cpgetc ('HLT', cval)
    PRINT *, 'HLT', cval

    CALL cpgetr ('HLW', zval) 
    PRINT *, 'HLW', zval 

    CALL cpgeti ('HLX', ival) 
    PRINT *, 'HLX', ival 

    CALL cpgeti ('HLY', ival) 
    PRINT *, 'HLY', ival 

    CALL cpgetr ('ILA', zval) 
    PRINT *, 'ILA', zval 

    CALL cpgeti ('ILB', ival) 
    PRINT *, 'ILB', ival 

    CALL cpgeti ('ILC', ival) 
    PRINT *, 'ILC', ival 

    CALL cpgetr ('ILL', zval) 
    PRINT *, 'ILL', zval 

    CALL cpgeti ('ILP', ival) 
    PRINT *, 'ILP', ival 

    CALL cpgetr ('ILS', zval) 
    PRINT *, 'ILS', zval 

    CALL cpgetc ('ILT', cval)
    PRINT *, 'ILT', cval

    CALL cpgetr ('ILW', zval) 
    PRINT *, 'ILW', zval 

    CALL cpgetr ('ILX', zval) 
    PRINT *, 'ILX', zval 

    CALL cpgetr ('ILY', zval) 
    PRINT *, 'ILY', zval 

    CALL cpgeti ('IWM', ival) 
    PRINT *, 'IWM', ival 

    CALL cpgeti ('IWU', ival) 
    PRINT *, 'IWU', ival 

    CALL cpgeti ('LBC', ival) 
    PRINT *, 'LBC', ival 

    CALL cpgetr ('LBX', zval) 
    PRINT *, 'LBX', zval 

    CALL cpgetr ('LBY', zval) 
    PRINT *, 'LBY', zval 

    CALL cpgeti ('LIS', ival) 
    PRINT *, 'LIS', ival 

    DO ji=1,10
      CALL cpseti ('PAI', ji )
      CALL cpgetr ('LIT', zval )
      PRINT *, 'LIT(', ji,') ', zval 
    ENDDO

    CALL cpgeti ('LIU', ival) 
    PRINT *, 'LIU', ival 

    CALL cpgetr ('LLA', zval) 
    PRINT *, 'LLA', zval 

    CALL cpgeti ('LLB', ival) 
    PRINT *, 'LLB', ival 

    DO ji=1,incl   !NCL
      CALL cpseti ('PAI', ji )
      CALL cpgetr ('LLC', zval )
      PRINT *, 'LLC(',ji,') ', zval 
    ENDDO

    CALL cpgetr ('LLL', zval) 
    PRINT *, 'LLL', zval 

    CALL cpgeti ('LLO', ival) 
    PRINT *, 'LLO', ival 

    CALL cpgeti ('LLP', ival) 
    PRINT *, 'LLP', ival 

    CALL cpgetr ('LLS', zval) 
    PRINT *, 'LLS', zval 

    CALL cpgetc ('LLT', cval) 
    PRINT *, 'LLT', cval 

    CALL cpgetr ('LLW', zval) 
    PRINT *, 'LLW', zval 

    CALL cpgeti ('LOC', ival) 
    PRINT *, 'LOC', ival 

    CALL cpgetc ('LOT', cval) 
    PRINT *, 'LOT', cval 

    CALL cpgeti ('MAP', ival) 
    PRINT *, 'MAP', ival 

    CALL cpgeti ('NEL', ival) 
    PRINT *, 'NEL', ival 

    CALL cpgeti ('NET', ival) 
    PRINT *, 'NET', ival 

    CALL cpgeti ('NEU', ival) 
    PRINT *, 'NEU', ival 

    CALL cpgeti ('NLS', ival) 
    PRINT *, 'NLS', ival 

    CALL cpgeti ('NLZ', ival) 
    PRINT *, 'NLZ', ival 

    CALL cpgeti ('NOF', ival) 
    PRINT *, 'NOF', ival 

    CALL cpgeti ('NSD', ival) 
    PRINT *, 'NSD', ival 

    CALL cpgeti ('NVS', ival) 
    PRINT *, 'NVS', ival 

    CALL cpgetr ('ORV', zval) 
    PRINT *, 'ORV', zval 

    CALL cpgetr ('PC1', zval) 
    PRINT *, 'PC1', zval 

    CALL cpgetr ('PC2', zval) 
    PRINT *, 'PC2', zval 

    CALL cpgetr ('PC3', zval) 
    PRINT *, 'PC3', zval 

    CALL cpgetr ('PC4', zval) 
    PRINT *, 'PC4', zval 

    CALL cpgetr ('PC5', zval) 
    PRINT *, 'PC5', zval 

    CALL cpgetr ('PC6', zval) 
    PRINT *, 'PC6', zval 

    CALL cpgeti ('PIC', ival) 
    PRINT *, 'PIC', ival 

    CALL cpgeti ('PIE', ival) 
    PRINT *, 'PIE', ival 

    CALL cpgetr ('PW1', zval) 
    PRINT *, 'PW1', zval 

    CALL cpgetr ('PW2', zval) 
    PRINT *, 'PW2', zval 

    CALL cpgetr ('PW3', zval) 
    PRINT *, 'PW3', zval 

    CALL cpgetr ('PW4', zval) 
    PRINT *, 'PW4', zval 

    CALL cpgetr ('RC1', zval) 
    PRINT *, 'RC1', zval 

    CALL cpgetr ('RC2', zval) 
    PRINT *, 'RC2', zval 

    CALL cpgetr ('RC3', zval) 
    PRINT *, 'RC3', zval 

    CALL cpgeti ('RWC', ival) 
    PRINT *, 'RWC', ival 

    CALL cpgeti ('RWG', ival) 
    PRINT *, 'RWG', ival 

    CALL cpgeti ('RWM', ival) 
    PRINT *, 'RWM', ival 

    CALL cpgeti ('RWU', ival) 
    PRINT *, 'RWU', ival 

    CALL cpgeti ('SET', ival) 
    PRINT *, 'SET', ival 

    CALL cpgetr ('SFS', zval) 
    PRINT *, 'SFS', zval 

    CALL cpgetr ('SFU', zval) 
    PRINT *, 'SFU', zval 

    CALL cpgetr ('SPV', zval) 
    PRINT *, 'SPV', zval 

    CALL cpgetr ('SSL', zval) 
    PRINT *, 'SSL', zval 

    CALL cpgetr ('T2D', zval) 
    PRINT *, 'T2D', zval 

    CALL cpgetr ('T3D', zval) 
    PRINT *, 'T3D', zval 

    CALL cpgetr ('VPB', zval) 
    PRINT *, 'VPB', zval 

    CALL cpgetr ('VPL', zval) 
    PRINT *, 'VPL', zval 

    CALL cpgetr ('VPR', zval) 
    PRINT *, 'VPR', zval 

    CALL cpgetr ('VPT', zval) 
    PRINT *, 'VPT', zval 

    CALL cpgetr ('VPS', zval) 
    PRINT *, 'VPS', zval 

    CALL cpgetr ('WDB', zval) 
    PRINT *, 'WDB', zval 

    CALL cpgetr ('WDL', zval) 
    PRINT *, 'WDL', zval 

    CALL cpgetr ('WDR', zval) 
    PRINT *, 'WDR', zval 

    CALL cpgetr ('WDT', zval) 
    PRINT *, 'WDT', zval 

    CALL cpgeti ('WSO', ival) 
    PRINT *, 'WSO', ival 

    CALL cpgetr ('XC1', zval) 
    PRINT *, 'XC1', zval 

    CALL cpgetr ('XCM', zval) 
    PRINT *, 'XCM', zval 

    CALL cpgetr ('YC1', zval) 
    PRINT *, 'YC1', zval 

    CALL cpgetr ('YCN', zval) 
    PRINT *, 'YCN', zval 

    CALL cpgeti ('ZD1', ival) 
    PRINT *, 'ZD1', ival 

    CALL cpgeti ('ZDM', ival) 
    PRINT *, 'ZDM', ival 

    CALL cpgeti ('ZDN', ival) 
    PRINT *, 'ZDN', ival 

    CALL cpgeti ('ZDS', ival) 
    PRINT *, 'ZDS', ival 

!   CALL cpgetr ('ZDU', zval) 
!   PRINT *, 'ZDU', zval 

    CALL cpgetr ('ZDV', zval) 
    PRINT *, 'ZDV', zval 

    CALL cpgetr ('ZMN', zval) 
    PRINT *, 'ZMN', zval 

    CALL cpgetr ('ZMX', zval) 
    PRINT *, 'ZMX', zval 

  END SUBROUTINE showcp_param

  SUBROUTINE showar_param (cdfrom)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE showar_param  ***
    !!
    !! ** Purpose :  Shows AREA parameters 
    !!
    !! References :  man page area_params
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdfrom

    INTEGER(KIND=4)   :: ji, ival, incl
    REAL(KIND=4)      :: zval
    CHARACTER(LEN=80) :: cval
    !!----------------------------------------------------------------------
    PRINT *, '#SAP  AREAS parameters as seen in ',TRIM(cdfrom)

    CALL argeti ('AL', ival) 
    PRINT *, 'AL', ival 

    CALL argeti ('AT', ival) 
    PRINT *, 'AT', ival 

    CALL argetr ('AW', zval) 
    PRINT *, 'AW', zval 

    CALL argeti ('DB', ival) 
    PRINT *, 'DB', ival 

    CALL argeti ('DC', ival) 
    PRINT *, 'DC', ival 

    CALL argeti ('DI', ival) 
    PRINT *, 'DI', ival 

    CALL argetr ('ID', zval) 
    PRINT *, 'ID', zval 

    CALL argetr ('IS', zval) 
    PRINT *, 'IS', zval 

    CALL argeti ('LC', ival) 
    PRINT *, 'LC', ival 

    CALL argeti ('RC', ival) 
    PRINT *, 'RC', ival 

 END   SUBROUTINE showar_param

 SUBROUTINE showmp_param (cdfrom)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE  showmp_param  ***
    !!
    !! ** Purpose :  Show ezmap parameters
    !!
    !! References :   man pages ezmap_params
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cdfrom

    INTEGER(KIND=4)   :: ji, ival
    REAL(KIND=4)      :: zval
    CHARACTER(LEN=80) :: cval
    LOGICAL           :: lval
    !!----------------------------------------------------------------------
    PRINT *, '#SEZ EZMAP  parameters as seen in ',TRIM(cdfrom)
    
    CALL mpgetc ('AR', cval) 
    PRINT *, 'AR', cval 

    CALL mpgeti ('C1', ival) 
    PRINT *, 'C1', ival 

    CALL mpgeti ('C2', ival) 
    PRINT *, 'C2', ival 

    CALL mpgeti ('C3', ival) 
    PRINT *, 'C3', ival 

    CALL mpgeti ('C4', ival) 
    PRINT *, 'C4', ival 

    CALL mpgeti ('C5', ival) 
    PRINT *, 'C5', ival 

    CALL mpgeti ('C6', ival) 
    PRINT *, 'C6', ival 

    CALL mpgeti ('C7', ival) 
    PRINT *, 'C7', ival 

    CALL mpgeti ('C8', ival) 
    PRINT *, 'C8', ival 

    CALL mpgeti ('DA', ival) 
    PRINT *, 'DA', ival 

    CALL mpgeti ('DD', ival) 
    PRINT *, 'DD', ival 

    CALL mpgetl ('DL', lval) 
    PRINT *, 'DL', lval 

    CALL mpgetl ('DO', lval) 
    PRINT *, 'DO', lval 

    CALL mpgetl ('EL', lval) 
    PRINT *, 'EL', lval 

    CALL mpgetr ('GD', zval) 
    PRINT *, 'GD', zval 

    CALL mpgetr ('GP', zval) 
    PRINT *, 'GP', zval 

    CALL mpgetr ('GR', zval) 
    PRINT *, 'GR', zval 

    CALL mpgetr ('GT', zval) 
    PRINT *, 'GT', zval 

    CALL mpgetr ('GN', zval) 
    PRINT *, 'GN', zval 

    CALL mpgeti ('G1', ival) 
    PRINT *, 'G1', ival 

    CALL mpgeti ('G2', ival) 
    PRINT *, 'G2', ival 

    CALL mpgetl ('IN', lval) 
    PRINT *, 'IN', lval 

    CALL mpgetl ('LA', lval) 
    PRINT *, 'LA', lval 

    CALL mpgeti ('LS', ival) 
    PRINT *, 'LS', ival 

    CALL mpgetr ('MV', zval) 
    PRINT *, 'MV', zval 

    CALL mpgetc ('OU', cval) 
    PRINT *, 'OU', cval 

    CALL mpgetl ('PE', lval) 
    PRINT *, 'PE', lval 

    CALL mpgetr ('PN', zval) 
    PRINT *, 'PN', zval 

    CALL mpgetc ('PR', cval) 
    PRINT *, 'PR', cval 

    CALL mpgetr ('PT', zval) 
    PRINT *, 'PT', zval 
    
    CALL mpgetr ('P1', zval) 
    PRINT *, 'P1', zval 

    CALL mpgetr ('P2', zval) 
    PRINT *, 'P2', zval 

    CALL mpgetr ('P3', zval) 
    PRINT *, 'P3', zval 

    CALL mpgetr ('P4', zval) 
    PRINT *, 'P4', zval 

    CALL mpgetr ('P5', zval) 
    PRINT *, 'P5', zval 

    CALL mpgetr ('P6', zval) 
    PRINT *, 'P6', zval 

    CALL mpgetr ('P7', zval) 
    PRINT *, 'P7', zval 

    CALL mpgetr ('P8', zval) 
    PRINT *, 'P8', zval 

    CALL mpgetr ('RE', zval) 
    PRINT *, 'RE', zval 

    CALL mpgetr ('RO', zval) 
    PRINT *, 'RO', zval 

    CALL mpgetr ('SA', zval) 
    PRINT *, 'SA', zval 

    CALL mpgetr ('S1', zval) 
    PRINT *, 'S1', zval 

    CALL mpgetr ('S2', zval) 
    PRINT *, 'S2', zval 

    CALL mpgetr ('SR', zval) 
    PRINT *, 'SR', zval 

    CALL mpgeti ('VS', ival) 
    PRINT *, 'VS', ival 

    CALL mpgetr ('XL', zval) 
    PRINT *, 'XL', zval 

    CALL mpgetr ('XR', zval) 
    PRINT *, 'XR', zval 

    CALL mpgetr ('YB', zval) 
    PRINT *, 'YB', zval 

    CALL mpgetr ('YT', zval) 
    PRINT *, 'YT', zval 

 END  SUBROUTINE showmp_param 

END MODULE nclinfo

