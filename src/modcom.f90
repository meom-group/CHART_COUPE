MODULE modcom
  !!======================================================================
  !!                     ***  MODULE  modcom  ***
  !! Declaration of most of the variables used in CHART/COUPE
  !!=====================================================================
  !! History : 7.0 : 11/2010 : J.M. Molines : replace common.h 
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------
  USE modparam   ! size of arrays
  USE modoptflag ! define all flags set up in readargs

  IMPLICIT NONE
  PUBLIC

  LOGICAL      :: lo_debug  = .FALSE.  !: logical equivalent of opt_debug
  LOGICAL      :: lo_debug2 = .FALSE.  !: logical equivalent of opt_debug2 (more verbose)
  CHARACTER*8  :: cmap_proj, cmap_zone !: map projection, map zone
  CHARACTER*32 :: c_font               !: char font data base name
  REAL(KIND=4) :: rlat_map, rlon_map   !: center of proj for eg OR
  REAL(KIND=4) :: shift_map            !: eastward shift of map (deg)                        
  REAL(KIND=4) :: rmap_coord(4)        !: corner of the map, set by zoom or by file coord
  REAL(KIND=4) :: rmap_marg(4)         !: map coverage if greater than data coverage


! STRUCTURES
! bimg structure : extension of the DOCTOR norm : all bimg structures start with b
!                                                 if routine dummy argument with bd

  TYPE  bimgfile                       !: define data structure 
     SEQUENCE
     CHARACTER(LEN=80)  :: cstr1, cstr2, cstr3, cstr4  !# bimg comments line
     CHARACTER(LEN=4)   :: cver                        !# file version ID 
     CHARACTER(LEN=256) :: cfname                      !# file name associated to the stucture
     INTEGER(KIND=4)    :: nxfile, nyfile, nzfile      !# spatial file shape
     INTEGER(KIND=4)    :: nt, ndim                    !# time and dimension file shape
     INTEGER(KIND=4)    :: icod                        !# bimg icod : obsolete
     INTEGER(KIND=4)    :: nxdata,nydata               !# size of the data area /= file area 
     INTEGER(KIND=4)    :: nrecl                       !# record length of dimg file
     REAL(KIND=4)       :: x1mod, y1mod, x2mod, y2mod  !# initial data layout from file (modified by the code)
     REAL(KIND=4)       :: dx, dy                      !# pseudo/real resolution of data file
     REAL(KIND=4)       :: spval                       !# special value from data file
     LOGICAL            :: lspval0                     !# set T if spval == 0 , F if not
     REAL(KIND=4)       :: time                        !# time of the working frame
     INTEGER(KIND=4)    :: num                         !# logical unit of the open file
     INTEGER(KIND=4)    :: nlast_layer                 !# last layer read ( bimg file )
     INTEGER(KIND=4)    :: nlast_dim                   !# last dimension read (bimg file)
     INTEGER(KIND=4)    :: nlast_time                  !# last timestep read (bimf file )
     INTEGER(KIND=4)    :: nlast_rect                  !# last record number read (time )
     INTEGER(KIND=4)    :: nlast_recr                  !# last record number read in a time block
     INTEGER(KIND=4)    :: mask                        !# flag (1/0) to record if date are to be masked
     INTEGER(KIND=4)    :: ngrid                       !# flag to define the type of grid (1/2/3)
     INTEGER(KIND=4)    :: nshift                      !# flag (1/0) to indicate if data have already be shifted
!    REAL(KIND=4)       :: depth(NA)                   !# Array of depth read from data 
     REAL(KIND=4), DIMENSION(:), ALLOCATABLE  :: depth                   !# Array of depth read from data 
!    REAL(KIND=4)       :: timea(Nmaxtime)             !# Array of time read from data
     REAL(KIND=4), DIMENSION(:), ALLOCATABLE  :: timea             !# Array of time read from data
!    !EAL(KIND=4)       :: d_mask(NXX,NYY)             !# mask data on the same area than the zoom
     REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: d_mask !            !# mask data on the same area than the zoom
     REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: d_xgrid !            !# mask data on the same area than the zoom
     REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: d_ygrid !            !# mask data on the same area than the zoom
!    REAL(KIND=4)       :: d_xgrid(NXX)                !# longitude or pseudo longitude
!    REAL(KIND=4)       :: d_ygrid(NYY)                !# latitude or pseudo latitude
     REAL(KIND=4)       :: alphasup                    !# weight for vertical interpolation between levels
     CHARACTER(LEN=80)  :: cvarname                    !# NetCdf variable name for this structure
     INTEGER(KIND=4)    :: ncid                        !# NetCdf ID of the data file
     INTEGER(KIND=4)    :: nvarid                      !# varid of the   cvarname variable
     INTEGER(KIND=4)    :: ndims                       !# number of dimensions in the cdf file
     INTEGER(KIND=4)    :: nvars                       !# number of variables in the cdf file
     INTEGER(KIND=4)    :: ngatts                      !# number of global attributes
     INTEGER(KIND=4)    :: nunlimdimid                 !# dimid of the unlimited dimension of the cdf file
     INTEGER(KIND=4)    :: ndimv                       !# number of dimension of the variable
     CHARACTER(LEN=80)  :: cmodifier                   !# modifier for the file (string)
     LOGICAL            :: lcnt                        !# logical flag T for contouring data, F either
     LOGICAL            :: lclr                        !# logical flag T for coloring data, F either
  END TYPE bimgfile

  INTEGER(KIND=4)    ::  numbimg=900   !: ex f_unit LUN for readbimg

  REAL(KIND=4)    :: spval_new         !: new spval                    [-spval  ]
  REAL(KIND=4)    :: vmean0            !: new mean values              [-mean   ]
  REAL(KIND=4)    :: vmean0cnt         !: new mean values for cnt      [-cntmean ]
  REAL(KIND=4)    :: vmean0clr         !: new mean values for clr      [-clrmean ]
  REAL(KIND=4)    :: dscale            !: scaling factor for data      [-scale  ]
  REAL(KIND=4)    :: dscalecnt         !: scaling factor for cntdata   [-scalecnt ]
  REAL(KIND=4)    :: dscaleclr         !: scaling factor for clrdata   [-scaleclr ]
  REAL(KIND=4)    :: scal_cslab        !: scale factor for lbl char sz [-cslab  ]

!----------------------------------------------------------------------------------
! FILES:  files name start with cf     !!  used for                I/O [ option   ]
!----------------------------------------------------------------------------------
  CHARACTER(LEN=256)  :: cf_pal        !: palette file              I  [ -p       ]
  CHARACTER(LEN=256)  :: cf_bathy      !: bathymetric file          I  [ -b       ]
  CHARACTER(LEN=256)  :: cf_batgrid    !: bathymetric grid          I  [-bgrid    ]
  CHARACTER(LEN=80)   :: cf_cgm        !: alternate gmeta name      O  [ -o       ]
  CHARACTER(LEN=256)  :: cf_coords     !: output plotting coords.   O  [ -c       ]
! for sigma coordinates models :
  CHARACTER(LEN=256)  :: cf_zlevel     !: zlevel file for spem      I  [-sigma    ]
! color options :
  CHARACTER(LEN=256)  :: cf_clrdata    !: color data file           I  [-clrdata  ]
  CHARACTER(LEN=256)  :: cf_clrlim     !: color limit file          I  [-clrlim   ]
                                       !: vector limit file         I  [-veclim   ]
  CHARACTER(LEN=256)  :: cf_clrmask    !: color mask file           I  [-clrmask  ]
  CHARACTER(LEN=256)  :: cf_clrgrid    !: color grid file           I  [-clrgrid  ]
  CHARACTER(LEN=256)  :: cf_clrlout    !: color limit output file   O  [-clrlout  ]
  CHARACTER(LEN=256)  :: cf_clrmark    !: clrmark file              I  [-clrmark  ]
! contour options :
  CHARACTER(LEN=256)  :: cf_cntdata    !: contour data file         I  [-cntdata  ]
  CHARACTER(LEN=256)  :: cf_cntlim     !: contour limit file        I  [-cntlim   ]
  CHARACTER(LEN=256)  :: cf_cntmask    !: contour mask file         I  [-cntmask  ]
  CHARACTER(LEN=256)  :: cf_cntgrid    !: contour grid file         I  [-cntgrid  ]
  CHARACTER(LEN=256)  :: cf_cntlout    !: contour limit output file O  [-cntlout  ]
  CHARACTER(LEN=256)  :: cf_cntsav     !:
! vector options :
  CHARACTER(LEN=256)  :: cf_vecdata1   !: 3d vector data file       I  [-vecdata3d]
  !    multiple use   :: cf_vecdata1   !: 2d vector data file       I  [-vecdataxy]
  !    multiple use   :: cf_vecdata1   !: 1d vector data file       I  [-vecdatax ]
  CHARACTER(LEN=256)  :: cf_vecdata2   !: 1d vector data file       I  [-vecdatay ]
  CHARACTER(LEN=256)  :: cf_vecdata3   !: vert. vect data file      I  [-vecdataz ]
  CHARACTER(LEN=256)  :: cf_veclout    !: vector limit output file  O  [-veclout  ]
  CHARACTER(LEN=256)  :: cf_vecrotdata !: rotated data file         O  [-vecrot   ]
  CHARACTER(LEN=256)  :: cf_vecmask    !: vector mask file          I  [-vecmask  ]
  CHARACTER(LEN=256)  :: cf_vecgrid    !: vector grid file          I  [-vecgrid  ]

! overlay options
  CHARACTER(LEN=256)  :: cf_overdata   !: overdata file             I  [-overdata ]
  CHARACTER(LEN=256)  :: cf_overmark   !: overmarl file             I  [-overmark ]
  CHARACTER(LEN=256)  :: cf_overout    !: overlay output file       O  [-overout  ]
  CHARACTER(LEN=256)  :: cf_showgrid   !: grid file to overlay      I  [-showgrid ]
!----------------------------------------------------------------------------------

! text
  TYPE text_string      
     SEQUENCE
     REAL(KIND=4)       :: xpos, ypos              !# position of text to write
     REAL(KIND=4)       :: rcsize                  !# size of text 
     REAL(KIND=4)       :: align                   !# alignment [ -1.0 .... 1.0 ]
     REAL(KIND=4)       :: angle                   !# angle (deg) for the text. Horiz = 0
     INTEGER(KIND=4)    :: icolor                  !# text color index
     CHARACTER(LEN=256) :: cstr                    !# text to be written
  END TYPE text_string

  INTEGER(KIND=4) :: nstrcount   = 0               !: counter for strings 
  INTEGER(KIND=4) :: nstrcountr  = 0               !: counter for rotated strings
  INTEGER(KIND=4) :: nstrcountrc = 0               !: counter for rotated strings + colored 

  TYPE( text_string ) :: text(jp_txt), textr(jp_txt), textrc(jp_txt)

! palette
  INTEGER(KIND=4) :: nlbpos                        !: label position indicator 
                                                   !: 1: below/right 2: above/left 3: both
  INTEGER(KIND=4) :: ncol_jetpal                   !: number of color for automatic jetpal
  REAL(KIND=4)    :: rlbsc                         !: palette label size
  LOGICAL         :: ldefaultxypal                 !: true when using default palette position
! not palette -->  coordonnees
  LOGICAL         :: ldefaultxyplot                !: true when using default plot position

! clrmark option
  INTEGER(KIND=2) :: nmark
  REAL(KIND=4), DIMENSION(NCLRMARK) :: vclrmark    !: array of values read in clrmark file
  REAL(KIND=4), DIMENSION(NCLRMARK) :: xmark       !: absica on the color bar for the clrmark labels

! dep
  REAL(KIND=4)    :: req_dep                       !: required depth -dep option
  INTEGER(KIND=4) :: nkup                          !: level above req_dep all (-dep)
  INTEGER(KIND=4) :: nkclr, nkcnt, nkvec           !: level above -clrdep, -cntdep, -vecdep

! gridxy
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: xygr  !: coordinated of the grid  (:,:,[12])
  INTEGER(KIND=4) :: nigr, njgr                    !: size of the xygr array

!#################################
! contours
  INTEGER(KIND=4)    :: nct_cls                    !: Contour Level Selection flag
  INTEGER(KIND=4)    :: nct_lis                    !: Label Interval Specifier
  INTEGER(KIND=4)    :: nct_ilb                    !: Information Label Box flag
  INTEGER(KIND=4)    :: nct_ilc                    !: Infomation Label Color index

  INTEGER(KIND=4)    :: nct_lbc                    !: Label Box Color index
  INTEGER(KIND=4)    :: nct_llb                    !: Line Label Box flag
  INTEGER(KIND=4)    :: nct_llc                    !: Line Label Color index
  INTEGER(KIND=4)    :: nct_llp                    !: Line Label Positioning
  INTEGER(KIND=4)    :: nct_ilp                    !: Information Label Positioning

  REAL(KIND=4)       :: rct_cmn                    !: Contour Minimum
  REAL(KIND=4)       :: rct_cmx                    !: Contour Maximum
  REAL(KIND=4)       :: rct_cll1, rct_cll2         !: Contour Line Line width
  REAL(KIND=4)       :: rct_cis                    !: Contour Interval Specifier
  REAL(KIND=4)       :: rct_ilx, rct_ily           !: Information Label X coord, Y coord
  REAL(KIND=4)       :: rct_ill                    !: Information Label Line width
  REAL(KIND=4)       :: rct_cwm                    !: Character Width Multiplier
  REAL(KIND=4)       :: rct_sfs                    !: Scale Factor
  REAL(KIND=4)       :: rct_rc1, rct_rc2           !: Regular Scheme Constant 1 - 2 
  REAL(KIND=4)       :: rct_ils                    !: Information Label Size
  REAL(KIND=4)       :: rct_lls                    !: Line Label Size

  CHARACTER(LEN=100) :: cct_ilt                    !: Information Label Text

! ncdf
  CHARACTER(LEN=20)  :: cv_clr                      !: color netcdf variable name
  CHARACTER(LEN=20)  :: cv_cnt                      !: contour netcdf variable name
  CHARACTER(LEN=20)  :: cv_vecx, cv_vecy             !: vector netcdf variables name (U, V)
  CHARACTER(LEN=80)  :: cmodif_clr                  !: Color modifier
  CHARACTER(LEN=80)  :: cmodif_cnt                  !: Contour modifier
  CHARACTER(LEN=80)  :: cmodif_vecx, cmodif_vecy    !: Vector modifier (U, V)
 
  REAL(KIND=4)       :: prof_min, prof_max          !: Minimum and Maximum depth (m)

  CHARACTER(LEN=20)  :: cxaxist                     !: X-axis label name
  CHARACTER(LEN=20)  :: cyaxist                     !: Y-axis label name
  CHARACTER(LEN=20)  :: ckmaxist                    !: 'km' transect label

! Calcul
  INTEGER(KIND=4)    :: max_lev                     !: Max level 
  INTEGER(KIND=4)    :: max_tstp                    !: Max time step
  INTEGER(KIND=4)    :: ncl_dim                     !: Choosen dim (clrdim)
  INTEGER(KIND=4)    :: nct_dim                     !: Choosen dim (cntdim)

  INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: nlevel !: selected levels
  INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: ntime  !: selected time steps

! Overlays
  REAL(KIND=4)       :: rover_lw                    !: overlay line width
  INTEGER(KIND=4)    :: nover_ic                    !: overlay line color index

  INTEGER(KIND=4)    :: nover_mk                    !: overlay marker index
  INTEGER(KIND=4)    :: nover_mkic                  !: overlay marker color index
  REAL(KIND=4)       :: rover_mksc                  !: overlay marker size

! mark_step                       
  INTEGER(KIND=4)    :: mark_step                   !: option -mark or -mark1

! layout  in plot coordinates -xy... options
  REAL(KIND=4)    :: x1pos,x2pos,y1pos,y2pos       !: position of the plot area
  REAL(KIND=4)    :: x1bat,x2bat,y1bat,y2bat       !: position of the bathy (coupe)
  REAL(KIND=4)    :: x1pal,x2pal,y1pal,y2pal       !: position of the color bar
  REAL(KIND=4)    :: ylon,ylat,ykm                 !: y-position of label (coupe)

! zoom ?
  INTEGER(KIND=4)  :: nimin, nimax, njmin, njmax   !: index of selection to be plotted 
  TYPE( bimgfile ) bimgwk                          !: working bimg structure used with wwmuxy

! trace 
  REAL(KIND=4)    :: xstep, ystep, zstep, kmstep   !: tick distance on axis
  INTEGER(KIND=4) :: nxsub, nysub, nzsub, nkmsub   !: minor tick sampling
  REAL(KIND=4)    :: xlw, ylw, zlw                 !: Major Tick Line Width (X Y Z )

! vectors
  REAL(KIND=4)       :: rvv_vrl                     !: Vector Realized Length
  REAL(KIND=4)       :: rvv_vfr                     !: Vector FRactional Minimum
  REAL(KIND=4)       :: rvv_lwd                     !: vector Line WiDth
  REAL(KIND=4)       :: rvv_vlc                     !: Vector Low Cutoff 
  REAL(KIND=4)       :: rvv_vhc                     !: Vector High Cutoff
  REAL(KIND=4)       :: rvv_vrm                     !: Vector Reference Maximum
  REAL(KIND=4)       :: rvv_vmd                     !: Vector Minimum Distance  ??? -vecvmd undocumented
  REAL(KIND=4)       :: rvv_mns, rvv_mxs            !: vector Text Bloc Character size min, max
  REAL(KIND=4)       :: rvv_mnx, rvv_mny            !: Coordinates of Minimum Vector Text bloc
  REAL(KIND=4)       :: rvv_mxx, rvv_mxy            !: Coordinates of Maximum Vector Text bloc

  CHARACTER(LEN=36 ) :: cvv_mnt, cvv_mxt            !: Vector Text String (Minimum, Maximum)

  INTEGER(KIND=4)    :: nvv_subx, nvv_suby          !: Vector SUB sampling (X, Y )
  INTEGER(KIND=4)    :: nvv_mnp, nvv_mxp            !: Vector Text String Positioning (Maxi, Mini)
  INTEGER(KIND=4)    :: nvv_vst = 0                 !: Vector Statistic flag  (no option associated !)
  INTEGER(KIND=4)    :: nvv_met                     !: Vector coloring method
  INTEGER(KIND=4)    :: nvv_vpo                     !: Vector POsitioning
  INTEGER(KIND=4)    :: nvv_aclr                    !: Vector Filled arrow : Color index

! marks for palettes ...
  INTEGER(KIND=4)  :: marks(NCLRMARK)                   !: 
  INTEGER(KIND=4)  :: marks_count                  !:
  INTEGER(KIND=4)  :: marks_index(NCLRMARK+1)             !:

! sigma level for the SPEM nostalgic
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rys  !: (NXX+1, NYY+1)  sigma coordinate levels
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rzs  !: (NXX,NA) 
! for coupe 
  REAL(KIND=4), PUBLIC  ::  angled, cut_dist

  !!----------------------------------------------------------------------

END MODULE modcom
