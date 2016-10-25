MODULE util
  !!======================================================================
  !!                     ***  MODULE  util  ***
  !! A collection of utility subroutine for the modreadargs essentially
  !!=====================================================================
  !! History : 1.0  !  06/1993     E. Brown    : original code
  !!           7.0  !  12/2010    J.M. Molines : F90 and Doctor
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !!   routines      : description
  !! OpenNCAR_GKS
  !! FindMinMax 
  !! GetStringParameters 
  !! GetStringrParameters 
  !! GetStringrcParameters 
  !! GetStringtParameters 
  !! PrintMessage
  !! LevelOfDepth
  !! flush 
  !! FindFile 
  !!----------------------------------------------------------------------
  USE modcom

  IMPLICIT NONE

  PRIVATE

  INTEGER, PUBLIC,  PARAMETER :: jp_file_dta = 1  !: file type for FindArg
  INTEGER, PUBLIC,  PARAMETER :: jp_file_pal = 2  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_file_lim = 3  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_file_stp = 4  !:

  INTEGER, PUBLIC,  PARAMETER :: jp_Err      = 1  !:  Error Flag
  INTEGER, PUBLIC,  PARAMETER :: jp_Warn     = 0  !:  Warning Flag

  INTEGER, PUBLIC,  PARAMETER :: jp_ErrMsg   = 15 !:  Max number of error message

  INTEGER, PUBLIC,  PARAMETER :: jp_IncorLev = 1  !:  Mnemo for error message
  INTEGER, PUBLIC,  PARAMETER :: jp_CoordOut = 2  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_VecMisMa = 3  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_ChoosMet = 4  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_NoMoreCo = 5  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_NoFile   = 6  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_SyntStri = 7  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_SyntForm = 8  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_IncorTim = 9  !:
  INTEGER, PUBLIC,  PARAMETER :: jp_VecNoSup = 10 !:
  INTEGER, PUBLIC,  PARAMETER :: jp_DimNoSup = 11 !:
  INTEGER, PUBLIC,  PARAMETER :: jp_NoFilFor = 12 !:
  INTEGER, PUBLIC,  PARAMETER :: jp_NoClrDta = 13 !:
  INTEGER, PUBLIC,  PARAMETER :: jp_ErrCntSh = 14 !:
  INTEGER, PUBLIC,  PARAMETER :: jp_ErrPal   = 15 !:

  PUBLIC :: OpenNCAR_GKS
  PUBLIC :: FindMinMax
  PUBLIC :: GetStringParameters
  PUBLIC :: GetStringrParameters
  PUBLIC :: GetStringrcParameters
  PUBLIC :: GetStringtParameters
  PUBLIC :: PrintMessage
  PUBLIC :: LevelOfDepth
  PUBLIC :: FindFile

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

  CONTAINS 


  SUBROUTINE OpenNCAR_GKS()
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE OpenNCAR_GKS  ***
    !!
    !! ** Purpose : Open and initialise the NCAR metafile
    !!
    !! ** Method  :  Implement gesc (GKS ESCape) in order to change
    !!              the standard gmeta name  
    !!
    !! References :  man ncar
    !!----------------------------------------------------------------------
    CHARACTER(LEN=300)   :: cl_cmd
    CHARACTER(LEN=80)    :: cdum
    !!----------------------------------------------------------------------
    CALL gopks (6,0)       ! Open GKS, unit 6 being the error output. 0 is dummy

    IF (opt_outfile == 1) THEN         
    ! if file already exists, it must be removed (not documented, but verified !)
       WRITE (cl_cmd,702) TRIM(cf_cgm)
702    FORMAT('rm -f ',a)
       CALL system (cl_cmd )
       CALL gesc(-1391,1,cf_cgm,1,1,cdum)  ! -1391 = change metafile name
    ENDIF

                             !  CONID is a fortan logical unit
    IF (opt_pdf == 1 ) THEN
       IF (opt_port == 1 ) CALL gopwk (1, 149, 11)  !  WKID = 11,  CONID = 149  WKTYPE = 1 (PDF) portrait
       IF (opt_land == 1 ) CALL gopwk (1, 149, 12)  !  WKID = 11,  CONID = 149  WKTYPE = 1 (PDF) landscape
    ELSE
       CALL gopwk (1, 149, 1)   !  WKID = 1,  CONID = 149  WKTYPE = 1 (NCGM)
    ENDIF
    CALL gacwk (1)           !  Activate workstation
    CALL gopwk (9, 150, 3)   !  WKID = 9,  CONID = 150  WKTYPE = 3 (WISS)
                             !  This work station is used in conjonction with gflas

  END SUBROUTINE OpenNCAR_GKS


  INTEGER(KIND=4)  FUNCTION FindFile (cd_fname, kftype)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION FindFile  ***
    !!
    !! ** Purpose :   Try to find a file in the current directory, then
    !!              in default user directory for this type of file, then
    !!              finally in the directory pointed by BIMG_ROOT environment
    !!              variable.
    !!
    !! ** Method  : Usefull for palette an eventually limits. Return 0 if file
    !!              is found, 1 if not.
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(inout) :: cd_fname
    INTEGER(KIND=4),     INTENT(in) :: kftype

    CHARACTER(LEN=256)   :: cl_ftmp
    CHARACTER(LEN=256)   :: cl_rootdir, cl_env
    INTEGER(KIND=4)      :: ilen_f
    INTEGER(KIND=4)      :: ivalue, ji
    LOGICAL              :: lexist
    CHARACTER(LEN=4)     :: cl_types(4)
    CHARACTER(LEN=20)    :: cl_envnam(4)
    !!----------------------------------------------------------------------
    cl_types ( jp_file_dta ) = 'data' ; cl_envnam( jp_file_dta ) = 'BIMG_DATDIR'
    cl_types ( jp_file_pal ) = 'pal'  ; cl_envnam( jp_file_pal ) = 'BIMG_PALDIR'
    cl_types ( jp_file_lim ) = 'lim'  ; cl_envnam( jp_file_lim ) = 'BIMG_LIMDIR'
    cl_types ( jp_file_stp ) = 'stp'  ; cl_envnam( jp_file_stp ) = 'BIMG_STSDIR'

    ivalue = 1

    cl_env = cl_envnam(kftype)
    ilen_f = LEN_TRIM(cd_fname)

    IF (ilen_f == 0) THEN
       FindFile = 1
       RETURN
    ENDIF

    ! check in the current directory
    INQUIRE ( FILE=cd_fname, EXIST=lexist)
    IF ( lexist) THEN
       ivalue = 0
    ELSE
       ! look in user default directory
       CALL getenv(cl_env, cl_rootdir)
       IF (cl_rootdir /= ' ') THEN
          WRITE (cl_ftmp,600) TRIM(cl_rootdir),  TRIM(cd_fname)
          INQUIRE ( FILE=cl_ftmp, EXIST=lexist)

          IF ( lexist) THEN
             ivalue = 0
             cd_fname = cl_ftmp
          ENDIF
       ENDIF

       ! if still not found look in BIMG_ROOT
       IF (.NOT. lexist ) THEN
          CALL getenv('BIMG_ROOT', cl_rootdir)
          IF (cl_rootdir /= ' ') THEN
             cl_rootdir = TRIM(cl_rootdir)//'/'//TRIM(cl_types(kftype))

             WRITE (cl_ftmp,600) TRIM(cl_rootdir), TRIM(cd_fname)
             INQUIRE ( FILE=cl_ftmp, EXIST=lexist)

             IF ( lexist) THEN
                ivalue = 0
                cd_fname = cl_ftmp
             ENDIF
          ENDIF
       ENDIF
    ENDIF

600 FORMAT(a,'/',a)
    FindFile = ivalue

  END FUNCTION FindFile


  SUBROUTINE FindMinMax (bdimg, pdata_in, pmin, pmax)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE FindMinMax  ***
    !!
    !! ** Purpose : Return minimum and maximum of unmasked data in pdata_in
    !!
    !! ** Method  :   Use standard F90 functions
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ),             INTENT(in) :: bdimg
    REAL(KIND=4), DIMENSION(:,:), INTENT(in) :: pdata_in   !  (NXX,NYY)
    REAL(KIND=4),                INTENT(out) :: pmin,pmax
    
    INTEGER(KIND=4)       :: inx, iny
    !!----------------------------------------------------------------------
    inx = bdimg%nxdata
    iny = bdimg%nydata

    pmax=MAXVAL(pdata_in(1:inx,1:iny),(pdata_in(1:inx,1:iny) /= bdimg%spval ))
    pmin=MINVAL(pdata_in(1:inx,1:iny),(pdata_in(1:inx,1:iny) /= bdimg%spval ))

    IF ( pmin == huge(1.) .AND. pmax == -huge(1.) ) THEN
      pmax=0. ; pmin = 0.
      PRINT *, 'WARNING : all data values are masked (spval)'
    ENDIF

  END SUBROUTINE FindMinMax


  SUBROUTINE GetStringParameters (cd_in, sd_textstruct)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetStringParameters  ***
    !!
    !! ** Purpose :  Parse argument of the -string option when read from file
    !!
    !! ** Method  :   Look for 4 values separated by at least one blank character
    !!                and a string isolated either by '  ' or  "  "
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(inout) :: cd_in
    TYPE( text_string ), INTENT(inout) :: sd_textstruct

    INTEGER(KIND=4)  :: ib, ie  
    CHARACTER(LEN=1) :: cmatch
    !!----------------------------------------------------------------------
    READ( cd_in , * ) sd_textstruct%xpos,   sd_textstruct%ypos, &
        &             sd_textstruct%rcsize, sd_textstruct%align
    cmatch='"'
    ib = index ( cd_in,  cmatch )

    IF  ( ib == 0 ) THEN
       cmatch=''''
       ib = index ( cd_in, cmatch )
       IF ( ib == 0 ) THEN
         PRINT *, ' empty string '
         sd_textstruct%cstr = ' ' 
         RETURN
       ENDIF
    ENDIF

    ie= index ( cd_in, cmatch, .true. ) - 1 
    IF (ie == ib-1 )  ie = LEN_TRIM(cd_in)

    sd_textstruct%cstr = cd_in( ib+1 : ie )

  END SUBROUTINE GetStringParameters


  SUBROUTINE GetStringrParameters (cd_in, sd_textstruct)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetStringrParameters  ***
    !!
    !! ** Purpose :  Parse argument of the -stringr option when read from file
    !!
    !! ** Method  :   Look for 5 values separated by at least one blank character
    !!                and a string isolated either by '  ' or  "  "
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(inout) :: cd_in
    TYPE( text_string ), INTENT(inout) :: sd_textstruct

    INTEGER(KIND=4)  :: ib, ie
    CHARACTER(LEN=1) :: cmatch
    !!----------------------------------------------------------------------
    READ( cd_in , * ) sd_textstruct%xpos,   sd_textstruct%ypos, &
        &             sd_textstruct%rcsize, sd_textstruct%align, sd_textstruct%angle
    cmatch='"'
    ib = index ( cd_in,  cmatch )

    IF  ( ib == 0 ) THEN
       cmatch=''''
       ib = index ( cd_in, cmatch )
       IF ( ib == 0 ) THEN
         PRINT *, ' empty string '
         sd_textstruct%cstr = ' '
         RETURN
       ENDIF
    ENDIF

    ie= index ( cd_in, cmatch, .true. ) - 1
    IF (ie == ib-1 )  ie = LEN_TRIM(cd_in)

    sd_textstruct%cstr = cd_in( ib+1 : ie )

  END SUBROUTINE GetStringrParameters

  SUBROUTINE GetStringrcParameters (cd_in, sd_textstruct)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetStringrcParameters  ***
    !!
    !! ** Purpose :  Parse argument of the -stringr option when read from file
    !!
    !! ** Method  :   Look for 5 values separated by at least one blank character
    !!                and a string isolated either by '  ' or  "  "
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(inout) :: cd_in
    TYPE( text_string ), INTENT(inout) :: sd_textstruct

    INTEGER(KIND=4)  :: ib, ie
    CHARACTER(LEN=1) :: cmatch
    !!----------------------------------------------------------------------
    READ( cd_in , * ) sd_textstruct%xpos,   sd_textstruct%ypos, &
        &             sd_textstruct%rcsize, sd_textstruct%align, sd_textstruct%angle, &
        &             sd_textstruct%icolor
    cmatch='"'
    ib = index ( cd_in,  cmatch )

    IF  ( ib == 0 ) THEN
       cmatch=''''
       ib = index ( cd_in, cmatch )
       IF ( ib == 0 ) THEN
         PRINT *, ' empty string '
         sd_textstruct%cstr = ' '
         RETURN
       ENDIF
    ENDIF

    ie= index ( cd_in, cmatch, .true. ) - 1
    IF (ie == ib-1 )  ie = LEN_TRIM(cd_in)

    sd_textstruct%cstr = cd_in( ib+1 : ie )

  END SUBROUTINE GetStringrcParameters



  SUBROUTINE GetStringtParameters (cd_in, sd_textstruct)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetStringtParameters  ***
    !!
    !! ** Purpose :  Parse argument of the -title option when read from file
    !!
    !! ** Method  :   Look for  a string isolated either by '  ' or  "  "
    !!              Position and size already determined
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),    INTENT(inout) :: cd_in
    TYPE( text_string ), INTENT(inout) :: sd_textstruct

    INTEGER(KIND=4)  :: ib, ie
    CHARACTER(LEN=1) :: cmatch
    !!----------------------------------------------------------------------
    cmatch='"'
    ib = index ( cd_in,  cmatch )

    IF  ( ib == 0 ) THEN
       cmatch=''''
       ib = index ( cd_in, cmatch )
       IF ( ib == 0 ) THEN
         PRINT *, ' empty string '
         sd_textstruct%cstr = ' '
         RETURN
       ENDIF
    ENDIF

    ie= index ( cd_in, cmatch, .true. ) - 1
    IF (ie == ib-1 )  ie = LEN_TRIM(cd_in)

    sd_textstruct%cstr = cd_in( ib+1 : ie )

  END SUBROUTINE GetStringtParameters


  SUBROUTINE PrintMessage (kfatal, kmsgnum, cdmess )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE PrintMessage  ***
    !!
    !! ** Purpose :  Print an error message and stop if kfatal is 1 
    !!
    !! ** Method  : Prepare this routine for multi language.
    !!              default is now english, but french messages remains. 
    !!              cdmess is used to add extra words to the message to
    !!              make it clearer
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4),   INTENT(in) :: kfatal    ! 1 = fatal, 0 = warning only
    INTEGER(KIND=4),   INTENT(in) :: kmsgnum   ! number of the message to print
    CHARACTER(LEN=*), INTENT(in) :: cdmess    ! message to print in addition

    CHARACTER(LEN=80) ::  clErrMsg_fr(jp_ErrMsg)         !15
    CHARACTER(LEN=80) ::  clErrMsg_en(jp_ErrMsg)         !15
    !!----------------------------------------------------------------------

    DATA clErrMsg_fr &
    &  /'ERREUR : il n''y a pas suffisamment de couches dans: ',                 &  ! jp_IncorLev  ! 1
    &   'ERREUR : coordonnees de zoom hors du fichier.',                         &  ! jp_CoordOut  ! 2
    &   'ERREUR : les composantes vectorielles ne correspondent pas.',           &  ! jp_VecMisMa  ! 3
    &   'ERREUR : veuillez choisir la methode 1 ou 2',                           &  ! jp_ChoosMet  ! 4
    &   'ERREUR : 2D ou 3D deja specifie n''ajoutez pas de composantes',         &  ! jp_NoMoreCo  ! 5
    &   'ERREUR : ce fichier n''existe pas : ',                                  &  ! jp_NoFile    ! 6
    &   'ERREUR de syntaxe : option -string',                                    &  ! jp_SyntStru  ! 7
    &   'ERREUR de syntaxe option FORMAT',                                       &  ! jp_SyntForm  ! 8
    &   'ERREUR : il n''y a pas suffisamment de pas de temps dans: ',            &  ! jp_IncorTIM  ! 9
    &   'ERREUR : option -direct non encore supportee pour ce type de vecteur ', &  ! jp_VecNoSup  ! 10
    &   'ERREUR : option -direct non encore supportee pour dim /= 1 ',           &  ! jp_DimNoSup  ! 11
    &   'ERREUR : Version de format non supportee actuellement: ',               &  ! jp_NoFilFor  ! 12
    &   'WARNING : option -clrdata ignoree a cause de cntshade ',                &  ! jp_NoClrDta  ! 13
    &   'ERREUR : Pour cntshade il faut specifier cntdata  ',                    &  ! jp_ErrCntSh  ! 14
    &   'WARNING : Pour cntshade la palette est figee ! '/                          ! jp_ErrPal    ! 15


    DATA clErrMsg_en &
    &  /'ERROR : not enough layers in : ',                                      &  ! jp_IncorLev  ! 1
    &   'ERROR : coordinates out of file layout',                               &  ! jp_CoordOut  ! 2
    &   'ERROR : vector component does not have the same size',                 &  ! jp_VecMisMa  ! 3
    &   'ERROR : choose method 1 or method 2 ',                                 &  ! jp_ChoosMet  ! 4
    &   'ERROR : 2D or 3D already specified. No more components required',      &  ! jp_NoMoreCo  ! 5
    &   'ERROR : this file does not exist : ',                                  &  ! jp_NoFile    ! 6
    &   'ERROR : bad syntax in -string option ',                                &  ! jp_SyntStru  ! 7
    &   'ERROR : bad syntax in FORMAT string',                                  &  ! jp_SyntForm  ! 8
    &   'ERROR : not enough time steps in :  ',                                 &  ! jp_IncorTIM  ! 9
    &   'ERROR : -direct option not supported for this type of vector ',        &  ! jp_VecNoSup  ! 10
    &   'ERROR : -direct option not supported when dim /= 1 ',                  &  ! jp_DimNoSup  ! 11
    &   'ERROR : file format not supported  ',                                  &  ! jp_NoFilFor  ! 12
    &   'WARNING : -clrdata option ignored, -cntshade in use ',                 &  ! jp_NoClrDta  ! 13
    &   'ERROR : for cntshade, you must specify -cntdata  ',                    &  ! jp_ErrCntSh  ! 14
    &   'WARNING : for cntshade, the palettes in predetermined '/                  ! jp_ErrPal    ! 15

    IF ( opt_english == 1 ) THEN
       PRINT *, TRIM (clErrMsg_en(kmsgnum)) , TRIM (cdmess)
    ELSE
       PRINT *, TRIM (clErrMsg_fr(kmsgnum)) , TRIM (cdmess)
    ENDIF

    IF ( kfatal == jp_Err ) STOP

  END SUBROUTINE PrintMessage


  SUBROUTINE LevelOfDepth(bdimg, pdep, ksup, kinf, knear)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE LevelOfDepth  ***
    !!
    !! ** Purpose : Return the level indices for a given depth as argument.
    !!              return the level above (ksup), the level below, and
    !!              the nearest level.  
    !!
    !!----------------------------------------------------------------------
    TYPE( bimgfile ), INTENT(inout) :: bdimg
    REAL(KIND=4),        INTENT(in) :: pdep
    INTEGER(KIND=4),    INTENT(out) :: ksup, kinf, knear

    INTEGER(KIND=4)         :: ik
    INTEGER(KIND=4)         :: inz
    REAL(KIND=4)            :: zdd1, zdd2, zdep
    !!----------------------------------------------------------------------
    inz  = bdimg%nzfile
    zdep = -ABS(pdep)   ! in order not to change input pdep
    ik   = 1

    DO WHILE (zdep < bdimg%depth(ik))
       ik = ik + 1
       IF (ik > inz) THEN
          PRINT *,' Required depth is larger than maximum depth !'
          STOP
       ENDIF
    ENDDO

    ksup = ik-1
    kinf = ik
    zdd1 = bdimg%depth(ksup) - zdep
    zdd2 = zdep - bdimg%depth(kinf)
    IF (zdd1 < zdd2) THEN
       knear = ksup
    ELSE
       knear = kinf
    ENDIF
    bdimg%alphasup= ( zdep - bdimg%depth(kinf) )/( bdimg%depth(ksup) - bdimg%depth(kinf) )

  END SUBROUTINE LevelOfDepth


  SUBROUTINE flush (kk)
   INTEGER(KIND=4) :: kk
   ! dummy routine
  END SUBROUTINE flush

END MODULE util
