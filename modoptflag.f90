MODULE modoptflag
  !!======================================================================
  !!                     ***  MODULE  modoptflag  ***
  !! Define all the opt_xxx integer*3 variables used for command options
  !!=====================================================================
  !! History : 7.O  ! 11/1020  ! J.M. Molines : Original from common.h
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! CHART/COUPE 7.0 , MEOM 2010
  !! $Id$
  !! Copyright (c) 2010, J.-M. Molines.
  !! Software governed by the CeCILL licence (Licence/CHARTCeCILL.txt)
  !!----------------------------------------------------------------------

  IMPLICIT NONE

  PUBLIC

  ! All the option flags defined in this module correspond to command line options.
  ! They are all interg*2 variables, and their name exclusively starts with opt_
  ! This is an accepted violation of the DOCTOR norm used else where.

  INTEGER(KIND=2) :: opt_marks
  !       carte
  INTEGER(KIND=2) :: opt_map, opt_mapfill,opt_reverse, opt_perim
  INTEGER(KIND=2) :: opt_ocean, opt_showgrid
  INTEGER(KIND=2) :: opt_xgrid, opt_ygrid, opt_zgrid
  INTEGER(KIND=2) :: opt_marg, opt_zoom, opt_rlat,opt_rlon, opt_km
  INTEGER(KIND=2) :: opt_pal,opt_palbar, opt_vertpal, opt_palout,opt_prev
  INTEGER(KIND=2) :: opt_coords, opt_shift, opt_360,  opt_lev, opt_dep
  INTEGER(KIND=2) :: opt_grid, opt_grad, opt_xstep, opt_ystep,opt_zstep
  INTEGER(KIND=2) :: opt_kmstep, opt_ijgrid,opt_high
  INTEGER(KIND=2) :: opt_noproj, opt_labx, opt_laby, opt_labz
  INTEGER(KIND=2) :: opt_sigma, opt_sigmatr, opt_orca, opt_spval, opt_mean, opt_scale
  INTEGER(KIND=2) :: opt_scalecnt, opt_scaleclr, opt_meancnt, opt_meanclr
  INTEGER(KIND=2) :: opt_log, opt_clrlog, opt_cntlog

  INTEGER(KIND=2) :: opt_nolat, opt_nolon, opt_xybat

  INTEGER(KIND=2) :: opt_nobox
  !       texte
  INTEGER(KIND=2) :: opt_outfile, opt_dat, opt_team

  !       single/multiple frame (en k)
  INTEGER(KIND=2) :: opt_single

  !       chart/coupe: pas de valeurs par defaut, fixe dans coupe/chart
  INTEGER(KIND=2) :: opt_chart, opt_coupe
  !       couleur
  INTEGER(KIND=2) :: opt_clrdata, opt_clrlim, opt_clrmask,  opt_clrgrid 
  INTEGER(KIND=2) :: opt_clrmark
  INTEGER(KIND=2) :: opt_min,opt_max,opt_color, opt_noint, opt_msk 
  INTEGER(KIND=2) :: opt_print, opt_clrlout, opt_satur, opt_clrmod
  !       isocontours     
  INTEGER(KIND=2) :: opt_contdata, opt_cntint, opt_contmask,opt_contgrid
  INTEGER(KIND=2) :: opt_minc, opt_maxc, opt_cntlim
  INTEGER(KIND=2) :: opt_contours,opt_nollab, opt_cntlout, opt_cntsav
  INTEGER(KIND=2) :: opt_dash, opt_shade

  !       vecteurs
  INTEGER(KIND=2) :: opt_vectdata, opt_vectlim, opt_vectmask,opt_vectgrid
  INTEGER(KIND=2) :: opt_vect3D,opt_vect2D,opt_vectX,opt_vectY,opt_vectZ
  INTEGER(KIND=2) :: opt_vectors, opt_vectclr, opt_vectmod, opt_veclout
  INTEGER(KIND=2) :: opt_vecmin, opt_vecmax, opt_vecpsi,opt_vecnotr,opt_vecrot
  INTEGER(KIND=2) :: opt_cgrid, opt_vecshade

  !       overlay
  INTEGER(KIND=2) :: opt_overdata, opt_overmark, opt_overclr, opt_overout
  !       divers
  INTEGER(KIND=2) :: opt_forcexy
  INTEGER(KIND=2) :: opt_debug, opt_debug2 
  !       coupe
  INTEGER(KIND=2) :: opt_batgrid, opt_pmin, opt_pmax
  !       language
  INTEGER(KIND=2) :: opt_english
 

END MODULE modoptflag
