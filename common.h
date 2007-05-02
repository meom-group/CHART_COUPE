	include 'parameter.h'

        integer*2 opt_conv, opt_marks

c       carte
        integer*2 opt_map, opt_mapfill,opt_reverse, opt_perim
        integer*2 opt_ocean, opt_showgrid
        integer*2 opt_xgrid, opt_ygrid, opt_zgrid
        integer*2 opt_marg, opt_zoom, opt_rlat,opt_rlon, opt_km
        integer*2 opt_pal,opt_palbar, opt_vertpal, opt_palout,opt_prev
        integer*2 opt_coords, opt_shift, opt_360,  opt_lev, opt_dep
        integer*2 opt_grid, opt_grad, opt_xstep, opt_ystep,opt_zstep
        integer*2 opt_kmstep, opt_ijgrid,opt_high
        integer*2 opt_noproj, opt_labx, opt_laby, opt_labz
	integer*2 opt_sigma, opt_sigmatr, opt_orca, opt_spval, opt_mean, opt_scale, opt_log
        real*4 over_spval, zmean0, zscale

        integer*2 opt_nolat, opt_nolon, opt_xybat

        integer*2 opt_vorticity, nobox


c       texte
        integer*2 opt_outfile, opt_header1, opt_header2, opt_dat, opt_team

c       single/multiple frame (en k)
	integer*2 opt_single

c       chart/coupe: pas de valeurs par defaut, fixe dans coupe/chart
        integer*2 opt_chart, opt_coupe
	
c       couleur
        integer*2 opt_clrdata, opt_clrlim, opt_clrmask,  opt_clrgrid 
        integer*2 opt_clrmark
        integer*2 opt_min,opt_max,opt_color, opt_noint, opt_msk 
        integer*2 opt_print, opt_clrlout, opt_satur, opt_clrmod


c       isocontours     
        integer*2 opt_contdata, opt_contint, opt_contmask,opt_contgrid
        integer*2 opt_minc, opt_maxc, opt_contlim
        integer*2 opt_contours,opt_nollab, opt_cntlout, opt_cntsav
        integer*2 opt_dash, opt_shade

c       vecteurs
        integer*2 opt_vectdata, opt_vectlim, opt_vectmask,opt_vectgrid
        integer*2 opt_vect3D,opt_vect2D,opt_vectX,opt_vectY,opt_vectZ
        integer*2 opt_vectors, opt_vectclr, opt_vectmod, opt_veclout
        integer*2 opt_vecmin, opt_vecmax, opt_vecpsi,opt_vecnotr,opt_vecrot
        integer*2 opt_cgrid, opt_vecshade

c       overlay
	integer*2 opt_overdata, opt_overmark, opt_overclr, opt_overout

	integer*2 nmark
	real*4 clrmark(NCLRMARK), xmark(NCLRMARK)
	common /iclrmark/ nmark
	common /clrmark/  clrmark

        character*8 map_proj, map_zone
        character*32 c_font
        real map_rlat,map_rlon

        real map_coord(4), map_marg(4), map_shift

c       coupe
        integer*2 opt_batgrid, opt_pmin, opt_pmax

c       FORMAT BIMG

        TYPE  bimgfile
           SEQUENCE
           character*80 str1, str2, str3, str4
           character*4 VER
	       character*256 fname
           integer nxfile, nyfile, nxdata,nydata,ncou,nt,ndim,icod
           integer iversion,irecl
           real    x1mod,y1mod,x2mod,y2mod,dx,dy,spval
           logical spval0
           real    time
           integer f_unit, last_layer, last_dim, last_time
           integer last_rect, last_recr
           integer mask, grid
           integer shift
           real    depth(NA)
           real    timea(Ntime)
           real    d_mask(NXX,NYY)
           real    d_xgrid(NXX)
           real    d_ygrid(NYY)
           real    alphasup
           character*80 varname
		   integer ncid
           integer varid
           integer ndims
		   integer nvars
		   integer ngatts
		   integer unlimdimid
		   integer ndimv
		   character*80 modifier
        end  TYPE bimgfile


        common /map/map_coord, map_marg, map_shift,
     .              map_zone, map_rlat,map_rlon,map_proj,
     .              over_spval, zmean0, zscale

        real cs_scale

        common /config/cs_scale,c_font
                
        common /flag/opt_chart, opt_coupe,
     .               opt_pal,opt_palbar,opt_zoom,opt_outfile,opt_vertpal,
     .               opt_palout,opt_prev,nobox,
     .               opt_clrlout,opt_noint, opt_coords,opt_vorticity,
     .               opt_min,opt_max, opt_msk,opt_contours, opt_nollab, 
     .               opt_color, opt_grid, opt_satur,opt_perim,opt_ijgrid,
     .               opt_high,
     .               opt_minc, opt_maxc, opt_xybat, opt_marks,
     .               opt_reverse, opt_marg, opt_labx,opt_laby,opt_labz,
     .               opt_sigma, opt_sigmatr, opt_orca, opt_spval, opt_mean, opt_scale,opt_log,
     .               opt_conv,opt_map,opt_mapfill,opt_batgrid,opt_ocean,
     .               opt_showgrid,
     .               opt_rlat,opt_rlon,opt_km,opt_grad,opt_shift,opt_360,
     .               opt_nolat, opt_nolon, opt_print, opt_lev, opt_dep,
     .               opt_xgrid, opt_ygrid, opt_zgrid, opt_noproj,
     .               opt_clrmod,opt_cntlout,opt_pmin, opt_pmax,
     .               opt_contint, opt_vect3D, opt_vect2D, opt_vectclr,
     .               opt_vectX, opt_vectY, opt_vectZ, opt_vectors,
     .               opt_vectmod, opt_vecmin, opt_vecmax,opt_veclout,
     .               opt_clrdata, opt_clrlim, opt_clrmask, opt_clrgrid,
     .               opt_clrmark,
     .               opt_contdata,opt_contlim,opt_contmask,opt_contgrid,
     .               opt_vectdata,opt_vectlim,opt_vectmask,opt_vectgrid,
     .               opt_header1, opt_header2, opt_vecpsi,opt_vecnotr,
     .               opt_single, opt_dat, opt_team,
     .               opt_vecrot, opt_cgrid, opt_vecshade,
     .               opt_xstep, opt_ystep,opt_zstep,opt_kmstep,
     .               opt_cntsav, opt_dash, opt_shade,
     .               opt_overdata, opt_overmark, opt_overclr, opt_overout

      character*256 filepal, filebat
      character*256 filegrid
      character*80  fileout

      character*256 f_clrdata,f_clrlim,f_clrmask,f_clrgrid,f_clrlout
	  character*256 f_clrmark
      character*256 f_cntdata , f_cntlim, f_cntmask,f_cntgrid, f_cntlout
      character*256 f_vecdata1, f_vecdata2, f_vecdata3, f_veclout
      character*256 f_vecrotdata
      character*256 f_vecmask, f_vecgrid, f_veclim, f_icoddata, f_coords
      character*256 f_zlevel
      character*256 f_batgrid, f_showgrid, f_cntsav
      character*256 f_overdata, f_overmark, f_overout


      common /files/filepal,fileout, filebat,filegrid,f_icoddata,
     .              f_clrdata,f_clrlim,f_clrmask, f_clrgrid,f_clrlout, 
     .              f_clrmark,
     .              f_cntdata ,f_cntlim,f_cntmask,f_cntgrid,f_cntlout, 
     .              f_vecdata1 , f_vecdata2, f_vecdata3, f_veclout,
     .              f_vecrotdata,
     .              f_vecmask, f_vecgrid, f_veclim, f_coords, f_zlevel, 
     .              f_batgrid, f_showgrid, f_cntsav, 
     .              f_overdata, f_overmark, f_overout


      TYPE text_string      
            SEQUENCE
            real          xpos,ypos,csize,align,angle
            character*256 str
      end TYPE text_string

      integer strcount
      integer strcountr

      TYPE( text_string ) ::  text(10), textr(10)
      common /str/ text, strcount
      common /strr/ textr, strcountr
	
	integer*4 lbpos, ncol_pal
	real  rlbsc
	logical xyplotdefault, xypaldefault
      common /palette/ lbpos, xyplotdefault, xypaldefault, ncol_pal,rlbsc

	real req_dep
	integer kdessus
	common /dep/ req_dep, kdessus
       real xygr(0:NXX+1,0:NYY+1,2)
       integer nigr, njgr
        common /gridxy/xygr, nigr, njgr

	integer kclr, kcnt, kvec
	common /single/ kclr, kcnt, kvec

      integer       ct_cls,ct_lis,ct_ilb,ct_ilc
      integer       ct_lbc,ct_llb,ct_llc,ct_llp, ct_ilp
      real          ct_cmn,ct_cmx,ct_cll1, ct_cll2
      real          ct_cis,ct_ilx,ct_ily
      real          ct_ill,ct_cwm,ct_sfs,ct_rc1,ct_rc2
      real          ct_ils, ct_lls
      character*100 ct_ilt

      common /contour/ct_sfs,ct_ilp,ct_rc1,ct_rc2,
     .                ct_cmn,ct_cmx,ct_cls,ct_cis,ct_lis,
     .                ct_cwm,ct_ilx,ct_ily, ct_ilt,ct_ils, ct_lls,
     .                ct_cll1, ct_cll2, ct_ilb, ct_ilc, ct_ill,
     .                ct_lbc,ct_llb,ct_llc,ct_llp

	  character*20 v_clr, v_cnt, v_vecx, v_vecy
	  character*80 modif_clr, modif_cnt, modif_vecx, modif_vecy

	  common /ncdf/ v_clr, v_cnt, v_vecx, v_vecy,
     .              modif_clr, modif_cnt, modif_vecx, modif_vecy
