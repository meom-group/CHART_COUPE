c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe
c Fichier        :  readargs.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  01/07/93
c Mise a Jour    :  
c
c Description    :  contient la fonction ReadArgs, qui gere les options
c                   de la ligne de commande.
c 
c ---------------------------------------------------------------------

      subroutine ReadArgs (narg)
        
      implicit none

      include 'common.h'
      real angled, cut_dist
      common /cut_properties/ angled, cut_dist
      
      integer*4 state

      integer narg, i, lStr, lnblnk
      character*256  str_line
      character*30   str_opt
      character*256  str_arg
      character*30   dum
      character*256 setup, cmd
      integer FindArg,ibidon
      integer ParseOptionLine



      integer FindFile

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save   /layout/


c     lire les options et positionner les flags 

      i=1
      xyplotdefault=.true.
      xypaldefault=.true.

      do while (i.le.narg)
           
         call getarg(i,dum)

c     lecture d'un fichier de configuration

         if ((dum.eq.'-setup').or.
     .       (dum.eq.'-s')) then

            i=i+1
            call getarg(i,setup)
            i = i+1

            if (FindFile (setup, 4).eq.1) then
               call PrintMessage (1,6,setup)
            else 
               open (20,file=setup,iostat=state,status='old') 

 200           continue
               read (20, '(A)',end=210) str_line

               lStr = ParseOptionLine (str_line, str_opt, str_arg)

               if (lStr.ne.0) then                  
                  ibidon = FindArg (str_opt,ibidon,1,str_arg)
               endif 

               goto 200
               
 210           continue
               close (20)
            endif 
         else 
            i = FindArg (dum,i,0,str_arg)
         endif 
      enddo 
	if (opt_vecrot.eq.1) then
	write(cmd,100) angled, f_vecrotdata(1:lnblnk(f_vecrotdata))
100	format('vecrot ',f8.2,1x,a)
	print *,'Execution de: ',cmd(1:lnblnk(cmd))
	call system(cmd)
	endif

	if (opt_vecrot.eq.2) then
	write(cmd,101) angled, f_vecdata1(1:lnblnk(f_vecdata1)),f_vecdata2(1:lnblnk(f_vecdata2))
101	format('vecrot_opa ',f8.2,1x,a,1x,a)
	print *,'Execution de: ',cmd(1:lnblnk(cmd))
	call system(cmd)
	endif

C Correction de certaines valeurs par defaut selon les options retenues
C
	if (opt_vertpal.eq.1) then
           if (xypaldefault) then
              x1pal = 0.8
	      x2pal = 1.0
	      y1pal = 0.1
              y2pal = 0.9
           endif
           if (xyplotdefault) then
              x1pos = 0.1
              x2pos = 0.78
              y1pos = 0.1
              y2pos = 0.9
              ylon = 0.08 
              ylat = 0.06
              ct_ilx = 0.78
              ct_ily = 0.05
             if (opt_coupe .eq. 1 ) THEN
              ylon  = 0.08
              ylat = 0.05
              ct_ilx = 0.78
              ct_ily = 0.02
             endif
           endif
	endif
C Si on utilise l'option cntlim, le message contours de ...
C n'est plus pertinent car on ne connait pas les isovaleurs
      if (opt_contlim .EQ. 1 ) then
       ct_ilt =  ' '
      endif

        if (opt_shade.eq.1) then
           if (opt_contours.ne.1) then
            call PrintMessage(1,14,' ')
           else
           f_clrdata=f_cntdata
           endif
           if (opt_pal.ne.3) then
            call PrintMessage(0,15,' ')
            opt_pal = 3
           endif
        endif
      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  FindArg
c
c Parametres  :  arg   : l'option recherchee
c                i     : l'indice de l'argument dans la ligne de commande
c                itype : 0 pour une ligne de commande
c                        1 pour un fichier de commande
c
c Description :  Recherche dans la liste si arg est une option
c                acceptee et lit les parametres qui s'y rattachent
c                si c'est le cas.
c 
c ---------------------------------------------------------------------


      integer function FindArg (str_opt, i, itype, str_arg)

      implicit none

      character*30  str_opt
      character*(256) str_arg,cmd
      integer i,itype, len, narg
      integer iargc


      include 'common.h'

      save /str/
      
      integer couche(NA), couche_max, tstep(Ntime),tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/

      real    dummy
      character*30   strValue, strFormat, strValFmt

      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep,kmstep,
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw
      save    /trace/
      integer mark_step
      common /markc/ mark_step

      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist
      save   /cut/

      real    cl_min, cl_max
      integer cl_met, cl_exp
      common  /color/cl_min, cl_max, cl_met, cl_exp
      save    /color/

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save   /layout/

      real         vv_vrl, vv_vfr, vv_lwd, vv_vlc, vv_vhc
      real         vv_mns, vv_mnx, vv_mny, vv_mxs, vv_mxx, vv_mxy
      real         vv_vmd
      integer      vv_subx,vv_suby,vv_mnp,vv_mxp,vv_vst,vv_met, vv_vpo
      character*36 vv_mnt, vv_mxt

      common /vectors/ vv_subx, vv_suby, vv_vrl, vv_vfr, vv_lwd, 
     .                 vv_mns, vv_mnx, vv_mny, vv_mnp,
     .                 vv_mxs, vv_mxx, vv_mxy, vv_mxp,vv_met,vv_vpo,
     .                 vv_vst, vv_mnt, vv_mxt, vv_vlc, vv_vhc, vv_vmd
      save   /vectors/

      real over_lw, over_mksc
      integer over_mk, over_ic, over_mkic
      common /overl/ over_lw,over_ic
      common /overm/ over_mk, over_mksc, over_mkic


      character*64 pal, line

      integer FindFile
      integer lnblnk
      integer lstr_arg


      lstr_arg = lnblnk (str_arg)

c     fichiers ----------------------------------------

      
      if (str_opt.eq.'-p') then
         opt_pal=1

         call GetString (i, itype, str_arg, lstr_arg, filepal)

         if (FindFile (filepal, 2).eq.1) then
            call PrintMessage (1,6, filepal (1:lnblnk(filepal)))
         endif 

      else if (str_opt.eq.'-prev') then
	 opt_prev = 1
           
      else if (str_opt.eq.'-greyscale') then
	 opt_pal = 2
           
      else if (str_opt.eq.'-jetpal') then
	 opt_pal = 5
         call Get1integer (i, itype, str_arg, ncol_pal)

      else if (str_opt.eq.'-zebre') then
	 opt_pal = 6
         call Get1integer (i, itype, str_arg, ncol_pal)

      else if (str_opt.eq.'-palout') then
	 opt_palout = 1
           
      else if (str_opt.eq.'-english') then
       ct_ilt =  'MIN:  $CMN$ MAX: $CMX$ C.I.: $CIU$'
       vv_mnt = 'Minimum vector'
       vv_mxt = 'Maximum vector'
      
      else if (str_opt.eq.'-ioipsl' ) then
             modif_vecx = 'deptht=depthu'
             modif_vecy = 'deptht=depthv'

      else if (str_opt.eq.'-sesam' ) then
            modif_clr = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
            modif_cnt = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
            modif_vecx = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'
            modif_vecy = 'x=lon,y=lat,deptht=depth,time_counter=time,nav_lon=lon,nav_lat=lat'

      else if (str_opt.eq.'-orca' ) then
        opt_orca=1

      else if (str_opt.eq.'-spval' ) then
        opt_spval=1
        call Get1real (i, itype, str_arg, over_spval)

      else if (str_opt.eq.'-scale' ) then
        opt_scale=1
        call Get1real (i, itype, str_arg, zscale)

      else if (str_opt.eq.'-mean' ) then
        opt_mean=1
        call Get1real (i, itype, str_arg, zmean0)

      else if (str_opt.eq.'-b') then
         opt_map=1
         call GetString (i, itype, str_arg, lstr_arg, filebat)
            
         if (FindFile (filebat, 1).eq.1) then
            call PrintMessage (1,6, filebat (1:lnblnk(filebat)))
         endif 

      elseif (str_opt.eq.'-bgrid') then
         opt_batgrid = 1
         call GetString (i, itype, str_arg, lstr_arg, f_batgrid)
            
         if (FindFile (f_batgrid, 1).eq.1) then
            call PrintMessage (1,6, f_batgrid (1:lnblnk(f_batgrid)))
         endif 

      elseif (str_opt.eq.'-direct') then
         print *, 'Option -direct obsolete (warning)'

      else if ((str_opt.eq.'-lout').or.
     .         (str_opt.eq.'-clrlout')) then
         opt_clrlout=1
         call GetString (i, itype, str_arg, lstr_arg, f_clrlout)
                     
      else if (str_opt.eq.'-c') then
         opt_coords=1
         call GetString (i, itype, str_arg, lstr_arg, f_coords)

      else if (str_opt.eq.'-cntsav') then
         opt_cntsav=1
         call GetString (i, itype, str_arg, lstr_arg, f_cntsav)

      else if (str_opt.eq.'-o') then
         opt_outfile=1
         call GetString (i, itype, str_arg, lstr_arg, fileout)
              
      else if (str_opt.eq.'-icod') then
         call GetString (i, itype, str_arg, lstr_arg, f_icoddata)
         call ReadIcodConfig()

c     -----------------------------------------------

      elseif (str_opt.eq.'-showgrid') then
         opt_showgrid = 1
         call GetString (i, itype, str_arg, lstr_arg, f_showgrid)
            
         if (FindFile (f_showgrid, 1).eq.1) then
            call PrintMessage (1,6, f_showgrid (1:lnblnk(f_showgrid)))
         endif

      elseif (str_opt.eq.'-grid') then
         opt_batgrid  = 1
         opt_clrgrid  = 1
         opt_contgrid = 1
         opt_vectgrid  = 1

         call GetString (i, itype, str_arg, lstr_arg, f_batgrid)
            
         if (FindFile (f_batgrid, 1).eq.1) then
            call PrintMessage (1,6, f_batgrid (1:lnblnk(f_batgrid)))
         endif 

         f_clrgrid  = f_batgrid
         f_cntgrid = f_batgrid
         f_vecgrid  = f_batgrid

      elseif (str_opt.eq.'-ijgrid') then
         opt_ijgrid = 1
         opt_noproj = 1
         opt_map = 0

      elseif (str_opt.eq.'-gridxy') then
         opt_batgrid  = 3
         opt_clrgrid  = 3
         opt_contgrid = 3
         opt_vectgrid  = 3
         opt_noint = 1

         call GetString (i, itype, str_arg, lstr_arg, f_batgrid)
            
         if (FindFile (f_batgrid, 1).eq.1) then
            call PrintMessage (1,6, f_batgrid (1:lnblnk(f_batgrid)))
         endif 

         f_clrgrid  = f_batgrid
         f_cntgrid = f_batgrid
         f_vecgrid  = f_batgrid

      elseif (str_opt.eq.'-gridslow') then
         opt_batgrid  = 2
         opt_clrgrid  = 2
         opt_contgrid = 2
         opt_vectgrid  = 2

         call GetString (i, itype, str_arg, lstr_arg, f_batgrid)

         if (FindFile (f_batgrid, 1).eq.1) then
            call PrintMessage (1,6, f_batgrid (1:lnblnk(f_batgrid)))
         endif

         f_clrgrid  = f_batgrid
         f_cntgrid = f_batgrid
         f_vecgrid  = f_batgrid

      elseif (str_opt.eq.'-mask') then
         opt_clrmask = 1
         opt_contmask = 1
         opt_vectmask = 1
         call GetString (i, itype, str_arg, lstr_arg, f_clrmask)
            
         if (FindFile (f_clrmask, 1).eq.1) then
            call PrintMessage (1,6, f_clrmask(1:lnblnk(f_clrmask)))
         endif 

         f_cntmask = f_clrmask
         f_vecmask  = f_clrmask

      elseif (str_opt.eq.'-maskopa') then
         opt_clrmask = 2
         opt_contmask = 2
         opt_vectmask = 2
         call GetString (i, itype, str_arg, lstr_arg, f_clrmask)
            
         if (FindFile (f_clrmask, 1).eq.1) then
            call PrintMessage (1,6, f_clrmask(1:lnblnk(f_clrmask)))
         endif 

         f_cntmask = f_clrmask
         f_vecmask  = f_clrmask
c
c shell
c
      else if (str_opt.eq.'-system') then
        call GetString (i, itype, str_arg, lstr_arg, cmd)
	call system(cmd)


      else if (str_opt.eq.'-mark') then
         opt_marks = 1
	 mark_step = 1

      else if (str_opt.eq.'-mark1') then
         opt_marks = 1
         call Get1integer (i, itype, str_arg, mark_step)

      else if (str_opt.eq.'-vort') then
         opt_vorticity=1

      else if (str_opt.eq.'-rev') then
         opt_reverse=1
              
      else if (str_opt.eq.'-nomap') then
         opt_map = 0

      else if (str_opt.eq.'-map') then
         opt_map = 1

      else if (str_opt.eq.'-hi') then
         opt_high = 1

      else if (str_opt.eq.'-lo') then
         opt_high = 0

      else if (str_opt.eq.'-ocean') then
         opt_ocean = 1

      else if (str_opt.eq.'-noperim') then
         opt_perim = 0

      else if (str_opt.eq.'-perim') then
         opt_perim = 1

      else if (str_opt.eq.'-grad') then
         opt_grad = 1
         opt_labx = 1
         opt_laby = 1
         opt_labz = 1

      else if (str_opt.eq.'-nograd') then
         opt_grad = 0
         opt_labx = 0
         opt_laby = 0
         opt_labz = 0
         opt_nolon = 1
         opt_nolat = 1

      else if (str_opt.eq.'-noxlab') then
         opt_labx = 0

      else if (str_opt.eq.'-noylab') then
         opt_laby = 0

      else if (str_opt.eq.'-nozlab') then
         opt_labz = 0

      else if (str_opt.eq.'-noproj') then
         opt_noproj = 1
         opt_map = 0

      else if (str_opt.eq.'-outl') then
         opt_map     = 1
         opt_mapfill = 0

      else if (str_opt.eq.'-nolinelab') then
         opt_nollab=1
         ct_llp = 0

      else if ((str_opt.eq.'-nobar').or.
     .         (str_opt.eq.'-clrnopal')) then
         opt_palbar=0

      elseif (str_opt.eq.'-clrpal') then
         opt_palbar=1

      elseif (str_opt.eq.'-vertpal') then
         opt_vertpal=1

C Ajout d'otion le 7 juin 2002
      elseif (str_opt.eq.'-nobox') then
         nobox=-1

      elseif (str_opt.eq.'-lbpal') then
         call Get1integer (i, itype, str_arg, lbpos)

      elseif (str_opt.eq.'-lbsc') then
         call Get1real (i, itype, str_arg, rlbsc)

      else if ((str_opt.eq.'-nolat').or.
     .         (str_opt.eq.'-noxaxis'))then
         opt_nolat=1

      else if ((str_opt.eq.'-nolon').or.
     .         (str_opt.eq.'-noyaxis'))then
         opt_nolon=1

      else if (str_opt.eq.'-noint') then
         opt_noint = 1

      else if (str_opt.eq.'-km') then
         opt_km = 1

      else if (str_opt.eq.'-clrnoint') then
         opt_noint = 1

      else if (str_opt.eq.'-int') then
         opt_noint = 0 

      else if (str_opt.eq.'-zgrid') then
         opt_grid=1
         opt_zgrid=1
         
      else if (str_opt.eq.'-xgrid') then
         opt_grid=1
         opt_xgrid=1
         
      else if (str_opt.eq.'-ygrid') then
         opt_grid=1
         opt_ygrid=1
         
      else if ((str_opt.eq.'-print').or.
     .         (str_opt.eq.'-clriso')) then
         opt_print=1
      

      else if ((str_opt.eq.'-fn').or.
     .         (str_opt.eq.'-font')) then
         call GetString (i, itype, str_arg, lstr_arg, c_font)

      elseif (str_opt.eq.'-shift') then
         opt_shift = 1
         call Get1real (i, itype, str_arg, map_shift)

      elseif (str_opt.eq.'-360') then
         opt_360 = 1

      else if (str_opt.eq.'-lev') then
         opt_lev   = 1
         couche(1) = 0

         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call ParseLevelString (pal, couche, couche_max)
         else 
            call ParseLevelString (str_arg, couche, couche_max)
         endif 

      else if (str_opt.eq.'-dep') then
         opt_dep   = 1
         call Get1real (i, itype, str_arg, req_dep)

      else if (str_opt.eq.'-clrlev') then
         opt_single   = 1
         call Get1integer (i, itype, str_arg, kclr)

      else if (str_opt.eq.'-cntlev') then
         opt_single   = 1
         call Get1integer (i, itype, str_arg, kcnt)

      else if (str_opt.eq.'-veclev') then
         opt_single   = 1
         call Get1integer (i, itype, str_arg, kvec)

      else if (str_opt.eq.'-ndep') then
         opt_dep   = 2
         call Get1real (i, itype, str_arg, req_dep)

      else if (str_opt.eq.'-time') then
         tstep(1) = 0

         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call ParseTimeString (pal, tstep, tstep_max)
         else 
            call ParseTimeString (str_arg, tstep, tstep_max)
         endif 

      elseif (str_opt.eq.'-clrexp') then
         call Get1integer (i, itype, str_arg, cl_exp)
         call SetClrExp (cl_exp)

                       
      else if ((str_opt.eq.'-met').or.
     .         (str_opt.eq.'-clrmet')) then
         call Get1integer (i, itype, str_arg, cl_met)

         if ((cl_met.lt.1).or.(cl_met.gt.2)) then
            call PrintMessage (1, 4, ' ')
         endif 
            
      else if (str_opt.eq.'-pmin') then
         opt_pmin = 1
         call Get1real (i, itype, str_arg, prof_min)
         prof_min = -prof_min

      else if (str_opt.eq.'-pmax') then
         opt_pmax = 1
         call Get1real (i, itype, str_arg, prof_max)
         prof_max = -prof_max

      else if (str_opt.eq.'-xstep') then
         opt_xstep = 1
         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call GetMajorMinor (pal, xstep, dummy, xlw)
         else 
            read (str_arg,'(A)') line
            call GetMajorMinor (line, xstep, dummy, xlw)
         endif         

         xsub = int(dummy)
     
      else if (str_opt.eq.'-ystep') then
         opt_ystep = 1
         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call GetMajorMinor (pal, ystep, dummy, ylw)
         else 
            read (str_arg,'(A)') line
            call GetMajorMinor (line, ystep, dummy, ylw)
         endif 

         ysub = int(dummy)

      else if (str_opt.eq.'-zstep') then
         opt_zstep = 1
         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call GetMajorMinor (pal, zstep, dummy, zlw)
         else 
            read (str_arg,'(A)') line
            call GetMajorMinor (line, zstep, dummy, zlw)
         endif 

         zsub = int(dummy)

      else if (str_opt.eq.'-kmstep') then
         opt_kmstep = 1
         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call GetMajorMinor (pal, kmstep, dummy, zlw)
         else 
            read (str_arg,'(A)') line
            call GetMajorMinor (line, kmstep, dummy, zlw)
         endif 

         kmsub = int(dummy)

      else if (str_opt.eq.'-conv') then
         print *,'l''option -conv n''est plus disponible'

c         opt_conv = 1
         
c         if (itype.eq.0) then
c            do j=1,9
c               i=i+1
c               call getarg(i,pal) 
c               read(pal,*) conv(j)
c            enddo
c         else 
c            read (str_arg,*) (conv(k),k=1,9)
c         endif 

      else if (str_opt.eq.'-marg') then
         opt_marg = 1
         call Get4real (i, itype, str_arg,
     .                  map_marg(1),map_marg(2),
     .                  map_marg(3),map_marg(4))

      else if (str_opt.eq.'-zoom') then
         opt_zoom = 1
         call Get4real (i, itype, str_arg,
     .                  map_coord(1),map_coord(2),
     .                  map_coord(3),map_coord(4))

         if ((map_coord(2).lt.map_coord(1)).or.
     .       (map_coord(4).lt.map_coord(3))) then
            print *,'erreur : donnez les coordonnees en ordre croissant'
            stop
         endif 

      else if (str_opt.eq.'-pts') then
         opt_zoom = 1
         call Get4real (i, itype, str_arg,
     .                  map_coord(1),map_coord(2),
     .                  map_coord(3),map_coord(4))
	 call CalculateCutHeading(map_coord)
	 call CalculateCutDistance(map_coord, NXX)

      else if (str_opt.eq.'-cslab') then
         call Get1real (i, itype, str_arg, cs_scale)

      else if (str_opt.eq.'-sigma') then
         opt_sigma = 1
	 opt_print = 1
         call GetString (i, itype, str_arg, lstr_arg, f_zlevel)
            
         if (FindFile (f_zlevel, 1).eq.1) then
            call PrintMessage (1,6, f_zlevel(1:lnblnk(f_zlevel)))
         endif 

      else if (str_opt.eq.'-sigmatr') then
         opt_sigmatr = 1

      elseif (str_opt.eq.'-format') then
         if (itype.eq.0) then
            i=i+1
            call getarg(i,strValue)
            i=i+1
            call getarg(i,strFormat)

            call SetFormatString (strValue, strFormat)
         else 
            read (str_arg(1:30),'(A)') strValFmt
            call SplitValFmt (strValFmt,strValue, strFormat)
            call SetFormatString (strValue, strFormat)
         endif 

c     carte -----------------------------------------------------
        
      else if (str_opt.eq.'-rlat') then
         opt_rlat = 1
         call Get1real (i, itype, str_arg, map_rlat)

      else if (str_opt.eq.'-rlon') then
         opt_rlon = 1
         call Get1real (i, itype, str_arg, map_rlon)

      else if (str_opt.eq.'-proj') then
         call GetString (i, itype, str_arg, lstr_arg, map_proj)
         
         if (map_proj.eq.'GL' .OR. map_proj .eq. 'OR' ) then
            map_proj = 'OR'
            map_zone = 'MA'
         endif 

c     couleur ---------------------------------------------------

      elseif (str_opt.eq.'-clrdatamod') then
         opt_color  = 1
         opt_clrmod = 1

      else if (str_opt.eq.'-color') then
         opt_color=1

      else if ((str_opt.eq.'-clrnocol').or.
     .         (str_opt.eq.'-nocolor')) then
         opt_color=0

      else if ((str_opt.eq.'-l').or.
     .         (str_opt.eq.'-clrlim')) then
         cl_met = 3
         call GetString (i, itype, str_arg, lstr_arg, f_clrlim)
            
         if (FindFile (f_clrlim, 3).eq.1) then
            call PrintMessage (1,6, f_clrlim(1:lnblnk(f_clrlim)))
         endif 

      else if (str_opt.eq.'-clrmark') then
         cl_met = 1
         opt_clrmark = 1
         call GetString (i, itype, str_arg, lstr_arg, f_clrmark)
 
         if (FindFile (f_clrmark, 3).eq.1) then
            call PrintMessage (1,6, f_clrmark(1:lnblnk(f_clrmark)))
         endif   

      else if (str_opt.eq.'-clrmask') then
         opt_clrmask = 1
         call GetString (i, itype, str_arg, lstr_arg, f_clrmask)
            
         if (FindFile (f_clrmask, 1).eq.1) then
            call PrintMessage (1,6, f_clrmask(1:lnblnk(f_clrmask)))
         endif 

      else if (str_opt.eq.'-clrdata') then
         opt_clrdata = 1
         opt_color   = 1
	 if (opt_shade.eq.1) then
            call PrintMessage (0,13,' ')
         else
         call GetString (i, itype, str_arg, lstr_arg, f_clrdata)
 	 if (f_clrdata.ne.'vecrot.bimg') then           
         if (FindFile (f_clrdata, 1).eq.1) then
            call PrintMessage (1,6, f_clrdata(1:lnblnk(f_clrdata)))
         endif 
	 endif
         endif
      elseif ((str_opt.eq.'-spem').or.
     .        (str_opt.eq.'-clrgrid')) then
         opt_clrgrid = 1
         call GetString (i, itype, str_arg, lstr_arg, f_clrgrid)
            
         if (FindFile(f_clrgrid, 1).eq.1) then
            call PrintMessage (1,6, f_clrgrid(1:lnblnk(f_clrgrid)))
         endif 
     
      else if ((str_opt.eq.'-vmin').or.
     .         (str_opt.eq.'-clrmin')) then
         opt_min = 1
         call Get1real (i, itype, str_arg, cl_min)

      else if ((str_opt.eq.'-vmax').or.
     .         (str_opt.eq.'-clrmax')) then
         opt_max = 1
         call Get1real (i, itype, str_arg, cl_max)

      else if (str_opt.eq.'-clrmsk') then
         opt_msk = 1
         
      else if (str_opt.eq.'-clrdim') then
         call Get1integer (i, itype, str_arg, cl_dim)

      else if (str_opt.eq.'-clrvar') then
         call GetString(i, itype, str_arg, lstr_arg, v_clr)

      else if (str_opt.eq.'-clrmodif') then
         call GetString(i, itype, str_arg, lstr_arg, modif_clr)


c     contours --------------------------------------------------

      elseif  (str_opt.eq.'-contour ') then
         opt_contours = 1
         
      elseif (str_opt.eq.'-cntnum') then
         opt_contours = 1
         call Get1integer (i, itype, str_arg, ct_cls)
         ct_cls = -ct_cls
         
      elseif (str_opt.eq.'-cntlw') then
         if (itype.eq.0) then
            i=i+1
            call getarg(i,pal)
            call GetMajorMinor (pal, ct_cll1, ct_cll2, dummy)
         else 
            read (str_arg,'(A)') line
            call GetMajorMinor (line, ct_cll1, ct_cll2, dummy)
         endif         
     
      elseif (str_opt.eq.'-cntilb') then
         call Get1integer (i, itype, str_arg, ct_ilb)
         
      elseif (str_opt.eq.'-cntilc') then
         call Get1integer (i, itype, str_arg, ct_ilc)
         
      elseif (str_opt.eq.'-cntill') then
         call Get1real (i, itype, str_arg, ct_ill)
         
      elseif (str_opt.eq.'-cntrc1') then
         call Get1real (i, itype, str_arg, ct_rc1)
         
      elseif (str_opt.eq.'-cntrc2') then
         call Get1real (i, itype, str_arg, ct_rc2)
         
      elseif (str_opt.eq.'-cntcwm') then
         call Get1real (i, itype, str_arg, ct_cwm)
         
      elseif (str_opt.eq.'-cntlbc') then
         call Get1integer (i, itype, str_arg, ct_lbc)
         
      elseif (str_opt.eq.'-cntllb') then
         call Get1integer (i, itype, str_arg, ct_llb)
         
      elseif (str_opt.eq.'-cntlis') then
         call Get1integer (i, itype, str_arg, ct_lis)

      elseif (str_opt.eq.'-cntlabc') then
         call Get1integer (i, itype, str_arg, ct_llc)
         
      elseif (str_opt.eq.'-cntllp') then
         call Get1integer (i, itype, str_arg, ct_llp)
         
      elseif (str_opt.eq.'-cntexp') then
         call Get1real (i, itype, str_arg, ct_sfs)
         call SetCntExp (int(ct_sfs))
         
      elseif (str_opt.eq.'-cntilp') then
         call Get1integer (i, itype, str_arg, ct_ilp)
         
      else if ((str_opt.eq.'-cont').or.
     .         (str_opt.eq.'-cntint')) then
         opt_contours = 1
         opt_contint = 1
         call Get1real (i, itype, str_arg, ct_cis)

      else if (str_opt.eq.'-cntlim') then
         opt_contours = 1
         opt_contlim=1
         call GetString (i, itype, str_arg, lstr_arg, f_cntlim)
            
         if (FindFile (f_cntlim, 3).eq.1) then
            call PrintMessage (1,6, f_cntlim(1:lnblnk(f_cntlim)))
         endif 

      else if (str_opt.eq.'-cntlout') then
         opt_cntlout=1
         call GetString (i, itype, str_arg, lstr_arg, f_cntlout)
           
      else if (str_opt.eq.'-cntdata') then
         opt_contours = 1
         opt_contdata = 1
         call GetString (i, itype, str_arg, lstr_arg, f_cntdata)
	 if (f_cntdata.ne.'vecrot.bimg') then            
         if (FindFile (f_cntdata, 1).eq.1) then
            call PrintMessage (1,6, f_cntdata(1:lnblnk(f_cntdata)))
         endif 
	 endif

      else if (str_opt.eq.'-cntmask') then
         opt_contmask = 1
         call GetString (i, itype, str_arg, lstr_arg, f_cntmask)
            
         if (FindFile (f_cntmask, 1).eq.1) then
            call PrintMessage (1,6, f_cntmask(1:lnblnk(f_cntmask)))
         endif 

      else if (str_opt.eq.'-cntdash') then
         opt_dash = 1

      else if (str_opt.eq.'-cntshade') then
         opt_shade = 1
         opt_pal = 3
C supression affichage de la palette
         opt_palbar = 0
C force les clrdata (sans clrdata option), option clriso activee
         opt_clrdata = 1
	 opt_color = 1
         opt_print = 1
C force un mini et maxi symetrique pour clrdata, clrmet 1 pour
C que l'on ait cl_min  0 cl_max dans les limites de la palette bicolore
         opt_min = 1
         opt_max = 1
	 cl_met = 1
         cl_min = -1.e19
         cl_max = 1.e19

      else if (str_opt.eq.'-cntgrid') then
         opt_contgrid = 1
         call GetString (i, itype, str_arg, lstr_arg, f_cntgrid)
            
         if (FindFile (f_cntgrid, 1).eq.1) then
            call PrintMessage (1,6, f_cntgrid(1:lnblnk(f_cntgrid)))
         endif 

      else if ((str_opt.eq.'-vminc').or.
     .         (str_opt.eq.'-cntmin')) then
         opt_minc = 1
         call Get1real (i, itype, str_arg, ct_cmn)
     
      else if ((str_opt.eq.'-vmaxc').or.
     .         (str_opt.eq.'-cntmax')) then
         opt_maxc = 1
         call Get1real (i, itype, str_arg, ct_cmx)
     
      else if ((str_opt.eq.'-xyinfo').or.
     .         (str_opt.eq.'-cntilxy')) then     ! info contours
         call Get2real (i, itype, str_arg, ct_ilx, ct_ily)

      else if (str_opt.eq.'-cntils') then    ! Info Label Size
         call Get1real (i, itype, str_arg, ct_ils)

      else if (str_opt.eq.'-cntlls') then    ! Line Label Size
         call Get1real (i, itype, str_arg, ct_lls)

      else if (str_opt.eq.'-cntilt') then
         call GetString (i, itype, str_arg, lstr_arg, ct_ilt)

      else if (str_opt.eq.'-cntdim') then
         call Get1integer (i, itype, str_arg, ct_dim)         

      else if (str_opt.eq.'-cntvar') then
         call GetString(i, itype, str_arg, lstr_arg, v_cnt)

      else if (str_opt.eq.'-cntmodif') then
         call GetString(i, itype, str_arg, lstr_arg, modif_cnt)
         


c     vecteurs ---------------------------------------------------

      elseif (str_opt.eq.'-vecclr') then
         opt_vectclr = 1

      else if (str_opt.eq.'-vectorx') then
         opt_vectX=1

      else if (str_opt.eq.'-vectory') then
         opt_vectY=1

      else if (str_opt.eq.'-vectorxy') then
         opt_vect2D=1

      else if (str_opt.eq.'-vector3d') then
         opt_vect3D=1

      elseif (str_opt.eq.'-vecclrmod') then
         opt_vectmod = 1

      elseif (str_opt.eq.'-vecnotrace') then
         opt_vecnotr = 1

      elseif (str_opt.eq.'-Cgrid') then
         opt_cgrid =1

      elseif (str_opt.eq.'-vecshade') then
	 opt_pal = 4
         opt_palbar = 0
         opt_vecshade =1
         opt_color = 1
	 opt_ocean = 1
         cl_met = 1

      elseif (str_opt.eq.'-veclim') then
         opt_vectlim=1
    	 vv_met = 3
c        call GetString (i, itype, str_arg, lstr_arg, f_veclim)
         call GetString (i, itype, str_arg, lstr_arg, f_clrlim)
            
         if (FindFile (f_clrlim, 3).eq.1) then
            call PrintMessage (1,6, f_clrlim(1:lnblnk(f_clrlim)))
         endif 


      else if ((str_opt.eq.'-vecdata3d').or.
     .         (str_opt.eq.'-vecdata3D')) then 
         if (opt_vectors.eq.1) then
            print *, 'erreur : un fichier 3D doit etre specifie seul'
            stop
         endif 

         opt_vectors = 1
         opt_vect3D  = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata1)

         if (FindFile (f_vecdata1, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata1(1:lnblnk(f_vecdata1)))
         endif 

      else if ((str_opt.eq.'-vecdata2d').or.
     .         (str_opt.eq.'-vecdataxy').or.
     .         (str_opt.eq.'-vecdata2D')) then
         if (opt_vectors.eq.1) then
            print *, 'erreur : un fichier 2D doit etre specifie seul'
            stop
         endif 

         opt_vectors = 1
         opt_vect2D  = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata1)
            
         if (FindFile (f_vecdata1, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata1(1:lnblnk(f_vecdata1)))
         endif 

      else if (str_opt.eq.'-vecrotxy') then
	 opt_vecrot = 1

         call GetString (i, itype, str_arg, lstr_arg, f_vecrotdata)
            
         if (FindFile (f_vecrotdata, 1).eq.1) then
            call PrintMessage (1,6, f_vecrotdata(1:lnblnk(f_vecrotdata)))
         endif 

      else if (str_opt.eq.'-vecrotuv') then
C cas ou l'on passe les fichiers U et V separement sur grille C opa !!!
	 opt_vecrot = 2

         call Get2String (i, itype, str_arg, lstr_arg, f_vecdata1,f_vecdata2)
            
         if (FindFile (f_vecdata1, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata1(1:lnblnk(f_vecdata1)))
         endif 

         if (FindFile (f_vecdata2, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata2(1:lnblnk(f_vecdata2)))
         endif 


      else if (str_opt.eq.'-vecdatapsi') then
         if ((opt_vect2D.eq.1).or.(opt_vect3D.eq.1)) then
            call PrintMessage (1, 5, ' ')
         endif 

         opt_vectors = 1
         opt_vectX   = 1
         opt_vecpsi  = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata1)

         if (FindFile (f_vecdata1, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata1(1:lnblnk(f_vecdata1)))
         endif 

      else if ((str_opt.eq.'-vecdataX').or.
     .         (str_opt.eq.'-vecdatax')) then
         if ((opt_vect2D.eq.1).or.(opt_vect3D.eq.1)) then
            call PrintMessage (1, 5, ' ')
         endif 

         opt_vectors = 1
         opt_vectX   = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata1)

         if (FindFile (f_vecdata1, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata1(1:lnblnk(f_vecdata1)))
         endif 

      else if ((str_opt.eq.'-vecdataY').or.
     .         (str_opt.eq.'-vecdatay')) then
         if ((opt_vect2D.eq.1).or.(opt_vect3D.eq.1)) then
            call PrintMessage (1, 5, ' ')
         endif 

         opt_vectors = 1
         opt_vectY   = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata2)
            
         if (FindFile (f_vecdata2, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata2(1:lnblnk(f_vecdata2)))
         endif 

      else if ((str_opt.eq.'-vecdataZ').or.
     .         (str_opt.eq.'-vecdataz')) then
         if ((opt_vect2D.eq.1).or.(opt_vect3D.eq.1)) then
            call PrintMessage (1, 5, ' ')
         endif 

         opt_vectors = 1
         opt_vectZ   = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecdata3)

         if (FindFile (f_vecdata3, 1).eq.1) then
            call PrintMessage (1,6, f_vecdata3(1:lnblnk(f_vecdata3)))
         endif 

      else if (str_opt.eq.'-vecmask') then
         opt_vectmask = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecmask)
            
         if (FindFile (f_vecmask, 1).eq.1) then
            call PrintMessage (1,6, f_vecmask(1:lnblnk(f_vecmask)))
         endif 

      else if (str_opt.eq.'-vecgrid') then
         opt_vectgrid = 1
         call GetString (i, itype, str_arg, lstr_arg, f_vecgrid)
            
         if (FindFile (f_vecgrid, 1).eq.1) then
            call PrintMessage (1,6, f_vecgrid(1:lnblnk(f_vecgrid)))
         endif 


      else if (str_opt.eq.'-veclout') then
         opt_veclout=1
         call GetString (i, itype, str_arg, lstr_arg, f_veclout)
           
      else if (str_opt.eq.'-vecmet') then
         call Get1integer (i, itype, str_arg, vv_met)

         if ((vv_met.lt.1).or.(vv_met.gt.2)) then
            call PrintMessage (1, 4, ' ')
         endif 
            
      elseif (str_opt.eq.'-vecsub') then
         call Get2integer (i, itype, str_arg, vv_subx, vv_suby)

      elseif (str_opt.eq.'-vecvpo') then
         call Get1integer (i, itype, str_arg, vv_vpo)
            
      elseif (str_opt.eq.'-vecvrl') then
         call Get1real (i, itype, str_arg, vv_vrl)
            
      elseif (str_opt.eq.'-vecvfr') then
         call Get1real (i, itype, str_arg, vv_vfr)
            
      elseif (str_opt.eq.'-vecvmd') then
         call Get1real (i, itype, str_arg, vv_vmd)
            
      elseif (str_opt.eq.'-veclwd') then
         call Get1real (i, itype, str_arg, vv_lwd)
            
      elseif (str_opt.eq.'-vecmns') then
         call Get1real (i, itype, str_arg, vv_mns)
            
      elseif (str_opt.eq.'-vecmnxy') then
         call Get2real (i, itype, str_arg, vv_mnx, vv_mny)
            
      elseif (str_opt.eq.'-vecmnp') then
         call Get1integer (i, itype, str_arg, vv_mnp)
            
      elseif (str_opt.eq.'-vecmxs') then
         call Get1real (i, itype, str_arg, vv_mxs)
            
      elseif (str_opt.eq.'-vecmxxy') then
         call Get2real (i, itype, str_arg, vv_mxx, vv_mxy)
            
      elseif (str_opt.eq.'-vecmxp') then
         call Get1integer (i, itype, str_arg, vv_mxp)
            
      elseif (str_opt.eq.'-vecvlc') then
         opt_vecmin = 1
         call Get1real (i, itype, str_arg, vv_vlc)
            
      elseif (str_opt.eq.'-vecvhc') then
         opt_vecmax = 1
         call Get1real (i, itype, str_arg, vv_vhc)
            
      else if (str_opt.eq.'-vecmnt') then
         call GetString (i, itype, str_arg, lstr_arg, vv_mnt)

      else if (str_opt.eq.'-vecmxt') then
         call GetString (i, itype, str_arg, lstr_arg, vv_mxt)

      else if (str_opt.eq.'-vecvarx') then
         call GetString(i, itype, str_arg, lstr_arg, v_vecx)
         
      else if (str_opt.eq.'-vecvary') then
         call GetString(i, itype, str_arg, lstr_arg, v_vecy)

      else if (str_opt.eq.'-vecmodifx') then
         call GetString(i, itype, str_arg, lstr_arg, modif_vecx)

      else if (str_opt.eq.'-vecmodify') then
         call GetString(i, itype, str_arg, lstr_arg, modif_vecy)
         
         

c     overlay d'une courbe xy sur la carte -----------------------

      else if (str_opt.eq.'-overdata') then
         opt_overdata = 1
         call GetString (i, itype, str_arg, lstr_arg, f_overdata)

         if (FindFile (f_overdata, 1).eq.1) then
            call PrintMessage (1,6, f_overdata(1:lnblnk(f_overdata)))
         endif

      else if (str_opt.eq.'-overlw') then
         call Get1real (i, itype, str_arg, over_lw)

      else if (str_opt.eq.'-overclr') then
         opt_color = 1
    	 opt_overclr = 1
         call Get1integer (i, itype, str_arg, over_ic)
    	 over_ic = over_ic + 19

      else if (str_opt.eq.'-overmark') then
         opt_overmark = 1
         call GetString (i, itype, str_arg, lstr_arg, f_overmark)

         if (FindFile (f_overmark, 1).eq.1) then
            call PrintMessage (1,6, f_overmark(1:lnblnk(f_overmark)))
         endif
 
      else if (str_opt.eq.'-overmk') then
         call Get1integer (i, itype, str_arg, over_mk)

      else if (str_opt.eq.'-overmksc') then
         call Get1real (i, itype, str_arg, over_mksc)

      else if (str_opt.eq.'-overmkclr') then
         opt_color = 1
    	 opt_overclr = 1
         call Get1integer (i, itype, str_arg, over_mkic)
    	 over_mkic = over_mkic + 19

      else if (str_opt.eq.'-overout') then
         opt_overout = 1
         call GetString (i, itype, str_arg, lstr_arg, f_overout)


c     coordonnees ------------------------------------------------

      else if ((str_opt.eq.'-xybar').or.
     .         (str_opt.eq.'-clrxypal')) then         ! palette
         call Get4real (i, itype, str_arg, x1pal, x2pal, y1pal, y2pal)
	 xypaldefault=.false.

      else if (str_opt.eq.'-xyplot') then             ! plot frame
         call Get4real (i, itype, str_arg, x1pos, x2pos, y1pos, y2pos)
         xyplotdefault=.false.
            
      else if (str_opt.eq.'-xybat') then             ! bathy frame
         opt_xybat = 1
         call Get4real (i, itype, str_arg, x1bat, x2bat, y1bat, y2bat)
            
      else if ((str_opt.eq.'-ylon').or.
     .         (str_opt.eq.'-xaxisy')) then         ! echelle longitude
         call Get1real (i, itype, str_arg, ylon)

      else if ((str_opt.eq.'-ylat').or.
     .         (str_opt.eq.'-yaxisy')) then         ! echelle longitude
         call Get1real (i, itype, str_arg, ylat)

      else if (str_opt.eq.'-ykm') then              ! echelle km
	      opt_km = 1
         call Get1real (i, itype, str_arg, ykm)

      elseif (str_opt.eq.'-xaxist') then
         call GetString (i, itype, str_arg, lstr_arg, xaxist)

      elseif (str_opt.eq.'-yaxist') then
         call GetString (i, itype, str_arg, lstr_arg, yaxist)

      elseif (str_opt.eq.'-dat') then
         opt_dat = 1
 
      elseif (str_opt.eq.'-nodat') then
         opt_dat = 0
 
      elseif (str_opt.eq.'-team') then
         opt_team = 1
 
      elseif (str_opt.eq.'-noteam') then
         opt_team = 0
 
      elseif (str_opt.eq.'-title') then
         if (itype.eq.0) then
            strcount = strcount + 1
            i=i+1
            text(strcount)%xpos = 0.5
            text(strcount)%ypos = -1.
            text(strcount)%csize = 1.3
            text(strcount)%align = 0
            call getarg(i,text(strcount)%str)
            text(strcount)%angle=0.
         else
            strcount = strcount + 1
            text(strcount)%xpos = 0.5
            text(strcount)%ypos = -1.
            text(strcount)%csize = 1.3
            text(strcount)%align = 0
            text(strcount)%angle=0.
            call GetStringtParameters (str_arg, text(strcount))
         endif
                                          
      elseif (str_opt.eq.'-string') then
         if (itype.eq.0) then
            strcount = strcount + 1
            i=i+1
            call getarg(i,pal)
            read(pal,*) text(strcount)%xpos
            i=i+1
            call getarg(i,pal)
            read(pal,*) text(strcount)%ypos
            i=i+1
            call getarg(i,pal)
            read(pal,*) text(strcount)%csize
            i=i+1
            call getarg(i,pal)
            read(pal,*) text(strcount)%align
            i=i+1
            call getarg(i,text(strcount)%str)
	    text(strcount)%angle=0.
         else 
            strcount = strcount + 1
            call GetStringParameters (str_arg, text(strcount))
            
         endif 

      elseif (str_opt.eq.'-stringr') then
         if (itype.eq.0) then
            strcountr = strcountr + 1
            i=i+1
            call getarg(i,pal)
            read(pal,*) textr(strcountr)%xpos
            i=i+1
            call getarg(i,pal)
            read(pal,*) textr(strcountr)%ypos
            i=i+1
            call getarg(i,pal)
            read(pal,*) textr(strcountr)%csize
            i=i+1
            call getarg(i,pal)
            read(pal,*) textr(strcountr)%align
            i=i+1
            call getarg(i,pal)
            read(pal,*) textr(strcountr)%angle
            i=i+1
            call getarg(i,textr(strcountr)%str)
         else
            strcountr = strcountr + 1
            call GetStringrParameters (str_arg, textr(strcountr))
         endif

         

      else if (str_opt(1:1).eq.'#') then
         i=i                      


            
      else 
         len = lnblnk(str_opt)
         narg = iargc()
         if (len.ne.0) then
            print *,'erreur : parametre inconnu : ',str_opt
            call flush(6)
            stop
         endif 
      endif

      i = i+1
      FindArg = i
      return
      end



       
c ---------------------------------------------------------------------
c 
c Nom              :  SetDefaults
c Date de creation :  14 decembre 1993
c Parametres       :  
c Description      :  defini la page produite par chart par defaut
c 
c ---------------------------------------------------------------------

      subroutine SetDefaults ()

      implicit none

      include 'common.h'
      include 'color.h'

      integer bimg_unit
      common /f_unit/ bimg_unit
      save   /f_unit/


      integer i

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save   /layout/



      integer couche(NA),couche_max,tstep(Ntime),tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/


      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep, kmstep, 
     .               xsub, ysub, zsub, kmsub,
     .               xlw, ylw,zlw
      save    /trace/

      real    vv_vrl, vv_vfr, vv_lwd, vv_vlc, vv_vhc
      real    vv_mns, vv_mnx, vv_mny, vv_mxs, vv_mxx, vv_mxy
      real    vv_vmd
      character*36 vv_mnt, vv_mxt
      integer vv_subx, vv_suby,vv_mnp,vv_mxp, vv_vst, vv_met, vv_vpo

      common /vectors/ vv_subx, vv_suby, vv_vrl, vv_vfr, vv_lwd, 
     .                 vv_mns, vv_mnx, vv_mny, vv_mnp,
     .                 vv_mxs, vv_mxx, vv_mxy, vv_mxp, vv_met,vv_vpo,
     .                 vv_vst, vv_mnt, vv_mxt, vv_vlc, vv_vhc, vv_vmd
      save   /vectors/

      real over_lw, over_mksc
      integer over_mk, over_ic, over_mkic
      common /overl/ over_lw, over_ic
      common /overm/ over_mk, over_mksc, over_mkic


      real    cl_min, cl_max
      integer cl_met, cl_exp

      common  /color/cl_min, cl_max, cl_met, cl_exp
      save    /color/




c     initialisation des couches

      couche (1) = 1
      couche_max = 1

      do i=2,NA
         couche(i) = 0
      enddo 

c     initialisation des "time steps" 

      tstep (1) = 1
      tstep_max = 1

      do i=2,Ntime
         tstep(i) = 0
      enddo 


c     unite du premier bimg ouvert

      bimg_unit = 900

      
c     valeurs par defaut

      opt_header1  = 0
      opt_header2  = 0
      opt_dat      = 0
      opt_team     = 1   
      opt_single   = 0

      opt_pal      = 0
      opt_prev     = 0
      opt_vertpal  = 0
      nobox        = 1
      opt_palout   = 0
      opt_lev      = 0
      opt_dep      = 0
      opt_clrlout  = 0
      opt_palbar   = 1
      opt_zoom     = 0
      opt_noint    = 0
      opt_km       = 0
      opt_coords   = 0
      opt_cntsav   = 0
      opt_min      = 0
      opt_max      = 0
      opt_msk      = 0
      opt_minc     = 0
      opt_maxc     = 0
      opt_clrmask  = 0
      opt_clrmark  = 0
      opt_contmask = 0
      opt_vectmask  = 0
      opt_contours = 0
      opt_contlim  = 0
      opt_contdata = 0
      opt_nollab   = 0
      opt_color    = 0
      opt_grid     = 0
      opt_ijgrid   = 0
      opt_xgrid    = 0
      opt_ygrid    = 0
      opt_zgrid    = 0
      opt_satur    = 0
      opt_reverse  = 0
      opt_marg     = 0
      opt_conv     = 0
      opt_orca     = 0
      opt_spval    = 0
      opt_scale    = 0
      opt_mean     = 0
      opt_map      = 1
      opt_high     = 0
      opt_ocean    = 0
      opt_mapfill  = 1
      opt_rlat     = 0
      opt_rlon     = 0
      opt_perim    = 1
      opt_print    = 0
      opt_grad     = 1
      opt_labx     = 1
      opt_laby     = 1
      opt_labz     = 1
      opt_clrmod   = 0
      opt_shift    = 0
      opt_360      = 0
      opt_noproj   = 0

      opt_xstep    = 0
      opt_ystep    = 0
      opt_zstep    = 0

      opt_vectdata = 0
      opt_vect3D   = 0
      opt_vect2D   = 0
      opt_vectX    = 0
      opt_vectY    = 0
      opt_vectZ    = 0
      opt_vecmin   = 0
      opt_vecmax   = 0
      opt_veclout  = 0
      opt_vecpsi   = 0
      opt_vecnotr  = 0 
      opt_vecrot   = 0
      opt_cgrid    = 0
      opt_vecshade = 0
 
      opt_overdata = 0
      opt_overmark = 0
      opt_overout  = 0
      opt_overclr  = 0

      opt_cntlout  = 0
      opt_vorticity= 0

      opt_marks    = 0
      opt_dash     = 0
      opt_shade    = 0

      cl_met       = 2
      vv_met       = 2
      vv_vpo       = 0
      cl_min       = 0
      cl_max       = 0
      cl_dim       = 1
      ct_dim       = 1
      ct_rc1       = 0.10
      ct_rc2       = 0.15
     
      over_lw      = 1.0
      over_mk      = 4
      over_mksc    = 1.0
      over_ic      = 1
      over_mkic    = 1

c     coordonnees --------------------------------------------

      x1pos   = 0.10  ! zone max pour la carte
      x2pos   = 0.95
      y1pos   = 0.20
      y2pos   = 0.90

      x1pal   = 0.10  ! rectangle contenant la barre de palette
      x2pal   = 0.95
      y1pal   = 0.00
      y2pal   = 0.12


c     contours ---------------------------------------------------

      call cpgeti ('LLB - Line Label Box Flag          ', ct_llb)
      call cpgetr ('LLS - Line Label Size              ', ct_lls)
      call cpgeti ('LIS - Label Interval Specifier     ', ct_lis)
      call cpgeti ('LBC - Label Box Color index        ', ct_lbc)

      call cpgeti ('ILB - Information Label Box flag   ', ct_ilb)
      call cpgetr ('ILL - Information Label Line width ', ct_ill)
      call cpgetr ('ILS - Information Label Size       ', ct_ils)
      call cpgeti ('ILP - Information Label Positioning', ct_ilp)

      call cpgetr ('CWM - Character Width Multiplier   ', ct_cwm)
      call cpgeti ('CLS - Contour Level Selection flag ', ct_cls)

      ct_sfs  = 0.0
      ct_cis = -1.     
      ct_llc  = COLOR_ISOCONTOUR
      ct_llp  = 2
      ct_ilc  = 1

      ct_cll1  = 1.0    ! contour line line width
      ct_cll2  = 1.0    ! label line line width

      ct_ilx   = 0.95   ! cote droit de l'info isocontours
      ct_ily   = 0.16

      ct_ilt =  'Contours de $CMN$ a $CMX$ par intervalles de $CIU$'

c     Vecteurs ----------------------------------------------------
      vv_vmd  = 0.
      vv_subx = 1
      vv_suby = 1
      vv_vrl  = 1.0
c     vv_vfr  = 0.33
c     vv_vfr  = 0.
      vv_mny  = 0.2
      vv_mxy  = 0.2

      call vvgetr ('LWD - vector Line WiDth', vv_lwd)
      
      call vvgetr ('MNS - MiN vector text block char Size', vv_mns)
      call vvgetr ('MNX - MiN vector text block X coord  ', vv_mnx)
      call vvgeti ('MNP - MiN vector text block Pos. mode', vv_mnp)
      

      call vvgetr ('MXS - MaX vector text block char Size', vv_mxs)
      call vvgetr ('MXX - MaX vector text block X coord  ', vv_mxx)
      call vvgeti ('MXP - MaX vector text block Pos. mode', vv_mxp)

      vv_mnt = 'Vecteur minimum'
      vv_mxt = 'Vecteur maximum'


c     config ------------------------------------------------------

      cs_scale = 0.8
      lbpos    = 1
      rlbsc    = 1.

c     carte ------------------------------------------------------

      map_proj = 'CE'      
      map_zone = 'PO'
      map_shift= 0
      c_font   = 'PWRITX DATABASE   '

      xstep = 5.
      ystep = 5.

      xsub = 0
      ysub = 0

      xlw = 1.0
      ylw = 1.0

      map_coord(1) = 0.0
      map_coord(2) = 360.0
      map_coord(3) = -90.0
      map_coord(4) = 90.0

      map_marg(1) = 0.0
      map_marg(2) = 360.0
      map_marg(3) = -90.0
      map_marg(4) = 90.0

c     output file-name
      fileout = 'gmeta'

c     single frame -------------------------------------------------

	kclr = 1
	kcnt = 1
        kvec = 1

c     NetCdf
      v_clr  = 'none'
      v_cnt  = 'none'
      v_vecx = 'none'
      v_vecy = 'none'
c    
      modif_clr  = 'none'
      modif_cnt  = 'none'
      modif_vecx = 'none'
      modif_vecy = 'none'

      return
      end

       
c ---------------------------------------------------------------------
c 
c Nom              :  SetCoupeDefaults
c Date de creation :  14 decembre 1993
c Parametres       :  
c Description      :  defini la page produite par coupe par defaut
c 
c ---------------------------------------------------------------------

      subroutine SetCoupeDefaults ()

      implicit none

      include 'common.h'
      include 'color.h'


      integer couche(NA),couche_max,tstep(Ntime),tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/

      real    prof_min, prof_max
      character*20 xaxist, yaxist, kmaxist
      common /cut/prof_max,prof_min,xaxist, yaxist, kmaxist
      save   /cut/

      real x1pos,x2pos,y1pos,y2pos
      real x1bat,x2bat,y1bat,y2bat
      real x1pal,x2pal,y1pal,y2pal
      real ylon,ylat,ykm
      
      common /layout/x1pal,x2pal,y1pal,y2pal,
     .               ylon,ylat,ykm,
     .               x1pos,x2pos,y1pos,y2pos,
     .               x1bat,x2bat,y1bat,y2bat
      save   /layout/


      real    xstep, ystep, zstep, kmstep
      integer xsub, ysub, zsub, kmsub
      real    xlw, ylw,zlw
      common  /trace/xstep, ystep, zstep,kmstep,
     .               xsub, ysub, zsub, kmsub, 
     .               xlw, ylw,zlw
      save    /trace/


      opt_map      = 0
      opt_noproj   = 1
      opt_nolat    = 0
      opt_nolon    = 0
      opt_km       = 0
      opt_xybat    = 0
      opt_batgrid  = 0
      opt_showgrid = 0
      opt_pmin     = 0
      opt_pmax     = 0
      opt_sigma    = 0
      opt_sigmatr  = 0

      prof_min = 0.0
      prof_max = 4000.0

      y1pos   = 0.30

      x1bat = x1pos
      x2bat = x2pos
      y1bat = y1pos
      y2bat = y2pos

      ylon    = 0.25
      ylat    = 0.20
      ykm     = 0.16

      ct_ilx   = 0.95   ! cote droit de l'info isocontours
      ct_ily   = 0.02

      zstep = 200.
      kmstep = 200.     ! km

      zsub = 0
      kmsub  = 0
      zlw = 1.0

      xaxist = 'lon'
      yaxist = 'lat'
      kmaxist= 'Km '

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  SetLevels
c Parametres  :  
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine SetLevels ()

      implicit none

      include 'common.h'
      integer couche(NA),couche_max,tstep(Ntime),tstep_max,cl_dim,ct_dim
      real    conv(9)
      common /calc/conv,couche,couche_max,tstep,tstep_max,cl_dim,ct_dim
      save   /calc/

      integer i

c     initialisation des couches

      couche_max = 99999

      do i=1,NA
         couche(i) = 1
      enddo 


      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  PrintOptions
c Parametres  :  
c Description :  
c 
c ---------------------------------------------------------------------


      subroutine PrintOptions()

C Dummy routine
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  GetMajorMinor 
c 
c Parametres  :  arg   - chaine de caracteres a analyser
c                major - nombre de divisions principales (numerotees)
c                minor - nombre de subdivisions
c                lw    - largeur des lignes des divisions principales
c                        (line width)
c 
c Description :  Cette fonction analyse la chaine de caracteres et en 
c                extrait les informations decrivant les divisions des
c                echelles sur la carte.
c
c                Les differents champs sont divises par des :
c                On peut ne donner que les premiers champs. Par exemple
c
c                -xstep 10       -> une division tous les 10 degres
c                -xstep 10:2     -> une division tous les 5 degres, 
c                                   numerotees de 10 degres en 10 degres.
c                -xstep 10:2:1.5 -> une division tous les 5 degres, 
c                                   numerotees de 10 degres en 10 degres,
c                                   ligne numerotees 1.5 fois plus larges
c 
c ---------------------------------------------------------------------

      subroutine GetMajorMinor (arg, major, minor, lw)

      implicit none

      character*64 arg
      real         major,minor
      real         lw

      integer      len, i, sep,sep2
      integer      lnblnk


      len = lnblnk (arg)

      sep  = -1
      sep2 = -1
      do i=1,len
         if (arg(i:i).eq.':') then
            if (sep.eq.-1) then
               sep = i
            else 
               sep2 = i
            endif 
         endif 
      enddo 

      if ((sep.eq.-1).and.(sep2.eq.-1)) then
         read(arg,*) major
         minor = 0
         lw = 1.0
      else if ((sep.ne.-1).and.(sep2.eq.-1)) then
         read(arg(1:sep-1),*) major
         read(arg(sep+1:len),*)  minor
         lw = 1.0
      else if ((sep.ne.-1).and.(sep2.ne.-1)) then
         read(arg(1:sep-1),*) major
         read(arg(sep+1:sep2-1),*)  minor
         read(arg(sep2+1:len),*)  lw
      endif 

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  ParseOptionLine
c 
c Parametres  :  str_in  - chaine a interpreter
c                str_opt - option
c                str_arg - arguments de l'option
c
c Description :  Cette fonction interprete une ligne contenant une 
c                option et ses arguments. Elle retourne l'option 
c                dans str_opt et les arguments dans str_arg
c
c                par exemple:  -zoom -80 -20 20 40
c                              str_opt: "-zoom"
c                              str_arg: "-80 -20 20 40"
c
c Retour      : la fonction retourne la longueur de str_in
c 
c ---------------------------------------------------------------------

      integer function ParseOptionLine (str_in, str_opt, str_arg)

      implicit none


      character*256 str_in,str_arg
      character*30 str_opt

      integer LOPT, LARG
      


      integer i, length, found
      integer lnblnk
      integer opt_end,arg_start, opt_start

      LOPT = lnblnk(str_opt)
      LARG = lnblnk(str_arg)

c     efface les strings de retour

      do i = 1,LOPT
         str_opt(i:i) = ' '
      enddo 

      do i = 1,LARG
         str_arg(i:i) = ' '
      enddo 


      length = lnblnk(str_in)

      ParseOptionLine = length

      if (length.eq.0) return


      found  = 0
      opt_end = length
      arg_start = length+1

      i = 1
      do while (str_in(i:i).eq.' ')
         i = i+1
      enddo 
      
      opt_start = i

      do i = opt_start,length
         if (str_in(i:i).eq.' ') then
            found = 1
            opt_end = i-1
            goto 10
         endif 
      enddo 

 10   continue 

      if (found.eq.1) then
         do i = opt_end+1,length
            if (str_in(i:i).ne.' ') then
               arg_start = i
               goto 20
            endif 
         enddo 
      endif 

 20   continue

c     ecrit le resultat

      write (str_opt(1:opt_end-opt_start+1),'(A)')
     .       str_in (opt_start: opt_end)

      if (arg_start.le.length) then
         write (str_arg(1:length-arg_start+1),'(A)')
     .             str_in (arg_start: length)
      endif 
         
      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  LookForConfig
c 
c Parametres  :  icod - icod pour lequel on cherche une config
c 
c Description :  Cherche une configuration pour l'ICOD dans les
c                tables chart_icod.user (locale) et chart_icod.sys 
c                (systeme)
c 
c ---------------------------------------------------------------------

      integer function LookForConfig (icod, fileconfig)

      implicit none

      integer icod
      integer LookForIcod
      integer found

      character*256 icod_file
      character*256 fileconfig

      found = 0

      icod_file   = 'chart_icod.user'
      found = LookForIcod (icod_file, icod, fileconfig)

      if (found.eq.0) then
         icod_file = 'chart_icod.sys'
         found = LookForIcod (icod_file, icod, fileconfig)
      endif 

      LookForConfig = found

      return
      end






c ---------------------------------------------------------------------
c 
c Nom         :  LookForIcod
c 
c Parametres  :  filename   - nom de la table d'ICODs a trouver et lire
c                icod       - ICOD recherche
c                fileconfig - nom du fichier de config trouve
c 
c Description :  Cherche un ICODs dans une table et lit le fichier de
c                configuration qui lui est associe.
c 
c ---------------------------------------------------------------------

      integer function LookForIcod (filename, icod, strConfig)

      implicit none

      character*256 filename
      character*256 strLine
      character*256 strConfig
      character*30  strIcod
      integer icod, file_icod
      integer lStr

      integer FindFile
      integer ParseOptionLine
      integer lnblnk
      integer erreur, found

      integer*4 state

      erreur = FindFile (filename,4)
      found  = 0

      if (erreur.eq.0) then
         open (20,file=filename,iostat=state, status='old')

 20      continue
         read (20, '(A)', end=10) strLine
         
         lStr = ParseOptionLine (strLine, strIcod, strConfig)
         
         if (lStr.ne.0) then
            call FilterBlanks (strIcod)
            if (strIcod(1:1).ne.'#') then
               
               read (strIcod,*) file_icod
               
               if (icod.eq.file_icod) then
                  found = 1
                  goto 10
               endif 
            endif 
         endif 

         goto 20

 10      close (20)

         if (found.eq.1) then
            erreur = FindFile (strConfig, 4)
               
            if (erreur.eq.0) then               
               LookForIcod = 1
            else 
               call PrintMessage (1,6, strConfig(1:lnblnk(strConfig)))
            endif             
         else 
            LookForIcod = 0
         endif 
      else 
         LookForIcod = 0
      endif 
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  ReadIcodConfig
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine ReadIcodConfig ()

      implicit none

      include 'common.h'

      TYPE( bimgfile )  bimg

      character*256 strConfigFile

      integer LookForConfig
      integer BimgOpenFile
      integer lnblnk
      integer found 

      bimg%f_unit = BimgOpenFile (f_icoddata)
      call BimgReadHeader(bimg)
      close (bimg%f_unit)

      found = LookForConfig (bimg%icod, strConfigFile)

      if (found.eq.1) then
         print *, 'Config trouvee pour l''ICOD',bimg%icod
         print *, strConfigFile(1:lnblnk(strConfigFile))

         call ReadConfigFile (strConfigFile)
      else 
         print *,'config de base'
         opt_color = 1
      endif 

      if (opt_color.eq.1) then
         opt_clrdata = 1
         f_clrdata = f_icoddata
      endif 

      if (opt_contours.eq.1) then
         opt_contdata = 1
         f_cntdata = f_icoddata
      endif 

      if ((opt_vectX.eq.1).or.
     .    (opt_vectY.eq.1).or.
     .    (opt_vect2D.eq.1).or.
     .    (opt_vect3D.eq.1)) then
         opt_vectors = 1
         f_vecdata1 = f_icoddata
      endif 


      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  ParseLevelString
c 
c Parametres  :  str_lev    - chaine de caracteres a lire
c                draw_level - tableau a remplir, qui contiendra 1 pour
c                             les couches a dessiner et 0 pour les autres
c                couche_max - retourne la l'index de la couche maximale
c                             trouvee dans ls liste.      
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine ParseLevelString (str_lev, draw_level, couche_max)

      implicit none

      include 'common.h'

      character*30 str_lev
      character*10 strNum
      integer      draw_level(NA)
      integer      lnblnk
      integer      couche_max
      integer      i,j
      integer      length, list
      integer      last_level, level
      integer      last_sign, separator
      integer      index, next_index
      integer      IsNumber

      length = lnblnk(str_lev)
      index  = 1
      last_level = 1
      list = 0
      last_sign = 0
      separator = 0


      do while (index.le.length)
         do i = index,length
            if (str_lev(i:i).eq.'-') then
               list = 1
               separator = 1
               goto 10
            elseif (str_lev(i:i).eq.',') then
               list = 0
               separator = 1
               goto 10
            endif 
         enddo 

 10      continue

         next_index = i+1

         if (separator.eq.1) then
            i = i-1
            separator = 0
         endif 

         if (i.ne.0) then
            if (IsNumber(str_lev(index:i)).eq.0) then
               print *, 'erreur de syntaxe, option -lev'
               stop
            endif 

            read(str_lev(index:i),*) level
         
            if (level.eq.0) then
               couche_max = 99999

               do j = 1, NA
                  draw_level(j) = 1
               enddo 
            else if (level.gt.NA) then
               write (strNum, '(i10)') NA
               call FilterBlanks (strNum)
               print *,'erreur option -lev, couche max = ',strNum
               stop              
            else 
               if (last_sign.eq.1) then
                  do j = last_level+1,level
                     draw_level(j) = 1
                  enddo 
               else 
                  draw_level(level) = 1
               endif 
            endif 
            
            last_level = level
            last_sign = list
         else 
            last_sign = 0
         endif 

         index = next_index

         if (level.gt.couche_max) couche_max = level
      enddo 

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  ParseTimeString
c 
c Parametres  :  str_lev    - chaine de caracteres a lire
c                draw_level - tableau a remplir, qui contiendra 1 pour
c                             les couches a dessiner et 0 pour les autres
c                couche_max - retourne la l'index de la couche maximale
c                             trouvee dans ls liste.      
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine ParseTimeString (str_lev, draw_level, couche_max)

      implicit none

      include 'common.h'

      character*30 str_lev
      character*10 strNum
      integer      draw_level(Ntime)
      integer      lnblnk
      integer      couche_max
      integer      i,j
      integer      length, list
      integer      last_level, level
      integer      last_sign, separator
      integer      index, next_index
      integer      IsNumber

      length = lnblnk(str_lev)
      index  = 1
      last_level = 1
      list = 0
      last_sign = 0
      separator = 0


      do while (index.le.length)
         do i = index,length
            if (str_lev(i:i).eq.'-') then
               list = 1
               separator = 1
               goto 10
            elseif (str_lev(i:i).eq.',') then
               list = 0
               separator = 1
               goto 10
            endif 
         enddo 

 10      continue

         next_index = i+1

         if (separator.eq.1) then
            i = i-1
            separator = 0
         endif 

         if (i.ne.0) then
            if (IsNumber(str_lev(index:i)).eq.0) then
               print *, 'erreur de syntaxe, option -lev'
               stop
            endif 

            read(str_lev(index:i),*) level
         
            if (level.eq.0) then
               couche_max = 99999

               do j = 1, Ntime
                  draw_level(j) = 1
               enddo 
            else if (level.gt.Ntime) then
               write (strNum, '(i10)') Ntime
               call FilterBlanks (strNum)
               print *,'erreur option -lev, couche max = ',strNum
               stop              
            else 
               if (last_sign.eq.1) then
                  do j = last_level+1,level
                     draw_level(j) = 1
                  enddo 
               else 
                  draw_level(level) = 1
               endif 
            endif 
            
            last_level = level
            last_sign = list
         else 
            last_sign = 0
         endif 

         index = next_index

         if (level.gt.couche_max) couche_max = level
      enddo 

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  ReadConfigFile
c 
c Parametres  :  filename
c 
c Description :  lit un fichier de configuration et met les options 
c                a jour
c 
c ---------------------------------------------------------------------

      subroutine ReadConfigFile (filename)

      implicit none

      integer*4 state
      character*256 filename
      character*256 strLine
      character*256 strConfig
      character*30  strOpt
      integer lStr, ibidon
      
      integer ParseOptionLine
      integer FindArg
      
      open (20,file=filename,iostat=state,status='old') 
               
 200  continue
      read (20, '(A)',end=210) strLine

      lStr = ParseOptionLine (strLine, strOpt, strConfig)
      
      if (lStr.ne.0) then                  
         ibidon = FindArg (strOpt,ibidon,1,strConfig)
      endif 
      
      goto 200
      
 210  continue
      close (20)

      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  GetString
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine GetString (i, itype, str_arg, len, string)

      implicit none

      integer  len
      integer i, itype
      character*(*) string
      character*256 str_arg

      if (itype.eq.0) then
         i=i+1
         call getarg(i,string)
      else 
         write (string,'(A)') str_arg(1:len)
      endif 


      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  GetString
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get2String (i, itype, str_arg, len, string1, string2)

      implicit none

      integer  len
      integer i, itype
      character*(*) string1,string2
      character*256 str_arg

      integer len2, ii,iblank, lnblnk

      if (itype.eq.0) then
         i=i+1
         call getarg(i,string1)
         i=i+1
         call getarg(i,string2)
      else 
C cherche un character blanc comme separateur
         iblank=index(str_arg(1:len),' ')
        if (iblank .EQ. 0 ) then
         print *,' vecrotuv attend  le nom de 2 fichiers separes par des blancs'
         stop 'Get2String'
        endif
         string1=str_arg(1       :iblank)
         string2=str_arg(iblank+1:len   )
C enleve les blancs eventuellement devant string2
         len2 = lnblnk(string2)
         ii=1
         do while (ii .le. len2)
          if (string2(ii:ii) .EQ. ' ') THEN
           ii=ii+1
          else
           string2=string2(ii:len2)
           ii=len2+1
          endif
         enddo
      endif 


      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get1real (i, itype, str_arg, rValue)

      implicit none

      integer i, itype
      real rValue
      character*256 str_arg
      character*64 pal

      if (itype.eq.0) then
         i=i+1
         call getarg(i,pal)
         read(pal,*) rValue
      else 
         read (str_arg,*) rValue
      endif          
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get2real (i, itype, str_arg, rValue1, rValue2)

      implicit none

      integer i, itype
      real rValue1, rValue2
      character*256 str_arg
      character*64 pal

      if (itype.eq.0) then
         i=i+1
         call getarg(i,pal)
         i=i+1
         read(pal,*) rValue1
         call getarg(i,pal)
         read(pal,*) rValue2
      else 
         read (str_arg,*) rValue1, rValue2
      endif 
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get1integer (i, itype, str_arg, iValue)

      implicit none

      integer i, itype
      integer  iValue
      character*256 str_arg
      character*64 pal

      if (itype.eq.0) then
         i=i+1
         call getarg(i,pal)
         read(pal,*) iValue
      else 
         read (str_arg,*) iValue
      endif          
      
      return
      end

c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get2integer (i, itype, str_arg, iValue1, iValue2)

      implicit none

      integer i, itype
      integer iValue1, iValue2
      character*256 str_arg
      character*64 pal

      if (itype.eq.0) then
         i=i+1
         call getarg(i,pal)
         i=i+1
         read(pal,*) iValue1
         call getarg(i,pal)
         read(pal,*) iValue2
      else 
         read (str_arg,*) iValue1, iValue2
      endif 
      
      return
      end




c ---------------------------------------------------------------------
c 
c Nom         :  
c 
c Parametres  :  
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine Get4real (i, itype, str_arg, rValue1, rValue2,
     .                     rValue3, rValue4)

      implicit none

      integer i, itype
      real rValue1, rValue2, rValue3, rValue4
      character*256 str_arg
      character*64 pal


      if (itype.eq.0) then
         i=i+1
         call getarg(i,pal)
         i=i+1
         read(pal,*) rValue1
         call getarg(i,pal)
         i=i+1
         read(pal,*) rValue2
         call getarg(i,pal)
         i=i+1
         read(pal,*) rValue3
         call getarg(i,pal)
         read(pal,*) rValue4
      else 
         read (str_arg,*) rValue1, rValue2, rValue3, rValue4
      endif 
      
      return
      end



