c --------------------------------------------------------------------
c 
c Programme      :  chart, coupe, anim
c Fichier        :  utils.f
c Version        :  1.0
c 
c Auteur         :  Eric Brown
c Creation       :  30/06/93
c Mise a Jour    :  
c
c Description    :  ce fichier contient differentes fonctions de
c                   de lecture d'arguments et de conversion de format
c                   
c ---------------------------------------------------------------------


c ---------------------------------------------------------------------
c 
c Nom         :  OpenNCAR_GKS
c 
c Description :  Ouvre GKS.
c                Ouvre aussi deux "workstations", une pour le fichier
c                metacode et l'autre pour creer le dessin sous forme
c                de segments assembles.
c 
c ---------------------------------------------------------------------

      subroutine OpenNCAR_GKS()

      implicit none
      include 'common.h'

      integer lnblnk
      integer filelen
      character*300 command
      character*80  odr

      call gopks (6,0)


c     Change le nom du fichier de sortie

c     Note : si le fichier existe deja, il faut l'enlever
c            sinon, on aura le message "Error in end meta bit"
c            Pourquoi ? excellente question !

      if (opt_outfile.eq.1) then         
         filelen = lnblnk(fileout)
         write (command,702) fileout(1:filelen)
 702     format('rm ',a)
         call system (command)
         call gesc(-1391,1,fileout,1,1,odr)
      endif 

      call gopwk (1, 149, 1)
      call gacwk (1)

      call gopwk (9, 150, 3)  


      return
      end




c ---------------------------------------------------------------------
c 
c Nom              :  deftxt
c
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine deftxt (titre,icod)

      implicit none
      
      character *(*) titre
      integer   icod
      
      if (icod.eq.0) then
         titre='Bathymetry'
      else if (icod.eq.1) then
         titre='PSI '
      else if (icod.eq.3)then
         titre='Potential Vorticity'
      else if (icod.eq.4) then
         titre='MKE'
      else if (icod.eq.5)then
         titre='EKE'
      else if (icod.eq.6) then
         titre='RMS SSH'
      else if (icod.eq.7) then
         titre='TEMPERATURE'
      else if (icod.eq.8) then
         titre='SALINITE'
      else if (icod.eq.9) then
         titre='RHO'
      else if (icod.eq.10) then
         titre='VITESSE'
      else if (icod.eq.11) then
         titre='VITESSE-E'
      else if (icod.eq.12) then
         titre='VITESSE-N'
      else
         titre='Titre'
      endif
      
      
      return
      end
      
      
      
c ---------------------------------------------------------------------
c     
c Nom              :  defunit
c
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine defunit(unit,icod)
      
      character *(*) unit
      integer   icod
      
      if (icod.eq.-1) then
         unit='m'
      else if (icod.eq.1) then
         unit='m|S|2|N| s|S|-1|N|'
      else if (icod.eq.3)then
         unit='s|S|-1|N|'
      else if (icod.eq.4) then
         unit='cm|S|2|N| s|S|-2|N|'
      else if (icod.eq.5)then
         unit='cm|S|2|N| s|S|-2|N|'
      else if (icod.eq.6) then
         unit='cm'
      else
         unit='m'
      endif
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  expl
c
c Parametres  :  
c Description :  cette routine permet de transformer un chiffre reel
c                quelconque en une mantisse a 4 chiffres significatifs
c                et un exposant du type sxxx 10** exposant.
c
c                14 fevrier 1994 - seulement si le nombre est plus
c                                  grand 1000
c 
c ---------------------------------------------------------------------

      subroutine expl(x,mantisse,iexposant)

      implicit none

      real x, x1
      integer mantisse, iexposant, iex
      character*11 chif
      
      write(chif,100)x
 100  format(e11.4)
      read(chif,'(f6.4)') x1
      read(chif(9:11),'(i3)') iex
      mantisse=nint(x1*10000)
      
      iexposant=iex-3
      return
      end
      


c ---------------------------------------------------------------------
c 
c Nom         :  expl3
c
c Parametres  :  
c Description :  cette routine permet de transformer un chiffre reel 
c                quelconque en une mantisse a 3 chiffres significatifs
c                et un exposant du type sxxx 10** exposant
c 
c ---------------------------------------------------------------------

      subroutine expl3(x,mantisse,iexposant)
      
      implicit none
      
      real x, x1
      integer mantisse, iexposant, iex
      character*10 chif
      
      write(chif,100)x
 100  format(e10.3)
      read(chif,'(f6.3)') x1
      read(chif(8:10),'(i3)') iex
      mantisse=nint(x1*1000)
      iexposant=iex-3
      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  lirepc
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine lirepc(string)

      implicit none

      integer lnblnk
      character*(*) string
      integer l
      character*80 line
      
 10   read '(a)',line                 
      if (line(1:1).eq.'#') goto 10 ! skip comments
      l=lnblnk(line)
      string=line(1:l)
      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  lirepcf
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine lirepcf(string,iunit)
      
      implicit none
      
      integer lnblnk
      character*(*) string
      integer iunit, l
      character*80 line
      
      
 10   read (iunit,'(a)')line
      if (line(1:1).eq.'#') goto 10 ! skip comments
      l=lnblnk(line)
      if (l.eq.0) goto 10
      string=line(1:l)
      return
      end


c ---------------------------------------------------------------------
c 
c Nom              :  lireprf
c Parametres       :  
c Description      :  
c 
c ---------------------------------------------------------------------

      subroutine lireprf (wk,n,iunit,index)

      implicit none
      
      real wk(*)
      integer   n, iunit, index, i
      
      character*65 line
      
      call lirepcf(line,iunit)
c     read (line,'(i3,3f6.2,i3)')index,(wk(i),i=1,n),iflag
      read (line,*)index,(wk(i),i=1,n)
      return
      end
      


c ---------------------------------------------------------------------
c 
c Nom         :  FindFile
c 
c Parametres  :  
c Description :  cherche un fichier d'abord dans le repertoire courant,
c                puis dans le repertoire par defaut de l'utilisateur,
c                et finalement dans le repertoire par defaut BIMG_ROOT
c
c Retour      :  0 si trouve,  1 sinon
c
c
c Note        : filetype - 1 : data
c                          2 : palette
c                          3 : limites
c                          4 : setup
c 
c ---------------------------------------------------------------------

      integer function FindFile (filename, filetype)

      implicit none

      character*(256) filename
      integer filetype
      integer lnblnk
      
      character*256 filetmp
      character*256 rootdir, environment
      integer*4 state, dirlen, file_len, type_len
      integer   value,i
      
      character*4 dir_types(4)
      data (dir_types(i),i=1,4) /'data','pal','lim','sts'/
      
      
      character*20 dir_names(4)
      data (dir_names(i),i=1,4)
     .          /'BIMG_DATDIR','BIMG_PALDIR',
     .          'BIMG_LIMDIR','BIMG_STSDIR'/
      
      
      
      value = 1
      
      environment = dir_names(filetype)
      file_len = lnblnk(filename)
      
      if (file_len.eq.0) then
         FindFile = 1
         return
      endif 

c     verifie que le fichier de donnees existe
c     dans le repertoire courant

      open (200,file=filename,iostat=state,status='old')
      if (state.eq.0) then
         close (200)
         value = 0
      else
           
              
c     sinon: recherche du fichier dans le repertoire par defaut
c            de l'utilisateur.

         call getenv(environment,rootdir)
         if (rootdir.ne.' ') then
            dirlen  = lnblnk(rootdir)
                 
            if (rootdir(dirlen:dirlen).eq.'/') then
               dirlen = dirlen - 1
            endif 

            write (filetmp,600) rootdir(1:dirlen),
     .                filename(1:file_len)
                 
            open (200,file=filetmp,iostat=state,status='old')
            if (state.eq.0) then
               close (200)
               value = 0
               filename = filetmp
            endif 
         endif 
         
c     si on ne l'a toujours pas trouve, on cherche dans le repertoire
c     sous BIMG_ROOT, selon le type de fichier

         if (value.eq.1) then
            call getenv('BIMG_ROOT',rootdir)
            if (rootdir.ne.' ') then
               dirlen  = lnblnk(rootdir)
               
               if (rootdir(dirlen:dirlen).ne.'/') then
                  dirlen = dirlen + 1
                  rootdir(dirlen:dirlen) = '/'
               endif 
               
               type_len = lnblnk(dir_types(filetype))
               rootdir (dirlen+1:dirlen+type_len)=dir_types(filetype)
               dirlen = dirlen + type_len
               
               write (filetmp,600) rootdir(1:dirlen),
     .                filename(1:file_len)
               
               open (200,file=filetmp,iostat=state,status='old')
               if (state.eq.0) then
                  close (200)
                  value = 0
                  filename = filetmp
               endif 
            endif 
         endif 
      endif 
      

 600  format(a,'/',a)


      FindFile = value
      
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  FilterBlanks
c 
c Parametres  :  strInOut
c
c Description :  Cette fonction enleve les caracteres d'espacement
c                au debut d'une chaine de caracteres et recopie la
c                chaine sur elle-meme
c 
c ---------------------------------------------------------------------

      subroutine FilterBlanks (strIn)

      implicit none

      character*(*) strIn
      integer i, nStart, lStr, nCopy
      integer lnblnk

      lStr = lnblnk (strIn)


      do i=1,lStr
         if (strIn(i:i).ne.' ') then
            nStart = i
            goto 10
         endif 
      enddo 
      
 10   continue
      
      nCopy = 1
      
      do i=nStart,lStr
         strIn(nCopy:nCopy) = strIn(i:i)
         nCopy = nCopy + 1
      enddo 
      
      do i=nCopy,lStr
         strIn(i:i) = ' '
      enddo 
      
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  FindMinMax
c 
c Parametres  :  bimg    - description du tableau
c                data_in - tableau de donnees
c                vmin    - valeur min trouvee
c                vmax    - valeur max trouvee  
c
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine FindMinMax (bimg, data_in, vmin,vmax)

      implicit none

      include 'common.h'

      TYPE( bimgfile ) bimg

      integer i,j
      real    data_in(NXX,NYY)
      real    vmin,vmax

      vmin=1.e36
      vmax=-vmin

      do i=1,bimg%nxdata
         do j=1,bimg%nydata
            if (data_in(i,j).ne.bimg%spval) then
               vmin=amin1(vmin,data_in(i,j))
               vmax=amax1(vmax,data_in(i,j))
            endif
         enddo
      enddo


      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  GetStringParameters
c 
c Parametres  :  string_in  - chaine de characteres a analyser
c                string_out - structure a remplir
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine GetStringParameters (string_in, textstruct)

      implicit none
      include 'common.h'


      TYPE( text_string ) textstruct
      character*256 string_in

      integer blk1, blk2, blk3, blk4, blk5, blk6, blk7, blk8
      integer lnblnk, length
      integer i

      blk1 = -1
      blk2 = -1
      blk3 = -1
      blk4 = -1
      blk5 = -1
      blk6 = -1
      blk7 = -1
      blk8 = -1

      call FilterBlanks (string_in)
      
c     cherche le premier blanc

      length = lnblnk (string_in)

      do i = 1,length
         if (string_in(i:i).eq.' ') then
            blk1 = i
            goto 10
         endif 
      enddo 

 10   continue

c     elimine les blancs suivants

      do i = blk1+1, length
         if (string_in(i:i).ne.' ') then
            blk2 = i
            goto 11
         endif 
      enddo 

 11   continue

      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk2, length
         if (string_in(i:i).eq.' ') then
            blk3 = i
            goto 20
         endif 
      enddo 
      
 20   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk3+1, length
         if (string_in(i:i).ne.' ') then
            blk4 = i
            goto 21
         endif 
      enddo 
      
 21   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk4, length
         if (string_in(i:i).eq.' ') then
            blk5 = i
            goto 30
         endif 
      enddo 
      
 30   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk5+1, length
         if (string_in(i:i).ne.' ') then
            blk6 = i
            goto 31
         endif 
      enddo 
      
 31   continue

      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk6, length
         if (string_in(i:i).eq.' ') then
            blk7 = i
            goto 40
         endif 
      enddo 
      
 40   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk7+1, length
         if (string_in(i:i).ne.' ') then
            blk8 = i
            goto 41
         endif 
      enddo 
      
 41   continue

      if ((blk1.eq.-1).or.(blk2.eq.-1).or.
     .    (blk3.eq.-1).or.(blk4.eq.-1).or.
     .    (blk5.eq.-1).or.(blk6.eq.-1).or.
     .    (blk7.eq.-1).or.(blk8.eq.-1)) then
         call PrintMessage (1,7,' ')
      endif 

      read (string_in(1:blk1),*)    textstruct%xpos
      read (string_in(blk2:blk3),*) textstruct%ypos
      read (string_in(blk4:blk5),*) textstruct%csize
      read (string_in(blk6:blk7),*) textstruct%align


c     filtrage des caracteres " et ' au debut et a la fin de la
c     chaine
      textstruct%str = ' '

      if ((string_in(blk8:blk8).eq.'''').or.
     .    (string_in(blk8:blk8).eq.'"')) then
         blk8 = blk8+1
      endif 

      if ((string_in(length:length).eq.'''').or.
     .    (string_in(length:length).eq.'"')) then
         length = length-1
      endif 
      
      if (blk8.le.length) then
         read (string_in(blk8:length), '(A)') textstruct%str(1:length-blk8+1)
      else 
         textstruct%str = ' '
      endif 

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  GetStringrParameters
c 
c Parametres  :  string_in  - chaine de characteres a analyser
c                string_out - structure a remplir
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine GetStringrParameters (string_in, textstruct)

      implicit none
      include 'common.h'


      TYPE( text_string ) textstruct
      character*256 string_in

      integer blk1, blk2, blk3, blk4, blk5, blk6, blk7, blk8
      integer blk71, blk72
      integer lnblnk, length
      integer i

      blk1 = -1
      blk2 = -1
      blk3 = -1
      blk4 = -1
      blk5 = -1
      blk6 = -1
      blk7 = -1
      blk71 = -1
      blk72 = -1
      blk8 = -1

      call FilterBlanks (string_in)
      
c     cherche le premier blanc

      length = lnblnk (string_in)

      do i = 1,length
         if (string_in(i:i).eq.' ') then
            blk1 = i
            goto 10
         endif 
      enddo 

 10   continue

c     elimine les blancs suivants

      do i = blk1+1, length
         if (string_in(i:i).ne.' ') then
            blk2 = i
            goto 11
         endif 
      enddo 

 11   continue

      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk2, length
         if (string_in(i:i).eq.' ') then
            blk3 = i
            goto 20
         endif 
      enddo 
      
 20   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk3+1, length
         if (string_in(i:i).ne.' ') then
            blk4 = i
            goto 21
         endif 
      enddo 
      
 21   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk4, length
         if (string_in(i:i).eq.' ') then
            blk5 = i
            goto 30
         endif 
      enddo 
      
 30   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk5+1, length
         if (string_in(i:i).ne.' ') then
            blk6 = i
            goto 31
         endif 
      enddo 
      
 31   continue

      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk6, length
         if (string_in(i:i).eq.' ') then
            blk7 = i
            goto 310
         endif 
      enddo 

310    continue

      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 


      do i = blk7, length
         if (string_in(i:i).eq.' ') then
            blk71 = i
            goto 311
         endif 
      enddo 

311	continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 


      do i = blk71+1, length
         if (string_in(i:i).eq.' ') then
            blk72 = i
            goto 40
         endif 
      enddo 

 40   continue
      
      if (i.ge.length) then
         call PrintMessage (1,7,' ')
      endif 

      do i = blk72, length
         if (string_in(i:i).ne.' ') then
            blk8 = i
            goto 41
         endif 
      enddo 
      
 41   continue

      if ((blk1.eq.-1).or.(blk2.eq.-1).or.
     .    (blk3.eq.-1).or.(blk4.eq.-1).or.
     .    (blk5.eq.-1).or.(blk6.eq.-1).or.
     .    (blk71.eq.-1).or.(blk72.eq.-1).or.
     .    (blk7.eq.-1).or.(blk8.eq.-1)) then
         call PrintMessage (1,7,' ')
      endif 
      read (string_in(1:blk1),*)    textstruct%xpos
      read (string_in(blk2:blk3),*) textstruct%ypos
      read (string_in(blk4:blk5),*) textstruct%csize
      read (string_in(blk6:blk7),*) textstruct%align
      read (string_in(blk71:blk72),*) textstruct%angle


c     filtrage des caracteres " et ' au debut et a la fin de la
c     chaine

      if ((string_in(blk8:blk8).eq.'''').or.
     .    (string_in(blk8:blk8).eq.'"')) then
         blk8 = blk8+1
      endif 

      if ((string_in(length:length).eq.'''').or.
     .    (string_in(length:length).eq.'"')) then
         length = length-1
      endif 
      
      if (blk8.le.length) then
         read (string_in(blk8:length), '(A)') textstruct%str
      else 
         textstruct%str = ' '
      endif 

      return
      end


c ---------------------------------------------------------------------
c
c Nom         :  GetStringtParameters
c
c Parametres  :  string_in  - chaine de characteres a analyser
c                string_out - structure a remplir
c
c Description : Idem GetStringParameter mais pour les titres
c               la position taille et alignement sont deja definis
c
c ---------------------------------------------------------------------
 
      subroutine GetStringtParameters (string_in, textstruct)
 
      implicit none
      include 'common.h'
 
 
      TYPE( text_string ) textstruct
      character*256 string_in
 
      integer blk1, blk2, blk3, blk4, blk5, blk6, blk7, blk8
      integer blk71, blk72
      integer lnblnk, length
      integer i
 
      blk1 = -1
      blk2 = -1
      blk3 = -1
      blk4 = -1
      blk5 = -1
      blk6 = -1
      blk7 = -1
      blk71 = -1
      blk72 = -1
      blk8 = -1
 
      call FilterBlanks (string_in)
 
c     cherche le premier blanc
 
      length = lnblnk (string_in)
      blk8=1
 
c     filtrage des caracteres " et ' au debut et a la fin de la
c     chaine
 
      if ((string_in(blk8:blk8).eq.'''').or.
     .    (string_in(blk8:blk8).eq.'"')) then
         blk8 = blk8+1
      endif
 
      if ((string_in(length:length).eq.'''').or.
     .    (string_in(length:length).eq.'"')) then
         length = length-1
      endif
 
      if (blk8.le.length) then
         read (string_in(blk8:length), '(A)') textstruct%str
      else
         textstruct%str = ' '
      endif
 
      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  IsNumber
c 
c Parametres  :  str - caracteres a verifier
c 
c Description :  Verifie que tous les caracteres contenus dans une
c                chaine sont des nombre.
c                Retourne 0 si autre chose qu'un nombre est trouve.
c 
c ---------------------------------------------------------------------

      integer function IsNumber (str)

      implicit none

      character*(*) str
      integer lnblnk, length
      integer i, zero, nine

      length = lnblnk (str)
      zero   = ichar('0')
      nine   = ichar('9')

      IsNumber = 1

      if (length.eq.0) then
         IsNumber = 0
      else 
         do i = 1,length
            if ((ichar(str(i:i)).lt.zero).or.
     .                (ichar(str(i:i)).gt.nine)) then
               IsNumber = 0
               goto 10
            endif 
         enddo
         
 10      continue
             
      endif 

      return
      end





c ---------------------------------------------------------------------
c 
c Nom         :  PrintMessage
c 
c Parametres  :  fatal   - indique si oui ou non on doit stopper le
c                          programme
c                msgnum -  numero du message a afficher
c 
c Description :  
c 
c ---------------------------------------------------------------------

      subroutine PrintMessage (fatal, msgnum, string)

      implicit none

      integer fatal, msgnum

      character*(*) string

      character*80  ErrMsg(15)
      data ErrMsg
     .   /'erreur : il n''y a pas suffisamment de couches dans: ',
     .   'erreur : coordonnees de zoom hors du fichier.',
     .   'erreur : les composantes vectorielles ne correspondent pas.',
     .   'erreur : veuillez choisir la methode 1 ou 2',
     .   'erreur : 2D ou 3D deja specifie n''ajoutez pas de composantes'
     .  ,'erreur : ce fichier n''existe pas : ',
     .   'erreur de syntaxe : option -string',
     .   'erreur de syntaxe option FORMAT',
     .   'erreur : il n''y a pas suffisamment de pas de temps dans: ',
     .   'erreur : option -direct non encore supportee pour ce type de vecteur ',
     .   'erreur : option -direct non encore supportee pour dim .ne. 1 ',
     .   'erreur : Version de format non supportee actuellement: ',
     .   'warning : option -clrdata ignoree a cause de cntshade ',
     .   'erreur : Pour cntshade il faut specifier cntdata  ',
     .   'warning : Pour cntshade la palette est figee ! '/


      print *,ErrMsg(msgnum),string

      if (fatal.eq.1) stop

      return
      end

c ---------------------------------------------------------------------
c
c Nom         :  LevelOfDepth 
c
c Parametres  :  bimg : 
c                dep  : required depth pour option -dep
c                ksup : indice de la couche au dessus de dep
c                kinf : indice de la couche au dessous de dep (ksup+1)
c                knear: nearest level ...
c
c Description :  Cherche les indices de couche correspondant a dep
c
c ---------------------------------------------------------------------
	subroutine LevelOfDepth(bimg,dep,ksup,kinf,knear)
        implicit none
        include 'common.h'
      TYPE( bimgfile ) bimg
       real dep, dd1, dd2
       integer ksup,kinf, k, knear
       integer ncou
	
	ncou=bimg%ncou
        dep = -abs(dep)
        k = 1

	do while (dep.lt.bimg%depth(k))
          k = k + 1
          if (k .gt. ncou) then
           print *,' La profondeur requise est superieure au max !'
           stop
          endif
        enddo
        ksup=k-1
        kinf=k
	dd1=bimg%depth(ksup)-dep
	dd2=dep - bimg%depth(kinf)
	if (dd1.lt.dd2) then
           knear = ksup
        else
           knear = kinf
        endif
	bimg%alphasup= (dep - bimg%depth(kinf))/
     .              ( bimg%depth(ksup) - bimg%depth(kinf))
	
        return
	end



