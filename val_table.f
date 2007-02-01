c --------------------------------------------------------------------
c 
c Programme      :  chart et coupe
c Fichier        :  val_table.f
c 
c Auteur         :  Eric Brown
c
c Description    :  Ce fichier contient les fonction d'acces, de mise
c                   a jour et d'interpretation de la table des valeurs
c                   accessibles pour impression sur un dessin.
c 
c ---------------------------------------------------------------------




c ---------------------------------------------------------------------
c 
c Nom         :  InitValTable
c 
c Parametres  :  BimgInfo - information sur le fichier bimg traite
c
c Description :  Initialise les valeurs, ainsi que les formats
c 
c ---------------------------------------------------------------------

      subroutine InitValTable ()

      include 'val_table.h'

      integer iday, imon, iyear
      

      int_table (ICOUCHE )%id = 'COUCHE' 
      int_table (ITSTEP )%id  = 'TSTEP'  

      int_table (ICLR_NX   )%id = 'CLR_NX'    
      int_table (ICLR_NY   )%id = 'CLR_NY'    
      int_table (ICLR_NZ   )%id = 'CLR_NZ'    
      int_table (ICLR_EXP  )%id = 'CLR_EXP'

      int_table (ICNT_NX   )%id = 'CNT_NX'    
      int_table (ICNT_NY   )%id = 'CNT_NY'    
      int_table (ICNT_NZ   )%id = 'CNT_NZ'    
      int_table (ICNT_EXP  )%id = 'CNT_EXP'

      int_table (IVEC_NX   )%id = 'VEC_NX'    
      int_table (IVEC_NY   )%id = 'VEC_NY'    
      int_table (IVEC_NZ   )%id = 'VEC_NZ'    
      int_table (IVEC_EXP  )%id = 'VEC_EXP'

      int_table (ICLR_XAXIS)%id = 'X_AXIS'    
      int_table (ICLR_YAXIS)%id = 'Y_AXIS'    
      int_table (ICLR_ZAXIS)%id = 'Z_AXIS'    
      int_table (ICLR_PAL  )%id = 'PALETTE'    


      real_table(ICLR_MIN  )%id = 'CLR_MIN'   
      real_table(ICLR_MAX  )%id = 'CLR_MAX'   
      real_table(ICLR_SPVAL)%id = 'CLR_SPVAL' 
      real_table(ICLR_DX   )%id = 'CLR_DX'    
      real_table(ICLR_DY   )%id = 'CLR_DY'    
      real_table(ICLR_TIME )%id = 'CLR_TIME'     
      real_table(ICLR_TIMEDAY )%id = 'CLR_TIMEDAY'     
      real_table(ICLR_DEPTH )%id= 'CLR_DEPTH' 

      real_table(ICNT_MIN  )%id = 'CNT_MIN'   
      real_table(ICNT_MAX  )%id = 'CNT_MAX'   
      real_table(ICNT_SPVAL)%id = 'CNT_SPVAL' 
      real_table(ICNT_DX   )%id = 'CNT_DX'    
      real_table(ICNT_DY   )%id = 'CNT_DY'    
      real_table(ICNT_TIME )%id = 'CNT_TIME'     
      real_table(ICNT_DEPTH )%id= 'CNT_DEPTH' 

      real_table(IVEC_MIN  )%id = 'VEC_MIN'   
      real_table(IVEC_MAX  )%id = 'VEC_MAX'   
      real_table(IVEC_SPVAL)%id = 'VEC_SPVAL' 
      real_table(IVEC_DX   )%id = 'VEC_DX'    
      real_table(IVEC_DY   )%id = 'VEC_DY'    
      real_table(IVEC_TIME )%id = 'VEC_TIME'     
      real_table(IVEC_DEPTH )%id= 'VEC_DEPTH' 

      string_table(ICLR_STR1)%id = 'CLR_STR1'
      string_table(ICLR_STR2)%id = 'CLR_STR2'
      string_table(ICLR_STR3)%id = 'CLR_STR3'
      string_table(ICLR_STR4)%id = 'CLR_STR4'

      string_table(ICNT_STR1)%id = 'CNT_STR1'
      string_table(ICNT_STR2)%id = 'CNT_STR2'
      string_table(ICNT_STR3)%id = 'CNT_STR3'
      string_table(ICNT_STR4)%id = 'CNT_STR4'

      string_table(IVEC_STR1)%id = 'VEC_STR1'
      string_table(IVEC_STR2)%id = 'VEC_STR2'
      string_table(IVEC_STR3)%id = 'VEC_STR3'
      string_table(IVEC_STR4)%id = 'VEC_STR4'

      string_table(I_DATE)%id = 'DATE'
      string_table(CLR_ICDATE)%id = 'CLR_JCNES'
      string_table(CNT_ICDATE)%id = 'CNT_JCNES'
      string_table(VEC_ICDATE)%id = 'VEC_JCNES'
      string_table(CLIPPER_DATE)%id = 'CLIPPER_DAT'

      int_table (ICOUCHE   )%format = '(i10)'
      int_table (ITSTEP    )%format = '(i10)'

      int_table (ICLR_NX   )%format = '(i10)'
      int_table (ICLR_NY   )%format = '(i10)'
      int_table (ICLR_NZ   )%format = '(i10)'
      int_table (ICLR_EXP  )%format = '(i10)'

      int_table (ICNT_NX   )%format = '(i10)'
      int_table (ICNT_NY   )%format = '(i10)'
      int_table (ICNT_NZ   )%format = '(i10)'
      int_table (ICNT_EXP  )%format = '(i10)'

      int_table (IVEC_NX   )%format = '(i10)'
      int_table (IVEC_NY   )%format = '(i10)'
      int_table (IVEC_NZ   )%format = '(i10)'
      int_table (IVEC_EXP  )%format = '(i10)'

      int_table (ICLR_XAXIS)%format = '(i10)'    
      int_table (ICLR_YAXIS)%format = '(i10)'    
      int_table (ICLR_ZAXIS)%format = '(i10)'    
      int_table (ICLR_PAL  )%format = '(f10.2)'     


      real_table(ICLR_MIN  )%format = '(f10.2)'
      real_table(ICLR_MAX  )%format = '(f10.2)'
      real_table(ICLR_SPVAL)%format = '(f10.2)'
      real_table(ICLR_DX   )%format = '(f10.2)'
      real_table(ICLR_DY   )%format = '(f10.2)'
      real_table(ICLR_TIME )%format = '(f10.2)'
      real_table(ICLR_TIMEDAY )%format = '(f10.2)'
      real_table(ICLR_DEPTH)%format = '(f10.2)'

      real_table(ICNT_MIN  )%format = '(f10.2)'
      real_table(ICNT_MAX  )%format = '(f10.2)'
      real_table(ICNT_SPVAL)%format = '(f10.2)'
      real_table(ICNT_DX   )%format = '(f10.2)'
      real_table(ICNT_DY   )%format = '(f10.2)'
      real_table(ICNT_TIME )%format = '(f10.2)'
      real_table(ICNT_DEPTH)%format = '(f10.2)'

      real_table(IVEC_MIN  )%format = '(f10.2)'
      real_table(IVEC_MAX  )%format = '(f10.2)'
      real_table(IVEC_SPVAL)%format = '(f10.2)'
      real_table(IVEC_DX   )%format = '(f10.2)'
      real_table(IVEC_DY   )%format = '(f10.2)'
      real_table(IVEC_TIME )%format = '(f10.2)'
      real_table(IVEC_DEPTH)%format = '(f10.2)'

      string_table(ICLR_STR1)%format = '(A)'
      string_table(ICLR_STR2)%format = '(A)'
      string_table(ICLR_STR3)%format = '(A)'
      string_table(ICLR_STR4)%format = '(A)'

      string_table(ICNT_STR1)%format = '(A)'
      string_table(ICNT_STR2)%format = '(A)'
      string_table(ICNT_STR3)%format = '(A)'
      string_table(ICNT_STR4)%format = '(A)'

      string_table(IVEC_STR1)%format = '(A)'
      string_table(IVEC_STR2)%format = '(A)'
      string_table(IVEC_STR3)%format = '(A)'
      string_table(IVEC_STR4)%format = '(A)'



      string_table(I_DATE)%format = '(A)'


c     call idate (imon, iday, iyear)
      write (string_table(I_DATE)%str,100) iday,imon,iyear

 100  format (i2.2,'/',i2.2,'/',i2.2)

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  ParseString
c 
c Parametres  :  
c Description :  Lit la chaine de caracteres strIn et remplace les
c                variables $$ par leur valeur numerique.
c                Retourne le resultat dans strOut
c 
c ---------------------------------------------------------------------

      subroutine ParseString (strIn, strOut)

      implicit none

      character*256 strIn
      character*256 strOut
      integer i
      integer n1, n2, lStr, nOut, lOut
      integer reading_text
      integer lStrOut
      character*2  ctrlChar
      character*256 strValue

      integer lnblnk


      ctrlChar = '@'

      lStr = lnblnk (strIn)

      reading_text = 1
      n1   = 1
      n2   = 1
      nOut = 1


      do i=1,len(strOut)
         strOut(i:i) = ' '
      enddo 


      if (lStr.eq.0) then
         lStrOut = len (strOut)
         do i=1, lStrOut
            strOut(i:i) = ' '
         enddo 
         return
      endif 


      do while (n2.le.lStr)
         if (strIn (n2:n2).eq.ctrlChar(1:1)) then

            if (reading_text.eq.1) then              ! fin d'une section de texte
               if (n1.ne.n2) then  ! verifie que la string est non-nulle
                  lOut = n2 - n1 

                  write (strOut (nOut:nOut+lOut-1),'(A)') strIn(n1:n2-1)

                  nOut = nOut+lOut
               endif 
               
               reading_text = 0
            else
               if (n1.ne.n2) then  ! verifie que la string est non-nulle
                  call GetNumValue (strIn(n1:n2-1), strValue,
     .                              n2-n1, lStrOut)

                  write (strOut (nOut:nOut+lStrOut-1), '(A)')
     .                      strValue(1:lStrOut)

                  nOut = nOut+lStrOut
               endif 
               
               reading_text = 1
            endif 

            n1 = n2 + 1     ! n1 pointe sur le caractere juste apres le caractere de controle
        endif 

         n2 = n2 + 1
      enddo 

c     parametre sans caractere de controle a la fin

      if (reading_text.eq.0) then
         call PrintMessage (1,7,' ')

c     texte suivant le dernier caractere de controle

      else if (n2.ne.n1) then
         lOut = n2 - n1 - 1
         write (strOut (nOut:nOut+lOut), '(A)') strIn(n1:n2-1)                
               
         nOut = nOut+lOut+1         
      endif 


c     efface le reste de la chaine de caracteres
      
      lOut = lnblnk(strOut)
      
      if (nOut.le.lOut) then
         do i=nOut,lOut
            strOut(i:i) = ' '
         enddo 
      endif 


      return 
      end






c ---------------------------------------------------------------------
c 
c Nom         :  GetNumValue
c 
c Parametres  :  strIn  - identificateur de la variable a remplacer
c                strOut - valeur numerique correspondante
c      
c Description :  Cette fonction recherche strIn dans les valeurs
c                possibles. Lorsqu'un nom de variable est reconnu,
c                sa valeur numerique est ecrite dans strOut selon le
c                format par defini pour cette variable.
c
c                Les formats acceptes sont I, F et E
c 
c ---------------------------------------------------------------------

      subroutine GetNumValue (strIn, strOut, lStrIn, lStrOut)

      implicit none

      include 'val_table.h'

      integer IDformat
      integer lnblnk
      integer lStrIn, lStrOut
      integer i

      character*(*) strIn, strOut
      character*(256) strWork
   

      integer found


      found = 0

c     met la string de travail a zero

      do i=1,256
         strWork(i:i) = ' '
      enddo 

      do i=1, NUM_INT_VALS
         if (strIn(1:lStrIn).eq.int_table(i)%id) then
            found = 1

            if (IDformat (int_table(i)%format).eq.1) then
               write (strWork,int_table(i)%format) int_table(i)%iVal
            else 
               write (strWork,int_table(i)%format)
     .                                      float (int_table(i)%iVal)
            endif 
         endif 
      enddo 

      if (found.eq.0) then
         do i=1, NUM_REAL_VALS
            if (strIn(1:lStrIn).eq.real_table(i)%id) then
               found = 1

               if (IDformat (real_table(i)%format).eq.1) then
                  write (strWork,real_table(i)%format)
     .                                          int(real_table(i)%rVal)
               else 
                  write (strWork,real_table(i)%format)
     .                                              real_table(i)%rVal
               endif 
            endif 
         enddo 
      endif 

      if (found.eq.0) then
         do i=1, NUM_STR_VALS
            if (strIn(1:lStrIn).eq.string_table(i)%id) then
               found = 1

               write (strWork,'(A)') string_table(i)%str
            endif 
         enddo 
      endif 

      if (found.eq.0) then
         write (strWork,100) strIn(1:lStrIn)
 100     format('$',a,'$')
      endif 

c     enleve les caracteres d'espacement au debut et a la fin

      call FilterBlanks (strWork)
      lStrOut = lnblnk (strWork)
      write (strOut,'(A)') strWork(1:lStrOut)

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  IDformat
c 
c Parametres  :  strFmt - chaine contenant le format a identifier
c
c Description :  cette fonction retourne 1 si le format est pour un 
c                entier et 0 pour un reel.
c 
c ---------------------------------------------------------------------

      integer function IDformat (strFmt)


      implicit none

      character*10 strFmt

      integer lStr, i
      integer lnblnk
      integer fmtType
      

      lStr = lnblnk(strFmt)

      do i=1,lStr
         if ((strFmt(i:i).eq.'i').or.(strFmt(i:i).eq.'I')) then
            fmtType = 1
         else if ((strFmt(i:i).eq.'e').or.(strFmt(i:i).eq.'E').or.
     .            (strFmt(i:i).eq.'f').or.(strFmt(i:i).eq.'F')) then    
                  
            fmtType = 0
         endif 
      enddo 

      IDformat = fmtType

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  SetFormatString
c 
c Parametres  :  strVal    - valeur dont on veut definir le format
c                strFormat - nouveau format
c
c Description :  Cette fonction change le format associe a une valeur
c 
c ---------------------------------------------------------------------

      subroutine SetFormatString (strVal, strFormat)

      implicit none
      include 'val_table.h'

      character*(*) strVal, strFormat

      integer found,i, lStrVal
      integer lnblnk

      lStrVal = lnblnk(strVal)

      found = 0

      do i=1, NUM_INT_VALS
         if (strVal(1:lStrVal).eq.int_table(i)%id) then
            found = 1

            write (int_table(i)%format,100)
     .                                  strFormat(1:lnblnk(strFormat))
         endif 
      enddo 

      if (found.eq.0) then
         do i=1, NUM_REAL_VALS
            if (strVal(1:lStrVal).eq.real_table(i)%id) then
               found = 1
               write (real_table(i)%format,100)
     .                                  strFormat(1:lnblnk(strFormat))
            endif 
         enddo 
      endif 

      if (found.eq.0) then
         print *,'Valeur inconnue : ',strVal
      endif 

 100  format('(',a,')')

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  SplitValFmt (strValFmt, strValue, strFormat)
c 
c Parametres  :  
c Description :  Cette fonction lit une ligne sur laquelle se
c                trouvent une valeur et son format et les separe
c                en deux chaines de caracteres
c 
c ---------------------------------------------------------------------

      subroutine SplitValFmt (strValFmt, strValue, strFormat)

      implicit none
      
      character*(*) strValFmt, strValue, strFormat
      integer lnblnk
      integer lStrValFmt
      integer n1,n2,n3,i

      n1 = 1
      n2 = 1
      n3 = 1

      lStrValFmt = lnblnk (strValFmt)


c     trouve le premier caractere different de ' '
      
      do i=1,lStrValFmt
         if ((strValFmt(i:i).eq.' ').or.
     .        (strValFmt(i:i).eq.'''')) then
            n1 = i+1
         else 
            goto 10
         endif 
      enddo       

 10   continue


c     trouve le premier espace apres le premier mot

      do i=n1,lStrValFmt
c         if ((strValFmt(i:i).ne.' ').and.
c     .       (strValFmt(i:i).ne.'''')) then
         if (strValFmt(i:i).ne.' ') then
            n2 = i
         else 
            goto 20
         endif 
      enddo 

 20   continue


c     trouve le debut du format
      
      do i=n2+1,lStrValFmt
c         if ((strValFmt(i:i).eq.' ').or.
c     .       (strValFmt(i:i).eq.'''')) then
         if (strValFmt(i:i).eq.' ') then
            n3 = i+1
         else 
            goto 30
         endif 
      enddo 

 30   continue

      if (n2.eq.lStrValFmt) then
         call PrintMessage (1,8,' ')
      endif 

      if (n3.eq.lStrValFmt+1) then
         call PrintMessage (1,8,' ')
      endif 

      if (strValFmt(n3:n3).eq.'''') then
         n3 = n3+1
      endif 

      if (strValFmt(lStrValFmt:lStrValFmt).eq.'''') then
         lStrValFmt = lStrValFmt - 1
      endif 

      strValue = strValFmt(n1:n2)
      strFormat = strValFmt(n3:lStrValFmt)
      
      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  SetValTable
c 
c Parametres  :  Trois structures bimg, decrivant l'information
c                relative aux couleurs, contours et vecteurs
c 
c Description :  Initialise les differentes valeurs globales
c                associees a chaque fichier de donnees.
c 
c ---------------------------------------------------------------------

      subroutine SetValTable (bimgclr, bimgcnt, bimgvec)

      implicit none
      include 'common.h'
      include 'val_table.h'

      TYPE( bimgfile ) bimgclr, bimgcnt, bimgvec


      int_table (ICLR_NZ)%iVal = bimgclr%ncou
      int_table (ICNT_NZ)%iVal = bimgcnt%ncou
      int_table (IVEC_NZ)%iVal = bimgvec%ncou
      

      real_table(ICLR_SPVAL)%rVal = bimgclr%spval
      real_table(ICLR_DX   )%rVal = bimgclr%dx
      real_table(ICLR_DY   )%rVal = bimgclr%dy
      
      real_table(ICNT_SPVAL)%rVal = bimgcnt%spval
      real_table(ICNT_DX   )%rVal = bimgcnt%dx
      real_table(ICNT_DY   )%rVal = bimgcnt%dy

      real_table(IVEC_SPVAL)%rVal = bimgvec%spval
      real_table(IVEC_DX   )%rVal = bimgvec%dx
      real_table(IVEC_DY   )%rVal = bimgvec%dy


      string_table(ICLR_STR1)%str = bimgclr%str1
      string_table(ICLR_STR2)%str = bimgclr%str2
      string_table(ICLR_STR3)%str = bimgclr%str3
      string_table(ICLR_STR4)%str = bimgclr%str4

      string_table(ICNT_STR1)%str = bimgcnt%str1
      string_table(ICNT_STR2)%str = bimgcnt%str2
      string_table(ICNT_STR3)%str = bimgcnt%str3
      string_table(ICNT_STR4)%str = bimgcnt%str4

      string_table(IVEC_STR1)%str = bimgvec%str1
      string_table(IVEC_STR2)%str = bimgvec%str2
      string_table(IVEC_STR3)%str = bimgvec%str3
      string_table(IVEC_STR4)%str = bimgvec%str4

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

      subroutine UpdateValTable (tstep,clr_cou,cnt_cou,vec_cou
     +                           ,bimgclr,bimgcnt,bimgvec)

      implicit none

      include 'common.h'
      include 'val_table.h'

      integer clr_cou, cnt_cou, vec_cou, tstep

      TYPE( bimgfile ) bimgclr, bimgcnt, bimgvec

      int_table(ICOUCHE)%iVal  = clr_cou
      int_table(ITSTEP)%iVal   = tstep


      int_table (ICLR_NX)%iVal   = bimgclr%nxdata
      int_table (ICLR_NY)%iVal   = bimgclr%nydata

      real_table(ICLR_TIME)%rVal = bimgclr%time
      real_table(ICLR_TIMEDAY)%rVal = bimgclr%time/86400.
      print *,' TIME =', bimgclr%time 
      print *,' TIME DAY=', bimgclr%time/86400. 
      real_table(ICLR_DEPTH)%rVal = abs(bimgclr%depth(clr_cou))

      int_table (ICNT_NX)%iVal   = bimgcnt%nxdata
      int_table (ICNT_NY)%iVal   = bimgcnt%nydata
      real_table(ICNT_TIME)%rVal = bimgcnt%time
      real_table(ICNT_DEPTH)%rVal = abs(bimgcnt%depth(cnt_cou))

      int_table (IVEC_NX)%iVal   = bimgvec%nxdata
      int_table (IVEC_NY)%iVal   = bimgvec%nydata
      real_table(IVEC_TIME)%rVal = bimgvec%time
      real_table(IVEC_DEPTH)%rVal = abs(bimgvec%depth(vec_cou))

	if (opt_dep.eq.1) then
      real_table(ICLR_DEPTH)%rVal = abs(req_dep)
      real_table(ICNT_DEPTH)%rVal = abs(req_dep)
      real_table(IVEC_DEPTH)%rVal = abs(req_dep)
	endif


      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  SetClrMin, SetClrMax
c 
c Parametres  :  vmin
c 
c Description :  Met la valeur fournie en argument dans la table
c                des valeurs affichables
c 
c ---------------------------------------------------------------------

      subroutine SetClrMin (vmin)

      implicit none

      include 'val_table.h'
      real vmin

      real_table(ICLR_MIN)%rVal = vmin

      return
      end


      subroutine SetClrMax (vmax)

      implicit none

      include 'val_table.h'
      real vmax

      real_table(ICLR_MAX)%rVal = vmax

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  SetVecMin, SetVecMax
c 
c Parametres  :  vmin
c 
c Description :  Met la valeur fournie en argument dans la table
c                des valeurs affichables
c 
c ---------------------------------------------------------------------

      subroutine SetVecMin (vmin)

      implicit none

      include 'val_table.h'
      real vmin

      real_table(IVEC_MIN)%rVal = vmin

      return
      end


      subroutine SetVecMax (vmax)

      implicit none

      include 'val_table.h'
      real vmax

      real_table(IVEC_MAX)%rVal = vmax

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  SetCntExp
c 
c Parametres  :  exp
c 
c Description :  Met la valeur fournie en argument dans la table
c                des valeurs affichables
c 
c ---------------------------------------------------------------------

      subroutine SetCntExp (exp)

      implicit none

      include 'val_table.h'
      integer exp

      int_table(ICNT_EXP)%iVal = exp

      return
      end


c ---------------------------------------------------------------------
c 
c Nom         :  SetClrExp
c 
c Parametres  :  exp
c 
c Description :  Met la valeur fournie en argument dans la table
c                des valeurs affichables
c 
c ---------------------------------------------------------------------

      subroutine SetClrExp (exp)

      implicit none

      include 'val_table.h'
      integer exp

      int_table(ICLR_EXP)%iVal = exp

      return
      end



c ---------------------------------------------------------------------
c 
c Nom         :  SetCNESday
c 
c Parametres  :  day - jour cray
c 
c Description :  cette fonction calcule le jour CNES a partir d'un 
c                nombre et l'exprime sous forme de chaine de caracteres
c 
c ---------------------------------------------------------------------

      subroutine SetClrCNESday (day)

      implicit none
      include 'val_table.h'

      real    day
      character*17 strDay
      integer i

      call convcnes (nint(day),strDay)
      
      do i=1,80
         string_table(CLR_ICDATE)%str(i:i) = ' '
         string_table(CNT_ICDATE)%str(i:i) = ' '
         string_table(VEC_ICDATE)%str(i:i) = ' '
      enddo 

      string_table(CLR_ICDATE)%str(1:17) = strDay
      string_table(CNT_ICDATE)%str(1:17) = strDay
      string_table(VEC_ICDATE)%str(1:17) = strDay

      return
      end


c ---------------------------------------------------------------------
c ---------------------------------------------------------------------
c 
c Nom         :  SetCLIPPERday
c 
c Parametres  :  day - jour cray
c 
c Description :  cette fonction calcule le jour CNES a partir d'un 
c                nombre et l'exprime sous forme de chaine de caracteres
c 
c ---------------------------------------------------------------------

      subroutine SetCLIPPERday (day)

      implicit none
      include 'val_table.h'

      real    day
      character*10 strDay, clipperdat
      integer i

      strDay = clipperdat (nint(day))
      
      do i=1,80
         string_table(CLIPPER_DATE)%str(i:i) = ' '
      enddo 

      string_table(CLIPPER_DATE)%str(1:17) = strDay

      return
      end

c 
c Nom         :  SetCntMin, SetCntMax
c 
c Parametres  :  vmin
c 
c Description :  Met la valeur fournie en argument dans la table
c                des valeurs affichables
c 
c ---------------------------------------------------------------------

      subroutine SetCntMin (vmin)

      implicit none

      include 'val_table.h'
      real vmin

      real_table(ICNT_MIN)%rVal = vmin

      return
      end


      subroutine SetCntMax (vmax)

      implicit none

      include 'val_table.h'
      real vmax

      real_table(ICNT_MAX)%rVal = vmax

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

      subroutine convcnes(njd,jourjulien)

c     ******************************************
c       CONVERSION jour CNES --> jour julien
c     ******************************************
c       njd = jour CNES
c       nd  = jour
c       nm  = mois
c       na  = annee
c     ******************************************

      integer  n(12)
      character*9 mois(12)
      integer njd
      character*17 jourjulien
      
      data n /31,28,31,30,31,30,31,31,30,31,30,31/
      data mois/'  JANVIER','  FEVRIER','     MARS','    AVRIL',
     .          '      MAI','     JUIN','  JUILLET','     AOUT',
     .          'SEPTEMBRE', ' OCTOBRE',' NOVEMBRE',' DECEMBRE'/

      integer njul, na, nj, nb,nm, nd,j,nm1,m,nj3,ndj

      njul=njd+1
      na=njul/365
      nj=njul-na*365
      nb=(na+1)/ 4
      nj=nj-nb
      if(nj.gt.0) go to 1
      na=na+1949
      nm=12
      nd=nj+31
      goto 1234
 1    j=na-2-nb*4
      na=na+1950
      if(j.lt.0) go to 5000
 4000 if(60-nj)4500,7000,5000
 4500 nm1=60
      m=3
      go to 6000
 5000 nm1=0
      m=1
 6000 ndj=nm1+n(m)
      nj3=nj-ndj
      if(nj3.le.0) go to 8000
 6500 m=m+1
      nm1=ndj
      go to 6000
 7000 nm=2
      nd=29
      
      goto 1234
 8000 nm=m
      nd=nj-nm1
 9000 goto 1234
      
 1234 continue
      write(jourjulien,66) nd,mois(nm),na
 66   format(i2,1x,a9,1x,i4)

      return
      end
      
             function clipperdat(jclip)
CCC----------------------------------------------------------------------
CCC                   FUNCTION  CLIPPERDAT
CCC                   ******************
CCC
CCC    PURPOSE:
CCC    --------
CCC     This program takes a date as a clipper adatrj number
CCC    It returns the date in calendar format ndastp
CCC
CC     METHOD:
CC     =======
CC      1- Read clipper date
CC      2- Add the offset to have julian days (2440998)
CC      3- Convert julian day to calendar
CC      4- print the final date in the OPA format
CC
CC     CREDIT:
CC     =======
CC      The original sources for  caldat were found in the
CC      book Numerical Recipes.
CC
CC      
CC     AUTHOR:
CC     =======
CC        2000 : J.-M. Molines add.
CC
CCC----------------------------------------------------------------------
C 0.0 Declarations
C ----------------
C
      IMPLICIT NONE
C
      INTEGER jfin,mfin,ianfin
      INTEGER jclip,julfin
      CHARACTER*10 clipperdat
C
C
      julfin= jclip + 2433328
      IF ( julfin .GT. 2436250)julfin = jclip +2440998
      CALL caldat (julfin,mfin,jfin,ianfin)
C     PRINT '(i4.2,2i2.2)',ianfin,mfin,jfin
      write(clipperdat,'(i4.4,1h/,i2.2,1h/,i2.2)') ianfin,mfin,jfin
      RETURN
      END
C###
      SUBROUTINE caldat(kjulian,kmm,kid,kiyyy)
CCC -------------------------------------------------------------------
CCC                   SUBROUTINE CALDAT
CCC                   *****************
CCC   PURPOSE:
CCC   --------
CCC    This routine convert a julian day in calendar date.
CCC    It is the inverse of the function julday.  
CCC
CC    METHOD:
CC    -------
CC     This routine comes directly from the Numerical Recipe Book,
CC   press et al., numerical recipes, cambridge univ. press, 1986.
CC
CC    ARGUMENTS:
CC    ----------
CC     kjulian : input julian day number
CC     kmm     : output, corresponding month
CC     kid     : output, corresponding day
CC     kiyyy   : output, corresponding year, positive if a.d, negative b.c.
CC      
CC    
CC   AUTHOR:
CC   ------
CC     1998: J.M. Molines for the Doctor form.
CCC------------------------------------------------------------------------
C ... Declarations:
C
      IMPLICIT NONE
      INTEGER jpgreg
      PARAMETER (jpgreg = 2299161)
      INTEGER kjulian, kmm, kid, kiyyy
      INTEGER ia,ialpha,ib,ic,id,ie
C
C ... Cross over to Gregorian Calendar produces this correction:
C
      IF ( kjulian .GE. jpgreg) THEN
       ialpha = INT ((( kjulian - 1867216) - 0.25)/36524.25 )
       ia     = kjulian +1 + ialpha -INT (0.25*ialpha)
      ELSE
       ia = kjulian
      END IF
C
      ib = ia + 1524
      ic = INT (6680. + (( ib -2439870) - 122.1)/365.25 )
      id = 365* ic + INT (0.25*ic)
      ie = INT (( ib - id )/30.6001)
C
      kid = ib - id - INT (30.6001*ie)
      kmm = ie -1
      IF ( kmm .GT. 12 ) kmm = kmm - 12
      kiyyy = ic - 4715
      IF ( kmm   .GT. 2 ) kiyyy = kiyyy - 1
      IF ( kiyyy .LE. 0 ) kiyyy = kiyyy - 1
      RETURN
      END 
