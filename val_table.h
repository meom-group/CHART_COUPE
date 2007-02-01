c --------------------------------------------------------------------
c 
c Programme      :  chart et coupe
c Fichier        :  val_table.h
c 
c Auteur         :  Eric Brown
c
c Description    :  ce fichier contient la structure globale ou se
c                   trouvent les valeurs pouvant etre inclues dans une
c                   chaine de caracteres entre $$.
c 
c ---------------------------------------------------------------------


      integer NUM_INT_VALS, NUM_REAL_VALS, NUM_STR_VALS

      parameter (NUM_INT_VALS=18)
      parameter (NUM_REAL_VALS=22)
      parameter (NUM_STR_VALS=17)

      TYPE  INTEGER_TABLE_STRUCT
         SEQUENCE
         character*11 id        ! string d'identification du champ
         character*30 format    ! format d'impression du champ
         integer      iVal      ! valeur
      end TYPE  INTEGER_TABLE_STRUCT

      TYPE(INTEGER_TABLE_STRUCT) int_table(NUM_INT_VALS)

      TYPE  REAL_TABLE_STRUCT
         SEQUENCE
         character*11 id        ! string d'identification du champ
         character*30 format    ! format d'impression du champ
         real      rVal         ! valeur
      end TYPE  REAL_TABLE_STRUCT

      TYPE( REAL_TABLE_STRUCT ) real_table(NUM_REAL_VALS)
   

      TYPE  STRING_TABLE_STRUCT
         SEQUENCE
         character*11 id        ! string d'identification du champ
         character*30 format    ! format d'impression du champ
         character*80 str  ! chaine de caracteres
      end TYPE  STRING_TABLE_STRUCT

      TYPE( STRING_TABLE_STRUCT ) string_table(NUM_STR_VALS)

      common /table/int_table, real_table, string_table

      integer ICLR_MIN, ICLR_MAX, ICLR_NX, ICLR_NY, ICLR_NZ
      integer ICLR_SPVAL, ICLR_DX, ICLR_DY, ICLR_TIME, ICLR_TIMEDAY
      integer ICNT_MIN, ICNT_MAX, ICNT_NX, ICNT_NY, ICNT_NZ
      integer ICNT_SPVAL, ICNT_DX, ICNT_DY, ICNT_TIME
      integer IVEC_MIN, IVEC_MAX,  IVEC_NX, IVEC_NY, IVEC_NZ
      integer IVEC_SPVAL, IVEC_DX, IVEC_DY, IVEC_TIME
      integer ICLR_EXP, ICNT_EXP, IVEC_EXP
      integer ICLR_STR1, ICLR_STR2, ICLR_STR3, ICLR_STR4
      integer ICNT_STR1, ICNT_STR2, ICNT_STR3, ICNT_STR4
      integer IVEC_STR1, IVEC_STR2, IVEC_STR3, IVEC_STR4
      integer I_DATE, CLR_ICDATE,CNT_ICDATE,VEC_ICDATE,CLIPPER_DATE
      integer ICOUCHE, ITSTEP, IVEC_DEPTH, ICLR_DEPTH, ICNT_DEPTH


      integer ICLR_XAXIS, ICLR_YAXIS, ICLR_ZAXIS, ICLR_PAL

      parameter (ICOUCHE=1,
     .           ITSTEP=2, 
     .           ICLR_NX=3,
     .           ICLR_NY=4,
     .           ICLR_NZ=5,
     .           ICLR_XAXIS=6,
     .           ICLR_YAXIS=7,
     .           ICLR_ZAXIS=8,
     .           ICLR_PAL=9,
     .           ICNT_NX=10,
     .           ICNT_NY=11,
     .           ICNT_NZ=12,
     .           IVEC_NX=13,
     .           IVEC_NY=14,
     .           IVEC_NZ=15,
     .           ICLR_EXP=16,
     .           ICNT_EXP=17,
     .           IVEC_EXP=18)
      parameter (ICLR_SPVAL=1,
     .           ICLR_DX=2,
     .           ICLR_DY=3,
     .           ICLR_TIME=4, 
     .           ICLR_TIMEDAY=22, 
     .           ICLR_MIN=5,
     .           ICLR_MAX=6,
     .           ICLR_DEPTH=7)
      parameter (ICNT_SPVAL=8,
     .           ICNT_DX=9,
     .           ICNT_DY=10,
     .           ICNT_TIME=11, 
     .           ICNT_MIN=12,
     .           ICNT_MAX=13,
     .           ICNT_DEPTH=14)
      parameter (IVEC_SPVAL=15,
     .           IVEC_DX=16,
     .           IVEC_DY=17,
     .           IVEC_TIME=18, 
     .           IVEC_MIN=19,
     .           IVEC_MAX=20,
     .           IVEC_DEPTH=21)
      parameter (ICLR_STR1=1,
     .           ICLR_STR2=2,
     .           ICLR_STR3=3,
     .           ICLR_STR4=4,
     .           ICNT_STR1=5,
     .           ICNT_STR2=6,
     .           ICNT_STR3=7,
     .           ICNT_STR4=8,
     .           IVEC_STR1=9,
     .           IVEC_STR2=10,
     .           IVEC_STR3=11,
     .           IVEC_STR4=12)
	  parameter ( I_DATE=13,
     .           CLR_ICDATE=14,
     .           CNT_ICDATE=15,
     .           VEC_ICDATE=16,
     .           CLIPPER_DATE=17)


