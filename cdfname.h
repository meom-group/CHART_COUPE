        INTEGER jpstand
        PARAMETER (jpstand = 14)
        CHARACTER*80 clat, clon, cdepth, ctime, cdim
        CHARACTER*80 ctitle ,ccom1, ccom2, ccom3, ccom4
        CHARACTER*80 cmiss, clongname, cunits,clonxy,clatxy
        CHARACTER*80 cstandlist(jpstand), ctmpp(jpstand)
C
        COMMON /cdfname/  cstandlist, ctmpp
C
        EQUIVALENCE ( cstandlist(1 ),clon      )
        EQUIVALENCE ( cstandlist(2 ),clat      )
        EQUIVALENCE ( cstandlist(3 ),cdepth    )
        EQUIVALENCE ( cstandlist(4 ),ctime     )
        EQUIVALENCE ( cstandlist(5 ),cdim      )
        EQUIVALENCE ( cstandlist(6 ),ctitle    )
        EQUIVALENCE ( cstandlist(7 ),ccom2     )
        EQUIVALENCE ( cstandlist(8 ),ccom3     )
        EQUIVALENCE ( cstandlist(9 ),ccom4     )
        EQUIVALENCE ( cstandlist(10),cmiss     )
        EQUIVALENCE ( cstandlist(11),clongname )
        EQUIVALENCE ( cstandlist(12),cunits    )
        EQUIVALENCE ( cstandlist(13),clonxy    )
        EQUIVALENCE ( cstandlist(14),clatxy    )
