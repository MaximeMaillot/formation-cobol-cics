       01  MAP0I.
           02  FILLER PIC X(12).
           02  IDENTL    COMP  PIC  S9(4).
           02  IDENTF    PICTURE X.
           02  FILLER REDEFINES IDENTF.
             03 IDENTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  IDENTI  PIC X(18).
           02  JOURL    COMP  PIC  S9(4).
           02  JOURF    PICTURE X.
           02  FILLER REDEFINES JOURF.
             03 JOURA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  JOURI  PIC X(10).
           02  DATEL    COMP  PIC  S9(4).
           02  DATEF    PICTURE X.
           02  FILLER REDEFINES DATEF.
             03 DATEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DATEI  PIC X(10).
           02  HEUREL    COMP  PIC  S9(4).
           02  HEUREF    PICTURE X.
           02  FILLER REDEFINES HEUREF.
             03 HEUREA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  HEUREI  PIC X(8).
           02  CHOIXL    COMP  PIC  S9(4).
           02  CHOIXF    PICTURE X.
           02  FILLER REDEFINES CHOIXF.
             03 CHOIXA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHOIXI  PIC X(1).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSI  PIC X(79).
       01  MAP0O REDEFINES MAP0I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  IDENTC    PICTURE X.
           02  IDENTH    PICTURE X.
           02  IDENTO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  JOURC    PICTURE X.
           02  JOURH    PICTURE X.
           02  JOURO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  DATEC    PICTURE X.
           02  DATEH    PICTURE X.
           02  DATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  HEUREC    PICTURE X.
           02  HEUREH    PICTURE X.
           02  HEUREO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  CHOIXC    PICTURE X.
           02  CHOIXH    PICTURE X.
           02  CHOIXO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSH    PICTURE X.
           02  MESSO  PIC X(79).
