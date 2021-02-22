       01  MAP1I.
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
           02  NUMSTAGL    COMP  PIC  S9(4).
           02  NUMSTAGF    PICTURE X.
           02  FILLER REDEFINES NUMSTAGF.
             03 NUMSTAGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NUMSTAGI  PIC X(4).
           02  NOML    COMP  PIC  S9(4).
           02  NOMF    PICTURE X.
           02  FILLER REDEFINES NOMF.
             03 NOMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NOMI  PIC X(20).
           02  PRENOML    COMP  PIC  S9(4).
           02  PRENOMF    PICTURE X.
           02  FILLER REDEFINES PRENOMF.
             03 PRENOMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PRENOMI  PIC X(15).
           02  ADR1L    COMP  PIC  S9(4).
           02  ADR1F    PICTURE X.
           02  FILLER REDEFINES ADR1F.
             03 ADR1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADR1I  PIC X(30).
           02  ADR2L    COMP  PIC  S9(4).
           02  ADR2F    PICTURE X.
           02  FILLER REDEFINES ADR2F.
             03 ADR2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADR2I  PIC X(30).
           02  CODEPL    COMP  PIC  S9(4).
           02  CODEPF    PICTURE X.
           02  FILLER REDEFINES CODEPF.
             03 CODEPA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CODEPI  PIC X(5).
           02  VILLEL    COMP  PIC  S9(4).
           02  VILLEF    PICTURE X.
           02  FILLER REDEFINES VILLEF.
             03 VILLEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  VILLEI  PIC X(25).
           02  TELDOML    COMP  PIC  S9(4).
           02  TELDOMF    PICTURE X.
           02  FILLER REDEFINES TELDOMF.
             03 TELDOMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TELDOMI  PIC X(10).
           02  TELMOBL    COMP  PIC  S9(4).
           02  TELMOBF    PICTURE X.
           02  FILLER REDEFINES TELMOBF.
             03 TELMOBA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TELMOBI  PIC X(10).
           02  DATENL    COMP  PIC  S9(4).
           02  DATENF    PICTURE X.
           02  FILLER REDEFINES DATENF.
             03 DATENA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DATENI  PIC X(8).
           02  LCONFL    COMP  PIC  S9(4).
           02  LCONFF    PICTURE X.
           02  FILLER REDEFINES LCONFF.
             03 LCONFA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LCONFI  PIC X(19).
           02  CONFL    COMP  PIC  S9(4).
           02  CONFF    PICTURE X.
           02  FILLER REDEFINES CONFF.
             03 CONFA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CONFI  PIC X(1).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSI  PIC X(79).
       01  MAP1O REDEFINES MAP1I.
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
           02  NUMSTAGC    PICTURE X.
           02  NUMSTAGH    PICTURE X.
           02  NUMSTAGO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  NOMC    PICTURE X.
           02  NOMH    PICTURE X.
           02  NOMO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  PRENOMC    PICTURE X.
           02  PRENOMH    PICTURE X.
           02  PRENOMO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ADR1C    PICTURE X.
           02  ADR1H    PICTURE X.
           02  ADR1O  PIC X(30).
           02  FILLER PICTURE X(3).
           02  ADR2C    PICTURE X.
           02  ADR2H    PICTURE X.
           02  ADR2O  PIC X(30).
           02  FILLER PICTURE X(3).
           02  CODEPC    PICTURE X.
           02  CODEPH    PICTURE X.
           02  CODEPO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  VILLEC    PICTURE X.
           02  VILLEH    PICTURE X.
           02  VILLEO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  TELDOMC    PICTURE X.
           02  TELDOMH    PICTURE X.
           02  TELDOMO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  TELMOBC    PICTURE X.
           02  TELMOBH    PICTURE X.
           02  TELMOBO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  DATENC    PICTURE X.
           02  DATENH    PICTURE X.
           02  DATENO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  LCONFC    PICTURE X.
           02  LCONFH    PICTURE X.
           02  LCONFO  PIC X(19).
           02  FILLER PICTURE X(3).
           02  CONFC    PICTURE X.
           02  CONFH    PICTURE X.
           02  CONFO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSH    PICTURE X.
           02  MESSO  PIC X(79).
