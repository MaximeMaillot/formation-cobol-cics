      *----------------------------------------------------------*
      *    DESCRIPTION DE L'ENREGISTREMENT DU FICHIER FSTAGGG    *
      *     (FICHIER VSAM KSDS: LONG ENGT = 400 (LONG CLE = 4)   *
      *----------------------------------------------------------*

       01  E-STAGIAIRE.
      *--------------------------------------  NUMERO DU STAGIAIRE
           02 E-NUMERO             PIC X(4).
      *--------------------------------------  NOM
           02 E-NOM                PIC X(20).
      *--------------------------------------  PRENOM
           02 E-PRENOM             PIC X(20).
      *--------------------------------------  1ERE LIGNE ADRESSE
           02 E-ADR1               PIC X(30).
      *--------------------------------------  2EME LIGNE ADRESSE
           02 E-ADR2               PIC X(30).
      *--------------------------------------  CODE POSTAL
           02 E-CODEP              PIC X(5).
      *--------------------------------------  VILLE
           02 E-VILLE              PIC X(25).
      *--------------------------------------  TELEPHONE DOMICILE
           02 E-TELDOM             PIC X(10).
      *--------------------------------------  TELEPHONE PORTABLE
           02 E-TELPOR             PIC X(10).
      *--------------------------------------  DATE DE NAISSANCE
           02 E-DATE-NAISS         PIC 9(8).
      *--------------------------------------  DIPLOME
           02 E-DIPLOME            PIC X(30).
      *--------------------------------------  COMMERCIAL
           02 E-COMMERCIAL         PIC X(30).
      *--------------------------------------  DATE DEBUT DE PRESTATION
           02 E-DATE-DEB-PRESTA    PIC 9(8).
      *--------------------------------------  DATE FIN  DE PRESTATION
           02 E-DATE-FIN-PRESTA    PIC 9(8).
      *--------------------------------------  NOM DU CLIENT
           02 E-NOM-CLIENT         PIC X(30).
      *--------------------------------------  ADRESSE DU CLIENT
           02 E-ADR-CLIENT         PIC X(30).
      *--------------------------------------  SALAIRE DU STAGIAIRE
           02 E-SAL-STAG           PIC S9(5)V99 PACKED-DECIMAL.
      *--------------------------------------  NUMERO DU STAGIAIRE
           02 FILLER               PIC X(98).
