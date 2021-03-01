      *----------------------------------------------------------*
      *    DESCRIPTION DE L'ENREGISTREMENT DU FICHIER (TDQ :INFO)*
      *    LRECL=80  (//SDJTDINF DD DSN=SDJ.INFO.CICS)           *
      *             SDJ.CICS.COPY(CTDINFO)                       *
      *             (COPY  POUR  PROGRAMME  CICS)                *
      *----------------------------------------------------------*
       01  E-TD.
      *-------------------------------------------  NOM DU CICS
           05 CICS-TD               PIC X(4).
      *-------------------------------------------  USERID DU CICS
           05 USERID-CICS-TD        PIC X(8).
      *-------------------------------------------  USERID DU USER
           05 USERID-USER-TD        PIC X(8).
      *-------------------------------------------  NOM DU TERMINAL
           05 TERMINAL-TD           PIC X(4).
      *-------------------------------------------  DATE DU JOUR
           05 DATE-TD               PIC X(10).
      *-------------------------------------------  HEURE DE CREATION
           05 HEURE-TD              PIC X(8).
      *-------------------------------------------  VSAM/DB2
      *                                             V : VSAM
      *                                             D : DB2
           05 TYPE-DONNEES          PIC X.
      *-------------------------------------------  NOM RESSOURCE LUE
           05 RESSOURCE-TD          PIC X(8).
      *-------------------------------------------  NOM  DE TD
      *                                             C : CREATION
      *                                             M : MISE A JOUR
      *                                             S : SUPPRESSION
           05 FONCTION-TD           PIC X.
      *-------------------------------------------  NUMERO STAGIAIRE
           05 NUM-STAG-TD           PIC X(4).
      *-------- (F : POUR FACILITER L'INITIALIZE -  RESTE DU RECORD
           05 F                     PIC X(24).
