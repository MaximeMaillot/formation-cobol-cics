      *======================================================*
      *   I D E N T I F I C A T I O N     D I V I S I O N    *
      *======================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF2CI11.

      *======================================================*
      *   E N V I R O N M E N T       D I V I S I O N        *
      *======================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

      *======================================================*
      *           D A T A         D I V I S I O N            *
      *======================================================*
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *------------------------------------------------------*
      *   DESCRIPTION DETAILLEE DE LA ZONE DE COMMUNICATION  *
      *------------------------------------------------------*
       01  WS-COMMAREA.
           05 PROG-PRECEDENT      PIC X(8).
           05 PROG-COURANT        PIC X(8).
           05 PROG-SUIVANT        PIC X(8).
           05 FILLER              PIC X(76).

       01 C-R                     PIC S9(8) COMP.

       01 MON-PROG                PIC X(8) VALUE 'INF2CI11'.
       01 MA-MAP                  PIC X(8) VALUE 'MAP2C11 '.
       01 MA-TRX                  PIC X(4) VALUE 'T2CB'.
      *------------------------------------------------------*
      *   DESCRIPTION   DE   LA   MAP                        *
      *------------------------------------------------------*
       COPY MAP2C11.

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*
       01  MESSAGE-TXT            PIC X(79).

      *------------------------------------------------------*
      *   DESCRIPTION   DES  TOUCHES   FONCTIONS             *
      *------------------------------------------------------*
       COPY DFHAID.

      *------------------------------------------------------*
      *   DESCRIPTION   DES  ATTRIBUTS                       *
      *------------------------------------------------------*
       COPY DFHBMSCA.

      *======================================================*
      *          L I N K A G E     S E C T I O N             *
      *======================================================*
       
      * Liste des jours de la semaine 
       01 weekday-list.
         COPY WEEKDAY.
       01 FILLER REDEFINES weekday-list.
         05 weekday-name                 PIC X(10) occurs 7.
      
      * Structure d'un stagiaire
       COPY CSTAG.


      *  
       01 interval                       pic S9(15) comp-3.
       01 num-j                          PIC S9(5) comp.
       01 ident                          PIC X(17).

      * --------- Nom des sous-programmes
       01 pgm-name.
         02 pgm-accfile                  PIC X(8) value 'ACCFILE '.

      * ---------- Paramètre pour sous-programme ACCFILE
       01 accfile-param.
         05 file-name                    PIC X(8).
         05 code-fonction                PIC 99.
           88 c-read               value 1.
           88 c-write              value 2.
           88 c-rewrite            value 3.
           88 c-delete             value 4.
         05 code-retour                  PIC 99.
           88 cr-ok                value 0.
           88 cr-key-exists        value 1.
           88 cr-duplicate         value 2.
           88 cr-file-error        value 3.
           88 cr-eof               value 4.
           88 cr-unsupported       value 99.
         05 enrgmt                       PIC X(400).

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05 LK-COMMAREA                PIC X(100).

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.
       00000-INIT-PROGRAMME.
      *---------------------*
            PERFORM  10000-DEBUT-PROGRAMME
            PERFORM  20000-TRAIT-PROGRAMME
            GOBACK
           .


       10000-DEBUT-PROGRAMME.
      *---------------------*
      *   TEST DE PREMIERE ENTREE DANS LE PROGRAMME
      *   SI NON PROGRAMME  MENU ===>  ARRET  (ACCES INTERDIT)
           IF EIBCALEN = 0
              INITIALIZE      WS-COMMAREA
           ELSE
              MOVE LK-COMMAREA  TO WS-COMMAREA
           END-IF

           MOVE  PROG-COURANT     TO  PROG-PRECEDENT
           MOVE  MON-PROG         TO  PROG-COURANT
           .

       20000-TRAIT-PROGRAMME.
      *---------------------*
           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              PERFORM  21000-TRAIT-CHARGEMENT
              PERFORM  22000-TRAIT-ENVOI
           ELSE
              PERFORM  23000-TRAIT-RECEPTION
           END-IF
           .
      *********************************************************

       21000-TRAIT-CHARGEMENT.
      *----------------------*
      *  CHARGEMENT DE LA MAP AVANT AFFICHAGE
           MOVE LOW-VALUE TO MAP2O

      *  SI TRAITEMENT PARTICULIER AVANT AFFICHAGE
           PERFORM  21100-TRAIT-SPECIFIQUE
           .

       21100-TRAIT-SPECIFIQUE.
           continue
           .

       29000-FORMATE-HEADER.
           EXEC CICS 
                ASKTIME
                ABSTIME(interval)
           END-EXEC  
           EXEC CICS 
                FORMATTIME
                ABSTIME(interval)
                DDMMYYYY(dateo)
                datesep('/')
                time(heureo)
                timesep(':')
                dayofweek(num-j)
           END-EXEC

           STRING
            eibtrnid delimited by size
            '/' delimited by size
            eibtrmid delimited by size
            '/map2c11' delimited by size
            into idento
           END-STRING
           
           MOVE weekday-name(num-j) TO jouro
           .
      
       22000-TRAIT-ENVOI.
      *-----------------*
           perform 29000-FORMATE-HEADER 
           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              EXEC CICS SEND MAP    ('MAP2')
                             MAPSET (MA-MAP)
                             ERASE
              END-EXEC
           ELSE
              EXEC CICS SEND MAP    ('MAP2')
                             MAPSET (MA-MAP)
                             CURSOR
              END-EXEC
           END-IF
           MOVE PROG-COURANT TO PROG-SUIVANT

           EXEC CICS RETURN TRANSID  (MA-TRX)
                            COMMAREA (WS-COMMAREA)
                            LENGTH   (LENGTH OF WS-COMMAREA)
           END-EXEC
           .

       23000-TRAIT-RECEPTION.
      *---------------------*
           EVALUATE EIBAID
              WHEN DFHENTER
                   PERFORM  23100-TRAIT-ENTER
              WHEN DFHCLEAR
                   PERFORM  23200-TRAIT-FIN
              WHEN DFHPF3
                   MOVE 'INF0CI11' TO PROG-SUIVANT
                   perform 23110-PROG-SUIVANT
              WHEN DFHPF12
                   PERFORM  23200-TRAIT-FIN
              WHEN OTHER
                   PERFORM  90000-ERR-TOUCHE
           END-EVALUATE
           .

       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP   ('MAP2')
                             MAPSET(MA-MAP)
                             RESP  (C-R)
           END-EXEC

           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                   CONTINUE
              WHEN DFHRESP(MAPFAIL)
                   MOVE 'Champs obligatoires non rempli' to messo
                   PERFORM 22000-TRAIT-ENVOI
              WHEN OTHER
                   PERFORM 91000-ERREUR-CICS
           END-EVALUATE

           perform 23200-CHECK-DATA

           perform 23300-READ-STAGIAIRE
           .

       23200-CHECK-DATA.
      * -------------------------------------------------------*
      * ------------------ CHAMPS OBLIGATOIRES ----------------*
      * -------------------------------------------------------*
      *    Check le numero stagiaire
           IF NUMSTAGI = SPACE OR LOW-VALUE
             MOVE -1 to numstagl
             MOVE 'Veuillez saisir un numero' to messo
             perform 22000-TRAIT-ENVOI
           END-IF
           IF NUMSTAGI < 1000 OR NUMSTAGI > 5000
             MOVE -1 to numstagl
             MOVE 'Numero invalide' to messo
             perform 22000-TRAIT-ENVOI
           END-IF
           .    

       23300-READ-STAGIAIRE.
      *    Ecrit le stagiaire
           MOVE NUMSTAGI TO E-NUMERO

           EXEC CICS READ
                     FILE('FSTAG11 ')
                     RIDFLD(E-NUMERO)
                     INTO(E-STAGIAIRE)
                     RESP(C-R)
           END-EXEC.

           EVALUATE C-R
             WHEN DFHRESP(NORMAL)
               MOVE 'Read' to messo
      *        Unprot / mdt
               MOVE 'A' TO NOMA PRENOMA ADR1A ADR2A CODEPA VILLEA 
                           TELDOMA TELMOBA DATENA
      *        Askip
               MOVE '0' TO CNOMA CPRENOMA CADR1A CCODEPA CVILLEA 
                           CTELDOMA CTELMOBA CDATENA

              MOVE E-NOM        TO NOMO
              MOVE E-PRENOM     TO PRENOMO
              MOVE E-ADR1       TO ADR1O
              MOVE E-ADR2       TO ADR2O
              MOVE E-CODEP      TO CODEPO
              MOVE E-VILLE      TO VILLEO
              MOVE E-TELDOM     TO TELDOMO
              MOVE E-TELPOR     TO TELMOBO
              MOVE E-DATE-NAISS TO DATENO
             WHEN OTHER
               MOVE LOW-VALUE   TO MAP2O
               
               MOVE 'Echec'     to messo
           END-EVALUATE
           
           MOVE -1 to numstagl

           PERFORM 22000-TRAIT-ENVOI
           .

       23110-PROG-SUIVANT.
      *------------------*
           EXEC CICS XCTL  PROGRAM  (PROG-SUIVANT)
                           COMMAREA (WS-COMMAREA)
                           LENGTH   (LENGTH OF WS-COMMAREA)
           END-EXEC
           .

       23200-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       90000-ERR-TOUCHE.
      *----------------*
           MOVE 'TOUCHE DE FONCTION INVALIDE' TO messo
           move -1 to NUMSTAGL
           PERFORM  22000-TRAIT-ENVOI
           .

       91000-ERREUR-CICS.
      *-------------------*
           MOVE 'ERREUR CICS !!!  FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       99000-FIN-CICS.
      *--------------*
           EXEC CICS SEND FROM   (MESSAGE-TXT)
                          LENGTH (LENGTH OF MESSAGE-TXT)
                          ERASE
           END-EXEC

           EXEC CICS RETURN END-EXEC
           .
