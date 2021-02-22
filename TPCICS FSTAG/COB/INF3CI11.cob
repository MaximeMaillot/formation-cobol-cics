      *======================================================*
      *   I D E N T I F I C A T I O N     D I V I S I O N    *
      *======================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF3CI11.

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
           05 IS-LOCKED           PIC 9.
             88 IS-LOCKED-FALSE value 0.
             88 IS-LOCKED-TRUE  value 1.
           05 NUM-STAGI-PREC      PIC 9(4).
           05 FILLER              PIC X(71).

       01 C-R                     PIC S9(8) COMP.

       01 MON-PROG                PIC X(8) VALUE 'INF3CI11'.
       01 MA-MAP                  PIC X(8) VALUE 'MAP3C11 '.
       01 MA-TRX                  PIC X(4) VALUE 'T3CB'.
      *------------------------------------------------------*
      *   DESCRIPTION   DE   LA   MAP                        *
      *------------------------------------------------------*
       COPY MAP3C11.

       01 NUM-TEL-NUMERIC PIC 99.

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
         02 pgm-validdat                 PIC X(8) value 'VALIDDAT'.
         02 pgm-accfile                  PIC X(8) value 'ACCFILE '.

      * --------- Paramètre pour sous-programme VALIDDAT 
       01 validdat-param.
         02 date-to-validate             PIC X(8).
         02 CR-VALIDDAT                  PIC 9 value 0.
          88 cr-validdat-false                 value 0.
          88 cr-validdat-ok                    value 1.

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
           MOVE LOW-VALUE TO MAP3O

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
            '/map3c11' delimited by size
            into idento
           END-STRING
           
           MOVE weekday-name(num-j) TO jouro
           .
      
       22000-TRAIT-ENVOI.
      *-----------------*
           perform 29000-FORMATE-HEADER 
           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              EXEC CICS SEND MAP    ('MAP3')
                             MAPSET (MA-MAP)
                             ERASE
              END-EXEC
           ELSE
              EXEC CICS SEND MAP    ('MAP3')
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
                   SET IS-LOCKED-FALSE TO TRUE
                   INITIALIZE NUM-STAGI-PREC

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
           EXEC CICS RECEIVE MAP   ('MAP3')
                             MAPSET(MA-MAP)
                             RESP  (C-R)
           END-EXEC
           
           INITIALIZE messo

           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                   CONTINUE
              WHEN DFHRESP(MAPFAIL)
                   MOVE 'Champs obligatoires non rempli' to messo
                   PERFORM 22000-TRAIT-ENVOI
              WHEN OTHER
                   PERFORM 91000-ERREUR-CICS
           END-EVALUATE
           
           perform CHECK-NUMSTAGI

           if (NUM-STAGI-PREC NOT = NUMSTAGI)
              perform GET-STAGIAIRE
           END-IF

           if ( IS-LOCKED-TRUE )           
              perform 23200-CHECK-DATA
           END-IF
           
           perform 23400-CHECK-CONFIRMATION
           
           perform 23300-MODIFY-STAGIAIRE
           .
       
       GET-STAGIAIRE.
           MOVE NUMSTAGI TO E-NUMERO

           EXEC CICS READ
                     FILE('FSTAG11 ')
                     RIDFLD(E-NUMERO)
                     INTO(E-STAGIAIRE)
                     UPDATE
                     RESP(C-R)
           END-EXEC.

           EVALUATE C-R
             WHEN DFHRESP(NORMAL)

              MOVE E-NOM        TO NOMO
              MOVE E-PRENOM     TO PRENOMO
              MOVE E-ADR1       TO ADR1O
              MOVE E-ADR2       TO ADR2O
              MOVE E-CODEP      TO CODEPO
              MOVE E-VILLE      TO VILLEO
              MOVE E-TELDOM     TO TELDOMO
              MOVE E-TELPOR     TO TELMOBO
              MOVE E-DATE-NAISS TO DATENO

              MOVE E-NUMERO to NUM-STAGI-PREC
              perform SHOW-DATA
              
             WHEN OTHER
               MOVE LOW-VALUE   TO MAP3O
               
               MOVE 'Pas de stagiaire trouve'     to messo
           END-EVALUATE
           
           MOVE -1 to numstagl

           PERFORM 22000-TRAIT-ENVOI
           .

       CHECK-NUMSTAGI.
      *    Check le numero stagiaire
           IF NUMSTAGI = SPACE OR LOW-VALUE
             MOVE -1 to numstagl
             MOVE 'Veuillez saisir un numero' to messo
             MOVE NUMSTAGI to NUM-STAGI-PREC
             perform 22000-TRAIT-ENVOI
           END-IF
           IF NUMSTAGI IS NOT NUMERIC OR 
            NUMSTAGI < 1000 OR NUMSTAGI > 5000
             MOVE -1 to numstagl
             MOVE 'Numero invalide' to messo
             MOVE NUMSTAGI to NUM-STAGI-PREC
             perform 22000-TRAIT-ENVOI
           END-IF
           .

       SHOW-DATA.
      *        Unprot / mdt
               MOVE 'A' TO NOMA PRENOMA ADR1A ADR2A CODEPA VILLEA 
                           TELDOMA TELMOBA DATENA
      *        Askip / mdt
               MOVE '1' TO CNOMA CPRENOMA CADR1A CCODEPA CVILLEA 
                           CTELDOMA CTELMOBA CDATENA
           .

       23200-CHECK-DATA.
           perform SHOW-DATA
      * -------------------------------------------------------*
      * ------------------ CHAMPS OBLIGATOIRES ----------------*
      * -------------------------------------------------------*     
      *    Check Nom et Prenom
           IF NOMI = SPACE OR LOW-VALUE
              MOVE -1 to noml
              MOVE 'Veuillez saisir un nom' to messo
              perform 22000-TRAIT-ENVOI
           END-IF
           
           IF PRENOMI = SPACE OR LOW-VALUE 
              MOVE -1 to prenoml
              MOVE 'Veuillez saisir un prenom' to messo
              perform 22000-TRAIT-ENVOI
           END-IF 
      
      *    Check la date           
           IF DATENI = SPACE OR LOW-VALUE 
              MOVE -1 to datenl
              MOVE 'Veuillez saisir une date de naissance' to messo
              perform 22000-TRAIT-ENVOI
           END-IF

      * ---------- Appel sous programme VALIDDAT
           MOVE DATENI to date-to-validate
           CALL pgm-validdat using validdat-param

           IF cr-validdat-false
              MOVE -1 to datenl
              MOVE 'Date de naissance non valide' to messo
              perform 22000-TRAIT-ENVOI
           END-IF
      
      * -------------------------------------------------------*
      * ---------------- CHAMPS NON OBLIGATOIRES --------------*
      * -------------------------------------------------------*
      *    Check les champs d'adresses
           IF ((ADR1I   NOT = SPACE AND LOW-VALUE) OR
               (ADR2I   NOT = SPACE AND LOW-VALUE) OR
               (VILLEI  NOT = SPACE AND LOW-VALUE) OR
               (CODEPI  NOT = SPACE AND LOW-VALUE))
             IF (ADR1I  = SPACE OR LOW-VALUE)
               MOVE -1 to adr1l
               MOVE 'Rentrez tous les champs d''adresses' to messo
               perform 22000-TRAIT-ENVOI
             END-IF

             IF (CODEPI = SPACE OR LOW-VALUE)
               MOVE -1 to codepl
               MOVE 'Rentrez tous les champs d''adresses' to messo
               perform 22000-TRAIT-ENVOI
             END-IF
             IF (CODEPI IS NOT NUMERIC)
              MOVE -1 to codepl
              MOVE 'Code postal non numerique' to messo
              perform 22000-TRAIT-ENVOI
             END-IF

             IF (VILLEI = SPACE OR LOW-VALUE)
               MOVE -1 to villel
               MOVE 'Rentrez tous les champs d''adresses' to messo
               perform 22000-TRAIT-ENVOI
             END-IF
           END-IF


           IF TELDOMI NOT = SPACE AND LOW-VALUE
              IF (TELDOMI IS NOT NUMERIC)
                 MOVE -1 to TELDOML 
                 MOVE 'Numero telephone domicile invalide' to messo
                 perform 22000-TRAIT-ENVOI
              END-IF            
              MOVE TELDOMI(1:2) TO NUM-TEL-NUMERIC
              IF (NUM-TEL-NUMERIC  < 1 OR NUM-TEL-NUMERIC  > 5)
                 MOVE -1 to TELDOML 
                 MOVE 'Numero telephone domicile invalide' to messo
                 perform 22000-TRAIT-ENVOI
              END-IF
           END-IF
           
           IF TELMOBI NOT = SPACE AND LOW-VALUE 
              MOVE TELMOBI(1:2) TO NUM-TEL-NUMERIC
              IF (NUM-TEL-NUMERIC NOT = 6)
                 MOVE -1 to TELMOBL 
                 MOVE 'Numero telephone mobile invalide' to messo
                 perform 22000-TRAIT-ENVOI
              END-IF
           END-IF
           .    

       23300-MODIFY-STAGIAIRE.
      *    Ecrit le stagiaire           
           EXEC CICS READ
                     FILE('FSTAG11 ')
                     RIDFLD(E-NUMERO)
                     INTO(E-STAGIAIRE)
                     UPDATE
                     RESP(C-R)
           END-EXEC.

           MOVE NUMSTAGI  TO E-NUMERO
           MOVE NOMI      TO E-NOM
           MOVE PRENOMI   TO E-PRENOM
           MOVE ADR1I     TO E-ADR1
           MOVE ADR2I     TO E-ADR2
           MOVE CODEPI    TO E-CODEP
           MOVE VILLEI    TO E-VILLE
           MOVE TELDOMI   TO E-TELDOM
           MOVE TELMOBI   TO E-TELPOR
           MOVE DATENI    TO E-DATE-NAISS

           EXEC CICS REWRITE
                     FILE('FSTAG11 ')
                     FROM(E-STAGIAIRE)
                     RESP(C-R)
           END-EXEC

           EVALUATE C-R
             WHEN DFHRESP(NORMAL)
               MOVE 'Stagiaire modifie avec succes' to messo
             WHEN OTHER
               MOVE 'Echec de la modification' to messo
           END-EVALUATE
           
           move -1     to numstagl

           SET IS-LOCKED-FALSE TO TRUE

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
       
       23400-CHECK-CONFIRMATION.
           perform SHOW-DATA
           SET IS-LOCKED-TRUE TO TRUE 
           
           IF confi = SPACE OR LOW-VALUE 
      *       Prot / Highlight / askip / no mdt     
              MOVE '8'    TO lconfa
      *       Unprot / Highlight / no mdt        
              MOVE 'H'    TO  confa
              MOVE -1     TO confl

              MOVE '9'    TO NUMSTAGA NOMA PRENOMA ADR1A ADR2A CODEPA
                             VILLEA TELDOMA TELMOBA DATENA

              MOVE 'Confirmez votre choix' to messo

              MOVE SPACE  TO CONFO

              PERFORM 22000-TRAIT-ENVOI
           END-IF

           IF confi = 'N'
              SET IS-LOCKED-FALSE TO TRUE 
              MOVE 'Annulation' to messo
              move -1           to numstagl

              MOVE SPACE TO CONFO

              PERFORM 22000-TRAIT-ENVOI
           END-IF 

           IF confi NOT = 'O'
      *       Askip / Highlight / no mdt    
              MOVE '8'    TO lconfa
      *       Unprot / Highlight / no mdt        
              MOVE 'H'    TO confa
              MOVE -1     TO confl
              
      *       Askip / Highlight / mdt        
              MOVE '9'    TO NUMSTAGA NOMA PRENOMA ADR1A ADR2A CODEPA 
                             VILLEA TELDOMA TELMOBA DATENA

              MOVE 'Mauvais choix' TO messo

              MOVE SPACE  TO CONFO

              PERFORM 22000-TRAIT-ENVOI
           END-IF

           MOVE SPACE TO confo
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
