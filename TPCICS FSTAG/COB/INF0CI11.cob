      *======================================================*
      *   SQUELETTE DE PROG CICS EN PSEUDO CONVERSATIONNEL   *
      *======================================================*
      * MODIFIER INFNCG, N : NO TP, G : NO GROUPE            *
      *                      --------------------            *
      *======================================================*
      *   I D E N T I F I C A T I O N     D I V I S I O N    *
      *======================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF0CI11.

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

       01 MON-PROG                PIC X(8) VALUE 'INF0CI11'.
       01 MA-MAP                  PIC X(8) VALUE 'MAP0C11 '.
       01 MA-TRX                  PIC X(4) VALUE 'T0CB'.
      *------------------------------------------------------*
      *   DESCRIPTION   DE   LA   MAP                        *
      *------------------------------------------------------*
       COPY MAP0C11.

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
       
       01 weekday-list.
         COPY WEEKDAY.
       01 FILLER REDEFINES weekday-list.
         05 weekday-name              PIC X(10) occurs 7.

       01 interval                    pic S9(15) comp-3.
       01 num-j                       PIC S9(5) comp.
       01 ident                       PIC X(17).

       01 posCursor                   PIC S9(4) COMP.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05 LK-COMMAREA             PIC X(100).

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.
       00000-INIT-PROGRAMME.
      *---------------------*
            PERFORM  10000-DEBUT-PROGRAMME
            PERFORM  20000-TRAIT-PROGRAMME
            GOBACK.


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
           MOVE LOW-VALUE TO MAP0O

      *  SI TRAITEMENT PARTICULIER AVANT AFFICHAGE
           PERFORM  21100-TRAIT-SPECIFIQUE
           .
       21100-TRAIT-SPECIFIQUE.
           CONTINUE
           .


       22000-TRAIT-ENVOI.
      *-----------------*
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
            '/map0c11' delimited by size
            into idento
           END-STRING
           
           MOVE weekday-name(num-j) TO jouro
       
           move -1 to choixl
           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              EXEC CICS SEND MAP    ('MAP0')
                             MAPSET (MA-MAP)
                             ERASE
              END-EXEC
           ELSE
              EXEC CICS SEND MAP    ('MAP0')
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
              WHEN DFHPF12
                   PERFORM  23200-TRAIT-FIN
              WHEN DFHCLEAR
                   PERFORM  23200-TRAIT-FIN
              WHEN OTHER
                   PERFORM  90000-ERR-TOUCHE
           END-EVALUATE
           .

       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP  ('MAP0')
                             MAPSET('MAP0C11')
                             RESP (C-R)
           END-EXEC

           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                   CONTINUE
              WHEN DFHRESP(MAPFAIL)
                   perform 23120-handle-cursor
              WHEN OTHER
                   PERFORM 91000-ERREUR-CICS
           END-EVALUATE

           EVALUATE choixi
               WHEN 1
                 MOVE 'INF1CI11' TO PROG-SUIVANT
                 perform 23110-PROG-SUIVANT
               WHEN 2
                 MOVE 'INF2CI11' TO PROG-SUIVANT
                 perform 23110-PROG-SUIVANT
               WHEN 3
                 MOVE 'Choix 3' to messo
                 PERFORM 22000-TRAIT-ENVOI
               WHEN 4
                 MOVE 'Choix 4' to messo
                 PERFORM 22000-TRAIT-ENVOI
               WHEN 5
                 MOVE 'Choix 5' to messo
                 PERFORM 22000-TRAIT-ENVOI 
               WHEN OTHER                 
                 perform 23120-handle-cursor
           END-EVALUATE
           .

       23110-PROG-SUIVANT.
      *------------------*
           EXEC CICS XCTL  PROGRAM  (PROG-SUIVANT)
                           COMMAREA (WS-COMMAREA)
                           LENGTH   (LENGTH OF WS-COMMAREA)
           END-EXEC
           .
       
       23120-handle-cursor.
           IF (choixi NOT = SPACE AND LOW-VALUE)
               MOVE -1 to choixl
               MOVE 'H' to choixa
               MOVE 'Choix invalide' to messo
               PERFORM 22000-TRAIT-ENVOI
           ELSE
               COMPUTE posCursor = (EIBCPOSN / 80) + 1
               EVALUATE posCursor 
                 WHEN 5
                    MOVE 'INF1CI11' TO PROG-SUIVANT
                    perform 23110-PROG-SUIVANT
                 WHEN 7
                    MOVE 'INF2CI11' TO PROG-SUIVANT
                    perform 23110-PROG-SUIVANT
                 WHEN 9
                    MOVE 'Cursor on 3' to messo
                    perform 22000-TRAIT-ENVOI
                 WHEN 11
                    MOVE 'Cursor on 4' to messo
                    perform 22000-TRAIT-ENVOI
                 WHEN 13
                    MOVE 'Cursor on 5' to messo
                    perform 22000-TRAIT-ENVOI
                 WHEN OTHER
                    MOVE -1 to choixl
                    MOVE 'Veuillez saisir un choix'
                     to messo
                    perform 22000-TRAIT-ENVOI
               END-EVALUATE
           END-IF 
           .

       23200-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       90000-ERR-TOUCHE.
      *----------------*
           MOVE 'TOUCHE DE FONCTION INVALIDE' TO messo
           move -1 to choixl
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
