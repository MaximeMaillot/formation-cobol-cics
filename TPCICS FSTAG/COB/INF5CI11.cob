      *======================================================*
      *   SQUELETTE DE PROG CICS EN PSEUDO CONVERSATIONNEL   *
      *======================================================*
      * MODIFIER INFNCG, N : NO TP, G : NO GROUPE            *
      *                      --------------------            *
      *======================================================*
      *   I D E N T I F I C A T I O N     D I V I S I O N    *
      *======================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF5CI11.

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
           05 CURRENT-KEY         PIC 9(4).
           05 NUM-PAGE            PIC S9(4) COMP.
           05 STAGIAIRE-EOF       PIC 9.
           05 ZOOM-BOOLEAN        PIC 9.
             88 IS-ZOOM      value 1.
             88 IS-NOT-ZOOM  value 0.
           05 ZOOM-KEY            PIC X(4).
           05 LOCK-BOOLEAN        PIC 9.
             88 IS-NOT-LOCKED value 0.
             88 IS-LOCKED     value 1.
           05 FILLER              PIC X(58).
           05 TS-NAME             PIC X(8).

       77 NUM-PAGE-EDIT           PIC Z9.

       01 C-R                     PIC S9(8) COMP.

       01 MON-PROG                PIC X(8) VALUE 'INF5CI11'.
       01 MA-MAP                  PIC X(8) VALUE 'MAP5C11 '.
       01 MA-TRX                  PIC X(4) VALUE 'T5CB'.
      *------------------------------------------------------*
      *   DESCRIPTION   DE   LA   MAP                        *
      *------------------------------------------------------*
       COPY MAP5C11.

      *------------------------------------------------------*
      *   DESCRIPTION   DU FICHIER FSTAG                     *
      *------------------------------------------------------*
       COPY CSTAG.

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
       
      * ---------------- Header ------------------------ 
       01 weekday-list.
         COPY WEEKDAY.
       01 FILLER REDEFINES weekday-list.
         05 weekday-name              PIC X(10) occurs 7.

       01 interval                    pic S9(15) comp-3.
       01 num-j                       PIC S9(5) comp.
       01 ident                       PIC X(17).

      * ------ Structure d'une ligne d'affichage -------
       01 W-LIGNE.
        05 W-NUM PIC 9(4).
        05 PIC X(4).
        05 W-NOM PIC X(20).
        05 PIC X(5).
        05 W-PRENOM PIC X(20).
        05 PIC X(3).
        05 W-DEBUT-PR PIC X(10).
        05 PIC X(3).
        05 W-TEL-PORT PIC X(10).
      
      * -------- Pour recuperer la cle d'une ligne -------- 
       01 CHECK-LIGNE.
         05 LIGNE-KEY PIC 9(4).
         05 FILLER PIC X(74).

      * ------- Position du curseur en ligne -----------
       01 posCursor                   PIC S9(4) COMP.
      
      *------ Variable de parcours de tableau --------- 
       77 I                           PIC 99.
       77 posI                        PIC S99.   

      *======================================================*
      *          L I N K A G E     S E C T I O N             *
      *======================================================*      

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
           STRING 
              'T5CB' DELIMITED BY SIZE
              eibtrmid DELIMITED BY SIZE
            INTO ts-name
           END-STRING
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
           MOVE LOW-VALUE TO MAP5O

      *  SI TRAITEMENT PARTICULIER AVANT AFFICHAGE
           PERFORM  21100-TRAIT-SPECIFIQUE
           .

       21100-TRAIT-SPECIFIQUE.
           IF IS-ZOOM
              SET IS-NOT-ZOOM TO TRUE

              MOVE LOW-VALUE TO MAP5O

              MOVE ZOOM-KEY TO NUMSTAGO

              EXEC CICS READQ ts queue(ts-name)
                                 item(NUM-PAGE)
                                 into(map5o)
              END-EXEC
           END-IF
           .


       22000-TRAIT-ENVOI.
      *-----------------*
           MOVE NUM-PAGE TO NUM-PAGE-EDIT
           MOVE NUM-PAGE-EDIT TO NUMPAGEO
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
            MA-TRX delimited by size
            '/' delimited by size
            eibtrmid delimited by size
            '/map5c11' delimited by size
            into idento
           END-STRING
           
           MOVE weekday-name(num-j) TO jouro
       
           move -1 to NUMSTAGl


           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              EXEC CICS SEND MAP    ('MAP5')
                             MAPSET (MA-MAP)
                             ERASE
              END-EXEC
           ELSE
              EXEC CICS SEND MAP    ('MAP5')
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

      ******************************************************************
      ******************************************************************
      ******************************************************************     

       23000-TRAIT-RECEPTION.
      *---------------------*
           move SPACE to messo

           IF IS-LOCKED
              MOVE '/' TO NUMSTAGA
           END-IF 
           EVALUATE EIBAID
              WHEN DFHENTER
                 perform 23110-RECEIVE-MAP
                 IF IS-LOCKED
                   MOVE "Touche entrer verouille" to messo
                   perform 22000-TRAIT-ENVOI 
                 ELSE
                   IF NUMSTAGI < 1000 OR NUMSTAGI > 5000
                    MOVE 'Entrer un num stagiaire valide' to messo
                    perform 22000-TRAIT-ENVOI
                   END-IF 
                   PERFORM  23100-TRAIT-ENTER
                 END-IF
              WHEN DFHPF3
                   MOVE 'INF0CI11' TO PROG-SUIVANT
                   SET IS-NOT-LOCKED TO TRUE
                   perform 23800-PROG-SUIVANT
              WHEN DFHPF7
                 perform 23110-RECEIVE-MAP
                 IF IS-LOCKED
                   perform 23300-TRAIT-PAGE-PREV
                 ELSE
                   MOVE "Entrer un numero de stagiaire" TO MESSO
                   perform 22000-TRAIT-ENVOI
                 END-IF 
              WHEN DFHPF8
                 perform 23110-RECEIVE-MAP
                 IF IS-LOCKED
                   perform 23400-TRAIT-PAGE-NEXT
                 ELSE
                   MOVE "Entrer un numero de stagiaire" TO MESSO
                   perform 22000-TRAIT-ENVOI
                 END-IF
              WHEN DFHPF10
                   perform 23110-RECEIVE-MAP
                   MOVE SPACE TO NUMSTAGO
                   MOVE 'J' TO NUMSTAGA
                   SET IS-NOT-LOCKED TO TRUE
                   perform varying I FROM 1 BY 1 UNTIL I > 10
                       MOVE SPACE TO LIGNEO(I)
                   end-perform
                   perform 22000-TRAIT-ENVOI 
              WHEN DFHPF11
                   perform 23500-TRAIT-CURSOR
              WHEN DFHPF12
                   PERFORM  23900-TRAIT-FIN
              WHEN DFHCLEAR
                   PERFORM  23900-TRAIT-FIN
              WHEN OTHER
                   PERFORM  90000-ERR-TOUCHE
           END-EVALUATE
           .

      *--------------------------------------------------
      * -----------  TRAITEMENT DE BASE (ENTER) ---------
      *--------------------------------------------------

       23100-TRAIT-ENTER.
      *------------------*
           MOVE NUMSTAGI TO CURRENT-KEY

           EXEC CICS DELETEQ ts queue(ts-name) RESP(C-R) END-EXEC
           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                 continue
              WHEN DFHRESP(QIDERR)
                 continue
              WHEN OTHER 
                 continue
           END-EVALUATE

           MOVE 1 TO NUM-PAGE
           MOVE 9 TO STAGIAIRE-EOF
           
           EXEC CICS STARTBR
                     FILE('FSTAG11 ')
                     RIDFLD(CURRENT-KEY)
                     RESP(C-R)
           END-EXEC
      *
           EVALUATE C-R
               WHEN DFHRESP(NORMAL)
                   continue
               WHEN DFHRESP(NOTFND)
                   MOVE "Stagiaire non trouve" to messo
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                    MOVE SPACE TO LIGNEO(I)
                   END-PERFORM
                   PERFORM 22000-TRAIT-ENVOI
           END-EVALUATE

           MOVE '/' TO NUMSTAGA
           SET IS-LOCKED TO TRUE
           
           perform 23130-READ-STAGIAIRE

           perform 22000-TRAIT-ENVOI
           .

       23110-RECEIVE-MAP.
      *------------------*
           EXEC CICS RECEIVE MAP  ('MAP5')
                             MAPSET('MAP5C11')
                             RESP (C-R)
           END-EXEC
           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                   CONTINUE 
              WHEN OTHER
                   PERFORM 91000-ERREUR-CICS
           END-EVALUATE
           .

       23120-START-BR.
      *--------------*
           ADD 1 TO CURRENT-KEY

           EXEC CICS STARTBR
                     FILE('FSTAG11 ')
                     RIDFLD(CURRENT-KEY)
                     RESP(C-R)
           END-EXEC
      *
           EVALUATE C-R
               WHEN DFHRESP(NORMAL)
                   continue
               WHEN DFHRESP(NOTFND)
                   SUBTRACT 1 FROM NUM-PAGE
                   MOVE NUM-PAGE TO STAGIAIRE-EOF 
                   MOVE "Fin de fichier" to messo
                   PERFORM 22000-TRAIT-ENVOI
           END-EVALUATE
           .

       23130-READ-STAGIAIRE.
      *---------------------*
           move SPACE to W-LIGNE

           PERFORM VARYING I
            FROM 1 BY 1
            UNTIL I > 10
              EXEC CICS READNEXT
                     FILE('FSTAG11 ')
                     RIDFLD(CURRENT-KEY)
                     INTO(E-STAGIAIRE)
                     RESP(C-R)
              END-EXEC
      *
              EVALUATE C-R
               WHEN DFHRESP(NORMAL)
                   continue
               WHEN DFHRESP(ENDFILE)
                   MOVE NUM-PAGE TO STAGIAIRE-EOF 
                   perform varying I FROM I BY 1 UNTIL I > 10
                     MOVE SPACE TO LIGNEO(I)
                   END-PERFORM
                   EXEC CICS WRITEQ ts queue(ts-name)
                                       from (map5i)
                   END-EXEC
                   perform 22000-TRAIT-ENVOI
               WHEN OTHER 
                   perform 91000-ERREUR-CICS
              END-EVALUATE
              MOVE E-NUMERO TO W-NUM
              MOVE E-NOM TO W-NOM
              MOVE E-PRENOM TO W-PRENOM
              MOVE E-DATE-DEB-PRESTA TO W-DEBUT-PR
              MOVE E-TELPOR TO W-TEL-PORT
              
              MOVE W-LIGNE TO LIGNEO(I)
           END-PERFORM
           
           EXEC CICS WRITEQ ts queue(ts-name)
                               from(map5i)
           END-EXEC
           .           

      *--------------------------------------------------
      * --- TRAITEMENT DE LA PAGE PRECEDENTE (F7) -------
      *--------------------------------------------------        
       23300-TRAIT-PAGE-PREV.
      *-----------------------*
           IF (NUM-PAGE <= 1)
              MOVE "Premiere page atteinte" to messo
              perform 22000-TRAIT-ENVOI
           END-IF

           SUBTRACT 1 FROM NUM-PAGE

           MOVE LOW-VALUE TO MAP5O
                 
           EXEC CICS READQ ts queue(ts-name)
                              ITEM(NUM-PAGE)
                              INTO(MAP5O)
                              RESP(C-R)
           END-EXEC

           perform 22000-TRAIT-ENVOI          
           .


      *--------------------------------------------------
      * ----  TRAITEMENT DE LA PAGE SUIVANTE (F8) -------
      *--------------------------------------------------
       23400-TRAIT-PAGE-NEXT.
      *----------------------*            
           IF NUM-PAGE >= STAGIAIRE-EOF
              MOVE "Derniere page atteinte" TO MESSO
              perform 22000-TRAIT-ENVOI
           END-IF

           ADD 1 TO NUM-PAGE
           
           MOVE LOW-VALUE TO MAP5O
           
           EXEC CICS READQ ts queue(ts-name)
                           ITEM(NUM-PAGE)
                           INTO(MAP5O)
                           RESP(C-R)
           END-EXEC
           
           EVALUATE C-R
               WHEN DFHRESP(NORMAL)
                   continue
               WHEN DFHRESP(ITEMERR)
                   perform 23120-START-BR
                   perform 23130-READ-STAGIAIRE
               WHEN OTHER
                   perform 91000-ERREUR-CICS
           END-EVALUATE

           perform 22000-TRAIT-ENVOI
           .
       
      *--------------------------------------------------
      * -- TRAITEMENT DE LA POSITION DU CURSEUR (F11) ---
      *--------------------------------------------------
       23500-TRAIT-CURSOR.
      *-------------------* 
           perform 23110-RECEIVE-MAP

           COMPUTE posCursor = ((EIBCPOSN / 80) + 1) - 7
           IF posCursor  >= 1 AND posCursor <= 10
                 MOVE LIGNEI(posCursor) TO CHECK-LIGNE
                 MOVE LIGNE-KEY TO ZOOM-KEY
              IF LIGNE-KEY = SPACE OR LOW-VALUE OR LIGNE-KEY NOT NUMERIC
                 move "Mauvaise donnees de curseur" to messo
                 perform 22000-TRAIT-ENVOI
              ELSE
                 SET IS-ZOOM TO TRUE
                 MOVE 'INF2CI11' TO PROG-SUIVANT
                 perform 23800-PROG-SUIVANT
              END-IF
           ELSE
              move "Mauvaise position de curseur" to messo
               perform 22000-TRAIT-ENVOI
           END-IF 
           .


      ******************************************************************
      ******************************************************************
      ******************************************************************

       23800-PROG-SUIVANT.
      *------------------*
           EXEC CICS XCTL  PROGRAM  (PROG-SUIVANT)
                           COMMAREA (WS-COMMAREA)
                           LENGTH   (LENGTH OF WS-COMMAREA)
           END-EXEC
           .

       23900-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       90000-ERR-TOUCHE.
      *----------------*
           perform 23110-RECEIVE-MAP

           MOVE 'TOUCHE DE FONCTION INVALIDE' TO messo

           PERFORM  22000-TRAIT-ENVOI
           .

       91000-ERREUR-CICS.
      *-------------------*
           MOVE 'ERREUR CICS !!!  FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       99000-FIN-CICS.
      *--------------*
           EXEC CICS DELETEQ ts queue(ts-name) END-EXEC
           
           EXEC CICS SEND FROM   (MESSAGE-TXT)
                          LENGTH (LENGTH OF MESSAGE-TXT)
                          ERASE
           END-EXEC

           EXEC CICS RETURN END-EXEC
           .
