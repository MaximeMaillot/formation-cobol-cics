       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCFILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       copy CSTAG.

       01 C-R                     PIC S9(8) COMP.

       01 has-error PIC 9.
         88 has-error-false value 0.
         88 has-error-true value 1.

       LINKAGE SECTION.
       01 pgm-param.
         05 file-name PIC X(8).
         05 code-fonction PIC 99.
           88 c-read value 1.
           88 c-write value 2.
           88 c-rewrite value 3.
           88 c-delete value 4.
         05 code-retour PIC 99.
           88 cr-ok value 0.
           88 cr-key-exists value 1.
           88 cr-duplicate value 2.
           88 cr-file-error value 3.
           88 cr-eof value 4.
           88 cr-unsupported value 99.
         05 enrgmt PIC X(400).

       PROCEDURE DIVISION USING pgm-param.
       DEBUT.
           DISPLAY 'param debut : ' pgm-param
           MOVE 0 to has-error
           INITIALIZE C-R
           EVALUATE true
             WHEN file-name  = 'FSTAG'
               perform GESTION-FSTAG
             WHEN OTHER
               MOVE 1 to has-error
           END-EVALUATE
           perform EVALUATE-CODE-RETOUR
           DISPLAY 'fin => cr : ' code-retour 
           EXEC CICS RETURN END-EXEC
           .
       
       GESTION-FSTAG.
           EVALUATE TRUE
             WHEN c-read 
               MOVE enrgmt TO e-stagiaire
      *         CICS READ  
               move e-stagiaire to enrgmt
             WHEN c-rewrite 
               MOVE enrgmt TO e-stagiaire
      *         CICS REWRITE 
             WHEN c-delete
               MOVE enrgmt TO e-stagiaire
      *         CICS DELETE
             WHEN c-write
               move enrgmt to e-stagiaire
               EXEC CICS WRITE
                     FILE('FSTAG11 ')
                     RIDFLD(E-NUMERO)
                     FROM(E-STAGIAIRE)
                     RESP(C-R)
               END-EXEC
             WHEN OTHER
               MOVE 1 TO has-error
           END-EVALUATE
           .

       EVALUATE-CODE-RETOUR.
           EVALUATE true
             WHEN has-error-true
               MOVE 99 to code-retour
             WHEN C-R = DFHRESP(NORMAL)
               MOVE 0 to code-retour
             WHEN C-R = DFHRESP(DUPREC)
               MOVE 2 to code-retour
             WHEN OTHER
               MOVE 99 to code-retour
           END-EVALUATE
           .