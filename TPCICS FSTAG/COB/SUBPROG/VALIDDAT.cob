       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDDAT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           
       01 MONTH-V.
         COPY MONTHDAY.
       01 TAB-MONTH redefines MONTH-V.
         10 NB-JOURS                 PIC 99 occurs 12.
           
       01 MODULO-N.
         05 MODULO4                  PIC 99.
          88 MOD4       value 0.
         05 MODULO100                PIC 99.
          88 MOD100     value 0.
         05 MODULO400                PIC 99.
          88 MOD400     value 0.

       LINKAGE SECTION.
       01 pgm-param.
         02 DATE-ENTERED.
           05 DAY-E                  PIC 9(2).
           05 MONTH-E                PIC 9(2).
           05 YEAR-E                 PIC 9(4).
         02 CR                       PIC 9.
           88 cr-valide    value 0.
           88 cr-not-valid value 1.
               
       PROCEDURE DIVISION USING pgm-param.
           IF DATE-ENTERED IS NOT NUMERIC
             perform 21100-DATE-ERROR
           ELSE
             EVALUATE TRUE
      *        Check mois
             WHEN (MONTH-E > 12 OR MONTH-E < 1)
               perform 21100-DATE-ERROR
      *        Check jour
             WHEN (DAY-E > NB-JOURS(MONTH-E) OR DAY-E < 1)
               perform 21100-DATE-ERROR
      *        Check bisextile
             WHEN (MONTH-E = 2 AND DAY-E = 29)
               perform 21200-GET-MODULOS
               IF (NOT (MOD400 OR (MOD4 AND NOT MOD100)))
                 perform 21100-DATE-ERROR
               END-IF
             WHEN OTHER
               continue
             END-EVALUATE
           END-IF
           MOVE 1 TO CR
           GOBACK
           .

       21100-DATE-ERROR.
             MOVE 0 to CR
             GOBACK
           .

       21200-GET-MODULOS.
           DIVIDE YEAR-E BY 4 GIVING MODULO4 REMAINDER MODULO4
           DIVIDE YEAR-E BY 100 GIVING MODULO100 REMAINDER MODULO100
           DIVIDE YEAR-E BY 400 GIVING MODULO400 REMAINDER MODULO400
           .
              