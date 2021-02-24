        PRINT NOGEN
MAP5C11 DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES
* MAP DU LISTING DES STAGIAIRES
MAP5   DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),    X
               DSATTS=(COLOR,HILIGHT),CURSLOC=YES
* HEADER DU CICS
IDENT   DFHMDF POS=(1,1),                                              X
               ATTRB=ASKIP,LENGTH=18,                                  X
               INITIAL='SDFDGFXXXXXXXXXXXX'
        DFHMDF POS=(1,25),                                             X
               ATTRB=ASKIP,LENGTH=32,COLOR=RED,                        X
               INITIAL='LISTE DES STAGIAIRES'
JOUR    DFHMDF POS=(1,58),                                             X
               ATTRB=ASKIP,LENGTH=10,                                  X
               INITIAL='LEJOURAFFI'
DATE    DFHMDF POS=(1,69),                                             X
               ATTRB=ASKIP,LENGTH=10,                                  X
               INITIAL='XX/XX/XXXX'
*
HEURE   DFHMDF POS=(2,69),                                             X
               ATTRB=ASKIP,LENGTH=8,                                   X
               INITIAL='XX:XX:XX'
* NUMERO STAGIAIRE DE DEBUT DE LISTING
        DFHMDF POS=(4,10),                                             X
               ATTRB=ASKIP,LENGTH=26,                                  X
               INITIAL='N° DU STAGIAIRE DE DEBUT :'
NUMSTAG DFHMDF POS=(4,37),                                             X
               ATTRB=(UNPROT,NUM,FSET,IC),LENGTH=4
        DFHMDF POS=(4,42),ATTRB=ASKIP,LENGTH=1
        DFHMDF POS=(4,69),                                             X
               ATTRB=ASKIP,LENGTH=6,                                   X
               INITIAL='PAGE :'
NUMPAGE DFHMDF POS=(4,76),                                             X
               ATTRB=ASKIP,LENGTH=2,                                   X
               INITIAL='01'
* HEADER LISTE
        DFHMDF POS=(6,1),COLOR=TURQUOISE,                              X
               ATTRB=ASKIP,LENGTH=6,                                   X
               INITIAL='NUM'
        DFHMDF POS=(6,15),COLOR=TURQUOISE,                             X
               ATTRB=ASKIP,LENGTH=19,                                  X
               INITIAL='NOM'
        DFHMDF POS=(6,35),COLOR=TURQUOISE,                             X
               ATTRB=ASKIP,LENGTH=19,                                  X
               INITIAL='PRENOM'
        DFHMDF POS=(6,55),COLOR=TURQUOISE,                             X
               ATTRB=ASKIP,LENGTH=19,                                  X
               INITIAL='DATE NAISSANCE'
        DFHMDF POS=(6,75),COLOR=TURQUOISE,                             X
               ATTRB=ASKIP,LENGTH=5,                                   X
               INITIAL='TEL'
* LISTE DE STAGIAIRE
LIGNE   DFHMDF POS=(8,1),                                              X
               ATTRB=(ASKIP,FSET),LENGTH=79,OCCURS=10
* MESSAGE D'INFORMATION
MESS    DFHMDF POS=(21,1),COLOR=NEUTRAL,                               X
               ATTRB=ASKIP,LENGTH=79
* MENU DE TOUCHE UTILISABLE
        DFHMDF POS=(22,1),COLOR=YELLOW,                                X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF7 - PAGE PRECEDENTE'
        DFHMDF POS=(22,26),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF8 - PAGE SUIVANTE'
        DFHMDF POS=(22,51),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF10 - NOUVEL ENREG'
*
        DFHMDF POS=(23,1),COLOR=YELLOW,                                X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='CLEAR - FIN'
        DFHMDF POS=(23,26),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF3 - RETOUR MENU'
        DFHMSD TYPE=FINAL
        END
