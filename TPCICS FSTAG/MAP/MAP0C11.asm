        PRINT NOGEN
MAP0C11 DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES
* Menu principal de la gestion des stagiaires               
MAP0   DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),    X
               DSATTS=(COLOR,HILIGHT),CURSLOC=YES
* Header du CICS
IDENT   DFHMDF POS=(1,1),                                              X
               ATTRB=ASKIP,LENGTH=18,                                  X
               INITIAL='SDFDGFXXXXXXXXXXXX'
        DFHMDF POS=(1,25),                                             X
               ATTRB=ASKIP,LENGTH=32,COLOR=RED,                        X
               INITIAL='GESTION DES STAGIAIRES'
JOUR    DFHMDF POS=(1,58),                                             X
               ATTRB=(ASKIP,FSET),LENGTH=10,                           X
               INITIAL='LEJOURAFFI'
DATE    DFHMDF POS=(1,69),                                             X
               ATTRB=ASKIP,LENGTH=10,                                  X
               INITIAL='XX/XX/XXXX'
*               
HEURE   DFHMDF POS=(2,69),                                             X
               ATTRB=ASKIP,LENGTH=8,                                   X
               INITIAL='XX:XX:XX'
*
        DFHMDF POS=(5,25),                                             X
               ATTRB=ASKIP,LENGTH=50,                                  X
               INITIAL='1-CREATION D''UN STAGIAIRE'
*               
        DFHMDF POS=(7,25),                                             X
               ATTRB=ASKIP,LENGTH=50,                                  X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
*               
        DFHMDF POS=(9,25),                                             X
               ATTRB=ASKIP,LENGTH=50,                                  X
               INITIAL='3-MIS A JOUR D''UN STAGIAIRE'
*               
        DFHMDF POS=(11,25),                                            X
               ATTRB=ASKIP,LENGTH=50,                                  X
               INITIAL='4-SUPPRESSION D''UN STAGIAIRE'
*               
        DFHMDF POS=(13,25),                                            X
               ATTRB=ASKIP,LENGTH=50,                                  X
               INITIAL='5-LISTE DES STAGIAIRES'
* Choix du menu              
        DFHMDF POS=(15,30),                                            X
               ATTRB=ASKIP,LENGTH=19,                                  X
               INITIAL='TAPEZ VOTRE CHOIX :'               
CHOIX   DFHMDF POS=(15,50),                                            X
               ATTRB=(UNPROT,FSET,IC),LENGTH=1,                        X
               HILIGHT=UNDERLINE
        DFHMDF POS=(15,52),ATTRB=ASKIP,LENGTH=1
* Message d'information              
MESS    DFHMDF POS=(22,1),COLOR=NEUTRAL,                               X
               ATTRB=ASKIP,LENGTH=79
* Menu de touche utilisable               
        DFHMDF POS=(23,1),COLOR=YELLOW,                                X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF12 - FIN'
        DFHMDF POS=(23,26),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='ENTER - VALIDATION'
        DFHMSD TYPE=FINAL        
        END
