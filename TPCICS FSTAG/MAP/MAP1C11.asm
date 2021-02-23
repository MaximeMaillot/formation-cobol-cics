        PRINT NOGEN
MAP1C11 DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES
* Map de la creation d'un stagiaire
MAP1   DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),    X
               DSATTS=(COLOR,HILIGHT),CURSLOC=YES
* Header du CICS
IDENT   DFHMDF POS=(1,1),                                              X
               ATTRB=ASKIP,LENGTH=18,                                  X
               INITIAL='SDFDGFXXXXXXXXXXXX'
        DFHMDF POS=(1,25),COLOR=RED,                                   X
               ATTRB=ASKIP,LENGTH=32,                                  X
               INITIAL='CREATION D''UN STAGIAIRE'
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
* Numero Stagiaire
        DFHMDF POS=(5,10),                                             X
               ATTRB=ASKIP,LENGTH=22,                                  X
               INITIAL='NUMERO DE STAGIAIRE* :'
NUMSTAG DFHMDF POS=(5,33),COLOR=BLUE,                                  X
               ATTRB=(UNPROT,FSET,IC),LENGTH=4,                        X
               HILIGHT=UNDERLINE
        DFHMDF POS=(5,38),ATTRB=(ASKIP),LENGTH=1
* Nom et Prenom
        DFHMDF POS=(7,10),                                             X
               ATTRB=ASKIP,LENGTH=10,                                  X
               INITIAL='NOM* :'
NOM     DFHMDF POS=(7,21),COLOR=BLUE,                                  X
               ATTRB=(UNPROT,FSET),LENGTH=20,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(7,42),ATTRB=(ASKIP),LENGTH=1
        DFHMDF POS=(7,44),                                             X
               ATTRB=(ASKIP),LENGTH=10,                                X
               INITIAL='PRENOM* :'
PRENOM  DFHMDF POS=(7,55),COLOR=BLUE,                                  X
               ATTRB=(UNPROT,FSET),LENGTH=15,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(7,76),ATTRB=(ASKIP),LENGTH=1
* Adresse
        DFHMDF POS=(9,10),                                             X
               ATTRB=ASKIP,LENGTH=10,                                  X
               INITIAL='ADRESSE :'
ADR1    DFHMDF POS=(9,21),COLOR=BLUE,                                  X
               ATTRB=(UNPROT,FSET),LENGTH=30,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(9,52),ATTRB=(ASKIP),LENGTH=1
* Complement d'adresse
ADR2    DFHMDF POS=(10,21),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=30,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(10,52),ATTRB=(ASKIP),LENGTH=1
*  Code Postal et Ville
        DFHMDF POS=(11,20),                                            X
               ATTRB=ASKIP,LENGTH=4,                                   X
               INITIAL='CP :'
CODEP   DFHMDF POS=(11,25),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=5,                           X
               HILIGHT=UNDERLINE
        DFHMDF POS=(11,31),ATTRB=(ASKIP),LENGTH=1
        DFHMDF POS=(11,33),                                            X
               ATTRB=ASKIP,LENGTH=7,                                   X
               INITIAL='VILLE :'
VILLE   DFHMDF POS=(11,41),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=25,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(11,67),ATTRB=(ASKIP),LENGTH=1
* Telephone fixe
        DFHMDF POS=(13,10),                                            X
               ATTRB=ASKIP,LENGTH=20,                                  X
               INITIAL='TELEPHONE DOMICILE :'
TELDOM  DFHMDF POS=(13,31),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=10,                          X
               HILIGHT=UNDERLINE
        DFHMDF POS=(13,42),ATTRB=(ASKIP),LENGTH=1
* Telephone mobile
        DFHMDF POS=(15,10),                                            X
               ATTRB=ASKIP,LENGTH=20,                                  X
               INITIAL='TELEPHONE MOBILE :'
TELMOB  DFHMDF POS=(15,31),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=10,                          X
               HILIGHT=UNDERLINE                      
        DFHMDF POS=(15,42),ATTRB=(ASKIP),LENGTH=1
* Date de naissance
        DFHMDF POS=(17,10),                                            X
               ATTRB=ASKIP,LENGTH=20,                                  X
               INITIAL='DATE DE NAISSANCE* :'
DATEN   DFHMDF POS=(17,31),COLOR=BLUE,                                 X
               ATTRB=(UNPROT,FSET),LENGTH=8,                           X
               HILIGHT=UNDERLINE
        DFHMDF POS=(17,40),ATTRB=(ASKIP),LENGTH=1
* Message de confirmation
LCONF   DFHMDF POS=(20,35),COLOR=NEUTRAL,                              X
               ATTRB=(ASKIP,DRK),LENGTH=19,                            X
               INITIAL='CONFIRMATION (O/N) :'
CONF    DFHMDF POS=(20,55),COLOR=NEUTRAL,                              X
               ATTRB=(ASKIP,DRK),LENGTH=1
        DFHMDF POS=(20,57),ATTRB=(ASKIP),LENGTH=1
* Message d'information
MESS    DFHMDF POS=(22,1),COLOR=NEUTRAL,                               X
               ATTRB=ASKIP,LENGTH=79
* Menu de touche utilisable
        DFHMDF POS=(23,1),COLOR=YELLOW,                                X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF12 - FIN'
        DFHMDF POS=(23,26),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='PF3 - RETOUR MENU'
        DFHMDF POS=(23,51),COLOR=YELLOW,                               X
               ATTRB=ASKIP,LENGTH=24,                                  X
               INITIAL='ENTER : VALIDATION'
        DFHMSD TYPE=FINAL
        END
