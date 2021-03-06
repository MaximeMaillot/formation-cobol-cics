        PRINT NOGEN
MAP2C11 DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES
* Map de la consultation d'un stagiaire
MAP2   DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),    X
               DSATTS=(COLOR,HILIGHT),CURSLOC=YES
* Header du CICS
IDENT   DFHMDF POS=(1,1),                                              X
               ATTRB=ASKIP,LENGTH=18,                                  X
               INITIAL='SDFDGFXXXXXXXXXXXX'
        DFHMDF POS=(1,25),COLOR=RED,                                   X
               ATTRB=ASKIP,LENGTH=32,                                  X
               INITIAL='CONSULTATION D''UN STAGIAIRE'
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
CNOM    DFHMDF POS=(7,10),                                             X
               ATTRB=(ASKIP,DRK),LENGTH=10,                            X
               INITIAL='NOM* :'
NOM     DFHMDF POS=(7,21),COLOR=BLUE,                                  X
               ATTRB=(ASKIP,DRK),LENGTH=20
        DFHMDF POS=(7,42),ATTRB=(ASKIP),LENGTH=1
CPRENOM DFHMDF POS=(7,44),                                             X
               ATTRB=(ASKIP,DRK),LENGTH=10,                            X
               INITIAL='PRENOM* :'
PRENOM  DFHMDF POS=(7,55),COLOR=BLUE,                                  X
               ATTRB=(ASKIP,DRK),LENGTH=15
        DFHMDF POS=(7,76),ATTRB=(ASKIP),LENGTH=1
* Adresse
CADR1   DFHMDF POS=(9,10),                                             X
               ATTRB=(ASKIP,DRK),LENGTH=10,                            X
               INITIAL='ADRESSE :'
ADR1    DFHMDF POS=(9,21),COLOR=BLUE,                                  X
               ATTRB=(ASKIP,DRK),LENGTH=30
        DFHMDF POS=(9,52),ATTRB=(ASKIP),LENGTH=1
* Complement d'adresse
ADR2    DFHMDF POS=(10,21),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=30
        DFHMDF POS=(10,52),ATTRB=(ASKIP,DRK),LENGTH=1
*  Code Postal et Ville
CCODEP  DFHMDF POS=(11,20),                                            X
               ATTRB=(ASKIP,DRK),LENGTH=4,                             X
               INITIAL='CP :'
CODEP   DFHMDF POS=(11,25),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=5
        DFHMDF POS=(11,31),ATTRB=(ASKIP),LENGTH=1
CVILLE  DFHMDF POS=(11,33),                                            X
               ATTRB=(ASKIP,DRK),LENGTH=7,                             X
               INITIAL='VILLE :'
VILLE   DFHMDF POS=(11,41),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=25
        DFHMDF POS=(11,67),ATTRB=(ASKIP),LENGTH=1
* Telephone fixe
CTELDOM DFHMDF POS=(13,10),                                            X
               ATTRB=(ASKIP,DRK),LENGTH=20,                            X
               INITIAL='TELEPHONE DOMICILE :'
TELDOM  DFHMDF POS=(13,31),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=10
        DFHMDF POS=(13,42),ATTRB=(ASKIP),LENGTH=1
* Telephone mobile
CTELMOB DFHMDF POS=(15,10),                                            X
               ATTRB=(ASKIP,DRK),LENGTH=20,                            X
               INITIAL='TELEPHONE MOBILE :'
TELMOB  DFHMDF POS=(15,31),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=10
        DFHMDF POS=(15,42),ATTRB=(ASKIP),LENGTH=1
* Date de naissance
CDATEN  DFHMDF POS=(17,10),                                            X
               ATTRB=(ASKIP,DRK),LENGTH=20,                            X
               INITIAL='DATE DE NAISSANCE* :'
DATEN   DFHMDF POS=(17,31),COLOR=BLUE,                                 X
               ATTRB=(ASKIP,DRK),LENGTH=8
        DFHMDF POS=(17,40),ATTRB=(ASKIP),LENGTH=1
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
