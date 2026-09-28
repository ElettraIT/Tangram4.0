@ECHO OFF

REM  ############ /ABD/ETC/NEWAZI.BAT
REM  #
REM  # Procedura TANGRAM per la creazione di una nuova azienda XXX
REM  # all'interno di \ABD\AZI
REM  #
REM  # Input : %1 = Sigla della nuova azienda che si vuole creare XXX
REM  #
REM  ######    

REM  ######
REM  # - Controllo sul numero di parametri
REM  ###

IF '%1'=='' GOTO STEP0010
IF '%2'=='' GOTO STEP1000

:STEP0010

ECHO +=====================================================================+
ECHO "          # Errore nei parametri per il comando NEWAZI #             "
ECHO "         -----------------------------------------------             "
ECHO "                                                                     "
ECHO " Errore : E' richiesto un parametro per la sigla della nuova         "
ECHO "          azienda che si vuole creare.                               "
ECHO "                                                                     "
ECHO "          Non e' ammesso omettere il parametro, ne' fornire          "
ECHO "          piu' di un parametro.                                      "
ECHO "====================================================================="
GOTO STEP9999

:STEP1000

REM  ######
REM  # - Controllo sulla definizione delle variabili necessarie
REM  ###

:STEP1005

IF NOT "%DSKABD%"=="" GOTO STEP2000

:STEP1010

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando NEWAZI #
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKABD non risulta definita, per-
@ECHO          tanto non e' possibile stabilire l'esatta destinazione re-
@ECHO          lativa all'azienda da creare.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2000

REM  ######
REM  # - Creazione directory \ABD\AZI\XXX
REM  ###

MKDIR %DSKABD%\ABD\AZI\%1

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

