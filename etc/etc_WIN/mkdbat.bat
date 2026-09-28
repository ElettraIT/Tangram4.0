@ECHO OFF

REM  ############ /ABD/ETC/MKDBAT.BAT
REM  #
REM  # Procedura TANGRAM per creazione subdirectory \ABD\BAT
REM  #
REM  ######    

:STEP1000

REM  ######
REM  # - Controllo sulla definizione delle variabili necessarie
REM  ###

:STEP1005

IF NOT "%DSKABD%"=="" GOTO STEP2000

:STEP1010

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando MKDBAT
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKABD non risulta definita, per-
@ECHO          tanto non e' possibile stabilire l'esatta destinazione re-
@ECHO          lativa alla directory BAT che deve essere creata.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2000

REM  ######
REM  # - Creazione directory \ABD\BAT
REM  ###

MKDIR %DSKABD%\ABD\BAT

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
