@ECHO OFF

REM  ############ /ABD/ETC/MKDPRF.BAT
REM  #
REM  # Procedura TANGRAM per creazione subdirectory \ABD\PRF
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
@ECHO              # Errore per il comando MKDPRF #
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKABD non risulta definita, per-
@ECHO          tanto non e' possibile stabilire l'esatta destinazione re-
@ECHO          lativa alla directory PRF che deve essere creata.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2000

REM  ######
REM  # - Creazione directory \ABD\PRF
REM  ###

MKDIR %DSKABD%\ABD\PRF

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
