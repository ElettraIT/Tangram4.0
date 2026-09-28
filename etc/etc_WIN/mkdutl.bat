@ECHO OFF

REM  ############ /ABD/ETC/MKDUTL.BAT
REM  #
REM  # Procedura TANGRAM per la creazione di tutte le subdirectories
REM  # di \ABD relative all' utilizzo del software, eclusa le sub-
REM  # directories ETC e RUN
REM  #
REM  # - ASC 
REM  # - AZI 
REM  # - BAT 
REM  # - FDB 
REM  # - FPX
REM  # - OGT
REM  # - PBL
REM  # - PRF
REM  # - SPL
REM  # - SRT
REM  # - TAR
REM  # - TMP
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
@ECHO              # Errore per il comando MKDUTL #
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKABD non risulta definita, per-
@ECHO          tanto non e' possibile stabilire l'esatta destinazione re-
@ECHO          lativa alle directories che devono essere create.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2000

REM  ######
REM  # - Creazione subdirectories
REM  ###

CALL %DSKABD%\ABD\ETC\MKDASC
CALL %DSKABD%\ABD\ETC\MKDAZI
CALL %DSKABD%\ABD\ETC\MKDBAT
CALL %DSKABD%\ABD\ETC\MKDFDB
CALL %DSKABD%\ABD\ETC\MKDFPX
CALL %DSKABD%\ABD\ETC\MKDOGT
CALL %DSKABD%\ABD\ETC\MKDPBL
CALL %DSKABD%\ABD\ETC\MKDPRF
CALL %DSKABD%\ABD\ETC\MKDSPL
CALL %DSKABD%\ABD\ETC\MKDSRT
CALL %DSKABD%\ABD\ETC\MKDTAR
CALL %DSKABD%\ABD\ETC\MKDTMP

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
