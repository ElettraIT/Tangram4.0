@ECHO OFF

REM  ############ /ABD/ETC/AGGFDB.BAT
REM  #
REM  # Procedura TANGRAM per l'estrazione da supporto magnetico di /abd/fdb
REM  #
REM  # Input     $1: Pathname dell'unita' a nastro magnetico che deve
REM  #               essere utilizzata
REM  #
REM  # Nota      Se non viene passato alcun parametro viene utilizzata
REM  #           l'unita' per aggiornamento software definita dal va-
REM  #           lore della variabile AGSDEV
REM  #
REM  ######    

:STEP1000

REM  ######
REM  # - Controllo sul numero di parametri e sul loro valore
REM  ###

:STEP1005
IF "%1"=="" GOTO STEP1015
IF "%2"=="" GOTO STEP1030

:STEP1010

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando AGGFDB
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : Il comando deve essere richiamato senza parametri, oppure
@ECHO          al massimo con un parametro indicante il pathname relati-
@ECHO          vo all'unita' da utilizzarsi per gli aggiornamenti soft-
@ECHO          vare.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP1015

IF NOT "%AGSDEV%"=="" GOTO STEP1025

:STEP1020

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando AGGFDB
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : Il comando e' stato richiamato senza il parametro indicante
@ECHO          il pathname relativo all'unita' da utilizzarsi per gli ag-
@ECHO          giornamenti software.
@ECHO.
@ECHO          Inoltre la variabile di ambiente AGSDEV non risulta defini-
@ECHO          ta. E' pertanto impossibile stabilire il pathname relativo
@ECHO          all'unita' che deve essere utilizzata.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP1025

SET VARX01=%AGSDEV%
GOTO STEP2000

:STEP1030

SET VARX01=%1
GOTO STEP2000

:STEP2000

REM  ######
REM  # - Controllo sulla definizione delle variabili necessarie
REM  ###

:STEP2005

IF NOT "%DSKABD%"=="" GOTO STEP2015

:STEP2010

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando AGGFDB
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKABD non risulta definita, per-
@ECHO          tanto non e' possibile stabilire l'esatta destinazione re-
@ECHO          lativa ai dati da estrarre.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2015

IF NOT "%DSKSYS%"=="" GOTO STEP2025

:STEP2020

@ECHO ----------------------------------------------------------------------
@ECHO              # Errore per il comando AGGFDB
@ECHO              --------------------------------
@ECHO.
@ECHO Errore : La variabile di ambiente DSKSYS non risulta definita, per-
@ECHO          tanto non e' possibile stabilire la sigla dell'unita' che
@ECHO          contiene il sistema.
@ECHO.
@ECHO Il comando pertanto non e' stato eseguito.
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
GOTO STEP9999

:STEP2025
GOTO STEP3000

:STEP3000

REM  ######
REM  # - Esecuzione procedura di estrazione mediante comando TAR
REM  ###

REM  #
REM  # Spostamento su directory di destinazione
REM  #

%DSKABD%:
CD \ABD\FDB

REM  #
REM  # Messaggio pre-estrazione
REM  #

CLS
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.
@ECHO [] Estrazione dati da supporto magnetico ......
@ECHO.

REM  #
REM  # Comando per l'estrazione
REM  #

TAR xvf %VARX01%

REM  #
REM  # Messaggio post-estrazione
REM  #

@ECHO.
@ECHO [] ...... Fine estrazione dati
@ECHO.
@ECHO ----------------------------------------------------------------------
@ECHO.

REM  #
REM  # Spostamento sulla directory di base del sistema
REM  #

%DSKSYS%:


REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

