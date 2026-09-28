@ECHO OFF

REM  ############ /ABD/ETC/COPYDIR.BAT
REM  #
REM  # Procedura TANGRAM per la duplicazione dei contenuti di una directory
REM  #
REM  # Nota  : Directory origine e destinazione devono preesistere
REM  #
REM  # Input : %1 = Pathname completo directory di origine
REM  #
REM  # Input : %2 = Pathname completo directory di destinazione
REM  #
REM  ######    

REM  ######
REM  # - Controllo sul numero di parametri
REM  ###

IF '%1'=='' GOTO STEP0010
IF '%2'=='' GOTO STEP0010
IF '%3'=='' GOTO STEP1000

:STEP0010

ECHO +=====================================================================+
ECHO "         # Errore nei parametri per il comando COPYDIR #             "
ECHO "         -----------------------------------------------             "
ECHO "                                                                     "
ECHO " Errore : Sono richiesti due parametri                               "
ECHO "                                                                     "
ECHO "          1. parametro : Pathname della directory di origine         "
ECHO "                                                                     "
ECHO "          2. parametro : Pathname della directory di destinazione    "
ECHO "                                                                     "
ECHO "          Non e' ammesso omettere nessuno dei due parametri,         "
ECHO "          ne' fornire piu' di due parametri.                         "
ECHO "====================================================================="
GOTO STEP9999

:STEP1000

REM  ######
REM  # - Esecuzione del comando vero e proprio di copiatura
REM  ###

XCOPY %1 %2 /E/S/V > NUL

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
