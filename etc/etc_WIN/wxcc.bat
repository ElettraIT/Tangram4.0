@ECHO OFF

REM  ############ /ABD/ETC/WXCC.BAT
REM  #
REM  # Procedura per la compilazione
REM  #
REM  ######    

IF '%1'=='' GOTO STEP0010
IF '%4'=='' GOTO STEP1000

:STEP0010

ECHO +=====================================================================+
ECHO "          # Errore nei parametri per il comando WXCC #               "
ECHO "          --------------------------------------------               "
ECHO "                                                                     "
ECHO " Errore : Sono richiesti 3 parametri per il comando :                "
ECHO "          SSS : Sistema applicativo                                  "
ECHO "          AAA : Area gestionale                                      "
ECHO "          FFF : Fase gestionale                                      "
ECHO "                                                                     "
ECHO "          Esempio : wxcc swd xpg pxpg0001                            "
ECHO "                                                                     "
ECHO "          N.B.    : In caso di errore ricordarsi 'altrinit' !!!      "
ECHO "====================================================================="
GOTO STEP9999

:STEP1000

REM  ######
REM  # - Posizionamento sulla directory di base per programmi sorgenti
REM  ###

SET ABDSGE=%1
SET ABDAGE=%2
SET ABDPRG=%3
SET ABDRUN=C:\ABD\RUN
SET ABDOGT=C:\ABD\OGT\%1\%2\PRG\OBJ

CD C:\ABD\SRG

REM  ######
REM  # - Esecuzione compilatore
REM  ###

%ABDRUN%\ccbl386 -o %ABDOGT%\%ABDPRG% -Ta 32768 -Tb 8192 -Z20 -Zl -Sa %1\%2\PRG\CBL\%ABDPRG%.cbl

REM  ######
REM  # - Spostamento directory etc
REM  ###

CD C:\ABD\ETC

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

