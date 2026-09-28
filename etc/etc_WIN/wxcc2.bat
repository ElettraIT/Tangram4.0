@ECHO OFF

REM  ############ /ABD/ETC/WXCC.BAT
REM  #
REM  # Procedura per la compilazione
REM  #
REM  ######    

:STEP1000

REM  ######
REM  # - Posizionamento sulla directory di base per programmi sorgenti
REM  ###

REM %DSKABD%

REM CD H:\_NEW\swd\xpg\PRG\CBL

CD I:\SRG

REM  ######
REM  # - Esecuzione compilatore
REM  #   N.B.: si usa ccbl386 perche' supporta i files grandi (pbol3000 ecc.)
REM  ###

REM  ### SET DOS4G=quiet
ccbl386 -o I:\asc\pxpg0010 -C20 -Z20 -Ta 32768 -Tb 8192 -Zl -Za -x -Li -Cr -Sa swd\xpg\prg\cbl\pxpg0010.cbl

REM  ######
REM  # - Spostamento oggetto generato
REM  ###

REM  MOVE D:\abd\tmp\pxpg0010 I:\ogt\swd\xpg\prg\obj\pxpg0010

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

