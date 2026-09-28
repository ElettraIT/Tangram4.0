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

%DSKABD%
CD \ABD\SRG\

REM  ######
REM  # - Esecuzione compilatore
REM  ###

ccbl386 -o D:\abd\tmp\pbol3000 -Ta 32768 -Tb 8192 -Z20 -Zl -Za -x -Li -Sa pgm\bol\prg\cbl\pbol3000.cbl

REM  ######
REM  # - Spostamento oggetto generato
REM  ###

MOVE D:\abd\tmp\pbol3000 D:\abd\ogt\pgm\bol\prg\obj\pbol3000 > D:\abd\asc\RISULT

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

