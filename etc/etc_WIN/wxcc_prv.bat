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

SET ABDSRG=C:\ABD\SRG\SEC\TNS\PRG\CBL
CD C:\ABD\run

REM  ######
REM  # - Esecuzione compilatore
REM  ###

c:\abd\run\ccbl386 -o C:\abd\tmp\ptns7000 -Z20 -Zl -Za -x -Li -Sa %ABDSRG%\ptns7000.cbl

REM  ######
REM  # - Spostamento oggetto generato
REM  ###

MOVE C:\abd\tmp\ptns7000 C:\abd\ogt\sec\tns\prg\obj\ptns7000 > C:\abd\asc\RISULT
CD C:\ABD\ETC

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999

