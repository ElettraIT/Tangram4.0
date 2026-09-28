@ECHO OFF

REM  ############ /ABD/ETC/TANGRAM.BAT
REM  #
REM  # Procedura per l'esecuzione di TANGRAM in foreground
REM  #
REM  ######    

:STEP1000

REM  ######
REM  # - Posizionamento sulla directory di base per programmi oggetto
REM  ###

%DSKABD%
CD \ABD\OGT

REM  ######
REM  # - Set della variabile per file configurazione runtime cobol
REM  ###

SET A_CONFIG=%DSKABD%\ABD\RUN\RUNCBLCF.CFG

REM  ######
REM  # - Esecuzione TANGRAM
REM  ###

SET DOS4G=quiet
%DSKABD%\ABD\RUN\WRUNCBL.EXE -c C:\ABD\RUN\RUNCBLCF.CFG  -x SWD\XPG\PRG\OBJ\PXPG0000 "%DSKABD%\ABD" "master" "CON" "vt300" "00" "DOS 0" 
REM %DSKABD%\ABD\RUN\WRUNCBL.EXE -c C:\ABD\RUN\RUNCBLCF.CFG  -x SWD\XPG\PRG\OBJ\tangram "%DSKABD%\ABD" "master" "CON" "vt300" "00" "DOS 0" 

CD \

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
