@ECHO OFF

REM  ############ /ABD/ETC/ALTRBOOT.BAT
REM  #
REM  # Procedura di boot per l' ambiente TANGRAM
REM  #
REM  ############

:STEP1000

REM  ######
REM  # - Rimozione eventuali files temporanei residui
REM  ###
IF NOT EXIST %DSKABD%\ABD\OGT\SWD\XPG\PRG\OBJ\PXPG0000 GOTO STEP2000

FOR %%F IN (%DSKABD%\ABD\TMP\*.*) DO DEL %%F

:STEP2000

REM  ######
REM  # - Rimozione eventuali files di spool residui
REM  ###
IF NOT EXIST %DSKABD%\ABD\OGT\SWD\XPG\PRG\OBJ\PXPG0000 GOTO STEP3000

FOR %%F IN (%DSKABD%\ABD\SPL\*.*) DO DEL %%F

:STEP3000

REM  ######
REM  # - Rimozione eventuali files di sort residui
REM  ###
IF NOT EXIST %DSKABD%\ABD\OGT\SWD\XPG\PRG\OBJ\PXPG0000 GOTO STEP4000

FOR %%F IN (%DSKABD%\ABD\SRT\*.*) DO DEL %%F

:STEP4000

REM  ######
REM  # - Fine procedura
REM  ###

:STEP9999
