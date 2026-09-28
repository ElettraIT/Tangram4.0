@ECHO OFF

REM  ############ ALTRINIT.BAT
REM  #
REM  # Procedura di inizializzazione ambiente TANGRAM
REM  #
REM  #           Questa procedura deve essere copiata all'interno
REM  #           della directory di base del sistema. 
REM  #
REM  #           Dopodiche' la copia, che sara' quella effettiva-
REM  #           mente attiva, dovra' essere editata in modo da
REM  #           corrispondere alle esigenze dell'ambiente ope-
REM  #           rativo.
REM  #
REM  #           I punti che normalmente devono essere rettifi-
REM  #           cati sono contrassegnati da tre caratteri di
REM  #           sottolineatura : ___.
REM  #
REM  #           Inoltre viene proposto un valore, che in con-
REM  #           dizioni normali potrebbe essere adeguato. In
REM  #           questo caso sara' sufficiente eliminare i tre
REM  #           caratteri di sottolineatura ed il carattere
REM  #           di spazio di separazione immediatamente se-
REM  #           guente.
REM  #
REM  #           Se il valore proposto invece non dovesse ri-
REM  #           sultare adeguato, oltre a rimuovere i carat-
REM  #           teri di sottolineatura e lo spazio di sepa-
REM  #           razione, bisognera' modificare il valore so-
REM  #           stituendolo con quanto corrisponde alle esi-
REM  #           genze.
REM  #
REM  ############


REM  ######
REM  # - Set della variabile di ambiente che indica l'unita'
REM  #   disco che contiene il sistema
REM  ###

SET DSKSYS=C:


REM  ######
REM  # - Set della variabile di ambiente che indica l'unita'
REM  #   disco che contiene la directory \ABD per il sotto-
REM  #   sistema TANGRAM
REM  ###

SET DSKABD=C:


REM  ######
REM  # - Set della variabile di ambiente che indica l'unita' che
REM  #   deve essere utilizzata dal comando TAR, per quanto ri-
REM  #   guarda gli ggiornamenti software
REM  ###

SET AGSDEV=A:\TARFIL


REM  ######
REM  # - Set della variabile di ambiente che indica l'unita' che
REM  #   deve essere utilizzata dal comando TAR, per quanto ri-
REM  #   guarda i salvataggi ed i ripristini di dati
REM  ###

SET BAKDEV=A:\TARFIL


REM  ######
REM  # - Set della variabile di ambiente che indica l'unita' che
REM  #   deve essere utilizzata come unita' floppy disc
REM  ###

SET FLPDEV=A:


REM  ######
REM  # - Rettifica della variabile di ambiente PATH
REM  ###

SET PATH=%PATH%;%DSKABD%\ABD\ETC;%DSKABD%\ABD\RUN


REM  ######
REM  # - Richiamo della procedura di boot per l'ambiente TANGRAM
REM  ###

CALL %DSKABD%\ABD\ETC\ALTRBOOT.BAT

