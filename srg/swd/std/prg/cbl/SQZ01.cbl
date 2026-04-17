       Identification Division.
       Program-Id.                                 psqz___0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    sqz                 *
      *                                Settore:    ___                 *
      *                                   Fase:    ______              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del __/__/__    *
      *                       Ultima revisione:    NdK del __/__/__    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Sequenzializzazione archivio [___]          *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Nota : Questo programma non ha un corrispondente comando per   *
      *        l'esecuzione. Deve essere lanciato per mezzo della pri- *
      *        mitiva di menu' '.x'. Cio' lo rende eseguibile solo da  *
      *        un utente di tipo supervisore.                          *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

       Special-Names.       Decimal-Point is comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "sqz"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "___"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "______"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "p______n"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      SEQUENZIALIZZAZIONE ARCHIVIO      "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine acc-prm-sqz-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-acc-prm-sqz      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine cnt-tdo-fix-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-fix-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine cnt-tdo-ops-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ops-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine sqz-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sqz-rou-pri      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per padding campi alfanumerici con 'z'               *
      *    *-----------------------------------------------------------*
       01  w-pad-zzz.
           05  w-pad-zzz-alf.
               10  w-pad-zzz-alf-chr occurs 20
                                          pic  x(01)                  .
               10  w-pad-zzz-alf-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [___]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/___/fls/rec/rf___"                          .

      *    *===========================================================*
      *    * Work per accettazione parametri di sequenzializzazione    *
      *    *-----------------------------------------------------------*
       01  w-acc-sqz.
      *        *-------------------------------------------------------*
      *        * Parametri fissi                                       *
      *        *-------------------------------------------------------*
           05  w-acc-sqz-fix.
      *            *---------------------------------------------------*
      *            * Tipo operazione                                   *
      *            * - 01 : Da indice a sequenziale                    *
      *            * - 02 : Da sequenziale ad indice                   *
      *            *---------------------------------------------------*
               10  w-acc-sqz-fix-top      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Pathname del file sequenziale                     *
      *            *---------------------------------------------------*
               10  w-acc-sqz-fix-phs      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sostituzione elementi gia' presenti nell'indice   *
      *            * - 01 : No                                         *
      *            * - 02 : Si                                         *
      *            *---------------------------------------------------*
               10  w-acc-sqz-fix-rpx      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Accodamento su archivio sequenziale               *
      *            * - 01 : No, creazione di un sequenziale nuovo      *
      *            * - 02 : Si, in coda all'archivio sequenziale       *
      *            *---------------------------------------------------*
               10  w-acc-sqz-fix-ext      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Si/No selezioni ed opzioni                        *
      *            * - 01 : No                                         *
      *            * - 02 : Si                                         *
      *            *---------------------------------------------------*
               10  w-acc-sqz-fix-seo      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Parametri specifici                                   *
      *        *-------------------------------------------------------*
           05  w-acc-sqz-spc.
      *            *---------------------------------------------------*
      *            * Sigla del file                                    *
      *            *---------------------------------------------------*
               10  w-acc-sqz-spc-sgf      pic  x(04) value     "____" .
      *            *---------------------------------------------------*
      *            * Si/No selezioni/opzioni di sequenzializzazione    *
      *            *---------------------------------------------------*
               10  w-acc-sqz-spc-opz      pic  x(01) value     "_"    .
      *            *---------------------------------------------------*
      *            * Coordinate per il box per selezioni e/o opzioni   *
      *            *---------------------------------------------------*
               10  w-acc-sqz-spc-xbx.
      *                *-----------------------------------------------*
      *                * Linea superiore                               *
      *                *-----------------------------------------------*
                   15  w-acc-sqz-spc-xul pic  9(02) value      __     .
      *                *-----------------------------------------------*
      *                * Posizione sinistra                            *
      *                *-----------------------------------------------*
                   15  w-acc-sqz-spc-xlp pic  9(02) value      __     .
      *                *-----------------------------------------------*
      *                * Linea inferiore                               *
      *                *-----------------------------------------------*
                   15  w-acc-sqz-spc-xdl pic  9(02) value      __     .
      *                *-----------------------------------------------*
      *                * Posizione destra                              *
      *                *-----------------------------------------------*
                   15  w-acc-sqz-spc-xrp pic  9(02) value      __     .
      *        *-------------------------------------------------------*
      *        * Opzioni e selezioni                                   *
      *        *-------------------------------------------------------*
           05  w-acc-sqz-ops.
      *            *---------------------------------------------------*
      *            * _______                                           *
      *            *---------------------------------------------------*
               10  w-acc-sqz-ops-___      pic  _(__)                  .

      *        *-------------------------------------------------------*
      *        * [___]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/___/fls/rec/rf___"                          .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo operazione                            *
      *        *-------------------------------------------------------*
           05  w-exp-fix-top.
               10  w-exp-fix-top-num      pic  9(02)       value 2    .
               10  w-exp-fix-top-lun      pic  9(02)       value 40   .
               10  w-exp-fix-top-tbl.
                   15  filler             pic  x(40) value
                            "Trasferimento da indice a sequenziale   ".
                   15  filler             pic  x(40) value
                            "Trasferimento da sequenziale ad indice  ".
      *        *-------------------------------------------------------*
      *        * Work per : Sostituzione gia' presenti                 *
      *        *-------------------------------------------------------*
           05  w-exp-fix-rpx.
               10  w-exp-fix-rpx-num      pic  9(02)       value 3    .
               10  w-exp-fix-rpx-lun      pic  9(02)       value 02   .
               10  w-exp-fix-rpx-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Accodamento sequenziale                    *
      *        *-------------------------------------------------------*
           05  w-exp-fix-ext.
               10  w-exp-fix-ext-num      pic  9(02)       value 2    .
               10  w-exp-fix-ext-lun      pic  9(02)       value 40   .
               10  w-exp-fix-ext-tbl.
                   15  filler             pic  x(40) value
                            "No, creazione di un sequenziale nuovo   ".
                   15  filler             pic  x(40) value
                            "Si, in coda all'archivio sequenziale    ".
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su determinati records           *
      *        *-------------------------------------------------------*
           05  w-exp-fix-seo.
               10  w-exp-fix-seo-num      pic  9(02)       value 3    .
               10  w-exp-fix-seo-lun      pic  9(02)       value 02   .
               10  w-exp-fix-seo-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Preparazione titolo programma                   *
      *              *-------------------------------------------------*
           perform   pre-tit-pgm-000      thru pre-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
       main-200.
      *              *-------------------------------------------------*
      *              * Accettazione parametri                          *
      *              *-------------------------------------------------*
           perform   acc-prm-sqz-000      thru acc-prm-sqz-999        .
      *              *-------------------------------------------------*
      *              * Se parametri non confermati : uscita            *
      *              *-------------------------------------------------*
           if        w-cnt-acc-prm-sqz    not  = "S"
                     go to main-800.
       main-400.
      *              *-------------------------------------------------*
      *              * Esecuzione sequenzializzazione                  *
      *              *-------------------------------------------------*
           perform   sqz-rou-pri-000      thru sqz-rou-pri-999        .
      *              *-------------------------------------------------*
      *              * Se terminazione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-sqz-rou-pri    not  = spaces
                     go to main-800.
       main-600.
      *              *-------------------------------------------------*
      *              * Riciclo ad accettazione parametri               *
      *              *-------------------------------------------------*
           go to     main-200.
       main-800.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Preparazione titolo programma                             *
      *    *-----------------------------------------------------------*
       pre-tit-pgm-000.
           move      spaces               to   i-ide-des              .
           string    "   SEQUENZIALIZZAZIONE ARCHIVIO ["
                                delimited by   size
                     w-acc-sqz-spc-sgf
                                delimited by   spaces
                     "]"        delimited by   size
                                          into i-ide-des              .
       pre-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione, No controllo riservatezza       *
      *              *-------------------------------------------------*
           move      "Q+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione parametri per sequenzializzazione            *
      *    *-----------------------------------------------------------*
       acc-prm-sqz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-prm-sqz      .
       acc-prm-sqz-010.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
       acc-prm-sqz-020.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri fissi                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione : da indice a sequenziale   *
      *                  *---------------------------------------------*
           move      01                   to   w-acc-sqz-fix-top      .
      *                  *---------------------------------------------*
      *                  * Pathname del file sequenziale : su 'tmp'    *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-sqz-fix-phs      .
           string    "tmp/"     delimited by   size
                     w-acc-sqz-spc-sgf
                                delimited by   spaces
                                          into w-acc-sqz-fix-phs      .
      *                  *---------------------------------------------*
      *                  * Sostituzione elementi gia' presenti nell'-  *
      *                  * indice : zero, non significativa            *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-sqz-fix-rpx      .
      *                  *---------------------------------------------*
      *                  * Accodamento su archivio sequenziale : no,   *
      *                  * creazione di un sequenziale nuovo           *
      *                  *---------------------------------------------*
           move      01                   to   w-acc-sqz-fix-ext      .
      *                  *---------------------------------------------*
      *                  * Selezione e/o opzioni : no                  *
      *                  *---------------------------------------------*
           move      01                   to   w-acc-sqz-fix-seo      .
       acc-prm-sqz-030.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per parametri fissi e   *
      *              * valori di default per gli stessi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per tipo operazione                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione da eseguire      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore per tipo operazione                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-top-lun    to   v-car                  .
           move      w-exp-fix-top-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-top-tbl    to   v-txt                  .
           move      05                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-top    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per pathname file sequenziale        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pathname del file sequenziale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore per pathname file sequenziale        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-phs    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per sostituzione elementi gia' pre-  *
      *                  * senti nell'indice                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sostituzione degli elementi gia' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "           presenti nell'indice   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore per sostituzione elementi gia' pre-  *
      *                  * senti nell'indice                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-rpx-lun    to   v-car                  .
           move      w-exp-fix-rpx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-rpx-tbl    to   v-txt                  .
           move      09                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-rpx    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per accodamento su sequenziale       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "In coda all'archivio sequenziale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore per accodamento su sequenziale       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-ext-lun    to   v-car                  .
           move      w-exp-fix-ext-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-ext-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-ext    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per selezioni e/o opzioni            *
      *                  *---------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to acc-prm-sqz-032.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni e/o opzioni            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prm-sqz-032.
      *                  *---------------------------------------------*
      *                  * Valore per selezioni e/o opzioni            *
      *                  *---------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to acc-prm-sqz-034.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-seo-lun    to   v-car                  .
           move      w-exp-fix-seo-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-seo-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-seo    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prm-sqz-034.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prm-sqz-040.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri per selezioni/opzioni *
      *              *-------------------------------------------------*
           perform   nor-prm-opz-000      thru nor-prm-opz-999        .
       acc-prm-sqz-100.
      *              *-------------------------------------------------*
      *              * Accettazione parametri fissi                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           perform   acc-fix-top-000      thru acc-fix-top-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
       acc-prm-sqz-150.
      *                  *---------------------------------------------*
      *                  * Pathname del file sequenziale               *
      *                  *---------------------------------------------*
           perform   acc-fix-phs-000      thru acc-fix-phs-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
           if        v-key                =    "UP  "
                     go to acc-prm-sqz-100.
       acc-prm-sqz-200.
      *                  *---------------------------------------------*
      *                  * Sostituzione elementi gia' presenti nel-    *
      *                  * l'indice                                    *
      *                  *---------------------------------------------*
           perform   acc-fix-rpx-000      thru acc-fix-rpx-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
           if        v-key                =    "UP  "
                     go to acc-prm-sqz-150.
       acc-prm-sqz-250.
      *                  *---------------------------------------------*
      *                  * Accodamento su archivio sequenziale         *
      *                  *---------------------------------------------*
           perform   acc-fix-ext-000      thru acc-fix-ext-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
           if        v-key                =    "UP  "
                     go to acc-prm-sqz-200.
       acc-prm-sqz-300.
      *                  *---------------------------------------------*
      *                  * Si/No selezioni e/o opzioni                 *
      *                  *---------------------------------------------*
           perform   acc-fix-seo-000      thru acc-fix-seo-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
           if        v-key                =    "UP  "
                     go to acc-prm-sqz-250.
       acc-prm-sqz-600.
      *              *-------------------------------------------------*
      *              * Accettazione selezioni e/o opzioni              *
      *              *-------------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to acc-prm-sqz-600.
           if        w-acc-sqz-fix-seo    not  = 02
                     go to acc-prm-sqz-600.
           perform   acc-prm-opz-000      thru acc-prm-opz-999        .
           if        w-cnt-acc-prm-sqz    not  = spaces
                     go to acc-prm-sqz-999.
       acc-prm-sqz-800.
      *              *-------------------------------------------------*
      *              * Conferma parametri                              *
      *              *-------------------------------------------------*
       acc-prm-sqz-900.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-prm-sqz-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-prm-sqz-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-prm-sqz-930
           else if   v-key                =    "EXIT"
                     go to acc-prm-sqz-940
           else if   v-key                =    "UP  "
                     go to acc-prm-sqz-950
           else      go to acc-prm-sqz-900.
       acc-prm-sqz-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-prm-sqz-900
           else      move  "S"            to   w-cnt-acc-prm-sqz
                     go to acc-prm-sqz-999.
       acc-prm-sqz-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-prm-sqz      .
           go to     acc-prm-sqz-999.
       acc-prm-sqz-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazione parametri fissi         *
      *                      *-----------------------------------------*
           go to     acc-prm-sqz-100.
       acc-prm-sqz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo operazione                      *
      *    *-----------------------------------------------------------*
       acc-fix-top-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fix-top-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-top-lun    to   v-car                  .
           move      w-exp-fix-top-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-top-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      05                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-top    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-prm-sqz
                     go to acc-fix-top-999.
       acc-fix-top-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sqz-fix-top      .
       acc-fix-top-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo su ammissibilita' del valore      *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 01 and
                     w-acc-sqz-fix-top    not  = 02
                     go to acc-fix-top-100.
       acc-fix-top-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore impostato   *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    =    01
                     go to acc-fix-top-620
           else if   w-acc-sqz-fix-top    =    02
                     go to acc-fix-top-660
           else      go to acc-fix-top-100.
       acc-fix-top-620.
      *                  *---------------------------------------------*
      *                  * Se da indice a sequenziale                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di :  *
      *                      * - Sostituzione degli elementi gia' pre- *
      *                      *   senti nell'indice                     *
      *                      *-----------------------------------------*
           if        w-acc-sqz-fix-rpx    =    zero
                     go to acc-fix-top-622.
           move      zero                 to   w-acc-sqz-fix-rpx      .
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-rpx-lun    to   v-car                  .
           move      w-exp-fix-rpx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-rpx-tbl    to   v-txt                  .
           move      09                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-rpx    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-fix-top-622.
           go to     acc-fix-top-800.
       acc-fix-top-660.
      *                  *---------------------------------------------*
      *                  * Se da sequenziale a indice                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di :  *
      *                      * - Accodamento su sequenziale            *
      *                      *-----------------------------------------*
           if        w-acc-sqz-fix-ext    =    zero
                     go to acc-fix-top-662.
           move      zero                 to   w-acc-sqz-fix-ext      .
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-ext-lun    to   v-car                  .
           move      w-exp-fix-ext-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-ext-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-ext    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-fix-top-662.
           go to     acc-fix-top-800.
       acc-fix-top-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fix-top-999.
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-fix-top-100.
           move      "S"                  to   w-cnt-acc-prm-sqz      .
       acc-fix-top-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pathname sequenziale                 *
      *    *-----------------------------------------------------------*
       acc-fix-phs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fix-phs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      07                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-phs    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-prm-sqz
                     go to acc-fix-phs-999.
       acc-fix-phs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-sqz-fix-phs      .
       acc-fix-phs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo su ammissibilita' del valore      *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-phs    =    spaces
                     go to acc-fix-phs-100.
       acc-fix-phs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fix-phs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fix-phs-999.
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-fix-phs-100.
           move      "S"                  to   w-cnt-acc-prm-sqz      .
       acc-fix-phs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Sostituzione gia' presenti           *
      *    *-----------------------------------------------------------*
       acc-fix-rpx-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 02
                     go to acc-fix-rpx-999.
       acc-fix-rpx-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-rpx-lun    to   v-car                  .
           move      w-exp-fix-rpx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-rpx-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      09                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-rpx    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-prm-sqz
                     go to acc-fix-rpx-999.
       acc-fix-rpx-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sqz-fix-rpx      .
       acc-fix-rpx-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo su ammissibilita' del valore      *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-rpx    not  = 01 and
                     w-acc-sqz-fix-rpx    not  = 02
                     go to acc-fix-rpx-100.
       acc-fix-rpx-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fix-rpx-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fix-rpx-999.
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-fix-rpx-100.
           move      "S"                  to   w-cnt-acc-prm-sqz      .
       acc-fix-rpx-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Accodamento su sequenziale           *
      *    *-----------------------------------------------------------*
       acc-fix-ext-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 01
                     go to acc-fix-ext-999.
       acc-fix-ext-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-ext-lun    to   v-car                  .
           move      w-exp-fix-ext-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-ext-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-ext    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-prm-sqz
                     go to acc-fix-ext-999.
       acc-fix-ext-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sqz-fix-ext      .
       acc-fix-ext-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo su ammissibilita' del valore      *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-ext    not  = 01 and
                     w-acc-sqz-fix-ext    not  = 02
                     go to acc-fix-ext-100.
       acc-fix-ext-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fix-ext-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fix-ext-999.
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-fix-ext-100.
           move      "S"                  to   w-cnt-acc-prm-sqz      .
       acc-fix-ext-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Selezioni e/o opzioni                *
      *    *-----------------------------------------------------------*
       acc-fix-seo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to acc-fix-seo-999.
       acc-fix-seo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fix-seo-lun    to   v-car                  .
           move      w-exp-fix-seo-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-fix-seo-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-acc-sqz-fix-seo    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-prm-sqz
                     go to acc-fix-seo-999.
       acc-fix-seo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sqz-fix-seo      .
       acc-fix-seo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo su ammissibilita' del valore      *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-seo    not  = 01 and
                     w-acc-sqz-fix-seo    not  = 02
                     go to acc-fix-seo-100.
       acc-fix-seo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se no selezioni ed opzioni : normalizzazio- *
      *                  * ne opzioni e/o selezioni                    *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-seo    =    02
                     go to acc-fix-seo-800.
           perform   nor-prm-opz-000      thru nor-prm-opz-999        .
       acc-fix-seo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fix-seo-999.
           perform   cnt-tdo-fix-000      thru cnt-tdo-fix-999        .
           if        w-cnt-tdo-fix-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-fix-flg
                     go to acc-fix-seo-100.
           move      "S"                  to   w-cnt-acc-prm-sqz      .
       acc-fix-seo-999.
           exit.

      *    *===========================================================*
      *    * Controllo tasto Do su parametri fissi                     *
      *    *-----------------------------------------------------------*
       cnt-tdo-fix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-fix-flg      .
       cnt-tdo-fix-100.
      *              *-------------------------------------------------*
      *              * Tests di controllo su parametri fissi           *
      *              *-------------------------------------------------*
       cnt-tdo-fix-125.
      *                  *---------------------------------------------*
      *                  * Test su tipo operazione                     *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 01 and
                     w-acc-sqz-fix-top    not  = 02
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-150.
      *                  *---------------------------------------------*
      *                  * Test su pathname del file sequenziale       *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-phs    =    spaces
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-150.
      *                  *---------------------------------------------*
      *                  * Test su sostituzione elementi gia' presenti *
      *                  * nell'indice                                 *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 02
                     go to cnt-tdo-fix-175.
           if        w-acc-sqz-fix-rpx    not  = 01 and
                     w-acc-sqz-fix-rpx    not  = 02
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-175.
      *                  *---------------------------------------------*
      *                  * Test su accodamento su sequenziale          *
      *                  *---------------------------------------------*
           if        w-acc-sqz-fix-top    not  = 01
                     go to cnt-tdo-fix-200.
           if        w-acc-sqz-fix-ext    not  = 01 and
                     w-acc-sqz-fix-ext    not  = 02
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-200.
      *                  *---------------------------------------------*
      *                  * Test su selezioni e/o opzioni               *
      *                  *---------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to cnt-tdo-fix-300.
           if        w-acc-sqz-fix-seo    not  = 01 and
                     w-acc-sqz-fix-seo    not  = 02
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-300.
      *              *-------------------------------------------------*
      *              * Tests di controllo su opzioni e selezioni       *
      *              *-------------------------------------------------*
           if        w-acc-sqz-spc-opz    not  = "S"
                     go to cnt-tdo-fix-400.
           perform   cnt-tdo-ops-000      thru cnt-tdo-ops-999        .
           if        w-cnt-tdo-ops-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-ops-flg
                     move  "#"            to   w-cnt-tdo-fix-flg
                     go to cnt-tdo-fix-999.
       cnt-tdo-fix-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione opzioni e selezioni            *
      *              *-------------------------------------------------*
           perform   reg-prm-ops-000      thru reg-prm-ops-999        .
       cnt-tdo-fix-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Routine principale                       *
      *    *-----------------------------------------------------------*
       sqz-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sqz-rou-pri      .
      *              *-------------------------------------------------*
      *              * Trattini di separazione per contatori           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .


      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-acc-sqz-fix-top    =    01
                     go to sqz-rou-pri-___
           else if   w-acc-sqz-fix-top    =    02
                     go to sqz-rou-pri-___
           else      move  "#"            to   w-cnt-sqz-rou-pri
                     go to sqz-rou-pri-999.
       sqz-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Se da indice a sequenziale                      *
      *              *-------------------------------------------------*










       sqz-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Se da sequenziale a indice                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open input file sequenziale                 *
      *                  *---------------------------------------------*









____Continuare da qui, copiando in parte da qry-rou-pri____





       sqz-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione selezioni / opzioni                       *
      *    *-----------------------------------------------------------*
       nor-prm-opz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri specifici             *
      *              *-------------------------------------------------*
           move      ______               to   w-acc-sqz-spc-___      .
       nor-prm-opz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione parametri per selezioni e/o opzioni          *
      *    *-----------------------------------------------------------*
       acc-prm-opz-000.
           ____
       acc-prm-opz-999.
           exit.

      *    *===========================================================*
      *    * Controllo tasto Do su selezioni e/o opzioni               *
      *    *-----------------------------------------------------------*
       cnt-tdo-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ops-flg      .
       cnt-tdo-ops-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione parametri per selezioni e/o opzioni      *
      *    *-----------------------------------------------------------*
       reg-prm-ops-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione                                *
      *              *-------------------------------------------------*
       reg-prm-ops-999.
           exit.

