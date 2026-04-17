       Identification Division.
       Program-Id.                                 pcge3020           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge302              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/09/93    *
      *                       Ultima revisione:    NdK del 28/01/20    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pcge3021:        *
      *                                                                *
      *                    Stampa brogliaccio di contabilita'          *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

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
                     "cge"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge302"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge3020  "                                     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   STAMPA BROGLIACCIO DI CONTABILITA'   "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pcge3021  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/cge/prg/obj/pcge3021                "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

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
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-tip-stp                 pic  x(01)                  .
           05  rr-dat-ini                 pic  9(07)                  .
           05  rr-dat-fin                 pic  9(07)                  .
           05  rr-cod-cau                 pic  9(03)                  .
           05  rr-cod-cau-des             pic  x(40)                  .
           05  rr-tip-arc                 pic  x(01)                  .
           05  rr-cod-arc                 pic  9(07)                  .
           05  rr-cod-ute                 pic  x(08)                  .
           05  rr-cod-fas                 pic  x(06)                  .
           05  rr-tip-rfp                 pic  x(01)                  .

      *    *===========================================================*
      *    * Area per valori precedenti                                *
      *    *-----------------------------------------------------------*
       01  w-pre.
           05  w-pre-tip-stp              pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [zcc]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcc.
               10  w-let-arc-zcc-flg      pic  x(01)                  .
               10  w-let-arc-zcc-cod      pic  9(03)                  .
               10  w-let-arc-zcc-des      pic  x(30)                  .
               10  w-let-arc-zcc-tmi      pic  x(01)                  .
               10  w-let-arc-zcc-snb      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo stampa                                           *
      *        *-------------------------------------------------------*
           05  w-sav-tip-stp              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 02   .
               10  w-exp-tip-stp-lun      pic  9(02)       value 35   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(35) value
                            "per data Registrazione             "     .
                   15  filler             pic  x(35) value
                            "per data Immissione/ultima modifica"     .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo archivio                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-arc.
               10  w-exp-tip-arc-num      pic  9(02)       value 04   .
               10  w-exp-tip-arc-lun      pic  9(02)       value 35   .
               10  w-exp-tip-arc-tbl.
                   15  filler             pic  x(35) value
                            "Nessuno                            "     .
                   15  filler             pic  x(35) value
                            "Sottoconto di generale             "     .
                   15  filler             pic  x(35) value
                            "Clienti                            "     .
                   15  filler             pic  x(35) value
                            "Fornitori                          "     .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo regime fiscale                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-rfp.
               10  w-exp-tip-rfp-num      pic  9(02)       value 05   .
               10  w-exp-tip-rfp-lun      pic  9(02)       value 35   .
               10  w-exp-tip-rfp-tbl.
                   15  filler             pic  x(35) value
                            "Nessuna selezione                  "     .
                   15  filler             pic  x(35) value
                            "normale Regime fiscale             "     .
                   15  filler             pic  x(35) value
                            "regime di Scissione Pagamenti      "     .
                   15  filler             pic  x(35) value
                            "regime di reverse Charge           "     .
                   15  filler             pic  x(35) value
                            "Esclusione da dichiarazione annuale"     .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(60)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-750.
       main-450.
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria se funzionamento  *
      *                  * in background o foreground                  *
      *                  *---------------------------------------------*
           move      "BF"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se esecuzione in foreground                 *
      *                  *---------------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
           go to     main-750.
       main-500.
      *                  *---------------------------------------------*
      *                  * Se esecuzione in background                 *
      *                  *---------------------------------------------*
           perform   exe-pgm-bkg-000      thru exe-pgm-bkg-999        .
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
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
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
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
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per foreground       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-frg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-frg-200.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Visualizzazione eventuali errori di esecuzione  *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       exe-pgm-bkg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di background *
      *              *-------------------------------------------------*
           move      "OB"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per background       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "B"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-bkg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-bkg-200.
       exe-pgm-bkg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione  background  *
      *              * tramite chiamata al modulo di segreteria        *
      *              *-------------------------------------------------*
           move      "B+"                 to   s-ope                  .
           move      i-exe-pro            to   s-npb                  .
           move      i-exe-pat            to   s-pmo                  .
           move      i-ide-des            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mmessg"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       exe-pgm-bkg-999.
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
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-opn-000  thru cod-mne-zcc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-cls-000  thru cod-mne-zcc-cls-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo stampa                                 *
      *                  *---------------------------------------------*
           perform   acc-tip-stp-000      thru acc-tip-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data finale                                 *
      *                  *---------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Causale contabile                           *
      *                  *---------------------------------------------*
           perform   acc-cod-cau-000      thru acc-cod-cau-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           perform   acc-tip-arc-000      thru acc-tip-arc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Tipo regime fiscale                         *
      *                  *---------------------------------------------*
           perform   acc-tip-rfp-000      thru acc-tip-rfp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-ute-000      thru acc-cod-ute-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           perform   acc-cod-fas-000      thru acc-cod-fas-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#CNF"               to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo stampa                                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-stp-000      thru pmt-tip-stp-999        .
      *              *-------------------------------------------------*
      *              * Literal di selezione                            *
      *              *-------------------------------------------------*
           perform   pmt-lit-sel-000      thru pmt-lit-sel-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo stampa                   *
      *    *-----------------------------------------------------------*
       pmt-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordinamento stampa     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data iniziale                 *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo stampa          *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    "R"
                     go to pmt-dat-ini-300
           else if   rr-tip-stp           =    "I"
                     go to pmt-dat-ini-600.
       pmt-dat-ini-300.
      *              *-------------------------------------------------*
      *              * Se tipo stampa : per data registrazione         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione iniziale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-600.
      *              *-------------------------------------------------*
      *              * Se tipo stampa : per data ultima modifica       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data immissione/ultima modifica iniziale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data finale                   *
      *    *-----------------------------------------------------------*
       pmt-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo stampa          *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    "R"
                     go to pmt-dat-fin-300
           else if   rr-tip-stp           =    "I"
                     go to pmt-dat-fin-600.
       pmt-dat-fin-300.
      *              *-------------------------------------------------*
      *              * Se tipo stampa : per data registrazione         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione finale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-600.
      *              *-------------------------------------------------*
      *              * Se tipo stampa : per data ultima modifica       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data immissione/ultima modifica finale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Literal di selezione          *
      *    *-----------------------------------------------------------*
       pmt-lit-sel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lit-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Causale contabile             *
      *    *-----------------------------------------------------------*
       pmt-cod-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Causale contabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo archivio                 *
      *    *-----------------------------------------------------------*
       pmt-tip-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Tipo   archivio   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice archivio               *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Codice archivio   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo regime                   *
      *    *-----------------------------------------------------------*
       pmt-tip-rfp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Tipo   regime Iva :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-rfp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice utente                 *
      *    *-----------------------------------------------------------*
       pmt-cod-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Codice utente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice fase                   *
      *    *-----------------------------------------------------------*
       pmt-cod-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        - Programma         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-tip-stp           to   w-sav-tip-stp          .
       acc-tip-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "RI#"                to   v-msk                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      31                   to   v-pos                  .
           if        rr-tip-stp           =    "R"
                     move  01             to   v-num
           else if   rr-tip-stp           =    "I"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-stp-999.
       acc-tip-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "R"            to   rr-tip-stp
           else if   v-num                =    02
                     move  "I"            to   rr-tip-stp
           else      move  spaces         to   rr-tip-stp             .
       acc-tip-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = "R" and
                     rr-tip-stp           not  = "I"
                     go to acc-tip-stp-100.
       acc-tip-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato rispetto al prece-   *
      *                  * dente                                       *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    w-sav-tip-stp
                     go to acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate da prompts date       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      09                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dei prompt per le date      *
      *                  *---------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori per le date          *
      *                  *---------------------------------------------*
           perform   vis-dat-ini-000      thru vis-dat-ini-999        .
           perform   vis-dat-fin-000      thru vis-dat-fin-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo stampa      *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    "R"
                     go to acc-tip-stp-620
           else      go to acc-tip-stp-640.
       acc-tip-stp-620.
      *                  *---------------------------------------------*
      *                  * Se tipo stampa : per data registrazione     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Erase linee impegnate da prompts        *
      *                      *-----------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           move      17                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice utente           *
      *                      *-----------------------------------------*
           move      spaces               to   rr-cod-ute             .
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice fase             *
      *                      *-----------------------------------------*
           move      spaces               to   rr-cod-fas             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per causale      *
      *                      * contabile                               *
      *                      *-----------------------------------------*
           perform   pmt-cod-cau-000      thru pmt-cod-cau-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione causale contabile e sua *
      *                      * descrizione                             *
      *                      *-----------------------------------------*
           perform   vis-cod-cau-000      thru vis-cod-cau-999        .
           perform   vis-cod-cau-des-000  thru vis-cod-cau-des-999    .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per tipo archi-  *
      *                      * vio                                     *
      *                      *-----------------------------------------*
           perform   pmt-tip-arc-000      thru pmt-tip-arc-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo archivio           *
      *                      *-----------------------------------------*
           perform   vis-tip-arc-000      thru vis-tip-arc-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per codice ar-   *
      *                      * chivio                                  *
      *                      *-----------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice archivio         *
      *                      *-----------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per tipo regime  *
      *                      *-----------------------------------------*
           perform   pmt-tip-rfp-000      thru pmt-tip-rfp-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo regime             *
      *                      *-----------------------------------------*
           perform   vis-tip-rfp-000      thru vis-tip-rfp-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-640.
      *                  *---------------------------------------------*
      *                  * Se tipo stampa : per data ultima modifica   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Erase linee impegnate da prompts        *
      *                      *-----------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           move      17                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione causale contabile e sua *
      *                      * descrizione                             *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cau             .
           move      spaces               to   rr-cod-cau-des         .
      *                      *-----------------------------------------*
      *                      * Normalizzazione tipo archivio           *
      *                      *-----------------------------------------*
           move      spaces               to   rr-tip-arc             .
      *                      *-----------------------------------------*
      *                      * Normalizzazione codice archivio         *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-arc             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per codice       *
      *                      * utente e codice fase                    *
      *                      *-----------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice utente e codice  *
      *                      * fase                                    *
      *                      *-----------------------------------------*
           perform   vis-cod-ute-000      thru vis-cod-ute-999        .
           perform   vis-cod-fas-000      thru vis-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-stp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-stp-100.
       acc-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data iniziale              *
      *    *-----------------------------------------------------------*
       acc-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      08                   to   v-lin                  .
           if        rr-tip-stp           =    "R"
                     move  31             to   v-pos
           else      move  44             to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-ini           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-ini-999.
       acc-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-ini             .
       acc-dat-ini-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data a zero non accettabile salvo che in    *
      *                  * presenza del tasto 'Up'                     *
      *                  *---------------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to acc-dat-ini-600.
           if        v-key                =    "UP  "
                     go to acc-dat-ini-600
           else      go to acc-dat-ini-100.
       acc-dat-ini-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-ini-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-ini-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-ini-100.
       acc-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Data iniziale           *
      *    *-----------------------------------------------------------*
       vis-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      08                   to   v-lin                  .
           if        rr-tip-stp           =    "R"
                     move  31             to   v-pos
           else      move  44             to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data finale                *
      *    *-----------------------------------------------------------*
       acc-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           if        rr-tip-stp           =    "R"
                     move  31             to   v-pos
           else      move  44             to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-fin           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-fin-999.
       acc-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-fin             .
       acc-dat-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data a zero non accettabile salvo che in    *
      *                  * presenza del tasto 'Up'                     *
      *                  *---------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to acc-dat-fin-600.
           if        v-key                =    "UP  "
                     go to acc-dat-fin-600
           else      go to acc-dat-fin-100.
       acc-dat-fin-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-fin-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-fin-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-fin-100.
       acc-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Data finale             *
      *    *-----------------------------------------------------------*
       vis-dat-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           if        rr-tip-stp           =    "R"
                     move  31             to   v-pos
           else      move  44             to   v-pos                  .
           move      rr-dat-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Causale contabile          *
      *    *-----------------------------------------------------------*
       acc-cod-cau-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "R"
                     go to acc-cod-cau-999.
       acc-cod-cau-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
           move      rr-cod-cau           to   w-cod-mne-zcc-cod      .
           move      15                   to   w-cod-mne-zcc-lin      .
           move      31                   to   w-cod-mne-zcc-pos      .
           move      15                   to   w-cod-mne-zcc-dln      .
           move      41                   to   w-cod-mne-zcc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
       acc-cod-cau-110.
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           if        w-cod-mne-zcc-ope    =    "F+"
                     go to acc-cod-cau-115.
           if        w-cod-mne-zcc-ope    =    "AC"
                     go to acc-cod-cau-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cau-115.
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
           go to     acc-cod-cau-110.
       acc-cod-cau-120.
           move      w-cod-mne-zcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cau-999.
       acc-cod-cau-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-cau             .
       acc-cod-cau-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella causali                     *
      *                  *---------------------------------------------*
           move      rr-cod-cau           to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione causale          *
      *                  *---------------------------------------------*
           move      w-let-arc-zcc-des    to   rr-cod-cau-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione causale         *
      *                  *---------------------------------------------*
           perform   vis-cod-cau-des-000  thru vis-cod-cau-des-999    .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-flg    not  = spaces
                     go to acc-cod-cau-100.
       acc-cod-cau-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cau-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cau-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cau-100.
       acc-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Causale contabile       *
      *    *-----------------------------------------------------------*
       vis-cod-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rr-cod-cau           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descrizione causale     *
      *    *                                   contabile               *
      *    *-----------------------------------------------------------*
       vis-cod-cau-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cau-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cau-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo archivio                        *
      *    *-----------------------------------------------------------*
       acc-tip-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "R"
                     go to acc-tip-arc-999.
       acc-tip-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-arc-lun    to   v-car                  .
           move      w-exp-tip-arc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "NSCF#"              to   v-msk                  .
           move      w-exp-tip-arc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      16                   to   v-lin                  .
           move      31                   to   v-pos                  .
      *
           if        rr-tip-arc           =    spaces
                     move  01             to   v-num
           else if   rr-tip-arc           =    "G"
                     move  02             to   v-num
           else if   rr-tip-arc           =    "C"
                     move  03             to   v-num
           else if   rr-tip-arc           =    "F"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-arc-999.
       acc-tip-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   rr-tip-arc
           else if   v-num                =    02
                     move  "G"            to   rr-tip-arc
           else if   v-num                =    03
                     move  "C"            to   rr-tip-arc
           else if   v-num                =    04
                     move  "F"            to   rr-tip-arc
           else      move  spaces         to   rr-tip-arc             .
       acc-tip-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione codice archivio   *
      *                  *---------------------------------------------*
           if        rr-tip-arc           not  = spaces
                     go to acc-tip-arc-800.
           move      zero                 to   rr-cod-arc             .
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
       acc-tip-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-arc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-arc-100.
       acc-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo archivio                     *
      *    *-----------------------------------------------------------*
       vis-tip-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-arc-lun    to   v-car                  .
           move      w-exp-tip-arc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-exp-tip-arc-tbl    to   v-txt                  .
      *
           if        rr-tip-arc           =    spaces
                     move  01             to   v-num
           else if   rr-tip-arc           =    "G"
                     move  02             to   v-num
           else if   rr-tip-arc           =    "C"
                     move  03             to   v-num
           else if   rr-tip-arc           =    "F"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice archivio                      *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "R"
                     go to acc-cod-arc-999.
           if        rr-tip-arc           =    spaces
                     go to acc-cod-arc-999.
       acc-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-arc           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-arc-999.
       acc-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-arc             .
       acc-cod-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-arc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-arc-100.
       acc-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio                   *
      *    *-----------------------------------------------------------*
       vis-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo regime                          *
      *    *-----------------------------------------------------------*
       acc-tip-rfp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "R"
                     go to acc-tip-rfp-999.
       acc-tip-rfp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-rfp-lun    to   v-car                  .
           move      w-exp-tip-rfp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "NRSCE#"             to   v-msk                  .
           move      w-exp-tip-rfp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      18                   to   v-lin                  .
           move      31                   to   v-pos                  .
      *
           if        rr-tip-rfp           =    spaces
                     move  01             to   v-num
           else if   rr-tip-rfp           =    "N"
                     move  02             to   v-num
           else if   rr-tip-rfp           =    "S"
                     move  03             to   v-num
           else if   rr-tip-rfp           =    "R"
                     move  04             to   v-num
           else if   rr-tip-rfp           =    "E"
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-rfp-999.
       acc-tip-rfp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   rr-tip-rfp
           else if   v-num                =    02
                     move  "N"            to   rr-tip-rfp
           else if   v-num                =    03
                     move  "S"            to   rr-tip-rfp
           else if   v-num                =    04
                     move  "R"            to   rr-tip-rfp
           else if   v-num                =    05
                     move  "E"            to   rr-tip-rfp
           else      move  spaces         to   rr-tip-rfp             .
       acc-tip-rfp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-rfp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-rfp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-rfp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-rfp-100.
       acc-tip-rfp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo regime                       *
      *    *-----------------------------------------------------------*
       vis-tip-rfp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-rfp-lun    to   v-car                  .
           move      w-exp-tip-rfp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-exp-tip-rfp-tbl    to   v-txt                  .
      *
           if        rr-tip-rfp           =    spaces
                     move  01             to   v-num
           else if   rr-tip-rfp           =    "N"
                     move  02             to   v-num
           else if   rr-tip-rfp           =    "S"
                     move  03             to   v-num
           else if   rr-tip-rfp           =    "R"
                     move  04             to   v-num
           else if   rr-tip-rfp           =    "E"
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-rfp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice utente              *
      *    *-----------------------------------------------------------*
       acc-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "I"
                     go to acc-cod-ute-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-cod-ute           not  = spaces
                     go to acc-cod-ute-100.
      *                  *---------------------------------------------*
      *                  * Informazioni generali dalla segreteria      *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-ute                to   rr-cod-ute             .
       acc-cod-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-ute           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-ute-999.
       acc-cod-ute-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-ute             .
       acc-cod-ute-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-ute-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-ute-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-ute-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-ute-100.
       acc-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Codice utente           *
      *    *-----------------------------------------------------------*
       vis-cod-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rr-cod-ute           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice fase                *
      *    *-----------------------------------------------------------*
       acc-cod-fas-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "I"
                     go to acc-cod-fas-999.
       acc-cod-fas-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-fas           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fas-999.
       acc-cod-fas-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-fas             .
       acc-cod-fas-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-fas-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fas-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fas-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fas-100.
       acc-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Codice fase             *
      *    *-----------------------------------------------------------*
       vis-cod-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rr-cod-fas           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo su data iniziale                      *
      *              *-------------------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca la data iniziale !                          
      -              "          "         to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su data finale                        *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to tdo-ric-sel-300.
           move      "Manca la data finale !                            
      -              "          "         to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data iniziale e finale             *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  < rr-dat-ini
                     go to tdo-ric-sel-400.
           move      "La data finale non puo' essere inferiore a quella 
      -              "iniziale !"         to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su tipo archivio                      *
      *              *-------------------------------------------------*
           if        rr-tip-arc           =    spaces
                     go to tdo-ric-sel-500.
           if        rr-cod-arc           not  = zero
                     go to tdo-ric-sel-500.
           move      "Indicare un codice archivio!                      
      -              "          "         to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo su codice archivio                    *
      *              *-------------------------------------------------*
           if        rr-tip-arc           not  = spaces
                     go to tdo-ric-sel-800.
           if        rr-cod-arc           =    zero
                     go to tdo-ric-sel-800.
           move      "Indicare un tipo archivio!                        
      -              "          "         to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio                   *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-tip-stp             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-cod-cau             .
           move      spaces               to   rr-cod-cau-des         .
           move      spaces               to   rr-tip-arc             .
           move      zero                 to   rr-cod-arc             .
           move      spaces               to   rr-tip-rfp             .
           move      spaces               to   rr-cod-ute             .
           move      spaces               to   rr-cod-fas             .
       nor-ric-sel-999.
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
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zcc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zcc-cod    =    zero
                     go to let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zcc-cod    to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcc-400.
       let-arc-zcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcc-des-cau       to   w-let-arc-zcc-des      .
           move      rf-zcc-tip-moi       to   w-let-arc-zcc-tmi      .
           move      rf-zcc-snx-bil       to   w-let-arc-zcc-snb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcc-999.
       let-arc-zcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcc-flg      .
           move      all   "."            to   w-let-arc-zcc-des      .
           go to     let-arc-zcc-600.
       let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-des      .
       let-arc-zcc-600.
           move      spaces               to   w-let-arc-zcc-tmi      .
           move      spaces               to   w-let-arc-zcc-snb      .
       let-arc-zcc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione della causale contabile    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acs"                   .
