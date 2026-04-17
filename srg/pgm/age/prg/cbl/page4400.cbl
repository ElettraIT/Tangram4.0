       Identification Division.
       Program-Id.                                 page4400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    mat                 *
      *                                   Fase:    age440              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/05/94    *
      *                       Ultima revisione:    NdK del 03/07/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma page4401:        *
      *                                                                *
      *                    Stampa elenco delle provvigioni maturate di *
      *                    un periodo, per agente                      *
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
                     "age"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mat"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "age440"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page4400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    STAMPA MATURAZIONI DI UN PERIODO    "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "page4401  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/age/prg/obj/page4401                "       .

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
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data iniziale del periodo                             *
      *        *-------------------------------------------------------*
           05  rr-dat-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data finale del periodo                               *
      *        *-------------------------------------------------------*
           05  rr-dat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di maturazione min                               *
      *        *-------------------------------------------------------*
           05  rr-mtz-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di maturazione max                               *
      *        *-------------------------------------------------------*
           05  rr-mtz-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente                                         *
      *        *  - Se codice zero : Tutti gli agenti                  *
      *        *-------------------------------------------------------*
           05  rr-cod-age                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, nominativo                             *
      *        *  - Se codice zero : 'Tutti gli agenti'                *
      *        *-------------------------------------------------------*
           05  rr-cod-age-nom             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, ragione sociale                        *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, via                                    *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-via             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, localita'                              *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per agenti                        *
      *        *  - 01 : Ordinamento per nominativo agente             *
      *        *  - 02 : Ordinamento per codice agente                 *
      *        *  - 03 : Ordinamento per mnemonico agente              *
      *        *                                                       *
      *        * Non significativo se selezionato un solo agente       *
      *        *-------------------------------------------------------*
           05  rr-tor-age                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di uso della stampa                              *
      *        *  - 01 : Ad uso interno, con stampa quindi dei riferi- *
      *        *         menti interni                                 *
      *        *  - 02 : Da consegnare agli agenti, quindi senza l'e-  *
      *        *         videnziazione dei riferimenti interni         *
      *        *-------------------------------------------------------*
           05  rr-tip-uso                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni generali                      *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Data di maturazione                                   *
      *        *-------------------------------------------------------*
           05  w-prs-dat-mtz              pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento agenti                    *
      *        *-------------------------------------------------------*
           05  w-exp-tor-age.
               10  w-exp-tor-age-num      pic  9(02)       value 03   .
               10  w-exp-tor-age-lun      pic  9(02)       value 30   .
               10  w-exp-tor-age-tbl.
                   15  filler             pic  x(30) value
                  "per Nominativo agente         "                    .
                   15  filler             pic  x(30) value
                  "per Codice agente             "                    .
                   15  filler             pic  x(30) value
                  "per Mnemonico agente          "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di uso della stampa                   *
      *        *-------------------------------------------------------*
           05  w-exp-tip-uso.
               10  w-exp-tip-uso-num      pic  9(02)       value 02   .
               10  w-exp-tip-uso-lun      pic  9(02)       value 30   .
               10  w-exp-tip-uso-tbl.
                   15  filler             pic  x(30) value
                  "ad uso Interno                "                    .
                   15  filler             pic  x(30) value
                  "da consegnare agli Agenti     "                    .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica agenti            *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)     value spaces .
               10  w-let-arc-age-cod      pic  9(07)     value zero   .
               10  w-let-arc-age-nom      pic  x(20)     value spaces .
               10  w-let-arc-age-rag      pic  x(40)     value spaces .
               10  w-let-arc-age-via      pic  x(40)     value spaces .
               10  w-let-arc-age-loc      pic  x(40)     value spaces .
               10  w-let-arc-age-mne      pic  x(10)     value spaces .
               10  w-let-arc-age-exc      pic  9(07)     value zero   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cpw"                   .

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
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data maturazione                            *
      *                  *---------------------------------------------*
           perform   prs-dat-mtz-000      thru prs-dat-mtz-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alla data di     *
      *    * maturazione                                               *
      *    *-----------------------------------------------------------*
       prs-dat-mtz-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age/age400[dat-mtz]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dat-mtz
           else      move  spaces         to   w-prs-dat-mtz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     move  "M"            to   w-prs-dat-mtz          .
       prs-dat-mtz-999.
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
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
      *                  * Data inizio periodo                         *
      *                  *---------------------------------------------*
           perform   acc-dat-min-000      thru acc-dat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data fine periodo                           *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data di maturazione min                     *
      *                  *---------------------------------------------*
           perform   acc-mtz-min-000      thru acc-mtz-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data di maturazione max                     *
      *                  *---------------------------------------------*
           perform   acc-mtz-max-000      thru acc-mtz-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per agenti              *
      *                  *---------------------------------------------*
           perform   acc-tor-age-000      thru acc-tor-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Tipo di uso della stampa                    *
      *                  *---------------------------------------------*
           perform   acc-tip-uso-000      thru acc-tip-uso-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
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
      *              * Data inizio periodo                             *
      *              *-------------------------------------------------*
           perform   pmt-dat-min-000      thru pmt-dat-min-999        .
      *              *-------------------------------------------------*
      *              * Data fine periodo                               *
      *              *-------------------------------------------------*
           perform   pmt-dat-max-000      thru pmt-dat-max-999        .
      *              *-------------------------------------------------*
      *              * Data di maturazione min                         *
      *              *-------------------------------------------------*
           perform   pmt-mtz-min-000      thru pmt-mtz-min-999        .
      *              *-------------------------------------------------*
      *              * Data di maturazione max                         *
      *              *-------------------------------------------------*
           perform   pmt-mtz-max-000      thru pmt-mtz-max-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           perform   pmt-tor-age-000      thru pmt-tor-age-999        .
      *              *-------------------------------------------------*
      *              * Tipo di uso della stampa                        *
      *              *-------------------------------------------------*
           perform   pmt-tip-uso-000      thru pmt-tip-uso-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data di inizio periodo                           *
      *    *-----------------------------------------------------------*
       pmt-dat-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data di inizio periodo     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data di fine periodo                             *
      *    *-----------------------------------------------------------*
       pmt-dat-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data di fine periodo       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data di maturazione min                          *
      *    *-----------------------------------------------------------*
       pmt-mtz-min-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     go to pmt-mtz-min-999.
       pmt-mtz-min-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Con maturazione        dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mtz-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data di maturazione max                          *
      *    *-----------------------------------------------------------*
       pmt-mtz-max-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     go to pmt-mtz-max-999.
       pmt-mtz-max-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mtz-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice agente                                    *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di ordinamento per agenti                   *
      *    *-----------------------------------------------------------*
       pmt-tor-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento agenti         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di uso della stampa                         *
      *    *-----------------------------------------------------------*
       pmt-tip-uso-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di stampa             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-uso-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di inizio periodo                     *
      *    *-----------------------------------------------------------*
       acc-dat-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-min-999.
       acc-dat-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-min             .
       acc-dat-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non accettabile               *
      *                  *---------------------------------------------*
           if        rr-dat-min           not  = zero
                     go to acc-dat-min-600.
           if        v-key                =    "UP  "
                     go to acc-dat-min-600
           else      go to acc-dat-min-100.
       acc-dat-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-min-100.
       acc-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di fine periodo                       *
      *    *-----------------------------------------------------------*
       acc-dat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-max-999.
       acc-dat-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-max             .
       acc-dat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non accettabile               *
      *                  *---------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to acc-dat-max-600.
           if        v-key                =    "UP  "
                     go to acc-dat-max-600
           else      go to acc-dat-max-100.
       acc-dat-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-max-100.
       acc-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di maturazione min                    *
      *    *-----------------------------------------------------------*
       acc-mtz-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     go to acc-mtz-min-999.
       acc-mtz-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-mtz-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-mtz-min-999.
       acc-mtz-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-mtz-min             .
       acc-mtz-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-mtz-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mtz-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-mtz-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-mtz-min-100.
       acc-mtz-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di maturazione max                    *
      *    *-----------------------------------------------------------*
       acc-mtz-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     go to acc-mtz-max-999.
       acc-mtz-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-mtz-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-mtz-max-999.
       acc-mtz-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-mtz-max             .
       acc-mtz-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-mtz-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mtz-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-mtz-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-mtz-max-100.
       acc-mtz-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente                              *
      *    *-----------------------------------------------------------*
       acc-cod-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-cod-age           to   w-cod-mne-age-cod      .
           move      11                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      11                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-cod-age-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-cod-age-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-cod-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-age-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-cod-age-110.
       acc-cod-age-120.
           move      w-cod-mne-age-cod    to   v-num                  .
       acc-cod-age-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-age-999.
       acc-cod-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-age             .
       acc-cod-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      rr-cod-age           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     move  "Tutti gli agenti                        "
                                          to   rr-cod-age-nom
                     move  spaces         to   rr-cod-age-rag
                     move  spaces         to   rr-cod-age-via
                     move  spaces         to   rr-cod-age-loc
           else      move  w-let-arc-age-nom
                                          to   rr-cod-age-nom
                     move  w-let-arc-age-rag
                                          to   rr-cod-age-rag
                     move  w-let-arc-age-via
                                          to   rr-cod-age-via
                     move  w-let-arc-age-loc
                                          to   rr-cod-age-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo agente           *
      *                  *---------------------------------------------*
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-625.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da zero si   *
      *                  * normalizza il tipo ordinamento agenti       *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     go to acc-cod-age-650.
           move      zero                 to   rr-tor-age             .
           perform   vis-tor-age-000      thru vis-tor-age-999        .
       acc-cod-age-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-cod-age-800.
       acc-cod-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-age-100.
       acc-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente                           *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Nominativo agente                       *
      *    *-----------------------------------------------------------*
       vis-cod-age-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-age-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento per agenti                *
      *    *-----------------------------------------------------------*
       acc-tor-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se selezionato un solo agente non si    *
      *                      * accetta il campo                        *
      *                      *-----------------------------------------*
           if        rr-cod-age           not  = zero
                     go to acc-tor-age-999.
       acc-tor-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      "NCM#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-age           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-age-999.
       acc-tor-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-age             .
       acc-tor-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-age-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-age           not  = 01 and
                     rr-tor-age           not  = 02 and
                     rr-tor-age           not  = 03
                     go to acc-tor-age-100.
       acc-tor-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-age-100.
       acc-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per agenti             *
      *    *-----------------------------------------------------------*
       vis-tor-age-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      rr-tor-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di uso della stampa                   *
      *    *-----------------------------------------------------------*
       acc-tip-uso-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-uso-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-uso-lun    to   v-car                  .
           move      w-exp-tip-uso-num    to   v-ldt                  .
           move      "IA#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-uso-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-uso           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-uso-999.
       acc-tip-uso-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-uso             .
       acc-tip-uso-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-uso-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-uso           not  = 01 and
                     rr-tip-uso           not  = 02
                     go to acc-tip-uso-100.
       acc-tip-uso-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-uso-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-uso-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-uso-100.
       acc-tip-uso-999.
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
      *              * Controllo su data di inizio periodo             *
      *              *-------------------------------------------------*
       tdo-ric-sel-105.
           if        rr-dat-min           not  = zero
                     go to tdo-ric-sel-110.
           move      "Manca la data di inizio periodo !                 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-110.
           go to     tdo-ric-sel-200.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su data di fine periodo               *
      *              *-------------------------------------------------*
       tdo-ric-sel-205.
           if        rr-dat-max           not  = zero
                     go to tdo-ric-sel-210.
           move      "Manca la data di fine periodo !                   
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-210.
           go to     tdo-ric-sel-300.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su data di inizio/fine periodo        *
      *              *-------------------------------------------------*
       tdo-ric-sel-305.
           if        rr-dat-max           not  < rr-dat-min
                     go to tdo-ric-sel-310.
           move      "La data di fine periodo e' inferiore a quella di i
      -              "nizio periodo !"    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-310.
           go to     tdo-ric-sel-350.
       tdo-ric-sel-350.
      *              *-------------------------------------------------*
      *              * Controllo su data di maturazione                *
      *              *-------------------------------------------------*
           if        rr-mtz-min           =    zero or
                     rr-mtz-max           =    zero
                     go to tdo-ric-sel-400.
           if        rr-mtz-max           not  < rr-mtz-min
                     go to tdo-ric-sel-400.
           move      "La maturazione finale non puo' essere inferiore al
      -              "l' iniziale !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per agenti      *
      *              *-------------------------------------------------*
           if        rr-cod-age           not  = zero
                     go to tdo-ric-sel-415.
           if        rr-tor-age           not  = zero
                     go to tdo-ric-sel-410.
           move      "Manca il tipo di ordinamento per gli agenti !     
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-410.
           if        rr-tor-age           =    01 or
                     rr-tor-age           =    02 or
                     rr-tor-age           =    03
                     go to tdo-ric-sel-415.
           move      "Tipo di ordinamento per gli agenti errato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-415.
           go to     tdo-ric-sel-500.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di uso della stampa            *
      *              *-------------------------------------------------*
       tdo-ric-sel-505.
           if        rr-tip-uso           not  = zero
                     go to tdo-ric-sel-510.
           move      "Manca il tipo di uso della stampa !               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-510.
           if        rr-tip-uso           =    01 or
                     rr-tip-uso           =    02
                     go to tdo-ric-sel-515.
           move      "Tipo di uso della stampa errato !                 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-515.
           go to     tdo-ric-sel-600.
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Se selezionato un solo agente si forza il tipo  *
      *              * ordinamento agenti per codice                   *
      *              *-------------------------------------------------*
           if        rr-cod-age           not  = zero
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione data maturazione max           *
      *              *-------------------------------------------------*
           if        rr-mtz-max           not  = zero
                     go to reg-ric-sel-999.
           if        rr-mtz-min           =    zero
                     move 9999999         to   rr-mtz-max
           else      move rr-mtz-min      to   rr-mtz-max             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data di inizio periodo                          *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-min             .
      *              *-------------------------------------------------*
      *              * Data di fine periodo                            *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-max             .
      *              *-------------------------------------------------*
      *              * Data di maturazione min                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-mtz-min             .
      *              *-------------------------------------------------*
      *              * Data di maturazione max                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-mtz-max             .
      *              *-------------------------------------------------*
      *              * Codice agente e dati correlati                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
           move      spaces               to   rr-cod-age-rag         .
           move      spaces               to   rr-cod-age-via         .
           move      spaces               to   rr-cod-age-loc         .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Tipo di uso della stampa                        *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-uso             .
       nor-ric-sel-999.
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
      *    * Routine di lettura archivio anagrafica agenti             *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    w-let-arc-age-exc
                     go to let-arc-age-900.
       let-arc-age-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [age] relativamente all'agente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-age-200
           else      go to let-arc-age-300.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente esistente                  *
      *              *-------------------------------------------------*
       let-arc-age-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-age-via-age       to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-age-loc-age       to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-let-arc-age-mne      .
       let-arc-age-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente non esistente              *
      *              *-------------------------------------------------*
       let-arc-age-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Se codice agente a zero                         *
      *              *-------------------------------------------------*
       let-arc-age-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente su valore precedente          *
      *                  *---------------------------------------------*
           move      w-let-arc-age-cod    to   w-let-arc-age-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

