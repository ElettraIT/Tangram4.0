       Identification Division.
       Program-Id.                                 pdtp7030           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:    val                 *
      *                                   Fase:    dtp703              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/12/93    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pdtp7031:        *
      *                                                                *
      *                    Stampa valorizzazione unitaria su distinta  *
      *                    base per sub-distinte virtuali              *
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
                     "dtp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "val"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dtp703"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdtp7030"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "VALORIZZAZIONE SUB-DISTINTE SU DISTINTA "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pdtp7031  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/dtp/prg/obj/pdtp7031                "       .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data per valorizzazione                               *
      *        *-------------------------------------------------------*
           05  rr-dat-val                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento                                   *
      *        *  - 01 : Per descrizione                               *
      *        *  - 02 : Per codice                                    *
      *        *  - 03 : Per sinonimo                                  *
      *        *-------------------------------------------------------*
           05  rr-tip-ord                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione minima e massima                          *
      *        *-------------------------------------------------------*
           05  rr-des-min                 pic  x(20)                  .
           05  rr-des-max                 pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice minimo e massimo                               *
      *        *-------------------------------------------------------*
           05  rr-cod-min                 pic  x(14)                  .
           05  rr-cod-max                 pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Sinonimo minimo e massimo                             *
      *        *-------------------------------------------------------*
           05  rr-sin-min                 pic  x(13)                  .
           05  rr-sin-max                 pic  x(13)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di valorizzazione per le materie prime incluse   *
      *        * in distinta                                           *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        *-------------------------------------------------------*
           05  rr-tva-map                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di valorizzazione per i semilavorati finali in-  *
      *        * clusi in distinta                                     *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        * - 99 : In funzione dei componenti                     *
      *        *                                                       *
      *        * Nota : Questa valorizzazione vale solamente per i se- *
      *        *        milavorati finali di un ciclo di lavorazione,  *
      *        *        ovvero quei semilavorati che hanno la voce a-  *
      *        *        nagrafica 'rf-dps-tip-sem' pari a 01.          *
      *        *        Gli altri semilavorati, ovvero quelli inter-   *
      *        *        medi di un ciclo di lavorazione, sono equipa-  *
      *        *        rati alle subdistinte virtuali, per cui assu-  *
      *        *        meranno il loro valore sempre in funzione del  *
      *        *        valore dei loro componenti.                    *
      *        *-------------------------------------------------------*
           05  rr-tva-sem                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per tipo di ordinamento                   *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ord              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 3    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 30   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(30) value
                            "Per descrizione               "          .
                   15  filler             pic  x(30) value
                            "Per codice                    "          .
                   15  filler             pic  x(30) value
                            "Per sinonimo                  "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione per le materie prime   *
      *        *-------------------------------------------------------*
           05  w-exp-tva-map.
               10  w-exp-tva-map-num      pic  9(02)       value 3    .
               10  w-exp-tva-map-lun      pic  9(02)       value 30   .
               10  w-exp-tva-map-tbl.
                   15  filler             pic  x(30) value
                            "Costo medio ponderato annuale "          .
                   15  filler             pic  x(30) value
                            "Ultimo costo d'acquisto       "          .
                   15  filler             pic  x(30) value
                            "Costo standard                "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione per i semilavorati     *
      *        *-------------------------------------------------------*
           05  w-exp-tva-sem.
               10  w-exp-tva-sem-num      pic  9(02)       value 4    .
               10  w-exp-tva-sem-lun      pic  9(02)       value 30   .
               10  w-exp-tva-sem-tbl.
                   15  filler             pic  x(30) value
                            "In funzione dei componenti    "          .
                   15  filler             pic  x(30) value
                            "Costo medio ponderato annuale "          .
                   15  filler             pic  x(30) value
                            "Ultimo costo d'acquisto       "          .
                   15  filler             pic  x(30) value
                            "Costo standard                "          .

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
      *    * Link-area per accettazione codice legame virtuale         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acl"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              * Open modulo accettazione codice sub-distinta    *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-opn-000  thru cod-cod-lgv-opn-999    .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sub-distinta   *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-cls-000  thru cod-cod-lgv-cls-999    .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
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
      *                  * Data per valorizzazione                     *
      *                  *---------------------------------------------*
           perform   acc-dat-val-000      thru acc-dat-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento                            *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Parametro minimo                            *
      *                  *---------------------------------------------*
           perform   acc-par-min-000      thru acc-par-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Parametro massimo                           *
      *                  *---------------------------------------------*
           perform   acc-par-max-000      thru acc-par-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione per materie prime       *
      *                  *---------------------------------------------*
           perform   acc-tva-map-000      thru acc-tva-map-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione per semilavorati        *
      *                  *---------------------------------------------*
           perform   acc-tva-sem-000      thru acc-tva-sem-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
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
      *              * Data per valorizzazione                         *
      *              *-------------------------------------------------*
           perform   pmt-dat-val-000      thru pmt-dat-val-999        .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento                                *
      *              *-------------------------------------------------*
           perform   pmt-tip-ord-000      thru pmt-tip-ord-999        .
      *              *-------------------------------------------------*
      *              * Parametro minimo                                *
      *              *-------------------------------------------------*
           perform   pmt-par-min-000      thru pmt-par-min-999        .
      *              *-------------------------------------------------*
      *              * Parametro massimo                               *
      *              *-------------------------------------------------*
           perform   pmt-par-max-000      thru pmt-par-max-999        .
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione per le materie prime     *
      *              *-------------------------------------------------*
           perform   pmt-tva-map-000      thru pmt-tva-map-999        .
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione per i semilavorati       *
      *              *-------------------------------------------------*
           perform   pmt-tva-sem-000      thru pmt-tva-sem-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data per valorizzazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Valorizzazione alla data   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo ordinamento                                 *
      *    *-----------------------------------------------------------*
       pmt-tip-ord-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di ordinamento        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Parametro minimo                                 *
      *    *-----------------------------------------------------------*
       pmt-par-min-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to pmt-par-min-100
           else if   rr-tip-ord           =    02
                     go to pmt-par-min-200
           else if   rr-tip-ord           =    03
                     go to pmt-par-min-300
           else      go to pmt-par-min-900.
       pmt-par-min-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Da descrizione             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-min-999.
       pmt-par-min-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Da codice                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-min-999.
       pmt-par-min-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Da sinonimo                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-min-999.
       pmt-par-min-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-min-999.
       pmt-par-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Parametro massimo                                *
      *    *-----------------------------------------------------------*
       pmt-par-max-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to pmt-par-max-100
           else if   rr-tip-ord           =    02
                     go to pmt-par-max-200
           else if   rr-tip-ord           =    03
                     go to pmt-par-max-300
           else      go to pmt-par-max-900.
       pmt-par-max-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "A  descrizione             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-max-999.
       pmt-par-max-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "A  codice                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-max-999.
       pmt-par-max-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "A  sinonimo                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-max-999.
       pmt-par-max-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-par-max-999.
       pmt-par-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di valorizzazione per le materie prime      *
      *    *-----------------------------------------------------------*
       pmt-tva-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione per :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          le materie prime  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  incluse in distinta base  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di valorizzazione per i semilavorati        *
      *    *-----------------------------------------------------------*
       pmt-tva-sem-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione per :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     i semilavorati finali  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  inclusi in distinta base  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tva-sem-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data per valorizzazione                    *
      *    *-----------------------------------------------------------*
       acc-dat-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-val-025.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del valore di de-    *
      *                  * fault                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore attuale a zero           *
      *                      *-----------------------------------------*
           if        rr-dat-val           not  zero
                     go to acc-dat-val-100.
      *                      *-----------------------------------------*
      *                      * Data da segreteria                      *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dat-val             .
       acc-dat-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-val           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-val-999.
       acc-dat-val-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-val             .
       acc-dat-val-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-val-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-dat-val           not  = zero
                     go to acc-dat-val-450.
           if        v-key                =    "UP  "
                     go to acc-dat-val-450
           else      go to acc-dat-val-600.
       acc-dat-val-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-dat-val-600.
       acc-dat-val-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-val-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-val-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-val-100.
       acc-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazzione : Data per valorizzazione                *
      *    *-----------------------------------------------------------*
       vis-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo ordinamento           *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ord-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-ord           to   w-sav-tip-ord          .
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-ord           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ord             .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ord-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = zero
                     go to acc-tip-ord-450.
           if        v-key                =    "UP  "
                     go to acc-tip-ord-450
           else      go to acc-tip-ord-600.
       acc-tip-ord-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tip-ord-600.
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-620.
      *                  *---------------------------------------------*
      *                  * Se valore immutato rispetto al valore pre-  *
      *                  * cedente : nessuna dipendenza                *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    w-sav-tip-ord
                     go to acc-tip-ord-800.
       acc-tip-ord-640.
      *                  *---------------------------------------------*
      *                  * Se valore precedente indeterminato : solo   *
      *                  * visualizzazione prompt                      *
      *                  *---------------------------------------------*
           if        w-sav-tip-ord        not  = zero
                     go to acc-tip-ord-660.
           perform   pmt-par-min-000      thru pmt-par-min-999        .
           perform   pmt-par-max-000      thru pmt-par-max-999        .
           go to     acc-tip-ord-800.
       acc-tip-ord-660.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori parametri minimi e   *
      *                  * massimi                                     *
      *                  *---------------------------------------------*
           move      spaces               to   rr-des-min             .
           move      spaces               to   rr-des-max             .
           move      spaces               to   rr-cod-min             .
           move      spaces               to   rr-cod-max             .
           move      spaces               to   rr-sin-min             .
           move      spaces               to   rr-sin-max             .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           perform   pmt-par-min-000      thru pmt-par-min-999        .
           perform   pmt-par-max-000      thru pmt-par-max-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione parametri                   *
      *                  *---------------------------------------------*
           perform   vis-par-min-000      thru vis-par-min-999        .
           perform   vis-par-max-000      thru vis-par-max-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ord-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ord-100.
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazzione : Tipo ordinamento                       *
      *    *-----------------------------------------------------------*
       vis-tip-ord-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      rr-tip-ord           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Parametro minimo                           *
      *    *-----------------------------------------------------------*
       acc-par-min-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to acc-par-min-100
           else if   rr-tip-ord           =    02
                     go to acc-par-min-200
           else if   rr-tip-ord           =    03
                     go to acc-par-min-300
           else      go to acc-par-min-900.
       acc-par-min-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           perform   acc-des-min-000      thru acc-des-min-999        .
           go to     acc-par-min-999.
       acc-par-min-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           perform   acc-cod-min-000      thru acc-cod-min-999        .
           go to     acc-par-min-999.
       acc-par-min-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           perform   acc-sin-min-000      thru acc-sin-min-999        .
           go to     acc-par-min-999.
       acc-par-min-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           go to     acc-par-min-999.
       acc-par-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Descrizione minima                         *
      *    *-----------------------------------------------------------*
       acc-des-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-des-min           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-des-min-999.
       acc-des-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-des-min             .
       acc-des-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-des-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-des-min-100.
       acc-des-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice minimo                              *
      *    *-----------------------------------------------------------*
       acc-cod-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lgv-ope      .
           move      "L"                  to   w-cod-cod-lgv-tac      .
           move      zero                 to   w-cod-cod-lgv-num      .
           move      rr-cod-min           to   w-cod-cod-lgv-alf      .
           move      09                   to   w-cod-cod-lgv-lin      .
           move      30                   to   w-cod-cod-lgv-pos      .
           move      zero                 to   w-cod-cod-lgv-dln      .
           move      zero                 to   w-cod-cod-lgv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
       acc-cod-min-110.
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           if        w-cod-cod-lgv-ope    =    "F+"
                     go to acc-cod-min-115.
           if        w-cod-cod-lgv-ope    =    "AC"
                     go to acc-cod-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-min-115.
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
           go to     acc-cod-min-110.
       acc-cod-min-120.
           move      w-cod-cod-lgv-num    to   v-num                  .
           move      w-cod-cod-lgv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-min-999.
       acc-cod-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-min             .
       acc-cod-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blank-embedded        *
      *                  *---------------------------------------------*
           move      rr-cod-min           to   w-all-str-alf          .
           move      14                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-min-100.
       acc-cod-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-min-100.
       acc-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sinonimo minimo                            *
      *    *-----------------------------------------------------------*
       acc-sin-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sin-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sin-min           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sin-min-999.
       acc-sin-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sin-min             .
       acc-sin-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sin-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sin-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sin-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sin-min-100.
       acc-sin-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Parametro minimo                        *
      *    *-----------------------------------------------------------*
       vis-par-min-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to vis-par-min-100
           else if   rr-tip-ord           =    02
                     go to vis-par-min-200
           else if   rr-tip-ord           =    03
                     go to vis-par-min-300
           else      go to vis-par-min-900.
       vis-par-min-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-des-min           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-min-999.
       vis-par-min-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-min           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-min-999.
       vis-par-min-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sin-min           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-min-999.
       vis-par-min-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-min-999.
       vis-par-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Parametro massimo                          *
      *    *-----------------------------------------------------------*
       acc-par-max-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to acc-par-max-100
           else if   rr-tip-ord           =    02
                     go to acc-par-max-200
           else if   rr-tip-ord           =    03
                     go to acc-par-max-300
           else      go to acc-par-max-900.
       acc-par-max-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           perform   acc-des-max-000      thru acc-des-max-999        .
           go to     acc-par-max-999.
       acc-par-max-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           perform   acc-cod-max-000      thru acc-cod-max-999        .
           go to     acc-par-max-999.
       acc-par-max-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           perform   acc-sin-max-000      thru acc-sin-max-999        .
           go to     acc-par-max-999.
       acc-par-max-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           go to     acc-par-max-999.
       acc-par-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Descrizione massimo                        *
      *    *-----------------------------------------------------------*
       acc-des-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-des-max           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-des-max-999.
       acc-des-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-des-max             .
       acc-des-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-des-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-des-max-100.
       acc-des-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice massimo                             *
      *    *-----------------------------------------------------------*
       acc-cod-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lgv-ope      .
           move      "L"                  to   w-cod-cod-lgv-tac      .
           move      zero                 to   w-cod-cod-lgv-num      .
           move      rr-cod-max           to   w-cod-cod-lgv-alf      .
           move      10                   to   w-cod-cod-lgv-lin      .
           move      30                   to   w-cod-cod-lgv-pos      .
           move      zero                 to   w-cod-cod-lgv-dln      .
           move      zero                 to   w-cod-cod-lgv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
       acc-cod-max-110.
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           if        w-cod-cod-lgv-ope    =    "F+"
                     go to acc-cod-max-115.
           if        w-cod-cod-lgv-ope    =    "AC"
                     go to acc-cod-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-max-115.
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
           go to     acc-cod-max-110.
       acc-cod-max-120.
           move      w-cod-cod-lgv-num    to   v-num                  .
           move      w-cod-cod-lgv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-max-999.
       acc-cod-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-max             .
       acc-cod-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blank-embedded        *
      *                  *---------------------------------------------*
           move      rr-cod-max           to   w-all-str-alf          .
           move      14                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-max-100.
       acc-cod-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-max-100.
       acc-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sinonimo massimo                           *
      *    *-----------------------------------------------------------*
       acc-sin-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sin-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sin-max           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sin-max-999.
       acc-sin-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sin-max             .
       acc-sin-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sin-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sin-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sin-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sin-max-100.
       acc-sin-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Parametro massimo                       *
      *    *-----------------------------------------------------------*
       vis-par-max-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to vis-par-max-100
           else if   rr-tip-ord           =    02
                     go to vis-par-max-200
           else if   rr-tip-ord           =    03
                     go to vis-par-max-300
           else      go to vis-par-max-900.
       vis-par-max-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per descrizione          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-des-max           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-max-999.
       vis-par-max-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per codice               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-max           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-max-999.
       vis-par-max-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento per sinonimo             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sin-max           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-max-999.
       vis-par-max-900.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento indeterminato            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-par-max-999.
       vis-par-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione per materie prime      *
      *    *-----------------------------------------------------------*
       acc-tva-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tva-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-map-lun    to   v-car                  .
           move      w-exp-tva-map-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tva-map-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tva-map           =    01
                     move  01             to   v-num
           else if   rr-tva-map           =    02
                     move  02             to   v-num
           else if   rr-tva-map           =    03
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tva-map-999.
       acc-tva-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tva-map
           else if   v-num                =    02
                     move  02             to   rr-tva-map
           else if   v-num                =    03
                     move  03             to   rr-tva-map             .
       acc-tva-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tva-map-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tva-map           not  = zero
                     go to acc-tva-map-450.
           if        v-key                =    "UP  "
                     go to acc-tva-map-450
           else      go to acc-tva-map-600.
       acc-tva-map-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tva-map-600.
       acc-tva-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tva-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tva-map-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tva-map-100.
       acc-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione materie prime       *
      *    *-----------------------------------------------------------*
       vis-tva-map-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-map-lun    to   v-car                  .
           move      w-exp-tva-map-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tva-map-tbl    to   v-txt                  .
           if        rr-tva-map           =    01
                     move  01             to   v-num
           else if   rr-tva-map           =    02
                     move  02             to   v-num
           else if   rr-tva-map           =    03
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione per semilavorati       *
      *    *-----------------------------------------------------------*
       acc-tva-sem-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tva-sem-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-sem-lun    to   v-car                  .
           move      w-exp-tva-sem-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tva-sem-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tva-sem           =    99
                     move  01             to   v-num
           else if   rr-tva-sem           =    01
                     move  02             to   v-num
           else if   rr-tva-sem           =    02
                     move  03             to   v-num
           else if   rr-tva-sem           =    03
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tva-sem-999.
       acc-tva-sem-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   rr-tva-sem
           else if   v-num                =    02
                     move  01             to   rr-tva-sem
           else if   v-num                =    03
                     move  02             to   rr-tva-sem
           else if   v-num                =    04
                     move  03             to   rr-tva-sem             .
       acc-tva-sem-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tva-sem-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tva-sem           not  = zero
                     go to acc-tva-sem-450.
           if        v-key                =    "UP  "
                     go to acc-tva-sem-450
           else      go to acc-tva-sem-600.
       acc-tva-sem-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tva-sem-600.
       acc-tva-sem-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tva-sem-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tva-sem-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tva-sem-100.
       acc-tva-sem-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione semilavorati        *
      *    *-----------------------------------------------------------*
       vis-tva-sem-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-sem-lun    to   v-car                  .
           move      w-exp-tva-sem-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tva-sem-tbl    to   v-txt                  .
           if        rr-tva-sem           =    99
                     move  01             to   v-num
           else if   rr-tva-sem           =    01
                     move  02             to   v-num
           else if   rr-tva-sem           =    02
                     move  03             to   v-num
           else if   rr-tva-sem           =    03
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tva-sem-999.
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
      *              * Controllo su data per valorizzazione            *
      *              *-------------------------------------------------*
       tdo-ric-sel-105.
           if        rr-dat-val           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca la data per la valorizzazione               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento e su relativi pa- *
      *              * rametri                                         *
      *              *-------------------------------------------------*
       tdo-ric-sel-201.
           if        rr-tip-ord           not  = zero
                     go to tdo-ric-sel-202.
           move      "Manca il tipo di ordinamento                      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-202.
           if        rr-tip-ord           =    01
                     go to tdo-ric-sel-210
           else if   rr-tip-ord           =    02
                     go to tdo-ric-sel-220
           else if   rr-tip-ord           =    03
                     go to tdo-ric-sel-230.
           move      "Tipo di ordinamento errato                        
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-210.
           if        rr-des-max           =    spaces
                     go to tdo-ric-sel-300.
           if        rr-des-min           not  > rr-des-max
                     go to tdo-ric-sel-300.
           move      "Descrizione minima superiore a descrizione massima
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-220.
           if        rr-cod-max           =    spaces
                     go to tdo-ric-sel-300.
           if        rr-cod-min           not  > rr-cod-max
                     go to tdo-ric-sel-300.
           move      "Codice minimo superiore a codice massimo          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-230.
           if        rr-sin-max           =    spaces
                     go to tdo-ric-sel-300.
           if        rr-sin-min           not  > rr-sin-max
                     go to tdo-ric-sel-300.
           move      "Sinonimo minimo superiore a sinonimo massimo      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su valorizzazione materie prime       *
      *              *-------------------------------------------------*
       tdo-ric-sel-305.
           if        rr-tva-map           not  = zero
                     go to tdo-ric-sel-310.
           move      "Manca il tipo di valorizzazione per le materie pri
      -              "me             "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-310.
           if        rr-tva-map           =    01 or
                     rr-tva-map           =    02 or
                     rr-tva-map           =    03
                     go to tdo-ric-sel-400.
           move      "Tipo di valorizzazione per le materie prime errato
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su valorizzazione semilavorati        *
      *              *-------------------------------------------------*
       tdo-ric-sel-405.
           if        rr-tva-sem           not  = zero
                     go to tdo-ric-sel-410.
           move      "Manca il tipo di valorizzazione per i semilavorati
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-410.
           if        rr-tva-sem           =    01 or
                     rr-tva-sem           =    02 or
                     rr-tva-sem           =    03 or
                     rr-tva-sem           =    99
                     go to tdo-ric-sel-500.
           move      "Tipo di valorizzazione per i semilavorati errato  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Fine controlli                                  *
      *              *-------------------------------------------------*
       tdo-ric-sel-501.
      *                  *---------------------------------------------*
      *                  * Normalizzazione parametri minimo e massimo  *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    01
                     go to tdo-ric-sel-510
           else if   rr-tip-ord           =    02
                     go to tdo-ric-sel-520
           else if   rr-tip-ord           =    03
                     go to tdo-ric-sel-530.
       tdo-ric-sel-510.
           move      spaces               to   rr-cod-min              .
           move      spaces               to   rr-cod-max              .
           move      spaces               to   rr-sin-min              .
           move      spaces               to   rr-sin-max              .
           if        rr-des-max           =    spaces
                     move  rr-des-min     to   rr-des-max             .
           move      rr-des-max           to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-des-max             .
           go to     tdo-ric-sel-600.
       tdo-ric-sel-520.
           move      spaces               to   rr-des-min              .
           move      spaces               to   rr-des-max              .
           move      spaces               to   rr-sin-min              .
           move      spaces               to   rr-sin-max              .
           if        rr-cod-max           =    spaces
                     move  rr-cod-min     to   rr-cod-max             .
           move      rr-cod-max           to   w-all-str-alf          .
           move      14                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-cod-max             .
           go to     tdo-ric-sel-600.
       tdo-ric-sel-530.
           move      spaces               to   rr-des-min              .
           move      spaces               to   rr-des-max              .
           move      spaces               to   rr-cod-min              .
           move      spaces               to   rr-cod-max              .
           if        rr-sin-max           =    spaces
                     move  rr-sin-min     to   rr-sin-max             .
           move      rr-sin-max           to   w-all-str-alf          .
           move      13                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sin-max             .
           go to     tdo-ric-sel-600.
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
       tdo-ric-sel-905.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio d'errore                *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       tdo-ric-sel-910.
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
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data per valorizzazione                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-val             .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento                             *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-ord             .
      *              *-------------------------------------------------*
      *              * Parametri minimi e massimi                      *
      *              *-------------------------------------------------*
           move      spaces               to   rr-des-min             .
           move      spaces               to   rr-des-max             .
           move      spaces               to   rr-cod-min             .
           move      spaces               to   rr-cod-max             .
           move      spaces               to   rr-sin-min             .
           move      spaces               to   rr-sin-max             .
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione per le materie prime     *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tva-map             .
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione per i semilavorati       *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tva-sem             .
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
           move      080                  to   w-cnt-stp-amp-lin      .
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice sub-distinta virtuale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acs"                   .
