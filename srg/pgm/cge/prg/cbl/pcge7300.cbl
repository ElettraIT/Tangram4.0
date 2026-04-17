       Identification Division.
       Program-Id.                                 pcge7300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    iva                 *
      *                                   Fase:    cge730              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/06/94    *
      *                       Ultima revisione:    NdK del 02/08/16    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pcge7301:        *
      *                                                                *
      *                    Stampa dichiarazione iva periodica          *
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
                     "iva"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge730"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge7300  "                                     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   STAMPA DICHIARAZIONE IVA PERIODICA   "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pcge7301  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/cge/prg/obj/pcge7301                "       .

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
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
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
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo stampa                                           *
      *        *                                                       *
      *        * - P : Di prova                                        *
      *        * - D : Definitiva                                      *
      *        *-------------------------------------------------------*
           05  rr-tip-stp                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo dichiarazione                                    *
      *        *                                                       *
      *        * - A : Annuale                                         *
      *        * - T : Trimestrale                                     *
      *        * - M : Mensile                                         *
      *        *-------------------------------------------------------*
           05  rr-tip-dic                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Anno per dichiarazione                                *
      *        *-------------------------------------------------------*
           05  rr-ann-dic                 pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Trimestre per dichiarazione                           *
      *        *                                                       *
      *        * N.B.: Solo se tipo dichiarazione Trimestrale          *
      *        *-------------------------------------------------------*
           05  rr-tri-dic                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Mese per dichiarazione                                *
      *        *                                                       *
      *        * N.B.: Solo se tipo dichiarazione Mensile              *
      *        *-------------------------------------------------------*
           05  rr-mes-dic                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data iniziale periodo richiesto                       *
      *        *-------------------------------------------------------*
           05  rr-dat-ini                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data finale periodo richiesto                         *
      *        *-------------------------------------------------------*
           05  rr-dat-fin                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Mese iniziale periodo richiesto                       *
      *        *-------------------------------------------------------*
           05  rr-mes-ini                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Mese finale periodo richiesto                         *
      *        *-------------------------------------------------------*
           05  rr-mes-fin                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/no tipo definitiva                                 *
      *        *                                                       *
      *        * - N : No                                              *
      *        * - S : Si                                              *
      *        *-------------------------------------------------------*
           05  rr-snx-tdf                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No carta vidimata                                  *
      *        *                                                       *
      *        *  - S : Si'                                            *
      *        *  - N : No                                             *
      *        *-------------------------------------------------------*
           05  rr-snx-cvd                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Ultima pagina stampata                                *
      *        *-------------------------------------------------------*
           05  rr-ult-pst                 pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Codice e titolo per la stampa registro                *
      *        *-------------------------------------------------------*
           05  w-ref-giv-ven.
               10  w-ref-giv-ven-cod      pic  x(06)                  .
               10  filler                 pic  x(01)                  .
               10  w-ref-giv-ven-des      pic  x(40)                  .
               10  filler                 pic  x(03)                  .

      *    *===========================================================*
      *    * Work per determinazione ed aggiornamento numero pagina    *
      *    *-----------------------------------------------------------*
       01  w-ylp-iva.
      *        *-------------------------------------------------------*
      *        * Data iniziale                                         *
      *        *-------------------------------------------------------*
           05  w-ylp-iva-dat-ini          pic  9(07)                  .
           05  w-ylp-iva-dat-saa          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valore ultimo numero pagina determinato               *
      *        *-------------------------------------------------------*
           05  w-ylp-iva-ult-pag          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore ultimo numero pagina da aggiornare             *
      *        *-------------------------------------------------------*
           05  w-ylp-iva-ult-agg          pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Data ultima stampa                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-ult-dat              pic  9(07) value zero       .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio tipo dichiarazione                        *
      *        *-------------------------------------------------------*
           05  w-sav-tip-dic              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo dichiarazione                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-dic.
               10  w-exp-tip-dic-num      pic  9(02)       value 3    .
               10  w-exp-tip-dic-lun      pic  9(02)       value 20   .
               10  w-exp-tip-dic-tbl.
                   15  filler             pic  x(20) value
                            "Mensile             "                    .
                   15  filler             pic  x(20) value
                            "Trimestrale         "                    .
                   15  filler             pic  x(20) value
                            "Annuale             "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Trimestre di dichiarazione                 *
      *        *-------------------------------------------------------*
           05  w-exp-tri-dic.
               10  w-exp-tri-dic-num      pic  9(02)       value 4    .
               10  w-exp-tri-dic-lun      pic  9(02)       value 20   .
               10  w-exp-tri-dic-tbl.
                   15  filler             pic  x(20) value
                            "Primo trimestre     "                    .
                   15  filler             pic  x(20) value
                            "Secondo trimestre   "                    .
                   15  filler             pic  x(20) value
                            "Terzo trimestre     "                    .
                   15  filler             pic  x(20) value
                            "Quarto trimestre    "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di stampa                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 2    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 20   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(20) value
                            "di Prova            "                    .
                   15  filler             pic  x(20) value
                            "Definitiva          "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no tipo definitiva                      *
      *        *-------------------------------------------------------*
           05  w-exp-snx-tdf.
               10  w-exp-snx-tdf-num      pic  9(02)       value 2    .
               10  w-exp-snx-tdf-lun      pic  9(02)       value 02   .
               10  w-exp-snx-tdf-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no carta vidimata                       *
      *        *-------------------------------------------------------*
           05  w-exp-snx-cvd.
               10  w-exp-snx-cvd-num      pic  9(02)       value 2    .
               10  w-exp-snx-cvd-lun      pic  9(02)       value 02   .
               10  w-exp-snx-cvd-tbl.
                   15  filler             pic  x(02)       value "Si" .
                   15  filler             pic  x(02)       value "No" .

      *    *===========================================================*
      *    * Work per routine cnt-ese-sgi-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-cnt-ese-sgi.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        * - Spaces : Controllo superato                         *
      *        * - F      : Stampa giornale iva acquisti da eseguire   *
      *        * - C      : Stampa giornale iva vendite  da eseguire   *
      *        * - V      : Stampa giornale iva corrisp. da eseguire   *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo controllo                                        *
      *        * - M : Mensile                                         *
      *        * - T : Trimestrale                                     *
      *        * - A : Annuale                                         *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-tpc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Secolo/anno                                           *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-saa          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Trimestre o Mese                                      *
      *        * - Se tipo controllo trimestrale : numero trimestre    *
      *        * - Se tipo controllo mensile     : numero mese         *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-tom          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data iniziale per scansione                           *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-wdi          pic  9(07)                  .
           05  w-cnt-ese-sgi-wdi-r redefines
               w-cnt-ese-sgi-wdi.
               10  w-cnt-ese-sgi-wai      pic  9(03)                  .
               10  w-cnt-ese-sgi-wmi      pic  9(02)                  .
               10  w-cnt-ese-sgi-wgi      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data finale per scansione                             *
      *        *-------------------------------------------------------*
           05  w-cnt-ese-sgi-wdf          pic  9(07)                  .
           05  w-cnt-ese-sgi-wdf-r redefines
               w-cnt-ese-sgi-wdf.
               10  w-cnt-ese-sgi-waf      pic  9(03)                  .
               10  w-cnt-ese-sgi-wmf      pic  9(02)                  .
               10  w-cnt-ese-sgi-wgf      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              *-------------------------------------------------*
      *              * Flag di eventuale visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      s-sts                to   w-cnt-mfu-vis-sgr      .
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
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-020.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice e titolo per la stampa registro      *
      *                  *---------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/iva[giv-ven]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-giv-ven
           else      move  spaces         to   w-ref-giv-ven          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione referenze                   *
      *                  *---------------------------------------------*
           if        w-ref-giv-ven-cod    =    spaces
                     move  "givven"       to   w-ref-giv-ven-cod      .
           if        w-ref-giv-ven-des    =    spaces
                     move  "REGISTRO DELLE VENDITE"
                                          to   w-ref-giv-ven-des      .
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
      *              * [mgi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [mgi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
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
      *                  * Tipo dichiarazione                          *
      *                  *---------------------------------------------*
           perform   acc-tip-dic-000      thru acc-tip-dic-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Anno dichiarazione                          *
      *                  *---------------------------------------------*
           perform   acc-ann-dic-000      thru acc-ann-dic-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Trimestre dichiarazione                     *
      *                  *---------------------------------------------*
           perform   acc-tri-dic-000      thru acc-tri-dic-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Mese dichiarazione                          *
      *                  *---------------------------------------------*
           perform   acc-mes-dic-000      thru acc-mes-dic-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Tipo stampa                                 *
      *                  *---------------------------------------------*
           perform   acc-tip-stp-000      thru acc-tip-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Si/no tipo definitiva                       *
      *                  *---------------------------------------------*
           perform   acc-snx-tdf-000      thru acc-snx-tdf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Si/no carta vidimata                        *
      *                  *---------------------------------------------*
           perform   acc-snx-cvd-000      thru acc-snx-cvd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-800.
      *                  *---------------------------------------------*
      *                  * Ultima pagina stampata                      *
      *                  *---------------------------------------------*
           perform   acc-ult-pst-000      thru acc-ult-pst-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-700.
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
      *              * Tipo dichiarazione                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-dic-000      thru pmt-tip-dic-999        .
      *              *-------------------------------------------------*
      *              * Anno dichiarazione                              *
      *              *-------------------------------------------------*
           perform   pmt-ann-dic-000      thru pmt-ann-dic-999        .
      *              *-------------------------------------------------*
      *              * Trimestre dichiarazione                         *
      *              *-------------------------------------------------*
           perform   pmt-tri-dic-000      thru pmt-tri-dic-999        .
      *              *-------------------------------------------------*
      *              * Mese dichiarazione                              *
      *              *-------------------------------------------------*
           perform   pmt-mes-dic-000      thru pmt-mes-dic-999        .
      *              *-------------------------------------------------*
      *              * Tipo stampa                                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-stp-000      thru pmt-tip-stp-999        .
      *              *-------------------------------------------------*
      *              * Si/no tipo definitiva                           *
      *              *-------------------------------------------------*
           perform   pmt-snx-tdf-000      thru pmt-snx-tdf-999        .
      *              *-------------------------------------------------*
      *              * Si/no carta vidimata                            *
      *              *-------------------------------------------------*
           perform   pmt-snx-cvd-000      thru pmt-snx-cvd-999        .
      *              *-------------------------------------------------*
      *              * Ultima pagina stampata                          *
      *              *-------------------------------------------------*
           perform   pmt-ult-pst-000      thru pmt-ult-pst-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo dichiarazione            *
      *    *-----------------------------------------------------------*
       pmt-tip-dic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di dichiarazione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Anno dichiarazione            *
      *    *-----------------------------------------------------------*
       pmt-ann-dic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                      Anno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ann-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Trimestre dichiarazione       *
      *    *-----------------------------------------------------------*
       pmt-tri-dic-000.
           if        rr-tip-dic           not  = "T"
                     go to pmt-tri-dic-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                 Trimestre :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tri-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Mese dichiarazione            *
      *    *-----------------------------------------------------------*
       pmt-mes-dic-000.
           if        rr-tip-dic           not  = "M"
                     go to pmt-mes-dic-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                      Mese :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mes-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo stampa                   *
      *    *-----------------------------------------------------------*
       pmt-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di stampa prospetto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/no tipo definitiva         *
      *    *-----------------------------------------------------------*
       pmt-snx-tdf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa tipo definitiva     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-tdf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/no carta vidimata          *
      *    *-----------------------------------------------------------*
       pmt-snx-cvd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa su carta vidimata   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-cvd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Ultima pagina stampata        *
      *    *-----------------------------------------------------------*
       pmt-ult-pst-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        rr-snx-tdf           not  = "S" and
                     rr-snx-cvd           not  = "N"
                     go to pmt-ult-pst-200.
       pmt-ult-pst-100.
      *              *-------------------------------------------------*
      *              * Se stampa NON vidimata                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ultima pagina stampata     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-ref-giv-ven-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-ult-pst-999.
       pmt-ult-pst-200.
      *              *-------------------------------------------------*
      *              * Se stampa vidimata                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-ult-pst-999.
       pmt-ult-pst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo richieste : Tipo dichiarazione         *
      *    *-----------------------------------------------------------*
       acc-tip-dic-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-dic           to   w-sav-tip-dic          .
       acc-tip-dic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dic-lun    to   v-car                  .
           move      w-exp-tip-dic-num    to   v-ldt                  .
           move      "MTA#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-dic-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        rr-tip-dic           =    "M"
                     move  01             to   v-num
           else if   rr-tip-dic           =    "T"
                     move  02             to   v-num
           else if   rr-tip-dic           =    "A"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-dic-999.
       acc-tip-dic-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "M"            to   rr-tip-dic
           else if   v-num                =    02
                     move  "T"            to   rr-tip-dic
           else if   v-num                =    03
                     move  "A"            to   rr-tip-dic
           else      move  spaces         to   rr-tip-dic             .
       acc-tip-dic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che esista un valore              *
      *                  *---------------------------------------------*
           if        rr-tip-dic           =    spaces
                     go to acc-tip-dic-100.
      *                  *---------------------------------------------*
      *                  * Se variato il tipo dichiarazione            *
      *                  *---------------------------------------------*
           if        rr-tip-dic           =    w-sav-tip-dic
                     go to acc-tip-dic-600.
           if        w-sav-tip-dic        =    spaces
                     go to acc-tip-dic-600.
      *                      *-----------------------------------------*
      *                      * Normalizzazione anno - trimestre - mese *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ann-dic             .
           move      zero                 to   rr-tri-dic             .
           move      zero                 to   rr-mes-dic             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione anno - trimestre - mese *
      *                      *-----------------------------------------*
           perform   vis-ann-dic-000      thru vis-ann-dic-999        .
           perform   vis-tri-dic-000      thru vis-tri-dic-999        .
           perform   vis-mes-dic-000      thru vis-mes-dic-999        .
       acc-tip-dic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
       acc-tip-dic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-dic-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-dic-100.
       acc-tip-dic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Anno dichiarazione         *
      *    *-----------------------------------------------------------*
       acc-ann-dic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-ann-dic           =    zero
                     move  s-ann          to   rr-ann-dic             .
       acc-ann-dic-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note                            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Impostazione di due cifre"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        rr-ann-dic           =    zero
                     move  "B"            to   v-edm
           else      move  "9"            to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ann-dic           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note                              *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ann-dic-999.
       acc-ann-dic-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ann-dic             .
       acc-ann-dic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione secolo                      *
      *                  *---------------------------------------------*
           move      "NS"                 to   s-ope                  .
           move      zero                 to   s-dat                  .
           move      rr-ann-dic           to   s-ann                  .
           move      01                   to   s-mes                  .
           move      01                   to   s-gio                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   rr-ann-dic             .
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-ann-dic           not  = zero
                     go to acc-ann-dic-500.
           if        v-key                =    "UP  "
                     go to acc-ann-dic-600
           else      go to acc-ann-dic-100.
       acc-ann-dic-500.
      *                  *---------------------------------------------*
      *                  * Visualizzazione anno                        *
      *                  *---------------------------------------------*
           perform   vis-ann-dic-000      thru vis-ann-dic-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione anno per esteso             *
      *                  *---------------------------------------------*
           perform   vis-ann-dic-ext-000  thru vis-ann-dic-ext-999    .
      *                  *---------------------------------------------*
      *                  * Se dichiarazione annuale controllo che sia- *
      *                  * no stati stampati tutti i movimenti del-    *
      *                  * l'anno                                      *
      *                  *---------------------------------------------*
           if        rr-tip-dic           not  = "A"
                     go to acc-ann-dic-600.
      *                      *-----------------------------------------*
      *                      * Richiamo routine di controllo           *
      *                      *-----------------------------------------*
           move      "A"                  to   w-cnt-ese-sgi-tpc      .
           move      rr-ann-dic           to   w-cnt-ese-sgi-saa      .
           perform   cnt-ese-sgi-000      thru cnt-ese-sgi-999        .
           if        w-cnt-ese-sgi-flg    =    spaces
                     go to acc-ann-dic-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing anno                        *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rr-ann-dic           to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Composizione messaggio - riga 1     *
      *                          *-------------------------------------*
           move      "La dichiarazione iva annuale definitiva puo' esser
      -              "e effettuata   "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Composizione messaggio - riga 2     *
      *                          *-------------------------------------*
           move      65                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "solamente dopo la stampa di tutti i giornali iva d
      -              "ell'anno  "         to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "!"                  to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-ann-dic-600.
       acc-ann-dic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ann-dic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ann-dic-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ann-dic-100.
       acc-ann-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Anno dichiarazione      *
      *    *-----------------------------------------------------------*
       vis-ann-dic-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        rr-ann-dic           =    zero
                     move  "B"            to   v-edm
           else      move  "9"            to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ann-dic           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Anno per esteso         *
      *    *-----------------------------------------------------------*
       vis-ann-dic-ext-000.
      *              *-------------------------------------------------*
      *              * Editing anno                                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rr-ann-dic           to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      06                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "("                  to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      ")"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-dic-ext-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo richieste : Trimestre dichiarazione    *
      *    *-----------------------------------------------------------*
       acc-tri-dic-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-dic           not  = "T"
                     go to acc-tri-dic-999.
       acc-tri-dic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tri-dic-lun    to   v-car                  .
           move      w-exp-tri-dic-num    to   v-ldt                  .
           move      "PSTQ#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tri-dic-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tri-dic           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tri-dic-999.
       acc-tri-dic-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tri-dic             .
       acc-tri-dic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-tri-dic           not  = zero
                     go to acc-tri-dic-500.
           if        v-key                =    "UP  "
                     go to acc-tri-dic-600
           else      go to acc-tri-dic-100.
       acc-tri-dic-500.
      *                  *---------------------------------------------*
      *                  * Test se valore ammesso                      *
      *                  *---------------------------------------------*
           if        rr-tri-dic           >    w-exp-tri-dic-num
                     go to acc-tri-dic-100.
       acc-tri-dic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tri-dic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tri-dic-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tri-dic-100.
       acc-tri-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Trimestre dichiarazione *
      *    *-----------------------------------------------------------*
       vis-tri-dic-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-dic           not  = "T"
                     go to vis-tri-dic-999.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tri-dic-lun    to   v-car                  .
           move      w-exp-tri-dic-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tri-dic-tbl    to   v-txt                  .
           move      rr-tri-dic           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tri-dic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo richieste : Mese dichiarazione         *
      *    *-----------------------------------------------------------*
       acc-mes-dic-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-dic           not  = "M"
                     go to acc-mes-dic-999.
       acc-mes-dic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-mes-dic           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-mes-dic-999.
       acc-mes-dic-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-mes-dic             .
       acc-mes-dic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-mes-dic           not  = zero
                     go to acc-mes-dic-500.
           if        v-key                =    "UP  "
                     go to acc-mes-dic-600
           else      go to acc-mes-dic-100.
       acc-mes-dic-500.
      *                  *---------------------------------------------*
      *                  * Test se valore ammesso                      *
      *                  *---------------------------------------------*
           if        rr-mes-dic           >    12
                     go to acc-mes-dic-100.
       acc-mes-dic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           perform   vis-mes-dic-000      thru vis-mes-dic-999        .
       acc-mes-dic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-mes-dic-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-mes-dic-100.
       acc-mes-dic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Mese dichiarazione      *
      *    *-----------------------------------------------------------*
       vis-mes-dic-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-dic           not  = "M"
                     go to vis-mes-dic-999.
      *              *-------------------------------------------------*
      *              * Valore numerico                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-mes-dic           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Test se zero                                    *
      *              *-------------------------------------------------*
           if        rr-mes-dic           not  = zero
                     go to vis-mes-dic-200.
       vis-mes-dic-100.
      *              *-------------------------------------------------*
      *              * Literal alfanumerico a spaces                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-mes-dic-999.
       vis-mes-dic-200.
      *              *-------------------------------------------------*
      *              * Literal del mese, da segreteria                 *
      *              *-------------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      rr-mes-dic           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Tra parentesi                                   *
      *              *-------------------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "("                  to   w-all-str-cat (1)      .
           move      s-alf                to   w-all-str-cat (2)      .
           move      ")"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Literal alfanumerico                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mes-dic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo richieste : Tipo stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data iniziale e finale       *
      *                  *---------------------------------------------*
           perform   det-ini-fin-000      thru det-ini-fin-999        .
       acc-tip-stp-050.
      *                  *---------------------------------------------*
      *                  * Controllo stampa giornali iva relativi al   *
      *                  * periodo richiesto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se dichiarazione annuale                *
      *                      *-----------------------------------------*
           if        rr-tip-dic           not  = "A"
                     go to acc-tip-stp-060.
           if        w-cnt-ese-sgi-flg    =    spaces
                     go to acc-tip-stp-100.
           move      "P"                  to   rr-tip-stp             .
           perform   vis-tip-stp-000      thru vis-tip-stp-999        .
           go to     acc-tip-stp-999.
       acc-tip-stp-060.
      *                      *-----------------------------------------*
      *                      * Richiamo routine di controllo           *
      *                      *-----------------------------------------*
           if        rr-tip-dic           =    "M"
                     move  "M"            to   w-cnt-ese-sgi-tpc
                     move  rr-ann-dic     to   w-cnt-ese-sgi-saa
                     move  rr-mes-dic     to   w-cnt-ese-sgi-tom
           else if   rr-tip-dic           =    "T"
                     move  "T"            to   w-cnt-ese-sgi-tpc
                     move  rr-ann-dic     to   w-cnt-ese-sgi-saa
                     move  rr-tri-dic     to   w-cnt-ese-sgi-tom      .
           perform   cnt-ese-sgi-000      thru cnt-ese-sgi-999        .
           if        w-cnt-ese-sgi-flg    =    spaces
                     go to acc-tip-stp-100.
      *                      *-----------------------------------------*
      *                      * Se controllo non superato si forza la   *
      *                      * stampa di prova e si esce               *
      *                      *-----------------------------------------*
           move      "P"                  to   rr-tip-stp             .
           perform   vis-tip-stp-000      thru vis-tip-stp-999        .
           go to     acc-tip-stp-999.
       acc-tip-stp-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "La stampa di PROVA non aggiorna i saldi per il mes
      -              "e successivo "      to   v-nt1                  .
           move      "La stampa DEFINITIVA aggiorna i saldi ma puo' esse
      -              "re ripetuta  "      to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      "PD#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tip-stp           =    "P"
                     move  01             to   v-num
           else if   rr-tip-stp           =    "D"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-stp-999.
       acc-tip-stp-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "P"            to   rr-tip-stp
           else if   v-num                =    02
                     move  "D"            to   rr-tip-stp
           else      move  spaces         to   rr-tip-stp             .
       acc-tip-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = spaces
                     go to acc-tip-stp-500.
           if        v-key                =    "UP  "
                     go to acc-tip-stp-600
           else      go to acc-tip-stp-100.
       acc-tip-stp-500.
      *                  *---------------------------------------------*
      *                  * Test se valore ammesso                      *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = "P" and
                     rr-tip-stp           not  = "D"
                     go to acc-tip-stp-100.
       acc-tip-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se stampa definitiva                        *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    "D"
                     go to  acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * No carta vidimata                           *
      *                  *---------------------------------------------*
           move      "N"                  to   rr-snx-cvd             .
           perform   vis-snx-cvd-000      thru vis-snx-cvd-999        .
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
      *    * Visualizzazione campo richieste : Tipo stampa             *
      *    *-----------------------------------------------------------*
       vis-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           if        rr-tip-stp           =    "P"
                     move  01             to   v-num
           else if   rr-tip-stp           =    "D"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo richieste : Si/no tipo definitiva      *
      *    *-----------------------------------------------------------*
       acc-snx-tdf-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    "D"
                     go to acc-snx-tdf-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-snx-tdf           =    spaces
                     move  "N"            to   rr-snx-tdf             .
       acc-snx-tdf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tdf-lun    to   v-car                  .
           move      w-exp-snx-tdf-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tdf-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-snx-tdf           =    "N"
                     move  01             to   v-num
           else if   rr-snx-tdf           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-tdf-999.
       acc-snx-tdf-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   rr-snx-tdf
           else if   v-num                =    02
                     move  "S"            to   rr-snx-tdf
           else      move  spaces         to   rr-snx-tdf             .
       acc-snx-tdf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-tdf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-tdf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-tdf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-tdf-100.
       acc-snx-tdf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no tipo definitiva             *
      *    *-----------------------------------------------------------*
       vis-snx-tdf-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tdf-lun    to   v-car                  .
           move      w-exp-snx-tdf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tdf-tbl    to   v-txt                  .
           if        rr-snx-tdf           =    "N"
                     move  01             to   v-num
           else if   rr-snx-tdf           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-tdf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no carta vidimata                 *
      *    *-----------------------------------------------------------*
       acc-snx-cvd-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare :                *
      *                  *---------------------------------------------*
           if        rr-snx-tdf           =    "S"
                     go to acc-snx-cvd-050.
           if        rr-tip-stp           not  = "D" and
                     rr-tip-stp           not  = "R"
                     go to acc-snx-cvd-999.
       acc-snx-cvd-050.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-snx-cvd           not  = spaces
                     go to acc-snx-cvd-100.
           move      "N"                  to   rr-snx-cvd             .
           perform   vis-snx-cvd-000      thru vis-snx-cvd-999        .
       acc-snx-cvd-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Si : il programma non stampa l'intestazione delle 
      -              "pagine"             to   v-nt1                  .
           move      "No : il programma stampa l'intestazione delle pagi
      -              "ne e le numera"     to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-cvd-lun    to   v-car                  .
           move      w-exp-snx-cvd-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-cvd-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-snx-cvd           =    "S"
                     move  01             to   v-num
           else if   rr-snx-cvd           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-cvd-999.
       acc-snx-cvd-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-snx-cvd
           else if   v-num                =    02
                     move  "N"            to   rr-snx-cvd
           else      move  spaces         to   rr-snx-cvd             .
       acc-snx-cvd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che esista un valore              *
      *                  *---------------------------------------------*
           if        rr-snx-cvd           =    spaces
                     go to acc-snx-cvd-100.
       acc-snx-cvd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se no carta vidimata                        *
      *                  *---------------------------------------------*
           if        rr-snx-cvd           =    "S"
                     move  zero           to   rr-ult-pst
                     go to  acc-snx-cvd-620.
      *                  *---------------------------------------------*
      *                  * Determinazione ultima pagina stampata       *
      *                  *---------------------------------------------*
           move      rr-dat-ini           to   w-ylp-iva-dat-ini      .
           perform   det-ult-pag-000      thru det-ult-pag-999        .
           move      w-ylp-iva-ult-pag    to   rr-ult-pst             .
       acc-snx-cvd-620.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ultima pagina stampata      *
      *                  *---------------------------------------------*
           perform   pmt-ult-pst-000      thru pmt-ult-pst-999        .
           perform   vis-ult-pst-000      thru vis-ult-pst-999        .
       acc-snx-cvd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-cvd-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-cvd-100.
       acc-snx-cvd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no carta vidimata              *
      *    *-----------------------------------------------------------*
       vis-snx-cvd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-cvd-lun    to   v-car                  .
           move      w-exp-snx-cvd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-cvd-tbl    to   v-txt                  .
      *
           if        rr-snx-cvd           =    "S"
                     move  01             to   v-num
           else if   rr-snx-cvd           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-cvd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Ultima pagina stampata               *
      *    *-----------------------------------------------------------*
       acc-ult-pst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
______*    if        rr-tip-stp           not  = "D" and
______*              rr-tip-stp           not  = "R"
______*              go to acc-ult-pst-999.
           if        rr-snx-cvd           not  = "N"
                     go to acc-ult-pst-999.
       acc-ult-pst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ult-pst           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ult-pst-999.
       acc-ult-pst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ult-pst             .
       acc-ult-pst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ult-pst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ult-pst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ult-pst-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ult-pst-100.
       acc-ult-pst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ultima pagina stampata            *
      *    *-----------------------------------------------------------*
       vis-ult-pst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ult-pst           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ult-pst-999.
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
      *              * Test su tipo dichiarazione                      *
      *              *-------------------------------------------------*
           if        rr-tip-dic           not  = spaces
                     go to tdo-ric-sel-200.
           move      "Manca la definizione del tipo dichiarazione iva da
      -              " effettuare    "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Test su anno dichiarazione                      *
      *              *-------------------------------------------------*
           if        rr-ann-dic           not  = zero
                     go to tdo-ric-sel-300.
           move      "Manca l'anno per la dichiarazione                 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Test su trimestre dichiarazione                 *
      *              *-------------------------------------------------*
           if        rr-tip-dic           not  = "T"
                     go to tdo-ric-sel-400.
           if        rr-tri-dic           not  = zero
                     go to tdo-ric-sel-400.
           move      "Manca il numero di trimestre per la dichiarazione 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Test su mese dichiarazione                      *
      *              *-------------------------------------------------*
           if        rr-tip-dic           not  = "M"
                     go to tdo-ric-sel-500.
           if        rr-mes-dic           not  = zero
                     go to tdo-ric-sel-500.
           move      "Manca la definizione del mese per la dichiarazione
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Test su tipo stampa                             *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = spaces
                     go to tdo-ric-sel-600.
           move      "Manca la definizione del tipo stampa da eseguire  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
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
      *                  * Emissione messaggio                         *
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
      *              * Determinazione data iniziale e finale           *
      *              *-------------------------------------------------*
           perform   det-ini-fin-000      thru det-ini-fin-999        .
       reg-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Si/no tipo definitiva                           *
      *              *-------------------------------------------------*
           if        rr-snx-tdf           =    spaces
                     move  "N"            to   rr-snx-tdf             .
      *              *-------------------------------------------------*
      *              * Ultimo numero di pagina stampata                *
      *              *-------------------------------------------------*
______*    if        rr-tip-stp           =    "P" or
______*              rr-snx-cvd           =    "S"
______*              move 00001           to   rr-ult-pst             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-tip-stp             .
           move      spaces               to   rr-tip-dic             .
           move      zero                 to   rr-ann-dic             .
           move      zero                 to   rr-tri-dic             .
           move      zero                 to   rr-mes-dic             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-mes-ini             .
           move      zero                 to   rr-mes-fin             .
           move      spaces               to   rr-snx-tdf             .
           move      spaces               to   rr-snx-cvd             .
           move      zero                 to   rr-ult-pst             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Controllo esecuzione stampa giornali iva                  *
      *    *-----------------------------------------------------------*
       cnt-ese-sgi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-ese-sgi-flg      .
      *              *-------------------------------------------------*
      *              * Controllo formale valori in input               *
      *              *-------------------------------------------------*
           if        w-cnt-ese-sgi-tpc    not  = "M" and
                     w-cnt-ese-sgi-tpc    not  = "T" and
                     w-cnt-ese-sgi-tpc    not  = "A"
                     go to cnt-ese-sgi-999.
           if        w-cnt-ese-sgi-saa    =    zero
                     go to cnt-ese-sgi-999.
           if       (w-cnt-ese-sgi-tpc    =    "M"  ) and
                    (w-cnt-ese-sgi-tom    <    01  or
                     w-cnt-ese-sgi-tom    >    12   )
                     go to cnt-ese-sgi-999.
           if       (w-cnt-ese-sgi-tpc    =    "T"  ) and
                    (w-cnt-ese-sgi-tom    <    01  or
                     w-cnt-ese-sgi-tom    >    04   )
                     go to cnt-ese-sgi-999.
       cnt-ese-sgi-100.
      *              *-------------------------------------------------*
      *              * Determinazione data iniziale per scansione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo controllo   *
      *                  *---------------------------------------------*
           if        w-cnt-ese-sgi-tpc    =    "M"
                     go to cnt-ese-sgi-120
           else if   w-cnt-ese-sgi-tpc    =    "T"
                     go to cnt-ese-sgi-140
           else if   w-cnt-ese-sgi-tpc    =    "A"
                     go to cnt-ese-sgi-160.
       cnt-ese-sgi-120.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Mensile                    *
      *                  *---------------------------------------------*
           move      31                   to   w-cnt-ese-sgi-wgi      .
           move      w-cnt-ese-sgi-tom    to   w-cnt-ese-sgi-wmi      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-wai      .
           go to     cnt-ese-sgi-200.
       cnt-ese-sgi-140.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Trimestrale                *
      *                  *---------------------------------------------*
           move      31                   to   w-cnt-ese-sgi-wgi      .
           move      w-cnt-ese-sgi-tom    to   w-cnt-ese-sgi-wmi      .
           multiply  3                    by   w-cnt-ese-sgi-wmi      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-wai      .
           go to     cnt-ese-sgi-200.
       cnt-ese-sgi-160.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Annuale                    *
      *                  *---------------------------------------------*
           move      31                   to   w-cnt-ese-sgi-wgi      .
           move      12                   to   w-cnt-ese-sgi-wmi      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-wai      .
           go to     cnt-ese-sgi-200.
       cnt-ese-sgi-200.
      *              *-------------------------------------------------*
      *              * Determinazione data finale per scansione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo controllo   *
      *                  *---------------------------------------------*
           if        w-cnt-ese-sgi-tpc    =    "M"
                     go to cnt-ese-sgi-220
           else if   w-cnt-ese-sgi-tpc    =    "T"
                     go to cnt-ese-sgi-240
           else if   w-cnt-ese-sgi-tpc    =    "A"
                     go to cnt-ese-sgi-260.
       cnt-ese-sgi-220.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Mensile                    *
      *                  *---------------------------------------------*
           move      01                   to   w-cnt-ese-sgi-wgf      .
           move      w-cnt-ese-sgi-tom    to   w-cnt-ese-sgi-wmf      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-waf      .
           go to     cnt-ese-sgi-300.
       cnt-ese-sgi-240.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Trimestrale                *
      *                  *---------------------------------------------*
           move      01                   to   w-cnt-ese-sgi-wgf      .
           move      w-cnt-ese-sgi-tom    to   w-cnt-ese-sgi-wmf      .
           multiply  3                    by   w-cnt-ese-sgi-wmf      .
           subtract  2                    from w-cnt-ese-sgi-wmf      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-waf      .
           go to     cnt-ese-sgi-300.
       cnt-ese-sgi-260.
      *                  *---------------------------------------------*
      *                  * Tipo controllo : Annuale                    *
      *                  *---------------------------------------------*
           move      01                   to   w-cnt-ese-sgi-wgf      .
           move      01                   to   w-cnt-ese-sgi-wmf      .
           move      w-cnt-ese-sgi-saa    to   w-cnt-ese-sgi-waf      .
           go to     cnt-ese-sgi-300.
       cnt-ese-sgi-300.
      *              *-------------------------------------------------*
      *              * Controllo ultimo movimento iva acquisti         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mgi]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "DATDOC    "         to   f-key                  .
           move      "F"                  to   rf-mgi-tip-rec         .
           move      w-cnt-ese-sgi-wdi    to   rf-mgi-dat-reg         .
           move      99                   to   rf-mgi-cod-num         .
           move      99999999999          to   rf-mgi-prt-iva         .
           move      9999999              to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : controllo superato    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-400.
      *                  *---------------------------------------------*
      *                  * Lettura ultimo record [mgi]                 *
      *                  *---------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se 'At End' : controllo superato        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-400.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo record                     *
      *                      *-----------------------------------------*
           if        rf-mgi-tip-rec       not  = "F"
                     go to cnt-ese-sgi-400.
      *                      *-----------------------------------------*
      *                      * Test su data registrazione              *
      *                      *-----------------------------------------*
           if        rf-mgi-dat-reg       <    w-cnt-ese-sgi-wdf
                     go to cnt-ese-sgi-400.
      *                  *---------------------------------------------*
      *                  * Test su flag di stampa                      *
      *                  *---------------------------------------------*
           if        rf-mgi-flg-gio       =    spaces
                     move  "F"            to   w-cnt-ese-sgi-flg
                     go to cnt-ese-sgi-999.
       cnt-ese-sgi-400.
      *              *-------------------------------------------------*
      *              * Controllo ultimo movimento iva vendite          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mgi]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "DATDOC    "         to   f-key                  .
           move      "C"                  to   rf-mgi-tip-rec         .
           move      w-cnt-ese-sgi-wdi    to   rf-mgi-dat-reg         .
           move      99                   to   rf-mgi-cod-num         .
           move      99999999999          to   rf-mgi-prt-iva         .
           move      9999999              to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : controllo superato    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-500.
      *                  *---------------------------------------------*
      *                  * Lettura ultimo record [mgi]                 *
      *                  *---------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se 'At End' : controllo superato        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo record                     *
      *                      *-----------------------------------------*
           if        rf-mgi-tip-rec       not  = "C"
                     go to cnt-ese-sgi-500.
      *                      *-----------------------------------------*
      *                      * Test su data registrazione              *
      *                      *-----------------------------------------*
           if        rf-mgi-dat-reg       <    w-cnt-ese-sgi-wdf
                     go to cnt-ese-sgi-500.
      *                  *---------------------------------------------*
      *                  * Test su flag di stampa                      *
      *                  *---------------------------------------------*
           if        rf-mgi-flg-gio       =    spaces
                     move  "C"            to   w-cnt-ese-sgi-flg
                     go to cnt-ese-sgi-999.
       cnt-ese-sgi-500.
      *              *-------------------------------------------------*
      *              * Controllo ultimo movimento iva corrispettivi    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mgi]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "DATDOC    "         to   f-key                  .
           move      "V"                  to   rf-mgi-tip-rec         .
           move      w-cnt-ese-sgi-wdi    to   rf-mgi-dat-reg         .
           move      99                   to   rf-mgi-cod-num         .
           move      99999999999          to   rf-mgi-prt-iva         .
           move      9999999              to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : controllo superato    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-600.
      *                  *---------------------------------------------*
      *                  * Lettura ultimo record [mgi]                 *
      *                  *---------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                      *-----------------------------------------*
      *                      * Se 'At End' : controllo superato        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-ese-sgi-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo record                     *
      *                      *-----------------------------------------*
           if        rf-mgi-tip-rec       not  = "V"
                     go to cnt-ese-sgi-600.
      *                      *-----------------------------------------*
      *                      * Test su data registrazione              *
      *                      *-----------------------------------------*
           if        rf-mgi-dat-reg       <    w-cnt-ese-sgi-wdf
                     go to cnt-ese-sgi-600.
      *                  *---------------------------------------------*
      *                  * Test su flag di stampa                      *
      *                  *---------------------------------------------*
           if        rf-mgi-flg-gio       =    spaces
                     move  "C"            to   w-cnt-ese-sgi-flg
                     go to cnt-ese-sgi-999.
       cnt-ese-sgi-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cnt-ese-sgi-999.
       cnt-ese-sgi-999.
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
      *    * Routine di determinazione data iniziale e finale          *
      *    *-----------------------------------------------------------*
       det-ini-fin-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo dichiarazione   *
      *              *-------------------------------------------------*
           if        rr-tip-dic           =    "M"
                     go to det-ini-fin-100
           else if   rr-tip-dic           =    "T"
                     go to det-ini-fin-200
           else if   rr-tip-dic           =    "A"
                     go to det-ini-fin-300.
       det-ini-fin-100.
      *              *-------------------------------------------------*
      *              * Tipo dichiarazione : Mensile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      01                   to   s-gio                  .
           move      rr-mes-dic           to   s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
           move      s-dat                to   rr-dat-ini             .
      *                  *---------------------------------------------*
      *                  * Data finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      31                   to   s-gio                  .
           move      rr-mes-dic           to   s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
       det-ini-fin-110.
           move      "CD"                 to   s-ope
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-ini-fin-110.
           move      s-dat                to   rr-dat-fin             .
      *                  *---------------------------------------------*
      *                  * Mese iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      rr-mes-dic           to   rr-mes-ini             .
      *                  *---------------------------------------------*
      *                  * Mese finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      rr-mes-dic           to   rr-mes-fin             .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-ini-fin-900.
       det-ini-fin-200.
      *              *-------------------------------------------------*
      *              * Tipo dichiarazione : Trimestrale                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      01                   to   s-gio                  .
           move      rr-tri-dic           to   s-mes                  .
           multiply  3                    by   s-mes                  .
           subtract  2                    from s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
           move      s-dat                to   rr-dat-ini             .
      *                  *---------------------------------------------*
      *                  * Data finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      31                   to   s-gio                  .
           move      rr-tri-dic           to   s-mes                  .
           multiply  3                    by   s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
       det-ini-fin-210.
           move      "CD"                 to   s-ope
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-ini-fin-210.
           move      s-dat                to   rr-dat-fin             .
      *                  *---------------------------------------------*
      *                  * Mese iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      rr-tri-dic           to   rr-mes-ini             .
           multiply  3                    by   rr-mes-ini             .
           subtract  2                    from rr-mes-ini             .
      *                  *---------------------------------------------*
      *                  * Mese finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      rr-tri-dic           to   rr-mes-fin             .
           multiply  3                    by   rr-mes-fin             .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-ini-fin-900.
       det-ini-fin-300.
      *              *-------------------------------------------------*
      *              * Tipo dichiarazione : Annuale                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      01                   to   s-gio                  .
           move      01                   to   s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
           move      s-dat                to   rr-dat-ini             .
      *                  *---------------------------------------------*
      *                  * Data finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      31                   to   s-gio                  .
           move      12                   to   s-mes                  .
           move      rr-ann-dic           to   s-saa                  .
           move      s-dat                to   rr-dat-fin             .
      *                  *---------------------------------------------*
      *                  * Mese iniziale periodo richiesto             *
      *                  *---------------------------------------------*
           move      01                   to   rr-mes-ini             .
      *                  *---------------------------------------------*
      *                  * Mese finale periodo richiesto               *
      *                  *---------------------------------------------*
           move      12                   to   rr-mes-fin             .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-ini-fin-900.
       det-ini-fin-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ini-fin-999.
       det-ini-fin-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione numero ultima pagina            *
      *    *-----------------------------------------------------------*
       det-ult-pag-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-ylp-iva-ult-pag      .
       det-ult-pag-100.
      *              *-------------------------------------------------*
      *              * Lettura year last page                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Secolo anno da data iniziale                *
      *                  *---------------------------------------------*
           move      w-ylp-iva-dat-ini    to   s-dat                  .
           move      s-saa                to   w-ylp-iva-dat-saa      .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "Yg"                 to   s-ope                  .
           move      w-ref-giv-ven-cod    to   s-fas                  .
           move      w-ylp-iva-dat-saa    to   s-saa                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to det-ult-pag-400.
       det-ult-pag-200.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "Yp"                 to   s-ope                  .
           move      w-ref-giv-ven-cod    to   s-fas                  .
           move      w-ylp-iva-dat-saa    to   s-saa                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     det-ult-pag-100.
       det-ult-pag-400.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore letto             *
      *                  *---------------------------------------------*
           move      s-num                to   w-ylp-iva-ult-pag      .
       det-ult-pag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ult-pag-999.
       det-ult-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

