       Identification Division.
       Program-Id.                                 porc3200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    mov                 *
      *                                   Fase:    orc320              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/01/94    *
      *                       Ultima revisione:    NdK del 19/07/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma porc3201:        *
      *                                                                *
      *                    Portafoglio ordini                          *
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
                     "orc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "orc320"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "porc3200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "           PORTAFOGLIO ORDINI           "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "porc3201  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/orc/prg/obj/porc3201                "       .

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
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo documento, se spaces : tutti                     *
      *        *-------------------------------------------------------*
           05  rr-tip-mov                 pic  x(05)                  .
           05  rr-tip-mov-des             pic  x(30)                  .
           05  rr-tip-mov-vld             pic  9(02)                  .
           05  rr-tip-mov-dpz             pic  9(02)                  .
           05  rr-tip-mov-tar             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data ordine min - max                                 *
      *        *-------------------------------------------------------*
           05  rr-dat-min                 pic  9(07)                  .
           05  rr-dat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data consegna richiesta min - max                     *
      *        *-------------------------------------------------------*
           05  rr-ric-min                 pic  9(07)                  .
           05  rr-ric-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero ordine per accettazione min - max              *
      *        *-------------------------------------------------------*
           05  rr-num-min                 pic  9(09)                  .
           05  rr-num-max                 pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Numero ordine interno min - max                       *
      *        *-------------------------------------------------------*
           05  rr-noc-min                 pic  9(11)                  .
           05  rr-noc-max                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente min - max                              *
      *        *-------------------------------------------------------*
           05  rr-cli-min                 pic  9(07)                  .
           05  rr-cli-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dipendenza del cliente min - max                      *
      *        *-------------------------------------------------------*
           05  rr-dpz-min                 pic  x(04)                  .
           05  rr-dpz-max                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente                                         *
      *        *-------------------------------------------------------*
           05  rr-cod-age                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, denominazione                          *
      *        *-------------------------------------------------------*
           05  rr-cod-age-den             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Stato degli ordini                                    *
      *        *                                                       *
      *        * - 01 : Solo quelli da evadere                         *
      *        * - 02 : Solo quelli in spedizione                      *
      *        * - 03 : Solo quelli evasi                              *
      *        * - 04 : Tutti                                          *
      *        *-------------------------------------------------------*
           05  rr-sts-orc                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Opzioni di stampa                                     *
      *        *                                                       *
      *        *  - Spaces : No                                        *
      *        *  - X      : Si                                        *
      *        *-------------------------------------------------------*
           05  rr-opz-stp.
      *            *---------------------------------------------------*
      *            * Stampa dei prezzi                                 *
      *            *---------------------------------------------------*
               10  rr-opz-stp-prz         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Stampa delle ubicazioni                           *
      *            *---------------------------------------------------*
               10  rr-opz-stp-ubi         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Solo ordini movimentati                           *
      *            *---------------------------------------------------*
               10  rr-opz-stp-mov         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Anche ordini Pro-forma                            *
      *            *---------------------------------------------------*
               10  rr-opz-stp-pfm         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Stampa completa ordine                            *
      *            *---------------------------------------------------*
               10  rr-opz-stp-tip         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento righe                                *
      *        *                                                       *
      *        * - 01 : Per progressivo riga                           *
      *        * - 02 : Per ubicazione                                 *
      *        * - 03 : Per codice prodotto                            *
      *        * - 04 : Per sinonimo prodotto                          *
      *        *-------------------------------------------------------*
           05  rr-ord-rig                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zoc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zoc.
               10  w-let-arc-zoc-flg      pic  x(01)                  .
               10  w-let-arc-zoc-cod      pic  x(05)                  .
               10  w-let-arc-zoc-des      pic  x(30)                  .
               10  w-let-arc-zoc-vld      pic  9(02)                  .
               10  w-let-arc-zoc-dpz      pic  9(02)                  .
               10  w-let-arc-zoc-ord      pic  9(02)                  .
               10  w-let-arc-zoc-prd      pic  9(02)                  .
               10  w-let-arc-zoc-sgl      pic  x(03)                  .
               10  w-let-arc-zoc-tar      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .

      *    *===========================================================*
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Numero ordine per accettazione                        *
      *        *-------------------------------------------------------*
           05  w-wrk-nft-acc              pic  9(09)                  .
           05  w-wrk-nft-acc-r            redefines
               w-wrk-nft-acc.
               10  w-wrk-num-saa          pic  9(03)                  .
               10  w-wrk-num-npg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero ordine internamente                            *
      *        *-------------------------------------------------------*
           05  w-wrk-nft-int              pic  9(11)                  .
           05  w-wrk-nft-int-r            redefines
               w-wrk-nft-int.
               10  w-wrk-noc-saa          pic  9(03)                  .
               10  w-wrk-noc-dpz          pic  9(02)                  .
               10  w-wrk-noc-npg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione tipi movimento              *
      *    *-----------------------------------------------------------*
       01  w-tmo.
      *        *-------------------------------------------------------*
      *        * Contatore di elementi                                 *
      *        *-------------------------------------------------------*
           05  w-tmo-ctr-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice tipo ordine di default                         *
      *        *-------------------------------------------------------*
           05  w-tmo-cod-tmo              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione tipo ordine di default                    *
      *        *-------------------------------------------------------*
           05  w-tmo-des-tmo              pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di numerazioni                              *
      *        *-------------------------------------------------------*
           05  w-tmo-ctr-num              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Sigla numerazione, se unica                           *
      *        *-------------------------------------------------------*
           05  w-tmo-cod-num              pic  x(03)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Status ordini clienti                      *
      *        *-------------------------------------------------------*
           05  w-exp-sts-orc.
               10  w-exp-sts-orc-num      pic  9(02)       value 04   .
               10  w-exp-sts-orc-lun      pic  9(02)       value 25   .
               10  w-exp-sts-orc-tbl.
                   15  filler             pic  x(25) value
                            "solo quelli Da evadere   "               .
                   15  filler             pic  x(25) value
                            "solo quelli In spedizione"               .
                   15  filler             pic  x(25) value
                            "solo quelli Gia' evasi   "               .
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
      *        *-------------------------------------------------------*
      *        * Work per : Opzioni di stampa                          *
      *        *-------------------------------------------------------*
           05  w-exp-opz-stp.
               10  w-exp-opz-stp-num      pic  9(02)       value 05   .
               10  w-exp-opz-stp-lun      pic  9(02)       value 40   .
               10  w-exp-opz-stp-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Stampa dei prezzi                   ".
                   15  filler             pic  x(40) value
                            "[ ] Stampa delle ubicazioni             ".
                   15  filler             pic  x(40) value
                            "[ ] Solo ordini movimentati             ".
                   15  filler             pic  x(40) value
                            "[ ] Anche ordini Pro-forma              ".
                   15  filler             pic  x(40) value
                            "[ ] Stampa completa ordine              ".
      *        *-------------------------------------------------------*
      *        * Work per : Ordinamento righe                          *
      *        *-------------------------------------------------------*
           05  w-exp-ord-rig.
               10  w-exp-ord-rig-num      pic  9(02)       value 04   .
               10  w-exp-ord-rig-lun      pic  9(02)       value 40   .
               10  w-exp-ord-rig-tbl.
                   15  filler             pic  x(40) value
                            "per progressivo di Inserimento          " .
                   15  filler             pic  x(40) value
                            "per Ubicazione / codice prodotto        " .
                   15  filler             pic  x(40) value
                            "per Codice prodotto                     " .
                   15  filler             pic  x(40) value
                            "per Sinonimo prodotto                   " .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento ordini clienti  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cpw"                   .

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
      *              * Filtraggio pathname dell'overlay di esecuzione  *
      *              *-------------------------------------------------*
           move      i-exe-pat            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
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
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      i-ide-des            to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-120.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-120.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-180.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo selezione    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
      *              *-------------------------------------------------*
      *              * Precaricamento tabella tipi movimento per de-   *
      *              * faults                                          *
      *              *-------------------------------------------------*
           perform   pre-tbl-tmo-000      thru pre-tbl-tmo-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Precaricamento tabella tipi movimento                     *
      *    *-----------------------------------------------------------*
       pre-tbl-tmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tmo-ctr-ele          .
           move      spaces               to   w-tmo-cod-tmo          .
           move      spaces               to   w-tmo-des-tmo          .
           move      zero                 to   w-tmo-ctr-num          .
           move      spaces               to   w-tmo-cod-num          .
       pre-tbl-tmo-100.
      *              *-------------------------------------------------*
      *              * Open file [zoc]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Inizio ciclo di caricamento                     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODTOC    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Se errore di start : uscita con flag di errore  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  pre-tbl-tmo-900.
       pre-tbl-tmo-200.
      *              *-------------------------------------------------*
      *              * Read-next su file [zoc]                         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Se errore di start : uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-tbl-tmo-900.
       pre-tbl-tmo-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore tipi ordine                *
      *              *-------------------------------------------------*
           add       1                    to   w-tmo-ctr-ele          .
      *              *-------------------------------------------------*
      *              * Bufferizzazioni per tipo ordine                 *
      *              *-------------------------------------------------*
           move      rf-zoc-cod-toc       to   w-tmo-cod-tmo          .
           move      rf-zoc-des-toc       to   w-tmo-des-tmo          .
       pre-tbl-tmo-400.
      *              *-------------------------------------------------*
      *              * Se memorizzati attualmente zero tipi di numera- *
      *              * zione : si pone il contatore di numerazioni a 1 *
      *              * e si memorizza il codice della numerazione, e   *
      *              * poi si ricicla ell'elemento successivo          *
      *              *-------------------------------------------------*
           if        w-tmo-ctr-num        =    zero
                     move  1              to   w-tmo-ctr-num
                     move  rf-zoc-sgl-num to   w-tmo-cod-num
                     go to pre-tbl-tmo-500.
      *              *-------------------------------------------------*
      *              * Se gia' memorizzato piu' di un tipo di numera-  *
      *              * zione : si ricicla ell'elemento successivo      *
      *              *-------------------------------------------------*
           if        w-tmo-ctr-num        >    1
                     go to pre-tbl-tmo-500.
      *              *-------------------------------------------------*
      *              * Se la sigla numerazione del tipo documento in   *
      *              * esame e' pari a quella gia' memorizzata : si    *
      *              * ricixla ell'elemento successivo                 *
      *              *-------------------------------------------------*
           if        rf-zoc-sgl-num       =    w-tmo-cod-num
                     go to pre-tbl-tmo-500.
      *              *-------------------------------------------------*
      *              * In caso contrario si incrementa, portando a 2,  *
      *              * il contatore dei tipi di numerazione            *
      *              *-------------------------------------------------*
           add       1                    to   w-tmo-ctr-num          .
       pre-tbl-tmo-500.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     pre-tbl-tmo-200.
       pre-tbl-tmo-900.
      *              *-------------------------------------------------*
      *              * Close file [zoc]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
       pre-tbl-tmo-999.
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
      *              * [zoc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento         *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zoc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento        *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
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
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   acc-tip-mov-000      thru acc-tip-mov-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Data ordine minima                          *
      *                  *---------------------------------------------*
           perform   acc-dat-min-000      thru acc-dat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-215.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data ordine massima                         *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-215.
      *                  *---------------------------------------------*
      *                  * Data consegna richiesta min                 *
      *                  *---------------------------------------------*
           perform   acc-ric-min-000      thru acc-ric-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-250.
       acc-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Data consegna richiesta max                 *
      *                  *---------------------------------------------*
           perform   acc-ric-max-000      thru acc-ric-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-215.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Numero ordine minimo                        *
      *                  *---------------------------------------------*
           perform   acc-num-min-000      thru acc-num-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-215.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Numero ordine massimo                       *
      *                  *---------------------------------------------*
           perform   acc-num-max-000      thru acc-num-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Codice cliente minimo                       *
      *                  *---------------------------------------------*
           perform   acc-cli-min-000      thru acc-cli-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Codice cliente massimo                      *
      *                  *---------------------------------------------*
           perform   acc-cli-max-000      thru acc-cli-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza cliente minimo            *
      *                  *---------------------------------------------*
           perform   acc-dpz-min-000      thru acc-dpz-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza cliente massimo           *
      *                  *---------------------------------------------*
           perform   acc-dpz-max-000      thru acc-dpz-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Selezione su stato ordini                   *
      *                  *---------------------------------------------*
           perform   acc-sts-orc-000      thru acc-sts-orc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Opzioni di stampa                           *
      *                  *---------------------------------------------*
           perform   acc-opz-stp-000      thru acc-opz-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Ordinamento righe                           *
      *                  *---------------------------------------------*
           perform   acc-ord-rig-000      thru acc-ord-rig-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-650.
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
      *              * Tipo ordine                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordine                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Data emissione ordine min e max                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data ordine            dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Data consegna richiesta min e max               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data consegna rich.    dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero ordine minimo e massimo                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero ordine          dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cliente minimo e massimo                        *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Dipendenza cliente minima e massima             *
      *              *-------------------------------------------------*
           perform   pmt-dpz-cli-000      thru pmt-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Cliente agente                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stato degli ordini                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stato degli ordini         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Opzioni di stampa                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Opzioni di stampa          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ordinamento righe                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento righe ordine   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per cliente min e max              *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo archivio rela-  *
      *              * tivo al tipo movimento                          *
      *              *-------------------------------------------------*
           if        rr-tip-mov-tar       =    "C"
                     go to pmt-cod-cli-100
           else if   rr-tip-mov-tar       =    "D"
                     go to pmt-cod-cli-500
           else      go to pmt-cod-cli-800.
       pmt-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Se Tipo archivio : cliente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente         dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-cli-900.
       pmt-cod-cli-500.
      *              *-------------------------------------------------*
      *              * Se Tipo archivio : ns. dipendenza               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice ns. dipendenza  dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-cli-900.
       pmt-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Per tutti gli altri tipi stampa                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente         dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-cli-900.
       pmt-cod-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-cod-cli-999.
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per dipendenza cliente min e max   *
      *    *-----------------------------------------------------------*
       pmt-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo archivio rela-  *
      *              * tivo al tipo movimento                          *
      *              *-------------------------------------------------*
           if        rr-tip-mov-tar       =    "C"
                     go to pmt-dpz-cli-100
           else if   rr-tip-mov-tar       =    "D"
                     go to pmt-dpz-cli-500
           else      go to pmt-dpz-cli-800.
       pmt-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Se Tipo archivio : cliente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza cliente     dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-dpz-cli-900.
       pmt-dpz-cli-500.
      *              *-------------------------------------------------*
      *              * Se Tipo archivio : ns. dipendenza               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-dpz-cli-900.
       pmt-dpz-cli-800.
      *              *-------------------------------------------------*
      *              * Per tutti gli altri tipi stampa                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza cliente     dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-dpz-cli-900.
       pmt-dpz-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dpz-cli-999.
       pmt-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo movimento             *
      *    *-----------------------------------------------------------*
       acc-tip-mov-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-tip-mov           not  = spaces
                     go to acc-tip-mov-100.
           if        w-tmo-ctr-ele        not  = 1
                     go to acc-tip-mov-100.
           if        w-tmo-cod-tmo        =    spaces
                     go to acc-tip-mov-100.
           move      w-tmo-cod-tmo        to   rr-tip-mov             .
           perform   vis-cod-mov-000      thru vis-cod-mov-999        .
           go to     acc-tip-mov-400.
       acc-tip-mov-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zoc-ope      .
           move      rr-tip-mov           to   w-cod-des-zoc-cod      .
           move      05                   to   w-cod-des-zoc-lin      .
           move      30                   to   w-cod-des-zoc-pos      .
           move      05                   to   w-cod-des-zoc-dln      .
           move      37                   to   w-cod-des-zoc-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
       acc-tip-mov-110.
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           if        w-cod-des-zoc-ope    =    "F+"
                     go to acc-tip-mov-115.
           if        w-cod-des-zoc-ope    =    "AC"
                     go to acc-tip-mov-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-mov-115.
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
           go to     acc-tip-mov-110.
       acc-tip-mov-120.
           move      w-cod-des-zoc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-mov-999.
       acc-tip-mov-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-tip-mov             .
       acc-tip-mov-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zoc]                       *
      *                  *---------------------------------------------*
           move      rr-tip-mov           to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice a spaces : normalizzazione della  *
      *                  * descrizione e degli altri parametri         *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces
                     move  "Tutti i tipi ordine           "
                                          to   w-let-arc-zoc-des
                     move  zero           to   w-let-arc-zoc-vld
                     move  zero           to   w-let-arc-zoc-dpz      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione e parametri      *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   rr-tip-mov-des         .
           move      w-let-arc-zoc-vld    to   rr-tip-mov-vld         .
           move      w-let-arc-zoc-dpz    to   rr-tip-mov-dpz         .
           move      w-let-arc-zoc-tar    to   rr-tip-mov-tar         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-mov-000      thru vis-des-mov-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-tip-mov-100.
      *                  *---------------------------------------------*
      *                  * Se codice a spaces : si saltano tutti i     *
      *                  * controlli                                   *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces
                     go to acc-tip-mov-600.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rr-tip-mov-vld       not  = 02
                     go to acc-tip-mov-600.
           if        rr-dpz-inu           =    rr-tip-mov-dpz
                     go to acc-tip-mov-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo ordine incompatibile con il codice dipendenza
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tip-mov-100.
       acc-tip-mov-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-mov-625.
      *                  *---------------------------------------------*
      *                  * Se codice a spaces, e sono presenti piu'    *
      *                  * numerazioni, e attualmente i numeri docu-   *
      *                  * mento sono diversi da zero : si normalizza- *
      *                  * no i numeri documento                       *
      *                  *---------------------------------------------*
           if        rr-tip-mov           not  = spaces
                     go to acc-tip-mov-700.
           if        w-tmo-ctr-num        not  > 1
                     go to acc-tip-mov-800.
           if        rr-num-min           =    zero and
                     rr-num-max           =    zero
                     go to acc-tip-mov-800.
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
           move      zero                 to   rr-noc-min             .
           move      zero                 to   rr-noc-max             .
           perform   vis-num-min-000      thru vis-num-min-999        .
           perform   vis-num-max-000      thru vis-num-max-999        .
       acc-tip-mov-700.
      *                  *---------------------------------------------*
      *                  * Se codice tipo movimento relativo a ns.     *
      *                  * dipendenza                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-mov-tar       not  = "D"
                     go to acc-tip-mov-800.
      *                      *-----------------------------------------*
      *                      * Prompt per codici min - max             *
      *                      *-----------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
           perform   pmt-dpz-cli-000      thru pmt-dpz-cli-999        .
       acc-tip-mov-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-mov-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-mov-100.
       acc-tip-mov-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice tipo movimento                   *
      *    *-----------------------------------------------------------*
       vis-cod-mov-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-mov           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mov-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : descrizione tipo movi-  *
      *    *                                   mento                   *
      *    *-----------------------------------------------------------*
       vis-des-mov-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rr-tip-mov-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mov-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data ordine minima         *
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
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
       acc-dat-min-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-min-400.
      *                  *---------------------------------------------*
      *                  * Find su ordini                              *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-min-100.
       acc-dat-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
      *    * Accettazione campo selezione : Data ordine massima        *
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
           move      07                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
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
       acc-dat-min-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-max-400.
      *                  *---------------------------------------------*
      *                  * Find su ordini                              *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-max-100.
       acc-dat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
      *    * Accettazione : Data consegna richiesta min                *
      *    *-----------------------------------------------------------*
       acc-ric-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ric-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ric-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-min-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ric-min-999.
       acc-ric-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-ric-min             .
       acc-ric-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ric-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ric-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ric-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ric-min-100.
       acc-ric-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data consegna richiesta max                *
      *    *-----------------------------------------------------------*
       acc-ric-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ric-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      08                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-ric-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-max-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ric-max-999.
       acc-ric-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-ric-max             .
       acc-ric-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ric-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ric-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ric-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ric-max-100.
       acc-ric-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero ordine minimo       *
      *    *-----------------------------------------------------------*
       acc-num-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-min-025.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipi movimento e presente piu'   *
      *                  * di una numerazione : uscita                 *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces and
                     w-tmo-ctr-num        >    1
                     go to acc-num-min-999.
       acc-num-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-min           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-min-999.
       acc-num-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-min             .
       acc-num-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-min-100.
       acc-num-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero ordine minimo                    *
      *    *-----------------------------------------------------------*
       vis-num-min-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero ordine massimo      *
      *    *-----------------------------------------------------------*
       acc-num-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-max-025.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipi movimento e presente piu'   *
      *                  * di una numerazione : uscita                 *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces and
                     w-tmo-ctr-num        >    1
                     go to acc-num-max-999.
       acc-num-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-max           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-max-999.
       acc-num-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-max             .
       acc-num-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-max-100.
       acc-num-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero ordine minimo                    *
      *    *-----------------------------------------------------------*
       vis-num-max-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-num-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice cliente minimo      *
      *    *-----------------------------------------------------------*
       acc-cli-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cli-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cli-min           to   w-cod-mne-dcc-cod      .
           move      11                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cli-min-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cli-min-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cli-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-min-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cli-min-110.
       acc-cli-min-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cli-min-999.
       acc-cli-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cli-min             .
       acc-cli-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cli-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cli-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cli-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cli-min-100.
       acc-cli-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice cliente massimo     *
      *    *-----------------------------------------------------------*
       acc-cli-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cli-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cli-max           to   w-cod-mne-dcc-cod      .
           move      11                   to   w-cod-mne-dcc-lin      .
           move      47                   to   w-cod-mne-dcc-pos      .
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cli-max-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cli-max-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cli-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-max-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cli-max-110.
       acc-cli-max-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cli-max-999.
       acc-cli-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cli-max             .
       acc-cli-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cli-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento dipendenze del cliente          *
      *                  *---------------------------------------------*
           if        rr-cli-min           =    zero
                     go to acc-cli-max-610.
           if        rr-cli-max           =    rr-cli-min or
                     rr-cli-max           =    zero
                     go to acc-cli-max-800.
       acc-cli-max-610.
           if        rr-dpz-min           =    spaces
                     go to acc-cli-max-620.
           move      spaces               to   rr-dpz-min             .
           perform   vis-dpz-min-000      thru vis-dpz-min-999        .
       acc-cli-max-620.
           if        rr-dpz-max           =    spaces
                     go to acc-cli-max-800.
           move      spaces               to   rr-dpz-max             .
           perform   vis-dpz-max-000      thru vis-dpz-max-999        .
       acc-cli-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cli-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cli-max-100.
       acc-cli-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice dipendenza minimo   *
      *    *-----------------------------------------------------------*
       acc-dpz-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-mov-tar       =    "D"
                     go to acc-dpz-min-999.
           if        rr-cli-min           =    zero
                     go to acc-dpz-min-999.
           if        rr-cli-max           =    rr-cli-min or
                     rr-cli-max           =    zero
                     go to acc-dpz-min-100
           else      go to acc-dpz-min-999.
       acc-dpz-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      rr-cli-min           to   w-cod-cod-dcc-cli      .
           move      rr-dpz-min           to   w-cod-cod-dcc-cod      .
           move      12                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      zero                 to   w-cod-cod-dcc-rln      .
           move      zero                 to   w-cod-cod-dcc-rps      .
           move      zero                 to   w-cod-cod-dcc-vln      .
           move      zero                 to   w-cod-cod-dcc-vps      .
           move      zero                 to   w-cod-cod-dcc-lln      .
           move      zero                 to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-dpz-min-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-min-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-min-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-min-110.
       acc-dpz-min-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dpz-min-999.
       acc-dpz-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-dpz-min             .
       acc-dpz-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dpz-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dpz-min-100.
       acc-dpz-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Dipendenza minima       *
      *    *-----------------------------------------------------------*
       vis-dpz-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dpz-min           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice dipendenza massimo  *
      *    *-----------------------------------------------------------*
       acc-dpz-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-mov-tar       =    "D"
                     go to acc-dpz-max-999.
           if        rr-cli-min           =    zero
                     go to acc-dpz-max-999.
           if        rr-cli-max           =    rr-cli-min or
                     rr-cli-max           =    zero
                     go to acc-dpz-max-100
           else      go to acc-dpz-max-999.
       acc-dpz-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      rr-cli-min           to   w-cod-cod-dcc-cli      .
           move      rr-dpz-max           to   w-cod-cod-dcc-cod      .
           move      12                   to   w-cod-cod-dcc-lin      .
           move      47                   to   w-cod-cod-dcc-pos      .
           move      zero                 to   w-cod-cod-dcc-rln      .
           move      zero                 to   w-cod-cod-dcc-rps      .
           move      zero                 to   w-cod-cod-dcc-vln      .
           move      zero                 to   w-cod-cod-dcc-vps      .
           move      zero                 to   w-cod-cod-dcc-lln      .
           move      zero                 to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-dpz-max-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-max-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-max-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-max-110.
       acc-dpz-max-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dpz-max-999.
       acc-dpz-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-dpz-max             .
       acc-dpz-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dpz-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dpz-max-100.
       acc-dpz-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Dipendenza massima      *
      *    *-----------------------------------------------------------*
       vis-dpz-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-dpz-max           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice agente                        *
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
           move      14                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      14                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
      *                  * Lettura file [age]                          *
      *                  *---------------------------------------------*
           move      rr-cod-age           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-nom    to   rr-cod-age-den         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione denominazione agente        *
      *                  *---------------------------------------------*
           perform   vis-cod-age-den-000  thru vis-cod-age-den-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non esistente : reimpostazione    *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
      *                  *---------------------------------------------*
      *                  * Se valore zero                              *
      *                  *---------------------------------------------*
           if        rr-cod-age           not  = zero
                     go to acc-cod-age-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'Tutti' al po-  *
      *                      * sto del codice                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Denominazione agente                    *
      *    *-----------------------------------------------------------*
       vis-cod-age-den-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-age-den       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-den-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Stato ordini clienti       *
      *    *-----------------------------------------------------------*
       acc-sts-orc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sts-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-orc-lun    to   v-car                  .
           move      w-exp-sts-orc-num    to   v-ldt                  .
           move      "DIGT#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-orc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-orc           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sts-orc-999.
       acc-sts-orc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sts-orc             .
       acc-sts-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-sts-orc           not  = zero
                     go to acc-sts-orc-600.
           if        v-key                =    "UP  "
                     go to acc-sts-orc-600.
           go to     acc-sts-orc-100.
       acc-sts-orc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sts-orc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sts-orc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sts-orc-100.
       acc-sts-orc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Opzioni di stampa                    *
      *    *-----------------------------------------------------------*
       acc-opz-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-opz-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-opz-stp-lun    to   v-car                  .
           move      w-exp-opz-stp-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-opz-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-opz-stp           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-opz-stp-999.
       acc-opz-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-opz-stp             .
       acc-opz-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-opz-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-opz-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-opz-stp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-opz-stp-100.
       acc-opz-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Ordinamento righe          *
      *    *-----------------------------------------------------------*
       acc-ord-rig-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-opz-stp-tip       =    spaces
                     go to acc-ord-rig-999.
       acc-ord-rig-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ord-rig-lun    to   v-car                  .
           move      w-exp-ord-rig-num    to   v-ldt                  .
           move      "IUCS#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-ord-rig-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-rig           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-rig-999.
       acc-ord-rig-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-rig             .
       acc-ord-rig-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-rig           =    zero
                     go to acc-ord-rig-100.
       acc-ord-rig-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione si/no stampa      *
      *                  * ubicazione                                  *
      *                  *---------------------------------------------*
           if        rr-ord-rig           not  = 02
                     go to acc-ord-rig-800.
           move      "X"                  to   rr-opz-stp-ubi         .
       acc-ord-rig-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-rig-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-rig-100.
       acc-ord-rig-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Ordinamento righe       *
      *    *-----------------------------------------------------------*
       vis-ord-rig-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ord-rig-lun    to   v-car                  .
           move      w-exp-ord-rig-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-ord-rig-tbl    to   v-txt                  .
           move      rr-ord-rig           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-rig-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-120.
      *              *-------------------------------------------------*
      *              * Test su data ordine minima e massima            *
      *              *-------------------------------------------------*
           if        rr-dat-min           =    zero or
                     rr-dat-max           =    zero
                     go to tdo-ric-sel-125.
           if        rr-dat-min           not  > rr-dat-max
                     go to tdo-ric-sel-125.
           move      "Data ordine iniziale maggiore di quella finale !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-125.
      *              *-------------------------------------------------*
      *              * Test su data consegna richiesta min - max       *
      *              *-------------------------------------------------*
           if        rr-ric-min           =    zero or
                     rr-ric-max           =    zero
                     go to tdo-ric-sel-130.
           if        rr-ric-max           not  < rr-ric-min
                     go to tdo-ric-sel-130.
           move      "Data consegna richiesta massima inferiore alla dat
      -              "a minima       "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-130.
      *              *-------------------------------------------------*
      *              * Test su numero ordine minimo e massimo          *
      *              *-------------------------------------------------*
           if        rr-num-min           =    zero or
                     rr-num-max           =    zero
                     go to tdo-ric-sel-140.
           if        rr-num-min           not  > rr-num-max
                     go to tdo-ric-sel-140.
           move      "Numero ordine iniziale maggiore di quello finale !
      -              " "
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-140.
      *              *-------------------------------------------------*
      *              * Test su codice cliente minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-cli-min           =    zero or
                     rr-cli-max           =    zero
                     go to tdo-ric-sel-150.
           if        rr-cli-min           not  > rr-cli-max
                     go to tdo-ric-sel-150.
           move      "Codice cliente iniziale maggiore di quella finale 
      -              "!"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-150.
      *              *-------------------------------------------------*
      *              * Test su dipendenza minima e massima             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-cli-min           =    zero
                     go to tdo-ric-sel-160.
           if        rr-cli-max           =    rr-cli-min or
                     rr-cli-max           =    zero
                     go to tdo-ric-sel-155
           else      go to tdo-ric-sel-160.
       tdo-ric-sel-155.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rr-dpz-min           =    spaces or
                     rr-dpz-max           =    spaces
                     go to tdo-ric-sel-160.
           if        rr-dpz-min           not  > rr-dpz-max
                     go to tdo-ric-sel-160.
           move      "Dipendenza cliente iniziale maggiore di quella fin
      -              "ale !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-160.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
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
      *              *-------------------------------------------------*
      *              * Data ordine massima                             *
      *              *-------------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to reg-ric-sel-050.
           if        rr-dat-min           =    zero
                     move  9999999        to   rr-dat-max
           else      move  rr-dat-min     to   rr-dat-max             .
       reg-ric-sel-050.
      *              *-------------------------------------------------*
      *              * Data consegna richiesta min - max               *
      *              *-------------------------------------------------*
           if        rr-ric-max           not  = zero
                     go to reg-ric-sel-100.
           if        rr-ric-min           =    zero
                     move 9999999         to   rr-ric-max
           else      move rr-ric-min      to   rr-ric-max             .
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Numero ordine massimo                           *
      *              *-------------------------------------------------*
           if        rr-tip-mov           =    spaces and
                     w-tmo-ctr-num        >    1
                     move  zero           to   rr-num-min
                     move  zero           to   rr-num-max
                     go to reg-ric-sel-200.
      *
           if        rr-num-max           not  = zero
                     go to reg-ric-sel-200.
           if        rr-num-min           =    zero
                     move  999999999      to   rr-num-max
           else      move  rr-num-min     to   rr-num-max             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione numero ordine interno minimo e *
      *              * massimo                                         *
      *              *-------------------------------------------------*
           if        rr-tip-mov           =    spaces and
                     w-tmo-ctr-num        >    1
                     move  zero           to   rr-noc-min
                     move  zero           to   rr-noc-max
                     go to reg-ric-sel-300.
      *
           move      rr-num-min           to   w-wrk-nft-acc          .
           move      w-wrk-num-saa        to   w-wrk-noc-saa          .
           move      rr-dpz-inu           to   w-wrk-noc-dpz
           move      w-wrk-num-npg        to   w-wrk-noc-npg          .
           move      w-wrk-nft-int        to   rr-noc-min             .
      *
           move      rr-num-max           to   w-wrk-nft-acc          .
           move      w-wrk-num-saa        to   w-wrk-noc-saa          .
           move      w-wrk-num-npg        to   w-wrk-noc-npg          .
           move      w-wrk-num-npg        to   w-wrk-noc-npg          .
           move      w-wrk-nft-int        to   rr-noc-max             .
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Codice cliente massimo                          *
      *              *-------------------------------------------------*
           if        rr-cli-max           not  = zero
                     go to reg-ric-sel-350.
           if        rr-cli-min           =    zero
                     move  9999999        to   rr-cli-max
           else      move  rr-cli-min     to   rr-cli-max             .
       reg-ric-sel-350.
      *              *-------------------------------------------------*
      *              * Codice dipendenza massimo                       *
      *              *-------------------------------------------------*
           if        rr-dpz-max           =    spaces
                     move  rr-dpz-min     to   rr-dpz-max             .
           move      rr-dpz-max           to   w-all-str-alf          .
           move      04                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-dpz-max             .
      *              *-------------------------------------------------*
      *              * Status ordini                                   *
      *              *-------------------------------------------------*
           if        rr-sts-orc           =    zero
                     move  01             to   rr-sts-orc             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-tip-mov             .
           move      spaces               to   rr-tip-mov-des         .
           move      zero                 to   rr-tip-mov-vld         .
           move      zero                 to   rr-tip-mov-dpz         .
           move      spaces               to   rr-tip-mov-tar         .
           move      zero                 to   rr-dat-min             .
           move      zero                 to   rr-dat-max             .
           move      zero                 to   rr-ric-min             .
           move      zero                 to   rr-ric-max             .
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
           move      zero                 to   rr-noc-min             .
           move      zero                 to   rr-noc-max             .
           move      zero                 to   rr-cli-min             .
           move      zero                 to   rr-cli-max             .
           move      spaces               to   rr-dpz-min             .
           move      spaces               to   rr-dpz-max             .
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-den         .
           move      zero                 to   rr-sts-orc             .
           move      spaces               to   rr-opz-stp             .
           move      zero                 to   rr-ord-rig             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zoc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zoc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zoc-cod    =    spaces
                     go to let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-let-arc-zoc-cod    to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zoc-400.
       let-arc-zoc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zoc-des-toc       to   w-let-arc-zoc-des      .
           move      rf-zoc-vld-dpz       to   w-let-arc-zoc-vld      .
           move      rf-zoc-cod-dpz       to   w-let-arc-zoc-dpz      .
           move      rf-zoc-org-doc       to   w-let-arc-zoc-ord      .
           move      rf-zoc-prv-doc       to   w-let-arc-zoc-prd      .
           move      rf-zoc-sgl-num       to   w-let-arc-zoc-sgl      .
           move      rf-zoc-def-tar       to   w-let-arc-zoc-tar      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zoc-999.
       let-arc-zoc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zoc-flg      .
           move      all   "."            to   w-let-arc-zoc-des      .
           go to     let-arc-zoc-520.
       let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-des      .
       let-arc-zoc-520.
           move      zero                 to   w-let-arc-zoc-vld      .
           move      zero                 to   w-let-arc-zoc-dpz      .
           move      zero                 to   w-let-arc-zoc-ord      .
           move      zero                 to   w-let-arc-zoc-prd      .
           move      spaces               to   w-let-arc-zoc-sgl      .
           move      spaces               to   w-let-arc-zoc-tar      .
       let-arc-zoc-999.
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
           move      50                   to   w-cnt-stp-lin-min      .
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
      *    * Find su archivio [oct]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oct-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porc3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-oct-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      rr-dpz-inu           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/orc/prg/obj/porc3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-oct-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-nom      .
           go to     let-arc-age-999.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
