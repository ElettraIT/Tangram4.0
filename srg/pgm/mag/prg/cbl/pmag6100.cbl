       Identification Division.
       Program-Id.                                 pmag6100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    inv                 *
      *                                   Fase:    mag610              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/11/92    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Stampa prospetto rilevazione inventario     *
      *                                                                *
      * Il programma si sviluppa nel seguente modo :                   *
      *                                                                *
      * pmag6100 : Main program. Richiesta del tipo prospetto          *
      *                                                                *
      * pmag610a : Overlay per stampa prospetto prodotti di vendita    *
      * pmag610c : Overlay per stampa prospetto semilavorati           *
      * pmag610e : Overlay per stampa prospetto materie prime          *
      * pmag610g : Overlay per stampa prospetto materiali vari         *
      * pmag610q : Overlay per stampa prospetto merci presso terzi     *
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
                     "mag"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "inv"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag610"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag6100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " STAMPA PROSPETTO RILEVAZIONE INVENTARIO"       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40)                  .

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
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [datinv]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rndatinv"                       .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [zmu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmu"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione semilavorati attiva                    *
      *        *-------------------------------------------------------*
           05  w-prs-dps-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materiali vari attiva                  *
      *        *-------------------------------------------------------*
           05  w-prs-mtv-snx              pic  x(01)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/pmag6100.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmu]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmu.
               10  w-let-arc-zmu-flg      pic  x(01)                  .
               10  w-let-arc-zmu-dpz      pic  9(02)                  .
               10  w-let-arc-zmu-cod      pic  x(03)                  .
               10  w-let-arc-zmu-des      pic  x(15)                  .
               10  w-let-arc-zmu-pdu      pic  9(01)                  .
               10  w-let-arc-zmu-tbl occurs 4.
                   15  w-let-arc-zmu-dpu  pic  x(10)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dpm]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/lzosdpm0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [mtv]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/lzosmtv0.ltw"                   .
              
      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-prc-ril              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Exp                               *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prospetto                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-psp.
               10  w-exp-tip-psp-num      pic  9(02)       value 5    .
               10  w-exp-tip-psp-lun      pic  9(02)       value 20   .
               10  w-exp-tip-psp-tbl.
                   15  filler             pic  x(20) value
                            "Prodotti di vendita "                    .
                   15  filler             pic  x(20) value
                            "Semilavorati         "                   .
                   15  filler             pic  x(20) value
                            "Materie prime        "                   .
                   15  filler             pic  x(20) value
                            "Materiali vari       "                   .
                   15  filler             pic  x(20) value
                            "Merci presso terzi   "                   .
               10  w-exp-tip-psp-tbl-r    redefines
                   w-exp-tip-psp-tbl.
                   15  w-exp-tip-psp-ele  occurs 05
                                          pic  x(20)                  .
               10  w-exp-tip-psp-stg.
                   15  filler             pic  x(05) value "PSMVT"    .
               10  w-exp-tip-psp-stg-r    redefines
                   w-exp-tip-psp-stg.
                   15  w-exp-tip-psp-tps  occurs 05
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prospetto limitato ai soli tipi am-   *
      *        *            messi                                      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ram.
               10  w-exp-tps-amm-num      pic  9(02)                  .
               10  w-exp-tps-amm-lun      pic  9(02) value 20         .
               10  w-exp-tps-amm-tbl.
                   15  w-exp-tps-amm-ele  occurs 05
                                          pic  x(20)                  .
               10  w-exp-tps-amm-ass.
                   15  w-exp-tps-amm-tps  occurs 05
                                          pic  x(01)                  .
               10  w-exp-tps-amm-wst.
                   15  w-exp-tps-amm-wtr  occurs 05
                                          pic  x(01)                  .
               10  w-exp-tps-amm-i01      pic  9(02)                  .
               10  w-exp-tps-amm-c01      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Percorso di rilevazione                    *
      *        *-------------------------------------------------------*
           05  w-exp-prc-ril.
               10  w-exp-prc-ril-num      pic  9(02)       value 2    .
               10  w-exp-prc-ril-lun      pic  9(02)       value 40   .
               10  w-exp-prc-ril-tbl.
                   15  filler             pic  x(40) value
                            "da Filtro di ordinamento prodotti       ".
                   15  filler             pic  x(40) value
                            "Ubicazione prodotti                     ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo selezione                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-sel.
               10  w-exp-tip-sel-num      pic  9(02)       value 3    .
               10  w-exp-tip-sel-lun      pic  9(02)       value 20   .
               10  w-exp-tip-sel-tbl.
                   15  filler             pic  x(20) value
                            "Tutto               "                    .
                   15  filler             pic  x(20) value
                            "Solo Clienti        "                    .
                   15  filler             pic  x(20) value
                            "Solo Fornitori      "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo salto pagina per magazzino            *
      *        *-------------------------------------------------------*
           05  w-exp-tsp-mag.
               10  w-exp-tsp-mag-num      pic  9(02)       value 4    .
               10  w-exp-tsp-mag-lun      pic  9(02)       value 20   .
               10  w-exp-tsp-mag-tbl.
                   15  filler             pic  x(20) value
                            "No                  "                    .
                   15  filler             pic  x(20) value
                            "Ad ogni classe      "                    .
                   15  filler             pic  x(20) value
                            "Ad ogni gruppo      "                    .
                   15  filler             pic  x(20) value
                            "Ad ogni sottogruppo "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo salto pagina per merci presso terzi   *
      *        *-------------------------------------------------------*
           05  w-exp-tsp-mpt.
               10  w-exp-tsp-mpt-num      pic  9(02)       value 2    .
               10  w-exp-tsp-mpt-lun      pic  9(02)       value 30   .
               10  w-exp-tsp-mpt-tbl.
                   15  filler             pic  x(30) value
                            "No                            "          .
                   15  filler             pic  x(30) value
                            "Ad ogni cliente o fornitore   "          .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No stampa giacenza presunta             *
      *        *-------------------------------------------------------*
           05  w-exp-snx-sgp.
               10  w-exp-snx-sgp-num      pic  9(02)       value 2    .
               10  w-exp-snx-sgp-lun      pic  9(02)       value 02   .
               10  w-exp-snx-sgp-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No selezione su giacenza presunta       *
      *        *-------------------------------------------------------*
           05  w-exp-snx-seg.
               10  w-exp-snx-seg-num      pic  9(02)       value 4    .
               10  w-exp-snx-seg-lun      pic  9(02)       value 40   .
               10  w-exp-snx-seg-tbl.
                   15  filler             pic  x(40) value
                            "solo prodotti con giacenza Positiva     ".
                   15  filler             pic  x(40) value
                            "Tutti i prodotti                        ".
                   15  filler             pic  x(40) value
                            "solo prodotti con giacenza Negativa     ".
                   15  filler             pic  x(40) value
                            "solo prodotti con giacenza a Zero       ".

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dcp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dps]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dpm]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/azosdpm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [mtv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo ubicazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmu0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per emissione box di errore                      *
      *        *-------------------------------------------------------*
           05  w-box-msg-err.
               10  w-box-msg-err-msg      pic  x(60)                  .

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
      *              *-------------------------------------------------*
      *              * Composizione nome e pathname programma di ese-  *
      *              * cuzione                                         *
      *              *-------------------------------------------------*
           perform   cmp-nep-pde-000      thru cmp-nep-pde-999        .
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
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione semilavorati attiva          *
      *                  *---------------------------------------------*
           perform   prs-dps-snx-000      thru prs-dps-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materie prime attiva         *
      *                  *---------------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materiali vari attiva        *
      *                  *---------------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
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
                     go to pre-exe-pgm-020.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti bufferizzazione codice dipendenza    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
           move      w-dpz-den-prg        to   rr-dpz-inu-den         .
           move      w-dpz-ctr-dpz        to   rr-dpz-ctr-dpz
      *              *-------------------------------------------------*
      *              * Lettura numerazione relativa agli inventari     *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatinv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-inv             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatinv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-inv             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatinv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-inv             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatinv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-inv             .
      *              *-------------------------------------------------*
      *              * Se fase di inventario non ancora in corso       *
      *              *-------------------------------------------------*
           if        rn-dat-inv-tip-iic   not  = spaces
                     go to pre-exe-pgm-100.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Messaggi nel box                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Titolo                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "                           A T T E N Z I O N E    
      -              "                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Prima riga                          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Programma non eseguibile !                        
      -              "                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Seconda riga                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Deve essere ancora dichiarato l'inizio fase di inv
      -              "entario.              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadre di delimitazione       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Accettazione carattere di presa visione *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      16                   to   v-lin                  .
           move      75                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita con errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Determinazione tipi prospetto ammessi           *
      *              *-------------------------------------------------*
           perform   det-tps-amm-000      thru det-tps-amm-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione semilavorati   *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dps-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dps[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dps-snx
           else      move  spaces         to   w-prs-dps-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dps-snx        =    "S" or
                     w-prs-dps-snx        =    "N"
                     go to prs-dps-snx-999.
           move      "N"                  to   w-prs-dps-snx          .
       prs-dps-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materie prime  *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dpm-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dpm-snx
           else      move  spaces         to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        =    "S" or
                     w-prs-dpm-snx        =    "N"
                     go to prs-dpm-snx-999.
           move      "N"                  to   w-prs-dpm-snx          .
       prs-dpm-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materiali vari *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-mtv-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mtv[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mtv-snx
           else      move  spaces         to   w-prs-mtv-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mtv-snx        =    "S" or
                     w-prs-mtv-snx        =    "N"
                     go to prs-mtv-snx-999.
           move      "N"                  to   w-prs-mtv-snx          .
       prs-mtv-snx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tipi prospetto ammessi                     *
      *    *-----------------------------------------------------------*
       det-tps-amm-000.
      *              *-------------------------------------------------*
      *              * Preparazione area w-exp limitata ai soli tipi   *
      *              * prospetto previsti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo prospetto 'Prodotti di vendita' :      *
      *                  * - Sempre ammesso                            *
      *                  *---------------------------------------------*
           move      "P"                  to   w-exp-tps-amm-tps (1)  .
      *                  *---------------------------------------------*
      *                  * Tipo prospetto 'Semilavorati' :             *
      *                  * - Se gestione semilavorati attiva lo si     *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S"
                     move  "S"            to   w-exp-tps-amm-tps (2)
           else      move  spaces         to   w-exp-tps-amm-tps (2)  .
      *                  *---------------------------------------------*
      *                  * Tipo prospetto 'Materie Prime' :            *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        =    "S"
                     move  "M"            to   w-exp-tps-amm-tps (3)
           else      move  spaces         to   w-exp-tps-amm-tps (3)  .
      *                  *---------------------------------------------*
      *                  * Tipo prospetto 'Materiale Vario' :          *
      *                  * - Se gestione materiale vario attiva lo si  *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        =    "S"
                     move  "V"            to   w-exp-tps-amm-tps (4)
           else      move  spaces         to   w-exp-tps-amm-tps (4)  .
      *                  *---------------------------------------------*
      *                  * Tipo prospetto 'Merci presso terzi' :       *
      *                  * - Sempre ammesso                            *
      *                  *---------------------------------------------*
           move      "T"                  to   w-exp-tps-amm-tps (5)  .
       det-tps-amm-050.
      *              *-------------------------------------------------*
      *              * Compattamento della lista dei tipi prospetto    *
      *              * ammessi e determinazione del numero di elementi *
      *              * in tabella                                      *
      *              *-------------------------------------------------*
           move      w-exp-tps-amm-ass    to   w-exp-tps-amm-wst      .
           move      spaces               to   w-exp-tps-amm-ass      .
           move      zero                 to   w-exp-tps-amm-num      .
           move      zero                 to   w-exp-tps-amm-c01      .
       det-tps-amm-052.
           add       1                    to   w-exp-tps-amm-c01      .
           if        w-exp-tps-amm-c01    >    5
                     go to det-tps-amm-060.
           if        w-exp-tps-amm-wtr
                    (w-exp-tps-amm-c01)   =    spaces
                     go to det-tps-amm-052.
           add       1                    to   w-exp-tps-amm-num      .
           move      w-exp-tps-amm-wtr
                    (w-exp-tps-amm-c01)   to   w-exp-tps-amm-tps
                                              (w-exp-tps-amm-num)     .
           go to     det-tps-amm-052.
       det-tps-amm-060.
      *              *-------------------------------------------------*
      *              * Preparazione delle descrizioni relative ai tipi *
      *              * prospetto ammessi                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tps-amm-c01      .
       det-tps-amm-100.
           add       1                    to   w-exp-tps-amm-c01      .
           if        w-exp-tps-amm-c01    >    05
                     go to det-tps-amm-999.
           move      zero                 to   w-exp-tps-amm-i01      .
           inspect   w-exp-tip-psp-stg
                                      tallying w-exp-tps-amm-i01
                     for   characters   before
                                       initial w-exp-tps-amm-tps
                                              (w-exp-tps-amm-c01)     .
           if        w-exp-tps-amm-i01    =    w-exp-tip-psp-num
                     move  spaces         to   w-exp-tps-amm-ele
                                              (w-exp-tps-amm-c01)
                     go to det-tps-amm-100.
           add       01                   to   w-exp-tps-amm-i01      .
           move      w-exp-tip-psp-ele
                    (w-exp-tps-amm-i01)   to   w-exp-tps-amm-ele
                                              (w-exp-tps-amm-c01)     .
           go to     det-tps-amm-100.
       det-tps-amm-999.
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
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice tipo ubicazione *
      *              *-------------------------------------------------*
           perform   cod-des-zmu-opn-000  thru cod-des-zmu-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-opn-fls-020.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-zos-dps-opn-000  thru cod-zos-dps-opn-999    .
       rou-opn-fls-020.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-opn-fls-040.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-zos-dpm-opn-000  thru cod-zos-dpm-opn-999    .
       rou-opn-fls-040.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-opn-fls-060.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-zos-mtv-opn-000  thru cod-zos-mtv-opn-999    .
       rou-opn-fls-060.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dcp]       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dps]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-opn-fls-100.
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dpm]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-opn-fls-200.
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/prg/obj/bzosdpm0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [mtv]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-opn-fls-300.
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/prg/obj/bzosmtv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       rou-opn-fls-300.
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *              *-------------------------------------------------*
      *              * [zmu]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice tipo ubicaz.   *
      *              *-------------------------------------------------*
           perform   cod-des-zmu-cls-000  thru cod-des-zmu-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-cls-fls-020.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-zos-dps-cls-000  thru cod-zos-dps-cls-999    .
       rou-cls-fls-020.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-cls-fls-040.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-zos-dpm-cls-000  thru cod-zos-dpm-cls-999    .
       rou-cls-fls-040.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-cls-fls-060.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-zos-mtv-cls-000  thru cod-zos-mtv-cls-999    .
       rou-cls-fls-060.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dcp]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dps]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-cls-fls-100.
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-100.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [dpm]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-cls-fls-200.
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/prg/obj/bzosdpm0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/dpm/prg/obj/bzosdpm0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * Filtro per selezione ed ordinamento [mtv]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-cls-fls-300.
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/prg/obj/bzosmtv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/mtv/prg/obj/bzosmtv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-300.
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *              *-------------------------------------------------*
      *              * [zmu]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
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
      *                  * Tipo prospetto                              *
      *                  *---------------------------------------------*
           perform   acc-tip-psp-000      thru acc-tip-psp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Abilitazione tasto 'Do'                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Filtro di selezione magazzino               *
      *                  *---------------------------------------------*
           perform   acc-fso-mag-000      thru acc-fso-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Percorso di rilevazione                     *
      *                  *---------------------------------------------*
           perform   acc-prc-ril-000      thru acc-prc-ril-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipo ubicazione                             *
      *                  *---------------------------------------------*
           perform   acc-tip-ubi-000      thru acc-tip-ubi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-225.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 1 - min             *
      *                  *---------------------------------------------*
           perform   acc-prm-mi1-000      thru acc-prm-mi1-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-400.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 1 - max             *
      *                  *---------------------------------------------*
           perform   acc-prm-ma1-000      thru acc-prm-ma1-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 2 - min             *
      *                  *---------------------------------------------*
           perform   acc-prm-mi2-000      thru acc-prm-mi2-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-500.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 2 - max             *
      *                  *---------------------------------------------*
           perform   acc-prm-ma2-000      thru acc-prm-ma2-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 3 - min             *
      *                  *---------------------------------------------*
           perform   acc-prm-mi3-000      thru acc-prm-mi3-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-600.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 3 - max             *
      *                  *---------------------------------------------*
           perform   acc-prm-ma3-000      thru acc-prm-ma3-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 4 - min             *
      *                  *---------------------------------------------*
           perform   acc-prm-mi4-000      thru acc-prm-mi4-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
           if        v-key                =    "DOWN"
                     go to acc-ric-sel-800.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 4 - max             *
      *                  *---------------------------------------------*
           perform   acc-prm-ma4-000      thru acc-prm-ma4-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Tipo selezione clienti o fornitori          *
      *                  *---------------------------------------------*
           perform   acc-tip-sel-000      thru acc-tip-sel-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-750.
      *                  *---------------------------------------------*
      *                  * Tipo salto pagina per magazzino             *
      *                  *---------------------------------------------*
           perform   acc-tsp-mag-000      thru acc-tsp-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-700.
       acc-ric-sel-800.
      *                  *---------------------------------------------*
      *                  * Tipo salto pagina per merci presso terzi    *
      *                  *---------------------------------------------*
           perform   acc-tsp-mpt-000      thru acc-tsp-mpt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-750.
       acc-ric-sel-850.
      *                  *---------------------------------------------*
      *                  * Si/No stampa giacenza presunta              *
      *                  *---------------------------------------------*
           perform   acc-snx-sgp-000      thru acc-snx-sgp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-800.
       acc-ric-sel-875.
      *                  *---------------------------------------------*
      *                  * Si/No selezione su giacenza presunta        *
      *                  *---------------------------------------------*
           perform   acc-snx-seg-000      thru acc-snx-seg-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-850.
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
           go to     acc-ric-sel-200.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo prospetto                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-psp-000      thru pmt-tip-psp-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo prospetto                *
      *    *-----------------------------------------------------------*
       pmt-tip-psp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo prospetto             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-psp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per linea trattini di separazione *
      *    *-----------------------------------------------------------*
       pmt-trt-sep-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-trt-sep-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Filtro selezione magazzino    *
      *    *-----------------------------------------------------------*
       pmt-fso-mag-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to pmt-fso-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        rr-tip-psp           =    "P"
                     move  08             to   v-lin
           else      move  09             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice filtro di selezione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           if        rr-tip-psp           =    "P"
                     go to pmt-fso-mag-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-psp           =    "P"
                     move  "       anagrafica prodotti  "
                                          to   v-alf
           else if   rr-tip-psp           =    "S"
                     move  "   anagrafica semilavorati  "
                                          to   v-alf
           else if   rr-tip-psp           =    "M"
                     move  "  anagrafica materie prime  "
                                          to   v-alf
           else if   rr-tip-psp           =    "V"
                     move  " anagrafica materiali vari  "
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Percorso di rilevazione                          *
      *    *-----------------------------------------------------------*
       pmt-prc-ril-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to pmt-prc-ril-999.
       pmt-prc-ril-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Percorso di rilevazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-prc-ril-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo ubicazione                                  *
      *    *-----------------------------------------------------------*
       pmt-tip-ubi-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to pmt-tip-ubi-999.
       pmt-tip-ubi-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ubicazione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 1 - min  *
      *    *-----------------------------------------------------------*
       pmt-prm-mi1-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to pmt-prm-mi1-999.
       pmt-prm-mi1-100.
      *              *-------------------------------------------------*
      *              * Min - max                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                          --min-- 
      -              "             --max--          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt generale                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Parametri di ubicazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        rr-tip-ubi-dpu (1)   not  = spaces
                     go to pmt-prm-mi1-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-mi1-999.
       pmt-prm-mi1-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 1                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ubi-dpu (1)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-mi1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 1 - max  *
      *    *-----------------------------------------------------------*
       pmt-prm-ma1-000.
       pmt-prm-ma1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 2        *
      *    *-----------------------------------------------------------*
       pmt-prm-mi2-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to pmt-prm-mi2-999.
       pmt-prm-mi2-100.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        rr-tip-ubi-dpu (2)   not  = spaces
                     go to pmt-prm-mi2-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-mi2-999.
       pmt-prm-mi2-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 2                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ubi-dpu (2)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-mi2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 2 - max  *
      *    *-----------------------------------------------------------*
       pmt-prm-ma2-000.
       pmt-prm-ma2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 3        *
      *    *-----------------------------------------------------------*
       pmt-prm-mi3-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to pmt-prm-mi3-999.
       pmt-prm-mi3-100.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        rr-tip-ubi-dpu (3)   not  = spaces
                     go to pmt-prm-mi3-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-mi3-999.
       pmt-prm-mi3-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 3                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ubi-dpu (3)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-mi3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 3 - max  *
      *    *-----------------------------------------------------------*
       pmt-prm-ma3-000.
       pmt-prm-ma3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 4        *
      *    *-----------------------------------------------------------*
       pmt-prm-mi4-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to pmt-prm-mi4-999.
       pmt-prm-mi4-100.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        rr-tip-ubi-dpu (4)   not  = spaces
                     go to pmt-prm-mi4-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-mi4-999.
       pmt-prm-mi4-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 4                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ubi-dpu (4)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-mi4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 4 - max  *
      *    *-----------------------------------------------------------*
       pmt-prm-ma4-000.
       pmt-prm-ma4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Filtro selezione magazzino    *
      *    *-----------------------------------------------------------*
       pmt-tip-sel-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to pmt-tip-sel-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di selezione          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per tipo salto pagina magazzino   *
      *    *-----------------------------------------------------------*
       pmt-tsp-mag-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to pmt-tsp-mag-999.
           if        rr-prc-ril           =    02
                     go to pmt-tsp-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        rr-tip-psp           =    "P"
                     move  19             to   v-lin
           else      move  13             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Salto pagina da effettuare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tsp-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per tipo salto pagina merci pres- *
      *    * so terzi                                                  *
      *    *-----------------------------------------------------------*
       pmt-tsp-mpt-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to pmt-tsp-mpt-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Salto pagina da effettuare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tsp-mpt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/No stampa giacenza presun- *
      *    * ta                                                        *
      *    *-----------------------------------------------------------*
       pmt-snx-sgp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di prospetto    *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "P"
                     go to pmt-snx-sgp-100
           else      go to pmt-snx-sgp-200.
       pmt-snx-sgp-100.
      *                  *---------------------------------------------*
      *                  * Per prodotti di vendita                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa giacenza presunta   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-snx-sgp-900.
       pmt-snx-sgp-200.
      *                  *---------------------------------------------*
      *                  * Tutti gli altri tipi di prospetto           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Evidenziazione in stampa   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       giacenza presunta    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-snx-sgp-900.
       pmt-snx-sgp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-snx-sgp-999.
       pmt-snx-sgp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/No selezione su giacenza   *
      *    * presunta                                                  *
      *    *-----------------------------------------------------------*
       pmt-snx-seg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di prospetto    *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "P"
                     go to pmt-snx-seg-100
           else      go to pmt-snx-seg-200.
       pmt-snx-seg-100.
      *                  *---------------------------------------------*
      *                  * Per prodotti di vendita                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni su giacenza      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-snx-seg-900.
       pmt-snx-seg-200.
      *                  *---------------------------------------------*
      *                  * Tutti gli altri tipi di prospetto           *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-snx-seg-900.
       pmt-snx-seg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-snx-seg-999.
       pmt-snx-seg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo prospetto             *
      *    *-----------------------------------------------------------*
       acc-tip-psp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-psp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tps-amm-lun    to   v-car                  .
           move      w-exp-tps-amm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tps-amm-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      zero                 to   w-exp-tps-amm-c01      .
           inspect   w-exp-tip-psp-stg
                                      tallying w-exp-tps-amm-c01
                     for   characters   before
                                       initial rr-tip-psp             .
           if        w-exp-tps-amm-c01    not  < w-exp-tip-psp-num
                     move  zero           to   v-num
           else      move  w-exp-tps-amm-c01
                                          to   v-num
                     add   1              to   v-num                  .
           if        rr-tip-psp           =    spaces
                     move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-psp-999.
       acc-tip-psp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-tps-amm-i01      .
           if        w-exp-tps-amm-i01    =    zero
                     move  spaces         to   rr-tip-psp
           else      move  w-exp-tps-amm-tps
                          (w-exp-tps-amm-i01)
                                          to   rr-tip-psp             .
       acc-tip-psp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a spazi non ammesso                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           =    spaces
                     go to acc-tip-psp-100.
       acc-tip-psp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts rimanenti           *
      *                  *---------------------------------------------*
           perform   pmt-trt-sep-000      thru pmt-trt-sep-999        .
           perform   pmt-fso-mag-000      thru pmt-fso-mag-999        .
           perform   pmt-prc-ril-000      thru pmt-prc-ril-999        .
           perform   pmt-tip-ubi-000      thru pmt-tip-ubi-999        .
           perform   pmt-prm-mi1-000      thru pmt-prm-mi1-999        .
           perform   pmt-prm-ma1-000      thru pmt-prm-ma1-999        .
           perform   pmt-prm-mi2-000      thru pmt-prm-mi2-999        .
           perform   pmt-prm-ma2-000      thru pmt-prm-ma2-999        .
           perform   pmt-prm-mi3-000      thru pmt-prm-mi3-999        .
           perform   pmt-prm-ma3-000      thru pmt-prm-ma3-999        .
           perform   pmt-prm-mi4-000      thru pmt-prm-mi4-999        .
           perform   pmt-prm-ma4-000      thru pmt-prm-ma4-999        .
           perform   pmt-tip-sel-000      thru pmt-tip-sel-999        .
           perform   pmt-tsp-mag-000      thru pmt-tsp-mag-999        .
           perform   pmt-tsp-mpt-000      thru pmt-tsp-mpt-999        .
           perform   pmt-snx-sgp-000      thru pmt-snx-sgp-999        .
           perform   pmt-snx-seg-000      thru pmt-snx-seg-999        .
       acc-tip-psp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-psp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-psp-100.
       acc-tip-psp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Filtro selezione magazzino *
      *    *-----------------------------------------------------------*
       acc-fso-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to acc-fso-mag-999.
       acc-fso-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo prospetto   *
      *                  *---------------------------------------------*
           if        rr-tip-psp           =    "P"
                     go to acc-fso-mag-120
           else if   rr-tip-psp           =    "S"
                     go to acc-fso-mag-140
           else if   rr-tip-psp           =    "M"
                     go to acc-fso-mag-160
           else if   rr-tip-psp           =    "V"
                     go to acc-fso-mag-180
           else      go to acc-fso-mag-999.
       acc-fso-mag-120.
      *                  *---------------------------------------------*
      *                  * Tipo prospetto : Prodotti di vendita        *
      *                  *---------------------------------------------*
           perform   acc-fso-dcp-000      thru acc-fso-dcp-999        .
           go to     acc-fso-mag-400.
       acc-fso-mag-140.
      *                  *---------------------------------------------*
      *                  * Tipo prospetto : Semilavorati               *
      *                  *---------------------------------------------*
           perform   acc-fso-dps-000      thru acc-fso-dps-999        .
           go to     acc-fso-mag-400.
       acc-fso-mag-160.
      *                  *---------------------------------------------*
      *                  * Tipo prospetto : Materie prime              *
      *                  *---------------------------------------------*
           perform   acc-fso-dpm-000      thru acc-fso-dpm-999        .
           go to     acc-fso-mag-400.
       acc-fso-mag-180.
      *                  *---------------------------------------------*
      *                  * Tipo prospetto : Materiali vari             *
      *                  *---------------------------------------------*
           perform   acc-fso-mtv-000      thru acc-fso-mtv-999        .
           go to     acc-fso-mag-400.
       acc-fso-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fso-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fso-mag-800.
       acc-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dcp]                 *
      *    *-----------------------------------------------------------*
       vis-fso-mag-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to vis-fso-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           if        rr-tip-psp           =    "P"
                     move  08             to   v-lin
           else      move  09             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-fso-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Descrizione codice filtro  *
      *    *                                per selezione ed ordina-   *
      *    *                                mento per il [dcp]         *
      *    *-----------------------------------------------------------*
       vis-des-fso-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to vis-des-fso-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           if        rr-tip-psp           =    "P"
                     move  08             to   v-lin
           else      move  09             to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-fso-mag-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fso-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dcp]                 *
      *    *-----------------------------------------------------------*
       acc-fso-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dcp-ope      .
           move      rr-fso-mag           to   w-cod-zos-dcp-cod      .
           move      08                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      08                   to   w-cod-zos-dcp-dln      .
           move      41                   to   w-cod-zos-dcp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
       acc-fso-dcp-110.
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           if        w-cod-zos-dcp-ope    =    "F+"
                     go to acc-fso-dcp-115.
           if        w-cod-zos-dcp-ope    =    "AC"
                     go to acc-fso-dcp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dcp-115.
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
           go to     acc-fso-dcp-110.
       acc-fso-dcp-120.
           move      w-cod-zos-dcp-cod    to   v-num                  .
       acc-fso-dcp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dcp-999.
       acc-fso-dcp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dcp]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dcp-cod      .
           perform   let-fso-dcp-000      thru let-fso-dcp-999        .
           move      w-let-fso-dcp-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-des-fso-000      thru vis-des-fso-999        .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dcp-flg    not  = spaces
                     go to acc-fso-dcp-100.
       acc-fso-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-mag-alf       to   f-key                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                =    "01"
                     move  01             to   rr-fso-mag-ord
           else if   f-sts                =    "02"
                     move  02             to   rr-fso-mag-ord
           else if   f-sts                =    "03"
                     move  03             to   rr-fso-mag-ord
           else if   f-sts                =    "04"
                     move  04             to   rr-fso-mag-ord
           else      move  01             to   rr-fso-mag-ord         .
       acc-fso-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dcp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dcp-100.
       acc-fso-dcp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dps]                 *
      *    *-----------------------------------------------------------*
       acc-fso-dps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dps-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dps-ope      .
           move      rr-fso-mag           to   w-cod-zos-dps-cod      .
           move      09                   to   w-cod-zos-dps-lin      .
           move      30                   to   w-cod-zos-dps-pos      .
           move      09                   to   w-cod-zos-dps-dln      .
           move      41                   to   w-cod-zos-dps-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
       acc-fso-dps-110.
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           if        w-cod-zos-dps-ope    =    "F+"
                     go to acc-fso-dps-115.
           if        w-cod-zos-dps-ope    =    "AC"
                     go to acc-fso-dps-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dps-115.
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
           go to     acc-fso-dps-110.
       acc-fso-dps-120.
           move      w-cod-zos-dps-cod    to   v-num                  .
       acc-fso-dps-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dps-999.
       acc-fso-dps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-dps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dps]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dps-cod      .
           perform   let-fso-dps-000      thru let-fso-dps-999        .
           move      w-let-fso-dps-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-des-fso-000      thru vis-des-fso-999        .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dps-flg    not  = spaces
                     go to acc-fso-dps-100.
       acc-fso-dps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-mag-alf       to   f-key                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                =    "01"
                     move  01             to   rr-fso-mag-ord
           else if   f-sts                =    "02"
                     move  02             to   rr-fso-mag-ord
           else if   f-sts                =    "03"
                     move  03             to   rr-fso-mag-ord
           else if   f-sts                =    "04"
                     move  04             to   rr-fso-mag-ord
           else      move  01             to   rr-fso-mag-ord         .
      *                  *---------------------------------------------*
      *                  * Preparazione default per tipo salto pagina  *
      *                  *---------------------------------------------*
           if        rr-fso-mag-ord       <    3
                     go to acc-fso-dps-800.
           move      1                    to   rr-tsp-mag             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           perform   vis-tsp-mag-000      thru vis-tsp-mag-999        .
       acc-fso-dps-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dps-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dps-100.
       acc-fso-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dpm]                 *
      *    *-----------------------------------------------------------*
       acc-fso-dpm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dpm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dpm-ope      .
           move      rr-fso-mag           to   w-cod-zos-dpm-cod      .
           move      09                   to   w-cod-zos-dpm-lin      .
           move      30                   to   w-cod-zos-dpm-pos      .
           move      09                   to   w-cod-zos-dpm-dln      .
           move      41                   to   w-cod-zos-dpm-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dpm-cll-000  thru cod-zos-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dpm-foi-000  thru cod-zos-dpm-foi-999    .
       acc-fso-dpm-110.
           perform   cod-zos-dpm-cll-000  thru cod-zos-dpm-cll-999    .
           if        w-cod-zos-dpm-ope    =    "F+"
                     go to acc-fso-dpm-115.
           if        w-cod-zos-dpm-ope    =    "AC"
                     go to acc-fso-dpm-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dpm-115.
           perform   cod-zos-dpm-foi-000  thru cod-zos-dpm-foi-999    .
           go to     acc-fso-dpm-110.
       acc-fso-dpm-120.
           move      w-cod-zos-dpm-cod    to   v-num                  .
       acc-fso-dpm-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dpm-999.
       acc-fso-dpm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-dpm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dpm]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dpm-cod      .
           perform   let-fso-dpm-000      thru let-fso-dpm-999        .
           move      w-let-fso-dpm-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-des-fso-000      thru vis-des-fso-999        .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dpm-flg    not  = spaces
                     go to acc-fso-dpm-100.
       acc-fso-dpm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-mag-alf       to   f-key                  .
           move      "pgm/dpm/prg/obj/bzosdpm0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                =    "01"
                     move  01             to   rr-fso-mag-ord
           else if   f-sts                =    "02"
                     move  02             to   rr-fso-mag-ord
           else if   f-sts                =    "03"
                     move  03             to   rr-fso-mag-ord
           else if   f-sts                =    "04"
                     move  04             to   rr-fso-mag-ord
           else      move  01             to   rr-fso-mag-ord         .
      *                  *---------------------------------------------*
      *                  * Preparazione default per tipo salto pagina  *
      *                  *---------------------------------------------*
           if        rr-fso-mag-ord       <    3
                     go to acc-fso-dpm-800.
           move      1                    to   rr-tsp-mag             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           perform   vis-tsp-mag-000      thru vis-tsp-mag-999        .
       acc-fso-dpm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dpm-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dpm-100.
       acc-fso-dpm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [mtv]                 *
      *    *-----------------------------------------------------------*
       acc-fso-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-mtv-ope      .
           move      rr-fso-mag           to   w-cod-zos-mtv-cod      .
           move      09                   to   w-cod-zos-mtv-lin      .
           move      30                   to   w-cod-zos-mtv-pos      .
           move      09                   to   w-cod-zos-mtv-dln      .
           move      41                   to   w-cod-zos-mtv-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
       acc-fso-mtv-110.
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           if        w-cod-zos-mtv-ope    =    "F+"
                     go to acc-fso-mtv-115.
           if        w-cod-zos-mtv-ope    =    "AC"
                     go to acc-fso-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-mtv-115.
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
           go to     acc-fso-mtv-110.
       acc-fso-mtv-120.
           move      w-cod-zos-mtv-cod    to   v-num                  .
       acc-fso-mtv-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-mtv-999.
       acc-fso-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [mtv]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-mtv-cod      .
           perform   let-fso-mtv-000      thru let-fso-mtv-999        .
           move      w-let-fso-mtv-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-des-fso-000      thru vis-des-fso-999        .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-mtv-flg    not  = spaces
                     go to acc-fso-mtv-100.
       acc-fso-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-mag-alf       to   f-key                  .
           move      "pgm/mtv/prg/obj/bzosmtv0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           if        f-sts                =    "01"
                     move  01             to   rr-fso-mag-ord
           else if   f-sts                =    "02"
                     move  02             to   rr-fso-mag-ord
           else if   f-sts                =    "03"
                     move  03             to   rr-fso-mag-ord
           else if   f-sts                =    "04"
                     move  04             to   rr-fso-mag-ord
           else      move  01             to   rr-fso-mag-ord         .
      *                  *---------------------------------------------*
      *                  * Preparazione default per tipo salto pagina  *
      *                  *---------------------------------------------*
           if        rr-fso-mag-ord       <    3
                     go to acc-fso-mtv-800.
           move      1                    to   rr-tsp-mag             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           perform   vis-tsp-mag-000      thru vis-tsp-mag-999        .
       acc-fso-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-mtv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-mtv-100.
       acc-fso-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Percorso di rilevazione                    *
      *    *-----------------------------------------------------------*
       acc-prc-ril-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to acc-prc-ril-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default se selezionato da filtro  *
      *                  * un solo prodotto od una classe              *
      *                  *---------------------------------------------*
           if        rr-fso-mag-alf (1 : 1)
                                          not  = "0" and
                     rr-prc-ril           =    zero
                     move  01             to   rr-prc-ril             .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-prc-ril          to   w-sav-prc-ril          .
       acc-prc-ril-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-prc-ril-lun    to   v-car                  .
           move      w-exp-prc-ril-num    to   v-ldt                  .
           move      "FU#"                to   v-msk                  .
           move      "X"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-prc-ril-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prc-ril           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prc-ril-999.
       acc-prc-ril-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-prc-ril             .
       acc-prc-ril-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-prc-ril           =    zero
                     go to acc-prc-ril-100.
       acc-prc-ril-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-prc-ril           =    w-sav-prc-ril
                     go to acc-prc-ril-800.
      *                  *---------------------------------------------*
      *                  * Se ordinamento da fltro                     *
      *                  *---------------------------------------------*
           if        rr-prc-ril           =    02
                     go to acc-prc-ril-700.
      *                      *-----------------------------------------*
      *                      * Normalizzazione valori ubicazione       *
      *                      *-----------------------------------------*
           move      spaces               to   rr-tip-ubi             .
           move      spaces               to   rr-tip-ubi-des         .
           move      zero                 to   rr-tip-ubi-pdu         .
           move      spaces               to   rr-tip-ubi-dpu (1)     .
           move      spaces               to   rr-tip-ubi-dpu (2)     .
           move      spaces               to   rr-tip-ubi-dpu (3)     .
           move      spaces               to   rr-tip-ubi-dpu (4)     .
           move      spaces               to   rr-prm-min (1)         .
           move      spaces               to   rr-prm-min (2)         .
           move      spaces               to   rr-prm-min (3)         .
           move      spaces               to   rr-prm-min (4)         .
           move      spaces               to   rr-prm-max (1)         .
           move      spaces               to   rr-prm-max (2)         .
           move      spaces               to   rr-prm-max (3)         .
           move      spaces               to   rr-prm-max (4)         .
      *                      *-----------------------------------------*
      *                      * Prompt per tipo salto pagina            *
      *                      *-----------------------------------------*
           perform   pmt-tsp-mag-000      thru pmt-tsp-mag-999        .
      *                      *-----------------------------------------*
      *                      * Preparazione default tipo salto pagina  *
      *                      *-----------------------------------------*
           if        rr-fso-mag-ord       <    3
                     go to acc-prc-ril-620.
           move      1                    to   rr-tsp-mag             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           perform   vis-tsp-mag-000      thru vis-tsp-mag-999        .
       acc-prc-ril-620.
      *                      *-----------------------------------------*
      *                      * Erase linee impegnate                   *
      *                      *-----------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      18                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-prc-ril-800.
       acc-prc-ril-700.
      *                  *---------------------------------------------*
      *                  * Se ordinamento per ubicazione               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts                                 *
      *                      *-----------------------------------------*
           perform   pmt-tip-ubi-000      thru pmt-tip-ubi-999        .
           perform   pmt-prm-mi1-000      thru pmt-prm-mi1-999        .
           perform   pmt-prm-mi2-000      thru pmt-prm-mi2-999        .
           perform   pmt-prm-mi3-000      thru pmt-prm-mi3-999        .
           perform   pmt-prm-mi4-000      thru pmt-prm-mi4-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-prc-ril-800.
       acc-prc-ril-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prc-ril-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prc-ril-100.
       acc-prc-ril-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Percorso di rilevazione                 *
      *    *-----------------------------------------------------------*
       vis-prc-ril-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to vis-prc-ril-999.
       vis-prc-ril-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-prc-ril-lun    to   v-car                  .
           move      w-exp-prc-ril-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-prc-ril-tbl    to   v-txt                  .
           move      rr-prc-ril           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prc-ril-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo di ubicazione                   *
      *    *-----------------------------------------------------------*
       acc-tip-ubi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-prc-ril           not  = 02
                     go to acc-tip-ubi-999.
       acc-tip-ubi-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "'?' : Prodotti senza ubicazione"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmu-ope      .
           move      rr-dpz-inu           to   w-cod-des-zmu-dpz      .
           move      rr-tip-ubi           to   w-cod-des-zmu-cod      .
           move      12                   to   w-cod-des-zmu-lin      .
           move      30                   to   w-cod-des-zmu-pos      .
           move      12                   to   w-cod-des-zmu-dln      .
           move      41                   to   w-cod-des-zmu-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zmu-cll-000  thru cod-des-zmu-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zmu-foi-000  thru cod-des-zmu-foi-999    .
       acc-tip-ubi-110.
           perform   cod-des-zmu-cll-000  thru cod-des-zmu-cll-999    .
           if        w-cod-des-zmu-ope    =    "F+"
                     go to acc-tip-ubi-115.
           if        w-cod-des-zmu-ope    =    "AC"
                     go to acc-tip-ubi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-ubi-115.
           perform   cod-des-zmu-foi-000  thru cod-des-zmu-foi-999    .
           go to     acc-tip-ubi-110.
       acc-tip-ubi-120.
           move      w-cod-des-zmu-cod    to   v-alf                  .
       acc-tip-ubi-150.
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ubi-999.
       acc-tip-ubi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-tip-ubi             .
       acc-tip-ubi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmu]                      *
      *                  *---------------------------------------------*
           move      rr-dpz-inu           to   w-let-arc-zmu-dpz      .
           move      rr-tip-ubi           to   w-let-arc-zmu-cod      .
           perform   let-arc-zmu-000      thru let-arc-zmu-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori                       *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    "?  "
                     move  "Senza ubicaz.  "
                                          to   rr-tip-ubi-des
           else if   rr-tip-ubi           =    spaces
                     move  "Tutti          "
                                          to   rr-tip-ubi-des
           else      move  w-let-arc-zmu-des
                                          to   rr-tip-ubi-des         .
           move      w-let-arc-zmu-pdu    to   rr-tip-ubi-pdu         .
           move      w-let-arc-zmu-dpu (1)
                                          to   rr-tip-ubi-dpu (1)     .
           move      w-let-arc-zmu-dpu (2)
                                          to   rr-tip-ubi-dpu (2)     .
           move      w-let-arc-zmu-dpu (3)
                                          to   rr-tip-ubi-dpu (3)     .
           move      w-let-arc-zmu-dpu (4)
                                          to   rr-tip-ubi-dpu (4)     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tip-ubi-des-000  thru vis-tip-ubi-des-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione parametri di ubicazione     *
      *                  *---------------------------------------------*
           perform   pmt-prm-mi1-000      thru pmt-prm-mi1-999        .
           perform   pmt-prm-mi2-000      thru pmt-prm-mi2-999        .
           perform   pmt-prm-mi3-000      thru pmt-prm-mi3-999        .
           perform   pmt-prm-mi4-000      thru pmt-prm-mi4-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-zmu-flg    not  = spaces
                     go to acc-tip-ubi-100.
       acc-tip-ubi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione eventuale dei parametri di  *
      *                  * ubicazione                                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi-pdu       =    zero
                     go to acc-tip-ubi-610
           else if   rr-tip-ubi-pdu       =    1
                     go to acc-tip-ubi-620
           else if   rr-tip-ubi-pdu       =    2
                     go to acc-tip-ubi-630
           else if   rr-tip-ubi-pdu       =    3
                     go to acc-tip-ubi-640
           else      go to acc-tip-ubi-800.
       acc-tip-ubi-610.
           move      spaces               to   rr-prm-min (1)         .
           move      spaces               to   rr-prm-max (1)         .
           perform   vis-prm-mi1-000      thru vis-prm-mi1-999        .
           perform   vis-prm-ma1-000      thru vis-prm-ma1-999        .
       acc-tip-ubi-620.
           move      spaces               to   rr-prm-min (2)         .
           move      spaces               to   rr-prm-max (2)         .
           perform   vis-prm-mi2-000      thru vis-prm-mi2-999        .
           perform   vis-prm-ma2-000      thru vis-prm-ma2-999        .
       acc-tip-ubi-630.
           move      spaces               to   rr-prm-min (3)         .
           move      spaces               to   rr-prm-max (3)         .
           perform   vis-prm-mi3-000      thru vis-prm-mi3-999        .
           perform   vis-prm-ma3-000      thru vis-prm-ma3-999        .
       acc-tip-ubi-640.
           move      spaces               to   rr-prm-min (4)         .
           move      spaces               to   rr-prm-max (4)         .
           perform   vis-prm-mi4-000      thru vis-prm-mi4-999        .
           perform   vis-prm-ma4-000      thru vis-prm-ma4-999        .
       acc-tip-ubi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ubi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ubi-100.
       acc-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ubicazione                   *
      *    *-----------------------------------------------------------*
       vis-tip-ubi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-ubi           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione ubicazione            *
      *    *-----------------------------------------------------------*
       vis-tip-ubi-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-tip-ubi-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ubi-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 1 - min                    *
      *    *-----------------------------------------------------------*
       acc-prm-mi1-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-mi1-999.
       acc-prm-mi1-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-min (1)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-mi1-999.
       acc-prm-mi1-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-min (1)         .
       acc-prm-mi1-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-min (1)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-mi1-100.
       acc-prm-mi1-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-mi1-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-mi1-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-mi1-100.
       acc-prm-mi1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 1 - min                 *
      *    *-----------------------------------------------------------*
       vis-prm-mi1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      rr-prm-min (1)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-mi1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 1 - max                    *
      *    *-----------------------------------------------------------*
       acc-prm-ma1-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-ma1-999.
       acc-prm-ma1-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-max (1)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-ma1-999.
       acc-prm-ma1-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-max (1)         .
       acc-prm-ma1-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-max (1)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ma1-100.
       acc-prm-ma1-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-ma1-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-ma1-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-ma1-100.
       acc-prm-ma1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 1 - max                 *
      *    *-----------------------------------------------------------*
       vis-prm-ma1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-prm-max (1)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ma1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 2 - min                    *
      *    *-----------------------------------------------------------*
       acc-prm-mi2-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-mi2-999.
           if        rr-tip-ubi-pdu       not  > 1
                     go to acc-prm-mi2-999.
       acc-prm-mi2-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-min (2)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-mi2-999.
       acc-prm-mi2-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-min (2)         .
       acc-prm-mi2-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-min (2)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-mi2-100.
       acc-prm-mi2-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-mi2-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-mi2-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-mi2-100.
       acc-prm-mi2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 2 - min                 *
      *    *-----------------------------------------------------------*
       vis-prm-mi2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      rr-prm-min (2)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-mi2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 2 - max                    *
      *    *-----------------------------------------------------------*
       acc-prm-ma2-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-ma2-999.
           if        rr-tip-ubi-pdu       not  > 1
                     go to acc-prm-ma2-999.
       acc-prm-ma2-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-max (2)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-ma2-999.
       acc-prm-ma2-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-max (2)         .
       acc-prm-ma2-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-max (2)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ma2-100.
       acc-prm-ma2-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-ma2-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-ma2-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-ma2-100.
       acc-prm-ma2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 2 - max                 *
      *    *-----------------------------------------------------------*
       vis-prm-ma2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-prm-max (2)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ma2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 3 - min                    *
      *    *-----------------------------------------------------------*
       acc-prm-mi3-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-mi3-999.
           if        rr-tip-ubi-pdu       not  > 2
                     go to acc-prm-mi3-999.
       acc-prm-mi3-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-min (3)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-mi3-999.
       acc-prm-mi3-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-min (3)         .
       acc-prm-mi3-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-min (3)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-mi3-100.
       acc-prm-mi3-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-mi3-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-mi3-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-mi3-100.
       acc-prm-mi3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 3 - min                 *
      *    *-----------------------------------------------------------*
       vis-prm-mi3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      rr-prm-min (3)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-mi3-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 3 - max                    *
      *    *-----------------------------------------------------------*
       acc-prm-ma3-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-ma3-999.
           if        rr-tip-ubi-pdu       not  > 2
                     go to acc-prm-ma3-999.
       acc-prm-ma3-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-max (3)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-ma3-999.
       acc-prm-ma3-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-max (3)         .
       acc-prm-ma3-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-max (3)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ma3-100.
       acc-prm-ma3-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-ma3-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-ma3-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-ma3-100.
       acc-prm-ma3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 3 - max                 *
      *    *-----------------------------------------------------------*
       vis-prm-ma3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-prm-max (3)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ma3-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 4 - min                    *
      *    *-----------------------------------------------------------*
       acc-prm-mi4-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-mi4-999.
           if        rr-tip-ubi-pdu       not  > 3
                     go to acc-prm-mi4-999.
       acc-prm-mi4-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-min (4)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-mi4-999.
       acc-prm-mi4-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-min (4)         .
       acc-prm-mi4-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-min (4)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-mi4-100.
       acc-prm-mi4-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-mi4-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-mi4-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-mi4-100.
       acc-prm-mi4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 4 - min                 *
      *    *-----------------------------------------------------------*
       vis-prm-mi4-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      rr-prm-min (4)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-mi4-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Parametro 4 - max                    *
      *    *-----------------------------------------------------------*
       acc-prm-ma4-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to acc-prm-ma4-999.
           if        rr-tip-ubi-pdu       not  > 3
                     go to acc-prm-ma4-999.
       acc-prm-ma4-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-prm-max (4)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prm-ma4-999.
       acc-prm-ma4-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-prm-max (4)         .
       acc-prm-ma4-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      rr-prm-max (4)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ma4-100.
       acc-prm-ma4-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prm-ma4-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prm-ma4-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prm-ma4-100.
       acc-prm-ma4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Parametro 4 - max                 *
      *    *-----------------------------------------------------------*
       vis-prm-ma4-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-prm-max (4)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ma4-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo salto pagina per mer- *
      *    * ci presso terzi                                           *
      *    *-----------------------------------------------------------*
       acc-tip-sel-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to acc-tip-sel-999.
       acc-tip-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sel-lun    to   v-car                  .
           move      w-exp-tip-sel-num    to   v-ldt                  .
           move      "TCF#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-sel-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-sel           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-sel-999.
       acc-tip-sel-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-sel             .
       acc-tip-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-sel           not  = zero
                     go to acc-tip-sel-600.
           if        v-key                =    "UP  "
                     go to acc-tip-sel-600
           else      go to acc-tip-sel-999.
       acc-tip-sel-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-sel-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-sel-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-sel-100.
       acc-tip-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Tipo salto pagina per ma-  *
      *    * gazzino                                                   *
      *    *-----------------------------------------------------------*
       vis-tip-sel-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to vis-tip-sel-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sel-lun    to   v-car                  .
           move      w-exp-tip-sel-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-sel-tbl    to   v-txt                  .
           move      rr-tip-sel           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo salto pagina per ma-  *
      *    * gazzino                                                   *
      *    *-----------------------------------------------------------*
       acc-tsp-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to acc-tsp-mag-999.
           if        rr-prc-ril           =    02
                     go to acc-tsp-mag-999.
           if        rr-fso-mag-ord       >    2
                     go to acc-tsp-mag-999.
       acc-tsp-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsp-mag-lun    to   v-car                  .
           move      w-exp-tsp-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           if        rr-tip-psp           =    "P"
                     move  19             to   v-lin
           else      move  13             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tsp-mag-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tsp-mag           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tsp-mag-999.
       acc-tsp-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tsp-mag             .
       acc-tsp-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tsp-mag           not  = zero
                     go to acc-tsp-mag-600.
           if        v-key                =    "UP  "
                     go to acc-tsp-mag-600
           else      go to acc-tsp-mag-999.
       acc-tsp-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tsp-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tsp-mag-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tsp-mag-100.
       acc-tsp-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Tipo salto pagina per ma-  *
      *    * gazzino                                                   *
      *    *-----------------------------------------------------------*
       vis-tsp-mag-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T"
                     go to vis-tsp-mag-999.
           if        rr-prc-ril           =    02
                     go to vis-tsp-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsp-mag-lun    to   v-car                  .
           move      w-exp-tsp-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           if        rr-tip-psp           =    "P"
                     move  19             to   v-lin
           else      move  13             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tsp-mag-tbl    to   v-txt                  .
           move      rr-tsp-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tsp-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo salto pagina per mer- *
      *    * ci presso terzi                                           *
      *    *-----------------------------------------------------------*
       acc-tsp-mpt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to acc-tsp-mpt-999.
       acc-tsp-mpt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsp-mpt-lun    to   v-car                  .
           move      w-exp-tsp-mpt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tsp-mpt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tsp-mpt           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tsp-mpt-999.
       acc-tsp-mpt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tsp-mpt             .
       acc-tsp-mpt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tsp-mpt           not  = zero
                     go to acc-tsp-mpt-600.
           if        v-key                =    "UP  "
                     go to acc-tsp-mpt-600
           else      go to acc-tsp-mpt-999.
       acc-tsp-mpt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tsp-mpt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tsp-mpt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tsp-mpt-100.
       acc-tsp-mpt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Tipo salto pagina per mer- *
      *    * ci presso terzi                                           *
      *    *-----------------------------------------------------------*
       vis-tsp-mpt-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "T"
                     go to vis-tsp-mpt-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsp-mpt-lun    to   v-car                  .
           move      w-exp-tsp-mpt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tsp-mpt-tbl    to   v-txt                  .
           move      rr-tsp-mpt           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tsp-mpt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Si/no giacenza presunta    *
      *    *-----------------------------------------------------------*
       acc-snx-sgp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-sgp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sgp-lun    to   v-car                  .
           move      w-exp-snx-sgp-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           if        rr-tip-psp           =    "P"
                     move  20             to   v-lin
           else      move  16             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-sgp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        rr-snx-sgp           =    "N"
                     move  01             to   v-num
           else if   rr-snx-sgp           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-sgp-999.
       acc-snx-sgp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   rr-snx-sgp
           else if   v-num                =    02
                     move  "S"            to   rr-snx-sgp
           else      move  spaces         to   rr-snx-sgp             .
       acc-snx-sgp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore spaces non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-snx-sgp           not  = spaces
                     go to acc-snx-sgp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-sgp-600
           else      go to acc-snx-sgp-999.
       acc-snx-sgp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-sgp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-sgp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-sgp-100.
       acc-snx-sgp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Si/no giacenza presunta    *
      *    *-----------------------------------------------------------*
       vis-snx-sgp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sgp-lun    to   v-car                  .
           move      w-exp-snx-sgp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           if        rr-tip-psp           =    "P"
                     move  20             to   v-lin
           else      move  16             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-sgp-tbl    to   v-txt                  .
           if        rr-snx-sgp           =    "N"
                     move  01             to   v-num
           else if   rr-snx-sgp           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-sgp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Si/no selezione su giacen- *
      *    *                                za presunta                *
      *    *-----------------------------------------------------------*
       acc-snx-seg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to acc-snx-seg-999.
       acc-snx-seg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-seg-lun    to   v-car                  .
           move      w-exp-snx-seg-num    to   v-ldt                  .
           move      "PTNZ#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-seg-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-seg           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-seg-999.
       acc-snx-seg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-seg             .
       acc-snx-seg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore spaces non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        rr-snx-seg           not  = zero
                     go to acc-snx-seg-600.
           if        v-key                =    "UP  "
                     go to acc-snx-seg-600
           else      go to acc-snx-seg-999.
       acc-snx-seg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-seg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-seg-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-seg-100.
       acc-snx-seg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Si/no selezione su      *
      *    *                                   giacenza presunta       *
      *    *-----------------------------------------------------------*
       vis-snx-seg-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to vis-snx-seg-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-seg-lun    to   v-car                  .
           move      w-exp-snx-seg-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-seg-tbl    to   v-txt                  .
           move      rr-snx-seg           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-seg-999.
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
      *              * Controllo su Percorso di rilevazione            *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "P"
                     go to tdo-ric-sel-800.
           if        rr-prc-ril           =    01 or
                     rr-prc-ril           =    02
                     go to tdo-ric-sel-200.
           move      "Percorso di rilevazione da impostare !            
      -              "          "         to   w-box-msg-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Parametri di ubicazione            *
      *              *-------------------------------------------------*
           if        rr-prc-ril           =    01
                     go to tdo-ric-sel-400.
           if        rr-tip-ubi           =    spaces or
                     rr-tip-ubi           =    "?  "
                     go to tdo-ric-sel-400.
       tdo-ric-sel-210.
      *                  *---------------------------------------------*
      *                  * Parametro 1 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (1)       =    spaces
                     go to tdo-ric-sel-220.
           if        rr-prm-max (1)       not  < rr-prm-min (1)
                     go to tdo-ric-sel-220.
           move      "Il parametro massimo 1 non puo' essere inferiore a
      -              "l minimo !"         to   w-box-msg-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-220.
      *                  *---------------------------------------------*
      *                  * Parametro 2 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (2)       =    spaces
                     go to tdo-ric-sel-230.
           if        rr-prm-max (2)       not  < rr-prm-min (2)
                     go to tdo-ric-sel-230.
           move      "Il parametro massimo 2 non puo' essere inferiore a
      -              "l minimo !"         to   w-box-msg-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-230.
      *                  *---------------------------------------------*
      *                  * Parametro 3 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (3)       =    spaces
                     go to tdo-ric-sel-240.
           if        rr-prm-max (3)       not  < rr-prm-min (3)
                     go to tdo-ric-sel-240.
           move      "Il parametro massimo 3 non puo' essere inferiore a
      -              "l minimo !"         to   w-box-msg-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-240.
      *                  *---------------------------------------------*
      *                  * Parametro 4 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (4)       =    spaces
                     go to tdo-ric-sel-400.
           if        rr-prm-max (4)       not  < rr-prm-min (4)
                     go to tdo-ric-sel-400.
           move      "Il parametro massimo 4 non puo' essere inferiore a
      -              "l minimo !"         to   w-box-msg-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri di ubicazione                     *
      *                  *---------------------------------------------*
           if        rr-prc-ril           =    01
                     move  spaces         to   rr-tip-ubi             .
           if        rr-tip-ubi           =    "?  "
                     go to tdo-ric-sel-505.
           if        rr-tip-ubi           not  = spaces
                     go to tdo-ric-sel-510.
       tdo-ric-sel-505.
           move      spaces               to   rr-prm-min (1)         .
           move      spaces               to   rr-prm-min (2)         .
           move      spaces               to   rr-prm-min (3)         .
           move      spaces               to   rr-prm-min (4)         .
           move      spaces               to   rr-prm-max (1)         .
           move      spaces               to   rr-prm-max (2)         .
           move      spaces               to   rr-prm-max (3)         .
           move      spaces               to   rr-prm-max (4)         .
           go to     tdo-ric-sel-800.
       tdo-ric-sel-510.
      *                  *---------------------------------------------*
      *                  * Parametro 1 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (1)       =    spaces
                     move  rr-prm-min (1) to   rr-prm-max (1)         .
           move      rr-prm-max (1)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-prm-max (1)         .
       tdo-ric-sel-520.
      *                  *---------------------------------------------*
      *                  * Parametro 2 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (2)       =    spaces
                     move  rr-prm-min (2) to   rr-prm-max (2)         .
           move      rr-prm-max (2)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-prm-max (2)         .
       tdo-ric-sel-530.
      *                  *---------------------------------------------*
      *                  * Parametro 3 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (3)       =    spaces
                     move  rr-prm-min (3) to   rr-prm-max (3)         .
           move      rr-prm-max (3)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-prm-max (3)         .
       tdo-ric-sel-540.
      *                  *---------------------------------------------*
      *                  * Parametro 4 - min - max                     *
      *                  *---------------------------------------------*
           if        rr-prm-max (4)       =    spaces
                     move  rr-prm-min (4) to   rr-prm-max (4)         .
           move      rr-prm-max (4)       to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-prm-max (4)         .
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo selezione                  *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T" and
                     rr-tip-sel           =    zero
                     move  01             to   rr-tip-sel             .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo salto pagina per magazzino *
      *              *-------------------------------------------------*
           if        rr-tip-psp           not  = "T" and
                     rr-tsp-mag           =    zero
                     move  01             to   rr-tsp-mag             .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo salto pagina per merci     *
      *              * presso terzi                                    *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "T" and
                     rr-tsp-mpt           =    zero
                     move  01             to   rr-tsp-mpt             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No stampa giacenza presunta  *
      *              *-------------------------------------------------*
           if        rr-snx-sgp           =    spaces
                     move  "N"            to   rr-snx-sgp             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No selezione su giacenza     *
      *              *-------------------------------------------------*
           if        rr-snx-seg           =    zero
                     move  01             to   rr-snx-seg             .
       reg-ric-sel-999.
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
           move      07                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-box-msg-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      70                   to   v-pos                  .
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
           move      71                   to   v-pos                  .
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
      *    * Richiamo sottoprogrammi per tipo prospetto                *
      *    *-----------------------------------------------------------*
       cmp-nep-pde-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo prospetto       *
      *              *-------------------------------------------------*
           if        rr-tip-psp           =    "P"
                     go to cmp-nep-pde-100
           else if   rr-tip-psp           =    "S"
                     go to cmp-nep-pde-200
           else if   rr-tip-psp           =    "M"
                     go to cmp-nep-pde-300
           else if   rr-tip-psp           =    "V"
                     go to cmp-nep-pde-400
           else if   rr-tip-psp           =    "T"
                     go to cmp-nep-pde-500
           else      go to cmp-nep-pde-999.
       cmp-nep-pde-100.
      *              *-------------------------------------------------*
      *              * Tipo prospetto : Prodotti di vendita            *
      *              *-------------------------------------------------*
           move      "pmag610a"           to   i-exe-pro              .
           move      "pgm/mag/prg/obj/pmag610a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           go to     cmp-nep-pde-999.
       cmp-nep-pde-200.
      *              *-------------------------------------------------*
      *              * Tipo prospetto : Semilavorati                   *
      *              *-------------------------------------------------*
           move      "pmag610c"           to   i-exe-pro              .
           move      "pgm/mag/prg/obj/pmag610c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           go to     cmp-nep-pde-999.
       cmp-nep-pde-300.
      *              *-------------------------------------------------*
      *              * Tipo prospetto : Materie prime                  *
      *              *-------------------------------------------------*
           move      "pmag610e"           to   i-exe-pro              .
           move      "pgm/mag/prg/obj/pmag610e"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           go to     cmp-nep-pde-999.
       cmp-nep-pde-400.
      *              *-------------------------------------------------*
      *              * Tipo prospetto : Materiali vari                 *
      *              *-------------------------------------------------*
           move      "pmag610g"           to   i-exe-pro              .
           move      "pgm/mag/prg/obj/pmag610g"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           go to     cmp-nep-pde-999.
       cmp-nep-pde-500.
      *              *-------------------------------------------------*
      *              * Tipo prospetto : Merci presso terzi             *
      *              *-------------------------------------------------*
           move      "pmag610q"           to   i-exe-pro              .
           move      "pgm/mag/prg/obj/pmag610q"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           go to     cmp-nep-pde-999.
       cmp-nep-pde-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-tip-psp             .
           move      zero                 to   rr-fso-mag             .
           move      spaces               to   rr-fso-mag-des         .
           move      zero                 to   rr-fso-mag-ord         .
           move      zero                 to   rr-prc-ril             .
           move      spaces               to   rr-tip-ubi             .
           move      spaces               to   rr-tip-ubi-des         .
           move      zero                 to   rr-tip-ubi-pdu         .
           move      spaces               to   rr-tip-ubi-dpu (1)     .
           move      spaces               to   rr-tip-ubi-dpu (2)     .
           move      spaces               to   rr-tip-ubi-dpu (3)     .
           move      spaces               to   rr-tip-ubi-dpu (4)     .
           move      spaces               to   rr-prm-min (1)         .
           move      spaces               to   rr-prm-min (2)         .
           move      spaces               to   rr-prm-min (3)         .
           move      spaces               to   rr-prm-min (4)         .
           move      spaces               to   rr-prm-max (1)         .
           move      spaces               to   rr-prm-max (2)         .
           move      spaces               to   rr-prm-max (3)         .
           move      spaces               to   rr-prm-max (4)         .
           move      zero                 to   rr-tip-sel             .
           move      zero                 to   rr-tsp-mag             .
           move      zero                 to   rr-tsp-mpt             .
           move      spaces               to   rr-snx-sgp             .
           move      zero                 to   rr-snx-seg             .
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
           if        rr-snx-sgp           =    "N"
                     move  106            to   w-cnt-stp-amp-lin
           else      move  132            to   w-cnt-stp-amp-lin      .
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
      *    * Routine di lettura archivio [zmu]                         *
      *    *-----------------------------------------------------------*
       let-arc-zmu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmu-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice tipo ubicazione a spaces         *
      *              *-------------------------------------------------*
           if        w-let-arc-zmu-cod    =    spaces or
                     w-let-arc-zmu-cod    =    "?  "
                     go to let-arc-zmu-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPUBI    "         to   f-key                  .
           move      w-let-arc-zmu-dpz    to   rf-zmu-cod-dpz         .
           move      w-let-arc-zmu-cod    to   rf-zmu-tip-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmu-400.
       let-arc-zmu-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmu-des-ubi       to   w-let-arc-zmu-des      .
           move      rf-zmu-num-pdu       to   w-let-arc-zmu-pdu      .
           move      rf-zmu-des-pdu (1)   to   w-let-arc-zmu-dpu (1)  .
           move      rf-zmu-des-pdu (2)   to   w-let-arc-zmu-dpu (2)  .
           move      rf-zmu-des-pdu (3)   to   w-let-arc-zmu-dpu (3)  .
           move      rf-zmu-des-pdu (4)   to   w-let-arc-zmu-dpu (4)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmu-999.
       let-arc-zmu-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmu-flg      .
           move      all   "."            to   w-let-arc-zmu-des      .
           go to     let-arc-zmu-600.
       let-arc-zmu-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmu-des      .
       let-arc-zmu-600.
           move      zero                 to   w-let-arc-zmu-pdu      .
           move      spaces               to   w-let-arc-zmu-dpu (1)  .
           move      spaces               to   w-let-arc-zmu-dpu (2)  .
           move      spaces               to   w-let-arc-zmu-dpu (3)  .
           move      spaces               to   w-let-arc-zmu-dpu (4)  .
       let-arc-zmu-999.
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
      *    * Routine lettura archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dpm]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/lzosdpm0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [mtv]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/lzosmtv0.lts"                   .
          
      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dcp]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dps]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dpm]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/azosdpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [mtv]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice tipo ubicazione *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmu0.acs"                   .

