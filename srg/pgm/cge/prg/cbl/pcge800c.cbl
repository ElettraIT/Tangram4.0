       Identification Division.
       Program-Id.                                 pcge800c           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    bil                 *
      *                                   Fase:    cge860              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/95    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Stampa movimenti di rettifica al bilancio   *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     " STAMPA MOVIMENTI DI RETTIFICA BILANCIO "       .

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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
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
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
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
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [datbil]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndatbil"                       .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data bilancio d'esercizio di riferimento di default   *
      *        *-------------------------------------------------------*
           05  rr-dtb-def                 pic  9(07)                  .
           05  rr-dtb-def-r01 redefines
               rr-dtb-def.
               10  rr-saa-def             pic  9(03)                  .
               10  rr-mes-def             pic  9(02)                  .
               10  rr-gio-def             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data bilancio d'esercizio di riferimento              *
      *        *-------------------------------------------------------*
           05  rr-dat-bil                 pic  9(07)                  .
           05  rr-dat-bil-r01 redefines
               rr-dat-bil.
               10  rr-saa-bil             pic  9(03)                  .
               10  rr-mes-bil             pic  9(02)                  .
               10  rr-gio-bil             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data movimenti di rettifica minima                    *
      *        *-------------------------------------------------------*
           05  rr-dtm-min                 pic  9(07)                  .
           05  rr-dtm-min-r01 redefines
               rr-dtm-min.
               10  rr-saa-min             pic  9(03)                  .
               10  rr-mes-min             pic  9(02)                  .
               10  rr-gio-min             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data movimenti di rettifica massima                   *
      *        *-------------------------------------------------------*
           05  rr-dtm-max                 pic  9(07)                  .
           05  rr-dtm-max-r01 redefines
               rr-dtm-max.
               10  rr-saa-max             pic  9(03)                  .
               10  rr-mes-max             pic  9(02)                  .
               10  rr-gio-max             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Anno di esercizio della scheda saldi contabili da     *
      *        * leggere                                               *
      *        *-------------------------------------------------------*
           05  rr-ann-ese                 pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su sottoconti da stampare                   *
      *        *  - 01 : Tutti                                         *
      *        *  - 02 : Solo i sottoconti con rettifiche              *
      *        *-------------------------------------------------------*
           05  rr-sel-stc                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/No lista movimenti per ogni sottoconto             *
      *        *  - S : Si                                             *
      *        *  - N : No                                             *
      *        *-------------------------------------------------------*
           05  rr-snx-lmr                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Per : Selezione su sottoconti da stampare             *
      *        *-------------------------------------------------------*
           05  w-exp-sel-stc.
               10  w-exp-sel-stc-num      pic  9(02)       value 2    .
               10  w-exp-sel-stc-lun      pic  9(02)       value 25   .
               10  w-exp-sel-stc-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
                   15  filler             pic  x(25) value
                            "Solo quelli rettificati  "               .
      *        *-------------------------------------------------------*
      *        * Per : Si/No lista movimenti per ogni sottoconto       *
      *        *-------------------------------------------------------*
           05  w-exp-snx-lmr.
               10  w-exp-snx-lmr-num      pic  9(02)       value 2    .
               10  w-exp-snx-lmr-lun      pic  9(02)       value 02   .
               10  w-exp-snx-lmr-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

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

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

      *    *===========================================================*
      *    * Buffer per records archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       01  w-buf-pdc-001.
      *        *-------------------------------------------------------*
      *        * Massimo numero di elementi nel buffer                 *
      *        *-------------------------------------------------------*
           05  w-buf-pdc-max-ele          pic  9(05)       value 1500 .
      *        *-------------------------------------------------------*
      *        * Numero di elementi presenti nel buffer                *
      *        *-------------------------------------------------------*
           05  w-buf-pdc-num-ele          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Indice su elementi presenti nel buffer                *
      *        *-------------------------------------------------------*
           05  w-buf-pdc-inx-ele          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codici dei sottoconti                                 *
      *        *-------------------------------------------------------*
       01  w-buf-pdc-002.
           05  w-buf-pdc-002-ele occurs 1500.
               10  w-buf-pdc-cod-pdc      pic  9(07)        comp-3    .
      *        *-------------------------------------------------------*
      *        * Descrizioni dei sottoconti                            *
      *        *-------------------------------------------------------*
       01  w-buf-pdc-003.
           05  w-buf-pdc-003-ele occurs 1500.
               10  w-buf-pdc-des-pdc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Progressivi dei sottoconti da scheda contabile        *
      *        *  - Saldo inizio anno                                  *
      *        *  - Progressivo anno dare                              *
      *        *  - Progressivo anno avere                             *
      *        *  - Saldo fine anno                                    *
      *        *  - Progressivo rettifiche dare                        *
      *        *  - Progressivo rettifiche avere                       *
      *        *  - Saldo di bilancio                                  *
      *        *-------------------------------------------------------*
       01  w-buf-pdc-004.
           05  w-buf-pdc-004-ele occurs 1500.
               10  w-buf-pdc-sdo-ini      pic s9(13)        comp-3    .
               10  w-buf-pdc-dar-ann      pic s9(13)        comp-3    .
               10  w-buf-pdc-ave-ann      pic s9(13)        comp-3    .
               10  w-buf-pdc-sdo-fin      pic s9(13)        comp-3    .
               10  w-buf-pdc-dar-ret      pic s9(13)        comp-3    .
               10  w-buf-pdc-ave-ret      pic s9(13)        comp-3    .
               10  w-buf-pdc-sdo-bil      pic s9(13)        comp-3    .

      *    *===========================================================*
      *    * Work per totali                                           *
      *    *-----------------------------------------------------------*
       01  w-tot.
      *        *-------------------------------------------------------*
      *        * Totali generali per                                   *
      *        *  - Saldo inizio anno                                  *
      *        *  - Progressivo anno dare                              *
      *        *  - Progressivo anno avere                             *
      *        *  - Saldo fine anno                                    *
      *        *  - Progressivo rettifiche dare                        *
      *        *  - Progressivo rettifiche avere                       *
      *        *  - Saldo di bilancio                                  *
      *        *-------------------------------------------------------*
           05  w-tot-gen.
               10  w-tot-gen-sdo-ini      pic s9(13)        comp-3    .
               10  w-tot-gen-dar-ann      pic s9(13)        comp-3    .
               10  w-tot-gen-ave-ann      pic s9(13)        comp-3    .
               10  w-tot-gen-sdo-fin      pic s9(13)        comp-3    .
               10  w-tot-gen-dar-ret      pic s9(13)        comp-3    .
               10  w-tot-gen-ave-ret      pic s9(13)        comp-3    .
               10  w-tot-gen-sdo-bil      pic s9(13)        comp-3    .
      *        *-------------------------------------------------------*
      *        * Totali generali per i movimenti stampati relativi al  *
      *        * singolo sottoconto                                    *
      *        *  - Numero movimenti stampati                          *
      *        *  - Progressivo rettifiche dare                        *
      *        *  - Progressivo rettifiche avere                       *
      *        *-------------------------------------------------------*
           05  w-tot-stc.
               10  w-tot-stc-num-mvs      pic  9(07)        comp-3    .
               10  w-tot-stc-dar-ret      pic s9(13)        comp-3    .
               10  w-tot-stc-ave-ret      pic s9(13)        comp-3    .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pcge4500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge8000.pgl"                   .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-tmn
                                               w-spg
                                               w-prs                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
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
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
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
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800
           else      go to main-250.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
           move      w-des-tit-pgm-ovy    to   v-alf                  .
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
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa       *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-pgm-frg-900.
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
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
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
           exit.


______*__________________Routines variabili_____________________________



      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Preparazione data di bilancio di riferimento di *
      *              * default                                         *
      *              *-------------------------------------------------*
           perform   pre-dtb-def-000      thru pre-dtb-def-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione data di bilancio di riferimento di default   *
      *    *-----------------------------------------------------------*
       pre-dtb-def-000.
      *              *-------------------------------------------------*
      *              * Lettura dati relativi all'ultimo bilancio, gia' *
      *              * chiuso, o in corso di esecuzione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open numerazione                            *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record numerazione          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Lettura record numerazione                  *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Close numerazione                           *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
       pre-dtb-def-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se esiste o no un 'ultimo  *
      *              * bilancio', sia esso gia' chiuso, sia esso in    *
      *              * corso di esecuzione                             *
      *              *-------------------------------------------------*
           if        rn-dat-bil-ese-bil   not  = zero
                     go to pre-dtb-def-300.
       pre-dtb-def-200.
      *              *-------------------------------------------------*
      *              * Se un 'ultimo bilancio' non esiste              *
      *              *-------------------------------------------------*
       pre-dtb-def-210.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione titolo programma        *
      *                      *-----------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Messaggio nel box                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "Questo programma e' eseguibile solamente se e' sta
      -              "to completato un"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "ciclo di bilancio, o almeno se e' in corso un cicl
      -              "o di bilancio ! "   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadre di delimitazione       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-dtb-def-220.
      *                  *---------------------------------------------*
      *                  * Accettazione per presa visione              *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      72                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-dtb-def-230.
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
       pre-dtb-def-240.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pre-dtb-def-900.
       pre-dtb-def-300.
      *              *-------------------------------------------------*
      *              * Se un 'ultimo bilancio' esiste                  *
      *              *-------------------------------------------------*
       pre-dtb-def-310.
      *                  *---------------------------------------------*
      *                  * Determinazione data bilancio d'esercizio di *
      *                  * riferimento di default                      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dtb-def             .
           move      rn-dat-bil-ese-bil   to   rr-saa-def             .
           move      w-prs-mes-chi        to   rr-mes-def             .
           move      31                   to   rr-gio-def             .
           if        w-prs-mes-chi        not  = 12
                     add   1              to   rr-saa-def             .
       pre-dtb-def-320.
           move      "CD"                 to   s-ope                  .
           move      rr-dtb-def           to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to pre-dtb-def-330.
           subtract  1                    from rr-gio-def             .
           go to     pre-dtb-def-320.
       pre-dtb-def-330.
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad Ok                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-dtb-def-340.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pre-dtb-def-900.
       pre-dtb-def-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-dtb-def-999.
       pre-dtb-def-999.
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
      *              * Si/No funzionamento ciclico stampa              *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
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
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [mgs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [mgs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
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
      *                  * Data bilancio di riferimento                *
      *                  *---------------------------------------------*
           perform   acc-dat-bil-000      thru acc-dat-bil-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Selezione su sottoconti da stampare         *
      *                  *---------------------------------------------*
           perform   acc-sel-stc-000      thru acc-sel-stc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Si/No lista movimenti per ogni sottoconto   *
      *                  *---------------------------------------------*
           perform   acc-snx-lmr-000      thru acc-snx-lmr-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
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
      *              * Data bilancio di riferimento                    *
      *              *-------------------------------------------------*
           perform   pmt-dat-bil-000      thru pmt-dat-bil-999        .
      *              *-------------------------------------------------*
      *              * Selezione su sottoconti da stampare             *
      *              *-------------------------------------------------*
           perform   pmt-sel-stc-000      thru pmt-sel-stc-999        .
      *              *-------------------------------------------------*
      *              * Si/No lista movimenti per ogni sottoconto       *
      *              *-------------------------------------------------*
           perform   pmt-snx-lmr-000      thru pmt-snx-lmr-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data bilancio di riferimento                     *
      *    *-----------------------------------------------------------*
       pmt-dat-bil-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data del bilancio cui le rettifiche si riferiscono
      -              "  :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-bil-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Selezione su sottoconti da stampare              *
      *    *-----------------------------------------------------------*
       pmt-sel-stc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa di tutti i sottoconti o solamente di quelli
      -              "  :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       per i quali sono stati registrati movimenti
      -              "   "                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                          di rettifica al bilancio
      -              "   "                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sel-stc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/No lista movimenti per ogni sottoconto        *
      *    *-----------------------------------------------------------*
       pmt-snx-lmr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa anche della lista dei movimenti di rettific
      -              "a :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      53                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        relativa ad ogni sottocont
      -              "o  "                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-lmr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data bilancio di riferimento               *
      *    *-----------------------------------------------------------*
       acc-dat-bil-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-bil-025.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale valore di default    *
      *                  *---------------------------------------------*
           if        rr-dat-bil           =    zero
                     move  rr-dtb-def     to   rr-dat-bil             .
       acc-dat-bil-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-bil           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-bil-999.
       acc-dat-bil-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-bil             .
       acc-dat-bil-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-bil-425.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia a zero                *
      *                  *---------------------------------------------*
           if        rr-dat-bil           =    zero
                     go to acc-dat-bil-100.
       acc-dat-bil-450.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia superiore alla data   *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data attuale da segreteria              *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Controllo                               *
      *                      *-----------------------------------------*
           if        rr-dat-bil           >    s-dat
                     go to acc-dat-bil-100.
       acc-dat-bil-475.
      *                  *---------------------------------------------*
      *                  * Che il valore corrisponda ad una data di    *
      *                  * fine esercizio                              *
      *                  *---------------------------------------------*
           if        rr-mes-bil           not  = rr-mes-def or
                     rr-gio-bil           not  = rr-gio-def
                     go to acc-dat-bil-100.
       acc-dat-bil-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-bil-625.
      *                  *---------------------------------------------*
      *                  * Preparazione data movimenti di rettifica    *
      *                  * minima                                      *
      *                  *---------------------------------------------*
           move      rr-dat-bil           to   rr-dtm-min             .
           if        rr-mes-min           =    12
                     add   1              to   rr-saa-min
                     move  1              to   rr-mes-min
                     move  1              to   rr-gio-min
           else      add   1              to   rr-mes-min
                     move  1              to   rr-gio-min             .
       acc-dat-bil-650.
      *                  *---------------------------------------------*
      *                  * Preparazione data movimenti di rettifica    *
      *                  * massima                                     *
      *                  *---------------------------------------------*
           move      rr-dat-bil           to   rr-dtm-max             .
           add       1                    to   rr-saa-max             .
       acc-dat-bil-675.
      *                  *---------------------------------------------*
      *                  * Preparazione anno di esercizio della scheda *
      *                  * saldi contabili da leggere                  *
      *                  *---------------------------------------------*
           move      rr-saa-bil           to   rr-ann-ese             .
           if        rr-mes-bil           not  = 12
                     subtract 1           from rr-ann-ese             .
       acc-dat-bil-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-bil-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-bil-100.
       acc-dat-bil-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Selezione su sottoconti da stampare        *
      *    *-----------------------------------------------------------*
       acc-sel-stc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sel-stc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sel-stc-lun    to   v-car                  .
           move      w-exp-sel-stc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-exp-sel-stc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-stc           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-stc-999.
       acc-sel-stc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-stc             .
       acc-sel-stc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-stc-425.
      *                  *---------------------------------------------*
      *                  * Che il valore sia ammissibile               *
      *                  *---------------------------------------------*
           if        rr-sel-stc           =    zero           or
                     rr-sel-stc           >    w-exp-sel-stc-num
                     go to acc-sel-stc-100.
       acc-sel-stc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-stc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-stc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-stc-100.
       acc-sel-stc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No lista movimenti per ogni sottoconto  *
      *    *-----------------------------------------------------------*
       acc-snx-lmr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-lmr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-lmr-lun    to   v-car                  .
           move      w-exp-snx-lmr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-exp-snx-lmr-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-snx-lmr           =    "S"
                     move  01             to   v-num
           else if   rr-snx-lmr           =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-lmr-999.
       acc-snx-lmr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-snx-lmr
           else if   v-num                =    02
                     move  "N"            to   rr-snx-lmr
           else      move  spaces         to   rr-snx-lmr             .
       acc-snx-lmr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-lmr-425.
      *                  *---------------------------------------------*
      *                  * Che il valore sia ammissibile               *
      *                  *---------------------------------------------*
           if        rr-snx-lmr           not  = "S" and
                     rr-snx-lmr           not  = "N"
                     go to acc-snx-lmr-100.
       acc-snx-lmr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-lmr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-lmr-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-lmr-100.
       acc-snx-lmr-999.
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
      *              * Test su data di bilancio                        *
      *              *-------------------------------------------------*
           if        rr-dat-bil           not  = zero
                     go to tdo-ric-sel-200.
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           move      "Manca la data del bilancio di riferimento !       
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Selezione su sottoconti da stampare             *
      *              *-------------------------------------------------*
           if        rr-sel-stc           not  = 01 and
                     rr-sel-stc           not  = 02
                     move  01             to   rr-sel-stc             .
      *              *-------------------------------------------------*
      *              * Si/No lista movimenti per ogni sottoconto       *
      *              *-------------------------------------------------*
           if        rr-snx-lmr           not  = "S" and
                     rr-snx-lmr           not  = "N"
                     move  "S"            to   rr-snx-lmr             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data bilancio di riferimento                    *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-bil             .
      *              *-------------------------------------------------*
      *              * Selezione su sottoconti da stampare             *
      *              *-------------------------------------------------*
           move      zero                 to   rr-sel-stc             .
      *              *-------------------------------------------------*
      *              * Si/No lista movimenti per ogni sottoconto       *
      *              *-------------------------------------------------*
           move      spaces               to   rr-snx-lmr             .
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
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : No                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-exe-rou-srt      .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione records archivio [pdc]          *
      *              *-------------------------------------------------*
           perform   buf-rec-pdc-000      thru buf-rec-pdc-999        .
      *              *-------------------------------------------------*
      *              * Se nessun record bufferizzato : uscita come per *
      *              * Start errata                                    *
      *              *-------------------------------------------------*
           if        w-buf-pdc-num-ele    =    zero
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-str-ini-999.
       prn-str-ini-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per scansione buffer    *
      *              * elementi bufferizzati                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-pdc-inx-ele      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione records archivio [pdc]                    *
      *    *-----------------------------------------------------------*
       buf-rec-pdc-000.
      *              *-------------------------------------------------*
      *              * Numero elementi bufferizzati : a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-pdc-num-ele      .
       buf-rec-pdc-100.
      *              *-------------------------------------------------*
      *              * Start su [pdc] per codice sottoconto            *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODSTC    "         to   f-key                  .
           move      zero                 to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a fine bufferizzazione        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rec-pdc-800.
       buf-rec-pdc-200.
      *              *-------------------------------------------------*
      *              * Read Next da [pdc] per codice sottoconto        *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Se fine file : a fine bufferizzazione           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rec-pdc-800.
       buf-rec-pdc-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione sottoconto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi bufferizzati     *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-pdc-num-ele      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice                      *
      *                  *---------------------------------------------*
           move      rf-pdc-cod-pdc       to   w-buf-pdc-cod-pdc
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione descrizione                 *
      *                  *---------------------------------------------*
           move      rf-pdc-des-pdc       to   w-buf-pdc-des-pdc
                                              (w-buf-pdc-num-ele)     .
       buf-rec-pdc-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione saldi da scheda contabile       *
      *              *-------------------------------------------------*
       buf-rec-pdc-410.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Read                                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD"             to   f-key                  .
           move      rr-ann-ese           to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      rf-pdc-cod-pdc       to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato si continua comunque  *
      *                  * con il record normalizzato                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rec-pdc-420.
       buf-rec-pdc-420.
      *                  *---------------------------------------------*
      *                  * Saldo inizio anno                           *
      *                  *---------------------------------------------*
           move      rf-mgs-sdo-ini       to   w-buf-pdc-sdo-ini
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Progressivo anno dare                       *
      *                  *---------------------------------------------*
           move      rf-mgs-dar-mes (01)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (02)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (03)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (04)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (05)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (06)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (07)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (08)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (09)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (10)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (11)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-dar-mes (12)  to   w-buf-pdc-dar-ann
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Progressivo anno avere                      *
      *                  *---------------------------------------------*
           move      rf-mgs-ave-mes (01)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (02)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (03)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (04)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (05)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (06)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (07)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (08)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (09)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (10)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (11)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
           add       rf-mgs-ave-mes (12)  to   w-buf-pdc-ave-ann
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Saldo fine anno                             *
      *                  *---------------------------------------------*
           move      w-buf-pdc-sdo-ini
                    (w-buf-pdc-num-ele)   to   w-buf-pdc-sdo-fin
                                              (w-buf-pdc-num-ele)     .
           add       w-buf-pdc-dar-ann
                    (w-buf-pdc-num-ele)   to   w-buf-pdc-sdo-fin
                                              (w-buf-pdc-num-ele)     .
           subtract  w-buf-pdc-ave-ann
                    (w-buf-pdc-num-ele)   from w-buf-pdc-sdo-fin
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Totale rettifiche dare                      *
      *                  *---------------------------------------------*
           move      rf-mgs-dar-bil       to   w-buf-pdc-dar-ret
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Totale rettifiche avere                     *
      *                  *---------------------------------------------*
           move      rf-mgs-ave-bil       to   w-buf-pdc-ave-ret
                                              (w-buf-pdc-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Saldo di bilancio                           *
      *                  *---------------------------------------------*
           move      w-buf-pdc-sdo-fin
                    (w-buf-pdc-num-ele)   to   w-buf-pdc-sdo-bil
                                              (w-buf-pdc-num-ele)     .
           add       w-buf-pdc-dar-ret
                    (w-buf-pdc-num-ele)   to   w-buf-pdc-sdo-bil
                                              (w-buf-pdc-num-ele)     .
           subtract  w-buf-pdc-ave-ret
                    (w-buf-pdc-num-ele)   from w-buf-pdc-sdo-bil
                                              (w-buf-pdc-num-ele)     .
       buf-rec-pdc-700.
      *              *-------------------------------------------------*
      *              * Se raggiunto il massimo numero di elementi buf- *
      *              * ferizzabili : fine bufferizzazione              *
      *              *-------------------------------------------------*
           if        w-buf-pdc-num-ele    =    w-buf-pdc-max-ele
                     go to buf-rec-pdc-800.
      *              *-------------------------------------------------*
      *              * Altrimenti riciclo su record successivo         *
      *              *-------------------------------------------------*
           go to     buf-rec-pdc-200.
       buf-rec-pdc-800.
      *              *-------------------------------------------------*
      *              * Fine bufferizzazione                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     buf-rec-pdc-999.
       buf-rec-pdc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessuna registrazione entro i limiti assegnati !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Incremento indice su buffer                     *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pdc-inx-ele      .
      *              *-------------------------------------------------*
      *              * Se oltre numero elementi bufferizzati : fine    *
      *              *-------------------------------------------------*
           if        w-buf-pdc-inx-ele    >    w-buf-pdc-num-ele
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Selezione su sottoconto                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se richiesta la stampa per tutti i sotto-   *
      *                  * conti : selezione comunque superata         *
      *                  *---------------------------------------------*
           if        rr-sel-stc           not  = 02
                     go to prn-sel-rec-999.
      *                  *---------------------------------------------*
      *                  * Se richiesta la stampa per i soli sottocon- *
      *                  * ti con rettifiche : si esegue il test di    *
      *                  * selezione                                   *
      *                  *---------------------------------------------*
           if        w-buf-pdc-dar-ret
                    (w-buf-pdc-inx-ele)   =    zero and
                     w-buf-pdc-ave-ret
                    (w-buf-pdc-inx-ele)   =    zero
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Inizializzazione totali generali                *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-gen-sdo-ini      .
           move      zero                 to   w-tot-gen-dar-ann      .
           move      zero                 to   w-tot-gen-ave-ann      .
           move      zero                 to   w-tot-gen-sdo-fin      .
           move      zero                 to   w-tot-gen-dar-ret      .
           move      zero                 to   w-tot-gen-ave-ret      .
           move      zero                 to   w-tot-gen-sdo-bil      .
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-cic-999.
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Test se righe residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    4
                     go to prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Interlinea doppia                               *
      *              *-------------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sopralineatura                                  *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "--  ------------------  ------------------  ------
      -              "------------  ------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-400.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-420.
      *              *-------------------------------------------------*
      *              * Prima linea di totali                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      013                  to   p-pos                  .
           move      "Totali generali                         "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo inizio anno                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-tot-gen-sdo-ini    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale anno dare                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-tot-gen-dar-ann    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale anno avere                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      095                  to   p-pos                  .
           move      w-tot-gen-ave-ann    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo fine anno                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      115                  to   p-pos                  .
           move      w-tot-gen-sdo-fin    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-500.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-520.
      *              *-------------------------------------------------*
      *              * Seconda linea di totali                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale rettifiche dare                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-tot-gen-dar-ret    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale rettifiche avere                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      095                  to   p-pos                  .
           move      w-tot-gen-ave-ret    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo di bilancio                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      115                  to   p-pos                  .
           move      w-tot-gen-sdo-bil    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-liv-det-050.
      *              *-------------------------------------------------*
      *              * Test se righe residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    4
                     go to prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Interlinea doppia                               *
      *              *-------------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-120.
      *              *-------------------------------------------------*
      *              * Prima linea                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing con appoggio a destra           *
      *                      *-----------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-buf-pdc-cod-pdc
                    (w-buf-pdc-inx-ele)   to   w-edt-cod-pdc-cod      .
           move      spaces               to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-adx-000      thru edt-pdc-adx-999        .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-edt-cod-pdc-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Descrizione sottoconto                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      013                  to   p-pos                  .
           move      w-buf-pdc-des-pdc
                    (w-buf-pdc-inx-ele)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo inizio anno                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-buf-pdc-sdo-ini
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale anno dare                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-buf-pdc-dar-ann
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale anno avere                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      095                  to   p-pos                  .
           move      w-buf-pdc-ave-ann
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo fine anno                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      115                  to   p-pos                  .
           move      w-buf-pdc-sdo-fin
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-220.
      *              *-------------------------------------------------*
      *              * Seconda linea                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale rettifiche dare                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-buf-pdc-dar-ret
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale rettifiche avere                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      095                  to   p-pos                  .
           move      w-buf-pdc-ave-ret
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Saldo di bilancio                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      115                  to   p-pos                  .
           move      w-buf-pdc-sdo-bil
                    (w-buf-pdc-inx-ele)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento totali generali                   *
      *              *-------------------------------------------------*
           add       w-buf-pdc-sdo-ini
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-sdo-ini      .
           add       w-buf-pdc-dar-ann
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-dar-ann      .
           add       w-buf-pdc-ave-ann
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-ave-ann      .
           add       w-buf-pdc-sdo-fin
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-sdo-fin      .
           add       w-buf-pdc-dar-ret
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-dar-ret      .
           add       w-buf-pdc-ave-ret
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-ave-ret      .
           add       w-buf-pdc-sdo-bil
                    (w-buf-pdc-inx-ele)   to   w-tot-gen-sdo-bil      .
       prn-liv-det-400.
      *              *-------------------------------------------------*
      *              * Se non e' stata richiesta la lista dei movimen- *
      *              * ti : ad uscita                                  *
      *              *-------------------------------------------------*
           if        rr-snx-lmr           not  = "S"
                     go to prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Se i progressivi contabili delle rettifiche in- *
      *              * dicano che non ci sono movimenti di rettifica   *
      *              * relativi al sottoconto : ad uscita              *
      *              *-------------------------------------------------*
           if        w-buf-pdc-dar-ret
                    (w-buf-pdc-inx-ele)   =    zero and
                     w-buf-pdc-ave-ret
                    (w-buf-pdc-inx-ele)   =    zero
                     go to prn-liv-det-900.
       prn-liv-det-500.
      *              *-------------------------------------------------*
      *              * Stampa lista movimenti                          *
      *              *-------------------------------------------------*
       prn-liv-det-505.
      *                  *---------------------------------------------*
      *                  * Inizializzazione totali relativi ai movi-   *
      *                  * menti stampati                              *
      *                  *---------------------------------------------*
           move      zero                 to   w-tot-stc-num-mvs      .
           move      zero                 to   w-tot-stc-dar-ret      .
           move      zero                 to   w-tot-stc-ave-ret      .
       prn-liv-det-510.
      *                  *---------------------------------------------*
      *                  * Start su [mgr] per sottoconto               *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PDCDAT    "         to   f-key                  .
           move      w-buf-pdc-cod-pdc
                    (w-buf-pdc-inx-ele)   to   rf-mgr-cod-pdc         .
           move      rr-dtm-min           to   rf-mgr-dat-reg         .
           move      zero                 to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Se errore su Start : a fine ciclo di stampa *
      *                  * movimenti di rettifica                      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-800.
       prn-liv-det-515.
      *                  *---------------------------------------------*
      *                  * Read Next da [mgr] per sottoconto           *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Se fine File : a fine ciclo di stampa dei   *
      *                  * movimenti di rettifica                      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-800.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-mgr-cod-pdc       not  = w-buf-pdc-cod-pdc
                                                (w-buf-pdc-inx-ele)
                     go to prn-liv-det-800.
           if        rf-mgr-dat-reg       >     rr-dtm-max
                     go to prn-liv-det-800.
       prn-liv-det-520.
      *                  *---------------------------------------------*
      *                  * Selezione sul tipo movimento, che sia di    *
      *                  * rettifica al bilancio                       *
      *                  *---------------------------------------------*
           if        rf-mgr-snx-mob       not  = "S"
                     go to prn-liv-det-515.
       prn-liv-det-525.
      *                  *---------------------------------------------*
      *                  * Incremento numero movimenti stampati        *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-stc-num-mvs      .
      *                  *---------------------------------------------*
      *                  * Incremento totale dare, o avere, dei movi-  *
      *                  * menti stampati                              *
      *                  *---------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     add   rf-mgr-imp-mov to   w-tot-stc-ave-ret
           else      add   rf-mgr-imp-mov to   w-tot-stc-dar-ret      .
       prn-liv-det-530.
      *                  *---------------------------------------------*
      *                  * Lettura testata registrazione corrispon-    *
      *                  * dente alla riga letta, con normalizzazione  *
      *                  * preliminare                                 *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      rf-mgr-dat-reg       to   rf-mgt-dat-reg         .
           move      rf-mgr-num-prt       to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato si normalizzano i va- *
      *                  * lori che in seguito saranno utilizzati      *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-liv-det-600.
           move      spaces               to   rf-mgt-des-cau         .
           move      all   "?"            to   rf-mgt-rig-cau (1)     .
       prn-liv-det-600.
      *                  *---------------------------------------------*
      *                  * Stampa movimento, linea 1                   *
      *                  *---------------------------------------------*
       prn-liv-det-605.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti, con  *
      *                      * eventuale reintestazione, e relativo    *
      *                      * test di interruzione forzata            *
      *                      *-----------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-610.
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-610.
      *                      *-----------------------------------------*
      *                      * Interlinea doppia                       *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data registrazione                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      13                   to   p-pos                  .
           move      rf-mgr-dat-reg       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 1. linea di descrizione, da testata     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-mgt-rig-cau (1)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Importo della rettifica                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           if        rf-mgr-dar-ave       =    "A"
                     move 095             to   p-pos
           else      move 075             to   p-pos                  .
           move      rf-mgr-imp-mov       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-625.
      *                  *---------------------------------------------*
      *                  * Stampa movimento, linea 2                   *
      *                  *---------------------------------------------*
       prn-liv-det-627.
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        rf-mgt-rig-cau (2)   =    spaces
                     go to prn-liv-det-650.
       prn-liv-det-630.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti, con  *
      *                      * eventuale reintestazione, e relativo    *
      *                      * test di interruzione forzata            *
      *                      *-----------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-635.
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-635.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 2. linea di descrizione, da testata     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-mgt-rig-cau (2)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-650.
      *                  *---------------------------------------------*
      *                  * Stampa movimento, linea 3                   *
      *                  *---------------------------------------------*
       prn-liv-det-652.
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        rf-mgt-rig-cau (3)   =    spaces
                     go to prn-liv-det-675.
       prn-liv-det-655.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti, con  *
      *                      * eventuale reintestazione, e relativo    *
      *                      * test di interruzione forzata            *
      *                      *-----------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-660.
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-660.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 3. linea di descrizione, da testata     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-mgt-rig-cau (3)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-675.
      *                  *---------------------------------------------*
      *                  * Stampa movimento, linea 4                   *
      *                  *---------------------------------------------*
       prn-liv-det-677.
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        rf-mgr-com-rig       =    spaces
                     go to prn-liv-det-700.
       prn-liv-det-680.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti, con  *
      *                      * eventuale reintestazione, e relativo    *
      *                      * test di interruzione forzata            *
      *                      *-----------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-685.
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-685.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Commento in riga                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-mgr-com-rig       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-700.
      *                  *---------------------------------------------*
      *                  * Stampa movimento, linea 5                   *
      *                  *---------------------------------------------*
       prn-liv-det-702.
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        rf-mgr-tip-arc       not  = "C" and
                     rf-mgr-tip-arc       not  = "F"
                     go to prn-liv-det-750.
       prn-liv-det-705.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti, con  *
      *                      * eventuale reintestazione, e relativo    *
      *                      * test di interruzione forzata            *
      *                      *-----------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-710.
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-710.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Deviazione per stampare la ragione so-  *
      *                      * ciale del cliente o del fornitore, a    *
      *                      * seconda del tipo archivio del movimento *
      *                      *-----------------------------------------*
           if        rf-mgr-tip-arc       =    "C"
                     go to prn-liv-det-720
           else      go to prn-liv-det-730.
       prn-liv-det-720.
      *                      *-----------------------------------------*
      *                      * Cliente                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-mgr-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-cli-rag-soc         .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-cli-rag-soc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     prn-liv-det-750.
       prn-liv-det-730.
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-mgr-cod-arc       to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-fnt-rag-soc         .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      rf-fnt-rag-soc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           go to     prn-liv-det-750.
       prn-liv-det-750.
      *                  *---------------------------------------------*
      *                  * Riciclo su riga movimenti successiva        *
      *                  *---------------------------------------------*
           go to     prn-liv-det-515.
       prn-liv-det-800.
      *                  *---------------------------------------------*
      *                  * Fine stampa lista movimenti                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     prn-liv-det-900.
       prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-dat-bil           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      spaces               to   w-cnt-tit-des-tit      .
           string    "MOVIMENTI DI RETTIFICA AL BILANCIO DEL "
                                delimited by   size
                     p-edt
                                delimited by   spaces
                                          into w-cnt-tit-des-tit      .
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
           move      p-pag                to   w-cnt-tit-num-pag      .
           add       1                    to   w-cnt-tit-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *              *-------------------------------------------------*
      *              * Se uscita per interruzione forzata              *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "           Codice e descrizione del conto         
      -              "     Saldo inizio anno    Totale dare anno   Total
      -              "e avere anno     Saldo fine anno"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "                                                  
      -              "                           Rettifiche dare    Rett
      -              "ifiche avere   Saldo di bilancio"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "          Lista dei movimenti di rettifica                                                                                          
      -              "                                                  
      -              "                                "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "--  ------------------  ------------------  ------
      -              "------------  ------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

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

