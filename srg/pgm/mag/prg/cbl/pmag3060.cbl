       Identification Division.
       Program-Id.                                 pmag3060           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    mov                 *
      *                                   Fase:    mag306              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/08/92    *
      *                       Ultima revisione:    NdK del 12/02/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pmag3061:        *
      *                                                                *
      *                    Stampa brogliaccio movimenti di magazzino   *
      *                    mancanti del costo unitario, valorizzati    *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    File di esportazione:                       *
      *                                                                *
      *                    /abd/asc/UTENTE/exp/AZI_mag306.csv          *
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
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag306"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag3060"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "STAMPA CON VALORE MOVIMENTI SENZA COSTO "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pmag3061  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/mag/prg/obj/pmag3061                "       .

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
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Denominazione dipendenza in uso                       *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu-den             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Numero totale dipendenze                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-dpz             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo stampa                                           *
      *        *                                                       *
      *        * - 01 : Per data registrazione                         *
      *        * - 02 : Per data immissione/ultima modifica            *
      *        *-------------------------------------------------------*
           05  rr-tip-stp                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data iniziale                                         *
      *        *-------------------------------------------------------*
           05  rr-dat-ini                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data finale                                           *
      *        *-------------------------------------------------------*
           05  rr-dat-fin                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Causale di magazzino da selezionare                   *
      *        *-------------------------------------------------------*
           05  rr-cau-mag                 pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per la causale di magazzino               *
      *        *-------------------------------------------------------*
           05  rr-cau-mag-des             pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di trattamento per il valore per la causale di   *
      *        * magazzino                                             *
      *        *                                                       *
      *        * - N : Nessun trattamento del valore                   *
      *        * - I : Trattamento del valore per impostazione         *
      *        * - S : Trattamento del valore, ma senza impostazione,  *
      *        *       in quanto valorizzazione al costo standard      *
      *        * - C : Trattamento del solo valore, senza quantita',   *
      *        *       costo aggiuntivo                                *
      *        * - X : Trattamento del solo valore, senza quantita',   *
      *        *       come costo aggiuntivo riferito alla fine e-     *
      *        *       sercizio precedente                             *
      *        * - R : Trattamento del solo valore, senza quantita',   *
      *        *       come rettifica ad un costo                      *
      *        * - Y : Trattamento del solo valore, senza quantita',   *
      *        *       come rettifica ad un costo riferito alla fi-    *
      *        *       ne esercizio precedente                         *
      *        *-------------------------------------------------------*
           05  rr-cau-mag-trv             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice archivio                                       *
      *        *-------------------------------------------------------*
           05  rr-cod-arc                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice utente da selezionare                          *
      *        *-------------------------------------------------------*
           05  rr-cod-ute                 pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Codice fase da selezionare                            *
      *        *-------------------------------------------------------*
           05  rr-cod-fas                 pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Tipo valorizzazione                                   *
      *        *                                                       *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        *-------------------------------------------------------*
           05  rr-tip-val                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo trattamento costo                                *
      *        *                                                       *
      *        * - 01 : Si mantiene quello memorizzato                 *
      *        * - 02 : Si forza quello richiesto                      *
      *        *-------------------------------------------------------*
           05  rr-tip-tcs                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data valorizzazione di magazzino                      *
      *        *-------------------------------------------------------*
           05  rr-dat-val                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo magazzino                                        *
      *        *-------------------------------------------------------*
           05  rr-tip-mag                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice filtro di selezione per il tipo di magazzino   *
      *        *-------------------------------------------------------*
           05  rr-fso-mag                 pic  9(08)                  .
           05  rr-fso-mag-alf redefines
               rr-fso-mag                 pic  x(08)                  .
           05  rr-fso-mag-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Si/No esportazione archivio                           *
      *        *                                                       *
      *        * - 01 : No                                             *
      *        * - 02 : Si                                             *
      *        * - 03 : Si, ma senza stampa                            *
      *        *-------------------------------------------------------*
           05  rr-snx-exp                 pic  9(01)                  .

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
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-tip-stp              pic  9(02)                  .
           05  w-sav-tip-mag              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmc.
               10  w-let-arc-zmc-flg      pic  x(01)                  .
               10  w-let-arc-zmc-cod      pic  9(05)                  .
               10  w-let-arc-zmc-des      pic  x(30)                  .
               10  w-let-arc-zmc-trv      pic  x(01)                  .

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
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 2    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(40) value
                            "Per data registrazione                  ".
                   15  filler             pic  x(40) value
                            "Per data immissione/ultima modifica     ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-val.
               10  w-exp-tip-val-num      pic  9(02)       value 3    .
               10  w-exp-tip-val-lun      pic  9(02)       value 40   .
               10  w-exp-tip-val-tbl.
                   15  filler             pic  x(40) value
                            "costo Medio ponderato annuale           ".
                   15  filler             pic  x(40) value
                            "Ultimo costo d'acquisto                 ".
                   15  filler             pic  x(40) value
                            "costo Standard                          ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo trattamento costo                     *
      *        *-------------------------------------------------------*
           05  w-exp-tip-tcs.
               10  w-exp-tip-tcs-num      pic  9(02)       value 2    .
               10  w-exp-tip-tcs-lun      pic  9(02)       value 40   .
               10  w-exp-tip-tcs-tbl.
                   15  filler             pic  x(40) value
                            "si Mantiene il valore memorizzato       ".
                   15  filler             pic  x(40) value
                            "si Forza il costo sopra definito        ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prodotto                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 05   .
               10  w-exp-tip-mag-lun      pic  9(02)       value 25   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
                   15  filler             pic  x(25) value
                            "Prodotti di vendita      "               .
                   15  filler             pic  x(25) value
                            "Semilavorati             "               .
                   15  filler             pic  x(25) value
                            "Materie prime            "               .
                   15  filler             pic  x(25) value
                            "Materiali vari           "               .
               10  w-exp-tip-mag-tbr redefines
                   w-exp-tip-mag-tbl.
                   15  w-exp-tip-mag-ele occurs 05
                                          pic  x(25)                  .
               10  w-exp-tip-mag-ast.
                   15  filler             pic  x(10) value
                            "9901020304"                              .
               10  w-exp-tip-mag-ass redefines
                   w-exp-tip-mag-ast.
                   15  w-exp-tip-mag-tpm occurs 05
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo magazzino limitato ai soli tipi am-   *
      *        *            messi                                      *
      *        *-------------------------------------------------------*
           05  w-exp-tpm-amm.
               10  w-exp-tpm-amm-num      pic  9(02)                  .
               10  w-exp-tpm-amm-lun      pic  9(02) value 25         .
               10  w-exp-tpm-amm-tbl.
                   15  w-exp-tpm-amm-ele occurs 05
                                          pic  x(25)                  .
               10  w-exp-tpm-amm-ass.
                   15  w-exp-tpm-amm-tpm occurs 05
                                          pic  9(02)                  .
               10  w-exp-tpm-amm-i01      pic  9(02)                  .
               10  w-exp-tpm-amm-c01      pic  9(02)                  .
               10  w-exp-tpm-amm-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no esportazione archivio                *
      *        *-------------------------------------------------------*
           05  w-exp-snx-exp.
               10  w-exp-snx-exp-num      pic  9(02)       value 3    .
               10  w-exp-snx-exp-lun      pic  9(02)       value 20   .
               10  w-exp-snx-exp-tbl.
                   15  filler             pic  x(20) value
                            "No                  "                    .
                   15  filler             pic  x(20) value
                            "Si                  "                    .
                   15  filler             pic  x(20) value
                            "si, senza La stampa "                    .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acl"                   .

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
      *              * Altrimenti memorizzazione in campo richieste    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
           move      w-dpz-den-prg        to   rr-dpz-inu-den         .
           move      w-dpz-ctr-dpz        to   rr-dpz-ctr-dpz         .
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
      *              * Determinazione tipi magazzino ammessi           *
      *              *-------------------------------------------------*
           perform   det-tpm-amm-000      thru det-tpm-amm-999        .
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
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione causale di magazzino   *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-opn-000  thru cod-mne-zmc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-opn-000  thru cod-zos-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dpm-opn-000  thru cod-zos-dpm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-opn-000  thru cod-zos-mtv-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione causale di magazzino  *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-cls-000  thru cod-mne-zmc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-cls-000  thru cod-zos-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dpm-cls-000  thru cod-zos-dpm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-cls-000  thru cod-zos-mtv-cls-999    .
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
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Tipo stampa                                 *
      *                  *---------------------------------------------*
           perform   acc-tip-stp-000      thru acc-tip-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data finale                                 *
      *                  *---------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Causale di magazzino                        *
      *                  *---------------------------------------------*
           perform   acc-cau-mag-000      thru acc-cau-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-ute-000      thru acc-cod-ute-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           perform   acc-cod-fas-000      thru acc-cod-fas-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione                         *
      *                  *---------------------------------------------*
           perform   acc-tip-val-000      thru acc-tip-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-750.
      *                  *---------------------------------------------*
      *                  * Tipo trattamento costo                      *
      *                  *---------------------------------------------*
           perform   acc-tip-tcs-000      thru acc-tip-tcs-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-700.
       acc-ric-sel-800.
      *                  *---------------------------------------------*
      *                  * Data valorizzazione                         *
      *                  *---------------------------------------------*
           perform   acc-dat-val-000      thru acc-dat-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-750.
       acc-ric-sel-825.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino                              *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-800.
       acc-ric-sel-850.
      *                  *---------------------------------------------*
      *                  * Codice filtro di selezione                  *
      *                  *---------------------------------------------*
           perform   acc-fso-mag-000      thru acc-fso-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-825.
       acc-ric-sel-875.
      *                  *---------------------------------------------*
      *                  * Si/no esportazione archivio                 *
      *                  *---------------------------------------------*
           perform   acc-snx-exp-000      thru acc-snx-exp-999        .
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
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Prompt selezioni                                *
      *              *-------------------------------------------------*
           perform   pmt-sel-stp-000      thru pmt-sel-stp-999        .
      *              *-------------------------------------------------*
      *              * Causale di magazzino                            *
      *              *-------------------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice utente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Codice fase                                     *
      *              *-------------------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo trattamento costo                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Movimenti gia' valorizzati :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Data valorizzazione                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Valorizzazione relativa al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo magazzino                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-mag-000      thru pmt-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice filtro di selezione                      *
      *              *-------------------------------------------------*
           perform   pmt-fso-mag-000      thru pmt-fso-mag-999        .
      *              *-------------------------------------------------*
      *              * Si/no esportazione archivio                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Esportazione archivio      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo stampa                      *
      *    *-----------------------------------------------------------*
       pmt-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo stampa                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data iniziale                    *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-stp           =    02
                     move  "Data immissione        dal :"
                                          to   v-alf
           else      move  "Data registrazione     dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data finale                      *
      *    *-----------------------------------------------------------*
       pmt-dat-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Prompt selezioni                 *
      *    *-----------------------------------------------------------*
       pmt-sel-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale di magazzino             *
      *    *-----------------------------------------------------------*
       pmt-cau-mag-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cau-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Causale di magazzino     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice archivio                  *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cod-arc-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice archivio          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice utente                    *
      *    *-----------------------------------------------------------*
       pmt-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 02
                     go to pmt-cod-ute-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Utente                   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fase                      *
      *    *-----------------------------------------------------------*
       pmt-cod-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-stp           =    02
                     move  "- Programma                :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo magazzino                   *
      *    *-----------------------------------------------------------*
       pmt-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nella tabella tipi magazzino ammessi e'  *
      *                  * stato rilevato un solo elemento il prompt   *
      *                  * non si visualizza                           *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    =    1
                     go to pmt-tip-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo prodotto              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice filtro di selezione       *
      *    *-----------------------------------------------------------*
       pmt-fso-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice filtro di selezione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
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
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-stp           to   v-num                  .
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
           move      v-num                to   rr-tip-stp             .
       acc-tip-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    zero
                     go to acc-tip-stp-100.
       acc-tip-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    w-sav-tip-stp
                     go to acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale 01 e precedente a zero :  *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    01   and
                     w-sav-tip-stp        =    zero
                     go to acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione prompt per data iniziale  *
      *                  *---------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *                  *---------------------------------------------*
      *                  * Se valore attuale : 01                      *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-tip-stp-700.
      *                      *-----------------------------------------*
      *                      * Prompt per Fase                         *
      *                      *-----------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Causale di magazzino         *
      *                      *-----------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *                      *-----------------------------------------*
      *                      * Codice Utente                           *
      *                      *-----------------------------------------*
           if        rr-cod-ute           not  = spaces
                     move  spaces         to   rr-cod-ute
                     perform vis-cod-ute-000
                                          thru vis-cod-ute-999        .
      *                      *-----------------------------------------*
      *                      * Codice Fase                             *
      *                      *-----------------------------------------*
           if        rr-cod-fas           not  = spaces
                     move  spaces         to   rr-cod-fas
                     perform vis-cod-fas-000
                                          thru vis-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-700.
      *                  *---------------------------------------------*
      *                  * Se valore attuale : 02                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per Utente                       *
      *                      *-----------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Fase                         *
      *                      *-----------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Causale di magazzino                    *
      *                      *-----------------------------------------*
           if        rr-cau-mag           not  = zero
                     move  zero           to   rr-cau-mag
                     move  spaces         to   rr-cau-mag-des
                     perform vis-cau-mag-000
                                          thru vis-cau-mag-999
                     perform vis-cau-mag-des-000
                                          thru vis-cau-mag-des-999    .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
       acc-dat-ini-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-ini-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-ini-100.
       acc-dat-ini-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
           move      07                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
       acc-dat-fin-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-fin-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-fin-100.
       acc-dat-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
      *    * Accettazione campo selezione : Causale di magazzino       *
      *    *-----------------------------------------------------------*
       acc-cau-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-cau-mag-999.
       acc-cau-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      rr-cau-mag           to   w-cod-mne-zmc-cod      .
           move      11                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      11                   to   w-cod-mne-zmc-dln      .
           move      36                   to   w-cod-mne-zmc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
       acc-cau-mag-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-mag-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-mag-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-mag-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-mag-110.
       acc-cau-mag-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cau-mag-999.
       acc-cau-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cau-mag             .
       acc-cau-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cau-mag-450.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      rr-cau-mag           to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   rr-cau-mag-des         .
           perform   vis-cau-mag-des-000  thru vis-cau-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento tipo trattamento valore         *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-trv    to   rr-cau-mag-trv         .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-mag-100.
       acc-cau-mag-500.
      *                  *---------------------------------------------*
      *                  * Se valore a zero                            *
      *                  *---------------------------------------------*
           if        rr-cau-mag           not  = zero
                     go to acc-cau-mag-550.
      *                      *-----------------------------------------*
      *                      * Visualizzazine literal 'Tutte'          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutte"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A dipendenze dall'impostazione          *
      *                      *-----------------------------------------*
           go to     acc-cau-mag-600.
       acc-cau-mag-550.
      *                  *---------------------------------------------*
      *                  * Se valore a non-zero                        *
      *                  *---------------------------------------------*
       acc-cau-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cau-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cau-mag-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cau-mag-100.
       acc-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Causale di magazzino    *
      *    *-----------------------------------------------------------*
       vis-cau-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cau-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descrizione causale di  *
      *    * magazzino                                                 *
      *    *-----------------------------------------------------------*
       vis-cau-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      rr-cau-mag-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-mag-des-999.
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
           if        rr-tip-stp           not  = 01
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
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
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
      *                  *---------------------------------------------*
      *                  * Se valore a zero                            *
      *                  *---------------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to acc-cod-arc-550.
      *                      *-----------------------------------------*
      *                      * Visualizzazine literal 'Tutti'          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti  "            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A dipendenze dall'impostazione          *
      *                      *-----------------------------------------*
           go to     acc-cod-arc-600.
       acc-cod-arc-550.
      *                  *---------------------------------------------*
      *                  * Se valore a non-zero                        *
      *                  *---------------------------------------------*
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
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
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
           if        rr-tip-stp           not  = 02
                     go to acc-cod-ute-999.
       acc-cod-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
      *                  *---------------------------------------------*
      *                  * Se valore a spaces                          *
      *                  *---------------------------------------------*
           if        rr-cod-ute           not  = spaces
                     go to acc-cod-ute-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'Tutti'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
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
           if        rr-tip-stp           not  = 02
                     go to acc-cod-fas-999.
       acc-cod-fas-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
      *                  *---------------------------------------------*
      *                  * Se valore a spaces                          *
      *                  *---------------------------------------------*
           if        rr-cod-fas           not  = spaces
                     go to acc-cod-fas-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'Tutti'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fas           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione                        *
      *    *-----------------------------------------------------------*
       acc-tip-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-val-lun    to   v-car                  .
           move      w-exp-tip-val-num    to   v-ldt                  .
           move      "CUS#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-val-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-val           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-val-999.
       acc-tip-val-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-val             .
       acc-tip-val-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-val           not  = zero
                     go to acc-tip-val-600.
           if        v-key                =    "UP  "
                     go to acc-tip-val-600
           else      go to acc-tip-val-999.
       acc-tip-val-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-val-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-val-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-val-100.
       acc-tip-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione                     *
      *    *-----------------------------------------------------------*
       vis-tip-val-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-val-lun    to   v-car                  .
           move      w-exp-tip-val-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-val-tbl    to   v-txt                  .
           move      rr-tip-val           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo trattamento costo                     *
      *    *-----------------------------------------------------------*
       acc-tip-tcs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-tcs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-tcs-lun    to   v-car                  .
           move      w-exp-tip-tcs-num    to   v-ldt                  .
           move      "MF#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-tcs-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-tcs           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-tcs-999.
       acc-tip-tcs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-tcs             .
       acc-tip-tcs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-tcs           not  = zero
                     go to acc-tip-tcs-600.
           if        v-key                =    "UP  "
                     go to acc-tip-tcs-600
           else      go to acc-tip-tcs-999.
       acc-tip-tcs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-tcs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-tcs-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-tcs-100.
       acc-tip-tcs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo trattamento costo                  *
      *    *-----------------------------------------------------------*
       vis-tip-tcs-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-tcs-lun    to   v-car                  .
           move      w-exp-tip-tcs-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-tcs-tbl    to   v-txt                  .
           move      rr-tip-tcs           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-tcs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data valorizzazione                        *
      *    *-----------------------------------------------------------*
       acc-dat-val-000.
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
           if        rr-dat-val           =    zero
                     move  s-dat          to   rr-dat-val             .
       acc-dat-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-val-150.
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
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, ameno che non si   *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-dat-val           not  = zero
                     go to acc-dat-val-600.
           if        v-key                =    "UP  "
                     go to acc-dat-val-600
           else      go to acc-dat-val-100.
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
      *    * Visualizzazione : Data valorizzazione                     *
      *    *-----------------------------------------------------------*
       vis-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo prodotto                 *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la tabella dei tipi magazzino ammessi e' *
      *                  * formata da un solo elemento : si forza il   *
      *                  * valore corrispondente e si esce             *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    not  = 1
                     go to acc-tip-mag-050.
           move      w-exp-tip-mag-tpm (1)
                                          to   rr-tip-mag             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-mag-999.
       acc-tip-mag-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-mag           to   w-sav-tip-mag          .
       acc-tip-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpm-amm-lun    to   v-car                  .
           move      w-exp-tpm-amm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tpm-amm-tbl    to   v-txt                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      zero                 to   w-exp-tpm-amm-c01      .
       acc-tip-mag-102.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tpm-amm-num
                     move  zero           to   v-num
                     go to acc-tip-mag-104.
           if        rr-tip-mag           =    w-exp-tpm-amm-tpm
                                              (w-exp-tpm-amm-c01)
                     move  w-exp-tpm-amm-c01
                                          to   v-num
                     go to acc-tip-mag-104.
           go to     acc-tip-mag-102.
       acc-tip-mag-104.
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-mag-999.
       acc-tip-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-tpm-amm-i01      .
           if        w-exp-tpm-amm-i01    =    zero
                     move  zero           to   rr-tip-mag
           else      move  w-exp-tpm-amm-tpm
                          (w-exp-tpm-amm-i01)
                                          to   rr-tip-mag             .
       acc-tip-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore sia accettabile          *
      *                  *---------------------------------------------*
           if        rr-tip-mag           =    zero
                     go to acc-tip-mag-100.
       acc-tip-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           if        rr-tip-mag           =    w-sav-tip-mag
                     go to acc-tip-mag-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice e descrizione filtro *
      *                  * di selezione                                *
      *                  *---------------------------------------------*
           move      zero                 to   rr-fso-mag             .
           move      spaces               to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice e descrizione filtro *
      *                  * di selezione                                *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-000      thru vis-fso-mag-999        .
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
       acc-tip-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-mag-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-mag-100.
       acc-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Tipo prodotto              *
      *    *-----------------------------------------------------------*
       vis-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mag-lun    to   v-car                  .
           move      w-exp-tip-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-mag-tbl    to   v-txt                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tip-mag           =    99
                     move  01             to   v-num
           else if   rr-tip-mag           =    01
                     move  02             to   v-num
           else if   rr-tip-mag           =    02
                     move  03             to   v-num
           else if   rr-tip-mag           =    03
                     move  04             to   v-num
           else if   rr-tip-mag           =    04
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice filtro di selezione    *
      *    *-----------------------------------------------------------*
       acc-fso-mag-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione del prompt in funzione del tipo *
      *              * magazzino                                       *
      *              *                                                 *
      *              * ATTUALMENTE INIBITO                             *
      *              *-------------------------------------------------*
           go to     acc-fso-mag-050.
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-mag           =    01
                     move  "       anagrafica prodotti  "
                                          to   v-alf
           else if   rr-tip-mag           =    02
                     move  "   anagrafica semilavorati  "
                                          to   v-alf
           else if   rr-tip-mag           =    03
                     move  "  anagrafica materie prime  "
                                          to   v-alf
           else if   rr-tip-mag           =    04
                     move  " anagrafica materiali vari  "
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-fso-mag-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di prodotto     *
      *              *-------------------------------------------------*
           if        rr-tip-mag           =    01
                     go to acc-fso-mag-100
           else if   rr-tip-mag           =    02
                     go to acc-fso-mag-200
           else if   rr-tip-mag           =    03
                     go to acc-fso-mag-300
           else if   rr-tip-mag           =    04
                     go to acc-fso-mag-400
           else      go to acc-fso-mag-999.
       acc-fso-mag-100.
      *                  *---------------------------------------------*
      *                  * Se Prodotto di vendita                      *
      *                  *---------------------------------------------*
           perform   acc-fso-dcp-000      thru acc-fso-dcp-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-200.
      *                  *---------------------------------------------*
      *                  * Se Semilavorato                             *
      *                  *---------------------------------------------*
           perform   acc-fso-dps-000      thru acc-fso-dps-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-300.
      *                  *---------------------------------------------*
      *                  * Se Materia prima                            *
      *                  *---------------------------------------------*
           perform   acc-fso-dpm-000      thru acc-fso-dpm-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-400.
      *                  *---------------------------------------------*
      *                  * Se Materiale vario                          *
      *                  *---------------------------------------------*
           perform   acc-fso-mtv-000      thru acc-fso-mtv-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice filtro di selezione [dcp]     *
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
           move      20                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      20                   to   w-cod-zos-dcp-dln      .
           move      41                   to   w-cod-zos-dcp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
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
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
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
      *    * Accettazione campo : Codice filtro di selezione [dps]     *
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
           move      20                   to   w-cod-zos-dps-lin      .
           move      30                   to   w-cod-zos-dps-pos      .
           move      20                   to   w-cod-zos-dps-dln      .
           move      41                   to   w-cod-zos-dps-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
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
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
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
      *    * Accettazione campo : Codice filtro di selezione [dpm]     *
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
           move      20                   to   w-cod-zos-dpm-lin      .
           move      30                   to   w-cod-zos-dpm-pos      .
           move      20                   to   w-cod-zos-dpm-dln      .
           move      41                   to   w-cod-zos-dpm-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
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
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
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
      *    * Accettazione campo : Codice filtro di selezione [mtv]     *
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
           move      20                   to   w-cod-zos-mtv-lin      .
           move      30                   to   w-cod-zos-mtv-pos      .
           move      20                   to   w-cod-zos-mtv-dln      .
           move      41                   to   w-cod-zos-mtv-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
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
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
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
      *    * Visualizzazione campo : Codice del filtro di selezione    *
      *    *-----------------------------------------------------------*
       vis-fso-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-fso-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione filtro selezione      *
      *    *-----------------------------------------------------------*
       vis-fso-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-fso-mag-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-mag-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No esportazione archivio                *
      *    *-----------------------------------------------------------*
       acc-snx-exp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-exp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-exp-lun    to   v-car                  .
           move      w-exp-snx-exp-num    to   v-ldt                  .
           move      "NSL#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-exp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-exp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-exp-999.
       acc-snx-exp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-exp             .
       acc-snx-exp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-snx-exp           not  = zero
                     go to acc-snx-exp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-exp-600
           else      go to acc-snx-exp-999.
       acc-snx-exp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-exp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-exp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-exp-100.
       acc-snx-exp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No esportazione archivio             *
      *    *-----------------------------------------------------------*
       vis-snx-exp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-exp-lun    to   v-car                  .
           move      w-exp-snx-exp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-exp-tbl    to   v-txt                  .
           move      rr-snx-exp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-exp-999.
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
      *              * Test su tipo stampa                             *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = zero
                     go to tdo-ric-sel-200.
           move      "Tipo stampa indefinito !                          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su data iniziale e finale             *
      *              *-------------------------------------------------*
           if        rr-dat-fin           =    zero
                     go to tdo-ric-sel-300.
           if        rr-dat-fin           not  < rr-dat-ini
                     go to tdo-ric-sel-300.
           move      "La data finale non puo' essere inferiore alla data
      -              " iniziale !    "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
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
      *              * Regolarizzazione data finale                    *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to reg-ric-sel-200.
           if        rr-dat-ini           =    zero
                     move 9999999         to   rr-dat-fin
           else      move rr-dat-ini      to   rr-dat-fin             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Tipo di valorizzazione                          *
      *              *-------------------------------------------------*
           if        rr-tip-val           =    zero
                     move  01             to   rr-tip-val             .
      *              *-------------------------------------------------*
      *              * Tipo trattamento costo                          *
      *              *-------------------------------------------------*
           if        rr-tip-tcs           =    zero
                     move  01             to   rr-tip-tcs             .
       reg-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione data di valorizzazione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           if        rr-dat-val           =    zero
                     move  s-dat          to   rr-dat-val             .
       reg-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No esportazione archivio     *
      *              *-------------------------------------------------*
           if        rr-snx-exp           =    zero
                     move  01             to   rr-snx-exp             .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           if        rr-snx-exp           =    03
                     move  "N"            to   w-cnt-fun-snx-stp      .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-tip-stp             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-cau-mag             .
           move      spaces               to   rr-cau-mag-des         .
           move      spaces               to   rr-cau-mag-trv         .
           move      zero                 to   rr-cod-arc             .
           move      spaces               to   rr-cod-ute             .
           move      spaces               to   rr-cod-fas             .
           move      zero                 to   rr-tip-val             .
           move      zero                 to   rr-tip-tcs             .
           move      zero                 to   rr-dat-val             .
           move      zero                 to   rr-tip-mag             .
           move      zero                 to   rr-fso-mag             .
           move      spaces               to   rr-fso-mag-des         .
           move      zero                 to   rr-snx-exp             .
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
      *    * Find su archivio [mmt]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-mmt-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pmag3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-mmt-999.
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
           move      "pgm/mag/prg/obj/pmag3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-mmt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zmc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zmc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zmc-cod    =    zero
                     go to let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zmc-cod    to   rf-zmc-cod-cau         .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmc-400.
       let-arc-zmc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmc-des-cau       to   w-let-arc-zmc-des      .
           move      rf-zmc-trt-val       to   w-let-arc-zmc-trv      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmc-999.
       let-arc-zmc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmc-flg      .
           move      all   "."            to   w-let-arc-zmc-des      .
           move      spaces               to   w-let-arc-zmc-trv      .
           go to     let-arc-zmc-999.
       let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-des      .
           move      spaces               to   w-let-arc-zmc-trv      .
       let-arc-zmc-999.
           exit.

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
      *    * Subroutines per l'accettazione del codice causale di ge-  *
      *    * stione magazzino                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acs"                   .

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
      *    * Determinazione tipi magazzino ammessi                     *
      *    *-----------------------------------------------------------*
       det-tpm-amm-000.
      *              *-------------------------------------------------*
      *              * Preparazione area w-exp limitata ai soli tipi   *
      *              * magazzino previsti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se e' attiva solamente la gestione dei      *
      *                  * Prodotti di vendita si forza come unico     *
      *                  * elemento nella lista                        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S" and
                     w-prs-dpm-snx        not  = "S" and
                     w-prs-mtv-snx        not  = "S"
                     move  01             to   w-exp-tip-mag-tpm (1)
                     move  01             to   w-exp-tpm-amm-num
                     go to det-tpm-amm-999.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Tutti' :                    *
      *                  * - Se attiva solo la gestione Prodotti di    *
      *                  *   di vendita non si inserisce nella lista   *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S" or
                     w-prs-dpm-snx        =    "S" or
                     w-prs-mtv-snx        =    "S"
                     move  99             to   w-exp-tpm-amm-tpm (1)
           else      move  zero           to   w-exp-tpm-amm-tpm (1)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Prodotto di vendita' :      *
      *                  * - Sempre ammesso                            *
      *                  *---------------------------------------------*
           move      01                   to   w-exp-tpm-amm-tpm (2)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Semilavorato' :             *
      *                  * - Se gestione semilavorati attiva lo si     *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S"
                     move  02             to   w-exp-tpm-amm-tpm (3)
           else      move  zero           to   w-exp-tpm-amm-tpm (3)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materia Prima' :            *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        =    "S"
                     move  03             to   w-exp-tpm-amm-tpm (4)
           else      move  zero           to   w-exp-tpm-amm-tpm (4)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materiale Vario' :          *
      *                  * - Se gestione materiale vario attiva lo si  *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        =    "S"
                     move  04             to   w-exp-tpm-amm-tpm (5)
           else      move  zero           to   w-exp-tpm-amm-tpm (5)  .
       det-tpm-amm-010.
      *              *-------------------------------------------------*
      *              * Compattamento della lista dei tipi magazzino    *
      *              * ammessi                                         *
      *              *-------------------------------------------------*
           if        w-exp-tpm-amm-tpm (1)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (2)
                                          to   w-exp-tpm-amm-tpm (1)
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (2)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (3)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (4)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
      *              *-------------------------------------------------*
      *              * Preparazione del numero di elementi in tabella  *
      *              * tipi magazzino ammessi                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-num      .
       det-tpm-amm-020.
           add       1                    to   w-exp-tpm-amm-num      .
           if        w-exp-tpm-amm-num    >    5
                     move  5              to   w-exp-tpm-amm-num
                     go to det-tpm-amm-040.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-num)   not  = zero
                     go to det-tpm-amm-020.
           subtract  1                    from w-exp-tpm-amm-num      .
       det-tpm-amm-040.
      *              *-------------------------------------------------*
      *              * Preparazione delle descrizioni relative ai tipi *
      *              * magazzino ammessi                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-c01      .
       det-tpm-amm-100.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tip-mag-num
                     go to det-tpm-amm-999.
           move      zero                 to   w-exp-tpm-amm-c02      .
       det-tpm-amm-120.
           add       1                    to   w-exp-tpm-amm-c02      .
           if        w-exp-tpm-amm-c02    >    5
                     go to det-tpm-amm-100.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-c01)   not  = w-exp-tip-mag-tpm
                                                (w-exp-tpm-amm-c02)
                     go to det-tpm-amm-120.
           move      w-exp-tip-mag-ele
                    (w-exp-tpm-amm-c02)   to   w-exp-tpm-amm-ele
                                              (w-exp-tpm-amm-c01)     .
           go to     det-tpm-amm-100.
       det-tpm-amm-999.
           exit.

