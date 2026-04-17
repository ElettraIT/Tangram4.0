       Identification Division.
       Program-Id.                                 pbol3020           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    mov                 *
      *                                   Fase:    bol302              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/05/95    *
      *                       Ultima revisione:    NdK del 26/05/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pbol3021:        *
      *                                                                *
      *                    Stampa brogliaccio documenti accompagnatori *
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
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol302"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol3020"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " STAMPA BROGLIACCIO DOCUM. DI TRASPORTO "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pbol3021  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/bol/prg/obj/pbol3021                "       .

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
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .

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
      *        * - 03 : bolle da fatturare                             *
      *        * - 04 : ordinata per cliente                           *
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
      *        * Codice vettore                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-vet                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice vettore, ragione sociale                       *
      *        *-------------------------------------------------------*
           05  rr-cod-vet-rag             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento da ricercare                           *
      *        *-------------------------------------------------------*
           05  rr-cod-tmb                 pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento da ricercare, descrizione              *
      *        *-------------------------------------------------------*
           05  rr-cod-tmb-des             pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-cli                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente, ragione sociale                       *
      *        *-------------------------------------------------------*
           05  rr-cod-cli-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice utente da selezionare                          *
      *        *-------------------------------------------------------*
           05  rr-cod-ute                 pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Codice fase da selezionare                            *
      *        *-------------------------------------------------------*
           05  rr-cod-fas                 pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero lotto (ex variante) da personalizzazione       *
      *        *-------------------------------------------------------*
           05  rr-sgl-vrn.
               10  rr-sgl-vrn-lot         pic  x(07)                  .
               10  rr-sgl-vrn-dat         pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice filtro di selezione per il tipo di magazzino   *
      *        *-------------------------------------------------------*
           05  rr-fso-dcp                 pic  9(08)                  .
           05  rr-fso-dcp-alf redefines
               rr-fso-dcp                 pic  x(08)                  .
           05  rr-fso-dcp-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice forma di pagamento                             *
      *        *-------------------------------------------------------*
           05  rr-cod-fop                 pic  9(07)                  .
           05  rr-cod-fop-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo stampa totale                                    *
      *        *                                                       *
      *        * - 01 : Non stampa totale documento                    *
      *        * - 02 : Stampa totale imponibile                       *
      *        * - 03 : Stampa totale documento                        *
      *        * - 04 : Stampa totale merce                            *
      *        * - 05 : Stampa totali documento, peso e colli          *
      *        *-------------------------------------------------------*
           05  rr-tip-stt                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/no solo documenti in sospeso                       *
      *        *-------------------------------------------------------*
           05  rr-snx-ebf                 pic  x(01)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-tip-stp              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [vet]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-vet.
               10  w-let-arc-vet-flg      pic  x(01)                  .
               10  w-let-arc-vet-cod      pic  9(07)                  .
               10  w-let-arc-vet-rag      pic  x(40)                  .
               10  w-let-arc-vet-via      pic  x(40)                  .
               10  w-let-arc-vet-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-tle      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-abi      pic  9(05)                  .
               10  w-let-arc-dcc-cab      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbi.
               10  w-let-arc-zbi-flg      pic  x(01)                  .
               10  w-let-arc-zbi-cod      pic  x(05)                  .
               10  w-let-arc-zbi-dpz      pic  9(02)                  .
               10  w-let-arc-zbi-des      pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.ltw"                   .
              
      *    *===========================================================*
      *    * Work area per personalizzazioni                           *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no possibilita' di escludere provvisoriamente la   *
      *        * bolla dal ciclo di fatturazione differita automatica  *
      *        *-------------------------------------------------------*
           05  w-prs-snx-ebf              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no accettazione sigla variante in riga             *
      *        *-------------------------------------------------------*
           05  w-prs-sgl-vrn.
               10  w-prs-sgl-vrn-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 4    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(40) value
                            "per data Registrazione                  ".
                   15  filler             pic  x(40) value
                            "per data Immissione/ultima modifica     ".
                   15  filler             pic  x(40) value
                            "bolle da Fatturare                      ".
                   15  filler             pic  x(40) value
                            "ordinata per Cliente                    ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa totale                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stt.
               10  w-exp-tip-stt-num      pic  9(02)       value 5    .
               10  w-exp-tip-stt-lun      pic  9(02)       value 40   .
               10  w-exp-tip-stt-tbl.
                   15  filler             pic  x(40) value
                            "Non stampa totale documento              ".
                   15  filler             pic  x(40) value
                            "stampa totale Imponibile                ".
                   15  filler             pic  x(40) value
                            "stampa totale Documento                 ".
                   15  filler             pic  x(40) value
                            "stampa totale Merce                     ".
                   15  filler             pic  x(40) value
                            "stampa totale documento, Peso e colli   ".
      *        *-------------------------------------------------------*
      *        * Work per : Solo documenti in sospeso                  *
      *        *-------------------------------------------------------*
           05  w-exp-snx-ebf.
               10  w-exp-snx-ebf-num      pic  9(02)       value 2    .
               10  w-exp-snx-ebf-lun      pic  9(02)       value 02   .
               10  w-exp-snx-ebf-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .

      *    *===========================================================*
      *    * Link-area per accettazione codice vettore                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per bol- *
      *    * lettazione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dcp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzfp0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

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
           move      i-exe-pat            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           move      i-exe-pat            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
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
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no possibilita' di escludere provviso-   *
      *                  * riamente la bolla dal ciclo di fatturazione *
      *                  * differita automatica                        *
      *                  *---------------------------------------------*
           perform   prs-snx-ebf-000      thru prs-snx-ebf-999        .
      *                  *---------------------------------------------*
      *                  * Si/no accettazione della sigla variante in  *
      *                  * riga documento                              *
      *                  *---------------------------------------------*
           perform   prs-sgl-vrn-000      thru prs-sgl-vrn-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no possibilita' di esclu-  *
      *    *                             dere provvisoriamente la bol- *
      *    *                             la dal ciclo di fatturazione  *
      *    *                             differita automatica          *
      *    *-----------------------------------------------------------*
       prs-snx-ebf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bol/mov/bol300[snx-ebf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-ebf
           else      move  spaces         to   w-prs-snx-ebf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-ebf        not   = "S"
                     move  "N"            to   w-prs-snx-ebf          .
       prs-snx-ebf-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no accettazione sigla      *
      *    *                             variante in riga documento    *
      *    *-----------------------------------------------------------*
       prs-sgl-vrn-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bol/mov/bol300[sgl-vrn]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-sgl-vrn
           else      move  spaces         to   w-prs-sgl-vrn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "S" and
                     w-prs-sgl-vrn-snx    not  = "L"
                     move  "N"            to   w-prs-sgl-vrn-snx      .
       prs-sgl-vrn-999.
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
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
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
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice vettore         *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-opn-000  thru cod-mne-vet-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per la  *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-opn-000  thru cod-des-zbi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-zfp-opn-000  thru cod-mne-zfp-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
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
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice vettore        *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-cls-000  thru cod-mne-vet-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-cls-000  thru cod-des-zbi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-zfp-cls-000  thru cod-mne-zfp-cls-999    .
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
       acc-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Codice vettore                              *
      *                  *---------------------------------------------*
           perform   acc-cod-vet-000      thru acc-cod-vet-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipo movimento da ricercare                 *
      *                  *---------------------------------------------*
           perform   acc-cod-tmb-000      thru acc-cod-tmb-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-225.
       acc-ric-sel-275.
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-ute-000      thru acc-cod-ute-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-275.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           perform   acc-cod-fas-000      thru acc-cod-fas-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Accettazione numero lotto (ex variante)     *
      *                  *---------------------------------------------*
           perform   acc-lot-vrn-000      thru acc-lot-vrn-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Accettazione data lotto (sigla variante)    *
      *                  *---------------------------------------------*
           perform   acc-dtl-vrn-000      thru acc-dtl-vrn-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-475.
      *                  *---------------------------------------------*
      *                  * Codice filtro di selezione                  *
      *                  *---------------------------------------------*
           perform   acc-fso-dcp-000      thru acc-fso-dcp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice forma pagamento                      *
      *                  *---------------------------------------------*
           perform   acc-cod-fop-000      thru acc-cod-fop-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-475.
       acc-ric-sel-525.
      *                  *---------------------------------------------*
      *                  * Tipo stampa totale                          *
      *                  *---------------------------------------------*
           perform   acc-tip-stt-000      thru acc-tip-stt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Solo documenti in sospeso                   *
      *                  *---------------------------------------------*
           perform   acc-snx-ebf-000      thru acc-snx-ebf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-525.
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
      *              * Codice vettore                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-vet-000      thru pmt-cod-vet-999        .
      *              *-------------------------------------------------*
      *              * Tipo movimento                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-tmb-000      thru pmt-cod-tmb-999        .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice utente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Codice fase                                     *
      *              *-------------------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *              *-------------------------------------------------*
      *              * Numero lotto (sigla variante)                   *
      *              *-------------------------------------------------*
           perform   pmt-lot-vrn-000      thru pmt-lot-vrn-999        .
      *              *-------------------------------------------------*
      *              * Data lotto (sigla variante)                     *
      *              *-------------------------------------------------*
           perform   pmt-dtl-vrn-000      thru pmt-dtl-vrn-999        .
      *              *-------------------------------------------------*
      *              * Codice filtro di selezione                      *
      *              *-------------------------------------------------*
           perform   pmt-fso-dcp-000      thru pmt-fso-dcp-999        .
      *              *-------------------------------------------------*
      *              * Codice filtro di selezione                      *
      *              *-------------------------------------------------*
           perform   pmt-fso-dcp-000      thru pmt-fso-dcp-999        .
      *              *-------------------------------------------------*
      *              * Codice forma pagamento                          *
      *              *-------------------------------------------------*
           perform   pmt-cod-fop-000      thru pmt-cod-fop-999        .
      *              *-------------------------------------------------*
      *              * Tipo stampa totale                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-stt-000      thru pmt-tip-stt-999        .
      *              *-------------------------------------------------*
      *              * Si/no solo documenti in sospeso                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-ebf-000      thru pmt-snx-ebf-999        .
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
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice vettore                *
      *    *-----------------------------------------------------------*
       pmt-cod-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Codice vettore          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo movimento da ricercare                        *
      *    *-----------------------------------------------------------*
       pmt-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Tipo di movimento       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice cliente                   *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cod-cli-999.
       pmt-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Codice cliente          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cli-999.
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
       pmt-cod-ute-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Utente                  :"
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
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-stp           =    02
                     move  " - Programma               :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero lotto (sigla variante)    *
      *    *-----------------------------------------------------------*
       pmt-lot-vrn-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "S" and
                     w-prs-sgl-vrn-snx    not  = "L"
                     go to pmt-lot-vrn-999.
       pmt-lot-vrn-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Lotto                   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lot-vrn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data lotto (sigla variante)      *
      *    *-----------------------------------------------------------*
       pmt-dtl-vrn-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "S" and
                     w-prs-sgl-vrn-snx    not  = "L"
                     go to pmt-dtl-vrn-999.
       pmt-dtl-vrn-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "Data :"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dtl-vrn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice filtro di selezione       *
      *    *-----------------------------------------------------------*
       pmt-fso-dcp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Filtro selezione prod.  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fso-dcp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice forma pagamento cliente                   *
      *    *-----------------------------------------------------------*
       pmt-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Forma di pagamento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo stampa totale               *
      *    *-----------------------------------------------------------*
       pmt-tip-stt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa totale documento    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-stt-999.
           exit.

      *    *===========================================================*
      *    * Prompt solo documenti in sospeso                          *
      *    *-----------------------------------------------------------*
       pmt-snx-ebf-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-snx-ebf        not   = "S"
                     go to pmt-snx-ebf-999.
       pmt-snx-ebf-100.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Solo documenti in sospeso  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ebf-999.
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
           move      "RIFC#"              to   v-msk                  .
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
      *                  * Erase linee preliminare                     *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo stampa      *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    01
                     go to acc-tip-stp-620
           else if   rr-tip-stp           =    02
                     go to acc-tip-stp-640
           else if   rr-tip-stp           =    03
                     go to acc-tip-stp-620
           else      go to acc-tip-stp-620.
       acc-tip-stp-620.
      *                  *---------------------------------------------*
      *                  * Se stampa ordinata per data registrazione o *
      *                  * per bolle da fatturare o per cliente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per Cliente                      *
      *                      *-----------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Fase                         *
      *                      *-----------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-640.
      *                  *---------------------------------------------*
      *                  * Se stampa per utente                        *
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
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
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
      *    * Accettazione campo : Codice vettore                       *
      *    *-----------------------------------------------------------*
       acc-cod-vet-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-vet-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-vet-ope      .
           move      rr-cod-vet           to   w-cod-mne-vet-cod      .
           move      12                   to   w-cod-mne-vet-lin      .
           move      30                   to   w-cod-mne-vet-pos      .
           move      12                   to   w-cod-mne-vet-rln      .
           move      41                   to   w-cod-mne-vet-rps      .
           move      zero                 to   w-cod-mne-vet-vln      .
           move      zero                 to   w-cod-mne-vet-vps      .
           move      zero                 to   w-cod-mne-vet-lln      .
           move      zero                 to   w-cod-mne-vet-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-vet-cll-000  thru cod-mne-vet-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-vet-foi-000  thru cod-mne-vet-foi-999    .
       acc-cod-vet-110.
           perform   cod-mne-vet-cll-000  thru cod-mne-vet-cll-999    .
           if        w-cod-mne-vet-ope    =    "F+"
                     go to acc-cod-vet-115.
           if        w-cod-mne-vet-ope    =    "AC"
                     go to acc-cod-vet-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-vet-115.
           perform   cod-mne-vet-foi-000  thru cod-mne-vet-foi-999    .
           go to     acc-cod-vet-110.
       acc-cod-vet-120.
           move      w-cod-mne-vet-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-vet-999.
       acc-cod-vet-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-vet             .
       acc-cod-vet-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [vet]                      *
      *                  *---------------------------------------------*
           move      rr-cod-vet           to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   rr-cod-vet-rag         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati vettore                *
      *                  *---------------------------------------------*
           perform   vis-rag-vet-000      thru vis-rag-vet-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-vet-flg    not  = spaces
                     go to acc-cod-vet-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : visualizzazione literal  *
      *                  * 'Tutti' al posto del codice                 *
      *                  *---------------------------------------------*
           if        rr-cod-vet           not  = zero
                     go to acc-cod-vet-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-vet-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-vet-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-vet-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-vet-100.
       acc-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice vettore            *
      *    *-----------------------------------------------------------*
       vis-cod-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-vet           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ragione sociale vettore   *
      *    *-----------------------------------------------------------*
       vis-rag-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-vet-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-vet-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo movimento da ricercare                *
      *    *-----------------------------------------------------------*
       acc-cod-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zbi-ope      .
           move      rr-cod-tmb           to   w-cod-des-zbi-cod      .
           move      13                   to   w-cod-des-zbi-lin      .
           move      30                   to   w-cod-des-zbi-pos      .
           move      13                   to   w-cod-des-zbi-dln      .
           move      41                   to   w-cod-des-zbi-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
       acc-cod-tmb-110.
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           if        w-cod-des-zbi-ope    =    "F+"
                     go to acc-cod-tmb-115.
           if        w-cod-des-zbi-ope    =    "AC"
                     go to acc-cod-tmb-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmb-115.
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
           go to     acc-cod-tmb-110.
       acc-cod-tmb-120.
           move      w-cod-des-zbi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-tmb-999.
       acc-cod-tmb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-tmb             .
       acc-cod-tmb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi]                      *
      *                  *---------------------------------------------*
           move      rr-cod-tmb           to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
           move      w-let-arc-zbi-des    to   rr-cod-tmb-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spazi : visualizzazione literal *
      *                  * 'Tutti' al posto del codice movimento       *
      *                  *---------------------------------------------*
           if        rr-cod-tmb           not  = spaces
                     go to acc-cod-tmb-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-tmb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tmb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-tmb-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-tmb-100.
       acc-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : tipo movimento da ricercare             *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-tmb           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : descrizione tipo movimento da ricercare *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-tmb-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice cliente                       *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    02
                     go to acc-cod-cli-999.
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli           to   w-cod-mne-dcc-cod      .
           move      14                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      14                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
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
       acc-cod-cli-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-cli             .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale cliente      *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-cod-cli-rag         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale cliente     *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : visualizzazione literal  *
      *                  * 'Tutti' al posto del codice                 *
      *                  *---------------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to acc-cod-cli-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente                          *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente , ragione sociale        *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
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
           move      14                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fas           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero lotto (sigla variante)        *
      *    *-----------------------------------------------------------*
       acc-lot-vrn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "S" and
                     w-prs-sgl-vrn-snx    not  = "L"
                     go to acc-lot-vrn-999.
       acc-lot-vrn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO"                 to   v-pfk (05)             .
           move      rr-sgl-vrn-lot       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-lot-vrn-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sgl-vrn-lot         .
       acc-lot-vrn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-lot-vrn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-lot-vrn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-lot-vrn-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-lot-vrn-100.
       acc-lot-vrn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero lotto (sigla variante)     *
      *    *-----------------------------------------------------------*
       vis-lot-vrn-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "S" and
                     w-prs-sgl-vrn-snx    not  = "L"
                     go to vis-lot-vrn-999.
       vis-lot-vrn-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sgl-vrn-lot       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lot-vrn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data lotto (sigla variante)          *
      *    *-----------------------------------------------------------*
       acc-dtl-vrn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "L"
                     go to acc-dtl-vrn-999.
       acc-dtl-vrn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO"                 to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           move      rr-sgl-vrn-dat       to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-lot-vrn-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-sgl-vrn-dat         .
       acc-dtl-vrn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dtl-vrn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dtl-vrn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dtl-vrn-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dtl-vrn-100.
       acc-dtl-vrn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data lotto (sigla variante)       *
      *    *-----------------------------------------------------------*
       vis-dtl-vrn-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-sgl-vrn-snx    not  = "L"
                     go to vis-dtl-vrn-999.
       vis-dtl-vrn-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      rr-sgl-vrn-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dtl-vrn-999.
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
           move      rr-fso-dcp           to   w-cod-zos-dcp-cod      .
           move      17                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      17                   to   w-cod-zos-dcp-dln      .
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
           move      v-num                to   rr-fso-dcp             .
       acc-fso-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dcp]                        *
      *                  *---------------------------------------------*
           move      rr-fso-dcp           to   w-let-fso-dcp-cod      .
           perform   let-fso-dcp-000      thru let-fso-dcp-999        .
           move      w-let-fso-dcp-des    to   rr-fso-dcp-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-fso-dcp-des-000  thru vis-fso-dcp-des-999    .
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
      *    * Visualizzazione campo : Descrizione filtro selezione      *
      *    *-----------------------------------------------------------*
       vis-fso-dcp-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-fso-dcp-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-dcp-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo stampa totale         *
      *    *-----------------------------------------------------------*
       acc-tip-stt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-stt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stt-lun    to   v-car                  .
           move      w-exp-tip-stt-num    to   v-ldt                  .
           move      "NIDMP#"             to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-stt           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-stt-999.
       acc-tip-stt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-stt             .
       acc-tip-stt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-stt           =    zero
                     go to acc-tip-stt-100.
       acc-tip-stt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-stt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-stt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-stt-100.
       acc-tip-stt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Forma di pagamento                         *
      *    *-----------------------------------------------------------*
       acc-cod-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zfp-ope      .
           move      rr-cod-fop           to   w-cod-mne-zfp-cod      .
           move      18                   to   w-cod-mne-zfp-lin      .
           move      30                   to   w-cod-mne-zfp-pos      .
           move      18                   to   w-cod-mne-zfp-dln      .
           move      41                   to   w-cod-mne-zfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-zfp-cll-000  thru cod-mne-zfp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zfp-foi-000  thru cod-mne-zfp-foi-999    .
       acc-cod-fop-110.
           perform   cod-mne-zfp-cll-000  thru cod-mne-zfp-cll-999    .
           if        w-cod-mne-zfp-ope    =    "F+"
                     go to acc-cod-fop-115.
           if        w-cod-mne-zfp-ope    =    "AC"
                     go to acc-cod-fop-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fop-115.
           perform   cod-mne-zfp-foi-000  thru cod-mne-zfp-foi-999    .
           go to     acc-cod-fop-110.
       acc-cod-fop-120.
           move      w-cod-mne-zfp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fop-999.
       acc-cod-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-fop             .
       acc-cod-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      rr-cod-fop           to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-fop           =    zero
                     move  "Tutte le forme di pagamento       "
                                          to   rr-cod-fop-des
           else      move  w-let-arc-zfp-des
                                          to   rr-cod-fop-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fop-000      thru vis-des-fop-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zfp-flg    not  = spaces
                     go to acc-cod-fop-100.
       acc-cod-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fop-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fop-100.
       acc-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Forma di pagamento                *
      *    *-----------------------------------------------------------*
       vis-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fop           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione pagamento             *
      *    *-----------------------------------------------------------*
       vis-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fop-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Solo documenti in sospeso  *
      *    *-----------------------------------------------------------*
       acc-snx-ebf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-snx-ebf        not   = "S"
                     go to acc-snx-ebf-999.
       acc-snx-ebf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ebf-lun    to   v-car                  .
           move      w-exp-snx-ebf-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-ebf-tbl    to   v-txt                  .
      *
           if        rr-snx-ebf           =    "N"
                     move  01             to   v-num
           else if   rr-snx-ebf           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-ebf-999.
       acc-snx-ebf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   rr-snx-ebf
           else if   v-num                =    02
                     move  "S"            to   rr-snx-ebf
           else      move   spaces        to   rr-snx-ebf             .
       acc-snx-ebf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-snx-ebf           =    spaces
                     go to acc-snx-ebf-100.
       acc-snx-ebf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ebf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-ebf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-ebf-100.
       acc-snx-ebf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Solo documenti in sospeso               *
      *    *-----------------------------------------------------------*
       vis-snx-ebf-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-ebf        not   = "S"
                     go to vis-snx-ebf-999.
       vis-snx-ebf-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ebf-lun    to   v-car                  .
           move      w-exp-snx-ebf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-ebf-tbl    to   v-txt                  .
      *
           if        rr-snx-ebf           =    "N"
                     move  01             to   v-num
           else if   rr-snx-ebf           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ebf-999.
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
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to reg-ric-sel-200.
           if        rr-dat-ini           =    zero
                     move 9999999         to   rr-dat-fin
           else      move rr-dat-ini      to   rr-dat-fin             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Tipo stampa totale                              *
      *              *-------------------------------------------------*
           if        rr-tip-stt           =    zero
                     move  01             to   rr-tip-stt             .
      *              *-------------------------------------------------*
      *              * Solo documenti in sospeso                       *
      *              *-------------------------------------------------*
           if        rr-snx-ebf           not  = "S"
                     move  "N"            to   rr-snx-ebf             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-tip-stp             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-cod-vet             .
           move      spaces               to   rr-cod-vet-rag         .
           move      spaces               to   rr-cod-tmb             .
           move      spaces               to   rr-cod-tmb-des         .
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-ute             .
           move      spaces               to   rr-cod-fas             .
           move      spaces               to   rr-sgl-vrn-lot         .
           move      zero                 to   rr-sgl-vrn-dat         .
           move      zero                 to   rr-cod-fop             .
           move      spaces               to   rr-cod-fop-des         .
           move      zero                 to   rr-tip-stt             .
           move      spaces               to   rr-snx-ebf             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [vet]                         *
      *    *-----------------------------------------------------------*
       let-arc-vet-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-vet-cod    =    zero
                     go to let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET"             to   f-key                  .
           move      w-let-arc-vet-cod    to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-vet-400.
       let-arc-vet-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-vet-rag-soc       to   w-let-arc-vet-rag      .
           move      rf-vet-via-vet       to   w-let-arc-vet-via      .
           move      rf-vet-loc-vet       to   w-let-arc-vet-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-vet-999.
       let-arc-vet-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-vet-flg      .
           move      all   "."            to   w-let-arc-vet-rag      .
           go to     let-arc-vet-600.
       let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-rag      .
       let-arc-vet-600.
           move      spaces               to   w-let-arc-vet-via      .
           move      spaces               to   w-let-arc-vet-loc      .
       let-arc-vet-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zbi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbi-cod    =    spaces
                     go to let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-zbi-cod    to   rf-zbi-cod-tmb         .
           move      w-let-arc-zbi-dpz    to   rf-zbi-cod-dpz         .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbi-400.
       let-arc-zbi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbi-des-tmb       to   w-let-arc-zbi-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbi-999.
       let-arc-zbi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbi-flg      .
           move      all   "."            to   w-let-arc-zbi-des      .
           go to     let-arc-zbi-999.
       let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-des      .
       let-arc-zbi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-cge      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-cge      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio dipendenza cliente in [dcc]      *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cod    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces, solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    spaces and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a "*   ", solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    "*   " and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-dcc-cod    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-abi       to   w-let-arc-dcc-abi      .
           move      rf-dcc-cod-cab       to   w-let-arc-dcc-cab      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Normalizzazione per codice dipendenza "*   "    *
      *              *-------------------------------------------------*
           move      "Sia la sede che tutte le dipendenze     "
                                          to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-abi      .
           move      zero                 to   w-let-arc-dcc-cab      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zfp]                            *
      *    *-----------------------------------------------------------*
       let-arc-zfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zfp-cod    =    zero
                     go to let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      w-let-arc-zfp-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfp-400.
       let-arc-zfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfp-des-fop       to   w-let-arc-zfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfp-999.
       let-arc-zfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfp-flg      .
           move      all   "."            to   w-let-arc-zfp-des      .
           go to     let-arc-zfp-999.
       let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-des      .
       let-arc-zfp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.lts"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice vettore         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per bolla   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dcp]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzfp0.acs"                   .

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
