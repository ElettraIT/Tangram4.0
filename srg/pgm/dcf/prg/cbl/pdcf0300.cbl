       Identification Division.
       Program-Id.                                 pdcf0300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    prs                 *
      *                                   Fase:    dcf030              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 08/06/92    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella Tipi spesa per fattura-    *
      *                    zione fornitori                             *
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
                     "dcf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prs"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcf030"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf0300"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  GESTIONE TIPI SPESA FATTURE FORNITORI "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *            * Per routine pos-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-let-reg-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-let-reg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-ins-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-ins      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-mod-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-mod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-vis-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-vis      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-nok      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione testata                           *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per controllo se chiave vuota                     *
      *            *---------------------------------------------------*
               10  w-cnt-key-vuo-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-nok-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-imp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzione                                     *
      *            * - I : Inserimento                                 *
      *            * - M : Modifica                                    *
      *            * - V : Visualizzazione                             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-fun      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di controllo su status visualizzazione titolo    *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis-tit          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione chiave                               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts chiave                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts testata                   *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati chiave                       *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati testata                      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per accettazioni singoli campi      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
               10  w-cnt-acc-flg-aum      pic  x(01)                  .
               10  w-cnt-acc-sav-tip      pic  x(01)                  .
               10  w-cnt-acc-sav-car      pic  9(02)                  .
               10  w-cnt-acc-sav-ldt      pic  9(02)                  .
               10  w-cnt-acc-sav-dec      pic  9(01)                  .
               10  w-cnt-acc-sav-sgn      pic  x(01)                  .
               10  w-cnt-acc-sav-edm      pic  x(10)                  .
               10  w-cnt-acc-sav-msk      pic  x(24)                  .
               10  w-cnt-acc-sav-lin      pic  9(02)                  .
               10  w-cnt-acc-sav-pos      pic  9(02)                  .
               10  w-cnt-acc-sav-alf      pic  x(80)                  .
               10  w-cnt-acc-sav-txt.
                   15  filler occurs 400  pic  x(01)                  .
               10  w-cnt-acc-sav-num      pic s9(13)v9(05)  trailing
                                                            separate
                                                            character .
               10  w-cnt-acc-sav-dat      pic  9(07)                  .
               10  w-cnt-acc-sav-ufk      pic  x(80)                  .
               10  w-cnt-acc-sav-mod      pic  x(01)                  .
               10  w-cnt-acc-sav-l23      pic  x(80)                  .
               10  w-cnt-acc-sav-l24      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Work area                                             *
      *        *-------------------------------------------------------*
           05  w-cnt-wrk.
               10  w-cnt-wrk-ctr-001      pic  9(05)                  .
               10  w-cnt-wrk-ctr-002      pic  9(05)                  .
               10  w-cnt-wrk-ctr-008      pic  9(05)                  .
               10  w-cnt-wrk-ctr-009      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ysf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfysf"                          .
      *        *-------------------------------------------------------*
      *        * [zln]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzln"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-num-spf          pic  9(01)                  .
               10  w-tes-cod-lng          pic  x(03)                  .
               10  w-tes-cod-lng-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-ves          pic  x(25)                  .
               10  w-tes-des-vri          pic  x(15)                  .
               10  w-tes-des-stp          pic  x(40)                  .
               10  w-tes-tfu-spe          pic  9(02)                  .
               10  w-tes-per-spe          pic  9(02)v9(01)            .
               10  w-tes-ibl-spe          pic  9(02)                  .
               10  w-tes-ibt-spe.
                   15  w-tes-ibx-spe
                               occurs 09  pic  x(01)                  .
               10  w-tes-imp-spe          pic  9(09)                  .
               10  w-tes-civ-spe          pic  9(05)                  .
               10  w-tes-civ-spe-des      pic  x(15)                  .
               10  w-tes-ccp-spe          pic  9(07)                  .
               10  w-tes-ccp-spe-des      pic  x(40)                  .
               10  w-tes-alx-exp          pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [ysf]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-ysf.
               10  w-fnd-arc-ysf-sel      pic  x(01)                  .
               10  w-fnd-arc-ysf-spf      pic  9(01)                  .
               10  w-fnd-arc-ysf-lng      pic  x(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zln]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zln.
               10  w-let-arc-zln-flg      pic  x(01)                  .
               10  w-let-arc-zln-cod      pic  x(03)                  .
               10  w-let-arc-zln-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per ricerca codice 'I'                          *
      *    *-----------------------------------------------------------*
       01  w-ric.
           05  w-ric-cod-ita.
               10  w-ric-cod-ita-flg      pic  x(01)                  .
               10  w-ric-cod-ita-spf      pic  9(01)                  .
               10  w-ric-cod-ita-ves      pic  x(25)                  .
               10  w-ric-cod-ita-vri      pic  x(15)                  .
               10  w-ric-cod-ita-tfu      pic  9(02)                  .
               10  w-ric-cod-ita-per      pic  9(02)v9(01)            .
               10  w-ric-cod-ita-ibl      pic  9(02)                  .
               10  w-ric-cod-ita-ibt.
                   15  w-ric-cod-ita-ibx
                               occurs 09  pic  x(01)                  .
               10  w-ric-cod-ita-imp      pic  9(09)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo funzionamento                         *
      *        *-------------------------------------------------------*
           05  w-exp-tfu-spe.
               10  w-exp-tfu-spe-num      pic  9(02)       value 05   .
               10  w-exp-tfu-spe-lun      pic  9(02)       value 30   .
               10  w-exp-tfu-spe-tbl.
                   15  filler             pic  x(30) value
                            "A percentuale fissa           "          .
                   15  filler             pic  x(30) value
                            "A percentuale variabile       "          .
                   15  filler             pic  x(30) value
                            "Ad importo fisso              "          .
                   15  filler             pic  x(30) value
                            "Ad importo variabile          "          .
                   15  filler             pic  x(30) value
                            "A seconda del fornitore       "          .
      *        *-------------------------------------------------------*
      *        * Work per : Imponibile per spesa                       *
      *        *-------------------------------------------------------*
           05  w-exp-ibl-spe.
               10  w-exp-ibl-spe-num      pic  9(02)       value 12   .
               10  w-exp-ibl-spe-lun      pic  9(02)       value 25   .
               10  w-exp-ibl-spe-tbl.
                   15  filler             pic  x(25) value
                            "Totale lordo             "               .
                   15  filler             pic  x(25) value
                            "Totale netto             "               .
                   15  filler             pic  x(25) value
                            "Totalizzatori di fattura "               .
                   15  filler             pic  x(25) value
                            "Totale merce             "               .
                   15  filler             pic  x(25) value
                            "Totale servizi           "               .
                   15  filler             pic  x(25) value
                            "Totale imballi           "               .
                   15  filler             pic  x(25) value
                            "Totale libero 4          "               .
                   15  filler             pic  x(25) value
                            "Totale libero 5          "               .
                   15  filler             pic  x(25) value
                            "Totale libero 6          "               .
                   15  filler             pic  x(25) value
                            "Totale libero 7          "               .
                   15  filler             pic  x(25) value
                            "Totale libero 8          "               .
                   15  filler             pic  x(25) value
                            "Totale extra             "               .
      *        *-------------------------------------------------------*
      *        * Work per : Tipi totalizzatori di fattura              *
      *        *-------------------------------------------------------*
           05  w-exp-ibx-spe.
               10  w-exp-ibx-spe-num      pic  9(02)       value 09   .
               10  w-exp-ibx-spe-lun      pic  9(02)       value 20   .
               10  w-exp-ibx-spe-tbl.
                   15  filler             pic  x(20) value
                            "Totale merci.......:"                    .
                   15  filler             pic  x(20) value
                            "Totale servizi.....:"                    .
                   15  filler             pic  x(20) value
                            "Totale imballi.....:"                    .
                   15  filler             pic  x(20) value
                            "Totale libero 4....:"                    .
                   15  filler             pic  x(20) value
                            "Totale libero 5....:"                    .
                   15  filler             pic  x(20) value
                            "Totale libero 6....:"                    .
                   15  filler             pic  x(20) value
                            "Totale libero 7....:"                    .
                   15  filler             pic  x(20) value
                            "Totale libero 8....:"                    .
                   15  filler             pic  x(20) value
                            "Totale extra.......:"                    .
               10  w-exp-ibx-spe-tbr redefines
                   w-exp-ibx-spe-tbl.
                   15  w-exp-ibx-spe-tbx 
                               occurs 09  pic  x(20)                  .
               10  w-exp-ibx-spe-c01      pic  9(02)                  .
               10  w-exp-ibx-spe-c02      pic  9(02)                  .
               10  w-exp-ibx-spe-c03      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice lingua                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzln0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(56)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di visualizzazione titolo  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-tit      .
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
                     go to main-900.
       main-050.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
       main-100.
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita "E" : fine programma         *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Lettura registrazione pre-esistente             *
      *              *-------------------------------------------------*
           perform   rou-let-reg-000      thru rou-let-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : riciclo su chiave       *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to main-100.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione campi non chiave       *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to main-130
           else if   w-cnt-mfu-tip-fun    =    "M"
                     go to main-140
           else if   w-cnt-mfu-tip-fun    =    "V"
                     go to main-150
           else      go to main-100.
       main-130.
           move      spaces               to   w-cnt-pre-acc-ins      .
           perform   pre-acc-ins-000      thru pre-acc-ins-999        .
           if        w-cnt-pre-acc-ins    =    spaces
                     go to main-180
           else      go to main-100.
       main-140.
           move      spaces               to   w-cnt-pre-acc-mod      .
           perform   pre-acc-mod-000      thru pre-acc-mod-999        .
           if        w-cnt-pre-acc-mod    =    spaces
                     go to main-180
           else      go to main-100.
       main-150.
           move      spaces               to   w-cnt-pre-acc-vis      .
           perform   pre-acc-vis-000      thru pre-acc-vis-999        .
           if        w-cnt-pre-acc-vis    =    spaces
                     go to main-180
           else      go to main-100.
       main-180.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di modifica di almeno un   *
      *              * campo non chiave                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Accettazione campi non chiave                   *
      *              *-------------------------------------------------*
           perform   acc-nok-reg-000      thru acc-nok-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "E"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Routine post-exit su campi non chiave   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Inserimento                      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-exi-ins-000
                                          thru pos-exi-ins-999
      *                          *-------------------------------------*
      *                          * Se Modifica                         *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-exi-mod-000
                                          thru pos-exi-mod-999
      *                          *-------------------------------------*
      *                          * Se Visualizzazione                  *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pos-exi-vis-000
                                          thru pos-exi-vis-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-200.
      *                  *---------------------------------------------*
      *                  * Se uscita per annullamento                  *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "X"
                     go to main-300.
      *                      *-----------------------------------------*
      *                      * Routine post-conferma di annullamento   *
      *                      *-----------------------------------------*
           perform   pos-cnf-ann-000      thru pos-cnf-ann-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-300.
      *                  *---------------------------------------------*
      *                  * Se uscita per conferma                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Routine post-conferma                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se post-conferma di Inserimento     *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-cnf-ins-000
                                          thru pos-cnf-ins-999
      *                          *-------------------------------------*
      *                          * Se post-conferma di Modifica        *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-cnf-mod-000
                                          thru pos-cnf-mod-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-exe-pgm      .
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
           if        w-cnt-pos-exe-pgm    not  = spaces
                     go to main-050.
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
      *              * Tasto di funzione Delt :                        *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : non abilitato    *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-100.
           move      "DELT"               to   v-pfk (19)             .
       exe-acc-cmp-100.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do :                          *
      *              *  - se Visualizzazione     : disabilitati        *
      *              *  - altrimenti             : inalterati          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri di accettazione           *
      *              *-------------------------------------------------*
           move      v-tip                to   w-cnt-acc-sav-tip      .
           move      v-car                to   w-cnt-acc-sav-car      .
           move      v-lin                to   w-cnt-acc-sav-lin      .
           move      v-pos                to   w-cnt-acc-sav-pos      .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  v-alf          to   w-cnt-acc-sav-alf      .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  v-dec          to   w-cnt-acc-sav-dec
                     move  v-sgn          to   w-cnt-acc-sav-sgn
                     move  v-edm          to   w-cnt-acc-sav-edm
                     move  v-msk          to   w-cnt-acc-sav-msk
                     move  v-num          to   w-cnt-acc-sav-num      .
           if        v-tip                =    "D"
                     move  v-dat          to   w-cnt-acc-sav-dat      .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  v-ldt          to   w-cnt-acc-sav-ldt
                     move  v-txt          to   w-cnt-acc-sav-txt      .
           move      v-ufk                to   w-cnt-acc-sav-ufk      .
       exe-acc-cmp-200.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione segnale di campo modificato      *
      *              *-------------------------------------------------*
           move      v-mod                to   w-cnt-acc-sav-mod      .
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Se il campo impostato ha modificato il valore   *
      *              * precedente : ripetizione impostazione           *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     go to exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 di note operative     *
      *              *-------------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      23                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l23      .
           move      "GL"                 to   v-ope                  .
           move      24                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l24      .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Richiesta conferma annullamento                 *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#DEL"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "DELT"               to   v-pfk (19)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "DELT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se function key DELT si esce                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     go to exe-acc-cmp-999.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Ripristino parametri di impostazione            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      w-cnt-acc-sav-tip    to   v-tip                  .
           move      w-cnt-acc-sav-car    to   v-car                  .
           move      w-cnt-acc-sav-lin    to   v-lin                  .
           move      w-cnt-acc-sav-pos    to   v-pos                  .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-alf
                                          to   v-alf                  .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-dec
                                          to   v-dec
                     move  w-cnt-acc-sav-sgn
                                          to   v-sgn
                     move  w-cnt-acc-sav-edm
                                          to   v-edm
                     move  w-cnt-acc-sav-msk
                                          to   v-msk
                     move  w-cnt-acc-sav-num
                                          to   v-num                  .
           if        v-tip                =    "D"
                     move  w-cnt-acc-sav-dat
                                          to   v-dat                  .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-ldt
                                          to   v-ldt
                     move  w-cnt-acc-sav-txt
                                          to   v-txt                  .
           move      w-cnt-acc-sav-ufk    to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Ritorno alla impostazione                       *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-200.
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
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
      *              * Test su flag di visualizzazione titolo          *
      *              *-------------------------------------------------*
           if        w-cnt-sts-vis-tit    not  = spaces
                     move  spaces         to   w-cnt-sts-vis-tit
                     go to vis-tit-pgm-999.
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
      *    * Visualizzazione tipo funzionamento                        *
      *    *-----------------------------------------------------------*
       vis-tip-fun-000.
           move      "TF"                 to   v-ope                  .
           move      w-cnt-mfu-tip-fun    to   v-tfu                  .
           move      w-cnt-acc-flg-aum    to   v-tfm                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
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
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice lingua          *
      *              *-------------------------------------------------*
           perform   cod-cod-zln-opn-000  thru cod-cod-zln-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione numero livelli del    *
      *              * piano dei conti                                 *
      *              *-------------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al numero di li- *
      *    * velli del piano dei conti                                 *
      *    *-----------------------------------------------------------*
       prs-liv-pdc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 2
                     move  3              to   w-prs-liv-pdc          .
       prs-liv-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice lingua         *
      *              *-------------------------------------------------*
           perform   cod-cod-zln-cls-000  thru cod-cod-zln-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [ysf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *              *-------------------------------------------------*
      *              * [zln]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzln"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zln                 .
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
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [ysf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *              *-------------------------------------------------*
      *              * [zln]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzln"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zln                 .
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
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave della registrazione             *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-fun      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo impostazione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-imp      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione registrazione                   *
      *              *-------------------------------------------------*
           perform   nor-key-nok-000      thru nor-key-nok-999        .
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
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per testata                         *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-tes      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : chiave                      *
      *              *-------------------------------------------------*
           move      "K"                  to   w-cnt-mfu-tip-imp      .
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice spesa                                *
      *                  *---------------------------------------------*
           perform   acc-num-spf-000      thru acc-num-spf-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice lingua                               *
      *                  *---------------------------------------------*
           perform   acc-cod-lng-000      thru acc-cod-lng-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-key      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visual. dati chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-key      .
      *              *-------------------------------------------------*
      *              * Test se chiave vuota                            *
      *              *-------------------------------------------------*
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-key-reg-999.
      *              *-------------------------------------------------*
      *              * Controllo globale su tasto Do su chiave         *
      *              *-------------------------------------------------*
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-key-reg-100.
           move      "S"                  to   w-cnt-tus-acc-key      .
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campi chiave della registrazione          *
      *    *-----------------------------------------------------------*
       vis-key-reg-000.
      *              *-------------------------------------------------*
      *              * Codice spesa                                    *
      *              *-------------------------------------------------*
           perform   vis-num-spf-000      thru vis-num-spf-999        .
      *              *-------------------------------------------------*
      *              * Codice lingua                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-lng-000      thru vis-cod-lng-999        .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice spesa                                    *
      *              *-------------------------------------------------*
           perform   pmt-num-spf-000      thru pmt-num-spf-999        .
      *              *-------------------------------------------------*
      *              * Codice lingua                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-lng-000      thru pmt-cod-lng-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice spesa                  *
      *    *-----------------------------------------------------------*
       pmt-num-spf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice spesa               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-spf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice lingua                 *
      *    *-----------------------------------------------------------*
       pmt-cod-lng-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice lingua              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-lng-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero spesa                  *
      *    *-----------------------------------------------------------*
       acc-num-spf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-spf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-spf        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-spf-999.
       acc-num-spf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-spf          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-spf-400.
      *                  *---------------------------------------------*
      *                  * Find su tabella [ysf]                       *
      *                  *---------------------------------------------*
           perform   fnd-arc-ysf-000      thru fnd-arc-ysf-999        .
           if        w-fnd-arc-ysf-sel    not  = spaces
                     go to acc-num-spf-100.
           move      w-fnd-arc-ysf-spf    to   w-tes-num-spf          .
           move      w-fnd-arc-ysf-lng    to   w-tes-cod-lng          .
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zln]                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-lng        to   w-let-arc-zln-cod      .
           perform   let-arc-zln-000      thru let-arc-zln-999        .
           move      w-let-arc-zln-des    to   w-tes-cod-lng-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazioni                             *
      *                  *---------------------------------------------*
           perform   vis-num-spf-000      thru vis-num-spf-999        .
           perform   vis-cod-lng-000      thru vis-cod-lng-999        .
           perform   vis-des-lng-000      thru vis-des-lng-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto Do                          *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-spf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : uscita                            *
      *                  *---------------------------------------------*
           if        w-tes-num-spf        not  = zero
                     go to acc-num-spf-450.
           move      "DO  "               to   v-key                  .
           go to     acc-num-spf-800.
       acc-num-spf-450.
      *                  *---------------------------------------------*
      *                  * Se valore maggiore di 6 : messaggio di er-  *
      *                  * rore e reimpostazione                       *
      *                  *---------------------------------------------*
           if        w-tes-num-spf        not  > 6
                     go to acc-num-spf-600.
           move      "Il codice non puo' essere maggiore di 6 !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-num-spf-100.
       acc-num-spf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-spf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-num-spf-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-spf-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-num-spf-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-num-spf-999.
       acc-num-spf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice spesa               *
      *    *-----------------------------------------------------------*
       vis-num-spf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-spf        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-spf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice lingua                 *
      *    *-----------------------------------------------------------*
       acc-cod-lng-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = spaces
                     go to acc-cod-lng-100.
           move      "I  "                to   w-tes-cod-lng          .
       acc-cod-lng-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zln-ope      .
           move      w-tes-cod-lng        to   w-cod-cod-zln-cod      .
           move      05                   to   w-cod-cod-zln-lin      .
           move      30                   to   w-cod-cod-zln-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zln-cll-000  thru cod-cod-zln-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zln-foi-000  thru cod-cod-zln-foi-999    .
       acc-cod-lng-110.
           perform   cod-cod-zln-cll-000  thru cod-cod-zln-cll-999    .
           if        w-cod-cod-zln-ope    =    "F+"
                     go to acc-cod-lng-115.
           if        w-cod-cod-zln-ope    =    "AC"
                     go to acc-cod-lng-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lng-115.
           perform   cod-cod-zln-foi-000  thru cod-cod-zln-foi-999    .
           go to     acc-cod-lng-110.
       acc-cod-lng-120.
           move      w-cod-cod-zln-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-lng-999.
       acc-cod-lng-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-lng          .
       acc-cod-lng-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [zln]                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-lng        to   w-let-arc-zln-cod      .
           perform   let-arc-zln-000      thru let-arc-zln-999        .
           move      w-let-arc-zln-des    to   w-tes-cod-lng-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-lng-000      thru vis-des-lng-999        .
      *                  *---------------------------------------------*
      *                  * Test sul flag                               *
      *                  *---------------------------------------------*
           if        w-let-arc-zln-flg    not  = spaces
                     go to acc-cod-lng-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = spaces
                     go to acc-cod-lng-600.
           if        v-key                =    "UP  "
                     go to acc-cod-lng-600
           else      go to acc-cod-lng-100.
       acc-cod-lng-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lng-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-lng-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-lng-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-lng-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-lng-999.
       acc-cod-lng-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice lingua              *
      *    *-----------------------------------------------------------*
       vis-cod-lng-000.
           move      "DS"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-lng        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lng-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione lingua        *
      *    *-----------------------------------------------------------*
       vis-des-lng-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-lng-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-lng-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi non chiave della registrazione         *
      *    *-----------------------------------------------------------*
       acc-nok-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-nok      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Assestamento status di impostazione se in modi- *
      *              * fica o visualizzazione, per testata             *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M" or
                     w-cnt-mfu-tip-fun    =    "V"
                     move  "#"            to   w-cnt-sts-imp-tes      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-key    =    spaces
                     perform pmt-key-reg-000
                                          thru pmt-key-reg-999
                     move    "#"          to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati campi chiave           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-vis-key    =    spaces
                     perform vis-key-reg-000
                                          thru vis-key-reg-999
                     move    "#"          to   w-cnt-sts-vis-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per testata                         *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-tes    =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-tes      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati testata                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-sts-imp-tes    not  = spaces
                     if    w-cnt-sts-vis-tes
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-tes      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione dati testata                   *
      *                  *---------------------------------------------*
           perform   acc-tes-reg-000      thru acc-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita definitivo                   *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "S" or
                     w-cnt-tus-acc-tes    =    "X" or
                     w-cnt-tus-acc-tes    =    "E"
                     move  w-cnt-tus-acc-tes
                                          to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999.
       acc-nok-reg-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nok-reg-810.
      *                  *---------------------------------------------*
      *                  * Test se sola visualizzazione                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to acc-nok-reg-820
           else      go to acc-nok-reg-830.
       acc-nok-reg-820.
      *                      *-----------------------------------------*
      *                      * Accettazione se sola visualizzazione    *
      *                      *-----------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#VIS"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "E"                  to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "E"
                     move  "EXIT"         to   v-key                  .
           go to     acc-nok-reg-840.
       acc-nok-reg-830.
      *                      *-----------------------------------------*
      *                      * Accettazione se inserimento o modifica  *
      *                      *-----------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-nok-reg-840.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-nok-reg-850
           else if   v-key                =    "EXIT"
                     go to acc-nok-reg-860
           else if   v-key                =    "UP  "
                     go to acc-nok-reg-870
           else      go to acc-nok-reg-810.
       acc-nok-reg-850.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
           else      move  spaces         to   w-cnt-tdo-nok-flg
                     go to acc-nok-reg-800.
       acc-nok-reg-860.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-nok      .
           go to     acc-nok-reg-999.
       acc-nok-reg-870.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazione testata                 *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-nok-reg-200.
       acc-nok-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione testata registrazione                        *
      *    *-----------------------------------------------------------*
       acc-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione function-key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : testata                     *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-mfu-tip-imp      .
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Descrizione estesa                          *
      *                  *---------------------------------------------*
           perform   acc-des-ves-000      thru acc-des-ves-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Descrizione ridotta                         *
      *                  *---------------------------------------------*
           perform   acc-des-vri-000      thru acc-des-vri-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Descrizione per stampa                      *
      *                  *---------------------------------------------*
           perform   acc-des-stp-000      thru acc-des-stp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           perform   acc-tfu-spe-000      thru acc-tfu-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Percentuale fissa                           *
      *                  *---------------------------------------------*
           perform   acc-per-spe-000      thru acc-per-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Imponibile per la percentuale               *
      *                  *---------------------------------------------*
           perform   acc-ibl-spe-000      thru acc-ibl-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Importo fisso                               *
      *                  *---------------------------------------------*
           perform   acc-imp-spe-000      thru acc-imp-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-450.
      *                  *---------------------------------------------*
      *                  * Codice iva spesa                            *
      *                  *---------------------------------------------*
           perform   acc-civ-spe-000      thru acc-civ-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Contropartita spesa                         *
      *                  *---------------------------------------------*
           perform   acc-ccp-spe-000      thru acc-ccp-spe-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-450.
       acc-tes-reg-900.
      *              *-------------------------------------------------*
      *              * Assestamento status di uscita                   *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     move   "S"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "DELT"
                     move   "X"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "EXIT"
                     move   "E"           to   w-cnt-tus-acc-tes
           else      move   "+"           to   w-cnt-tus-acc-tes      .
       acc-tes-reg-990.
      *              *-------------------------------------------------*
      *              * Flag di status impostazione testata             *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Flag di status visualizzazione dati testata     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-tes      .
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Descrizione estesa                              *
      *              *-------------------------------------------------*
           perform   vis-des-ves-000      thru vis-des-ves-999        .
      *              *-------------------------------------------------*
      *              * Descrizione ridotta                             *
      *              *-------------------------------------------------*
           perform   vis-des-vri-000      thru vis-des-vri-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per stampa                          *
      *              *-------------------------------------------------*
           perform   vis-des-stp-000      thru vis-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento                              *
      *              *-------------------------------------------------*
           perform   vis-tfu-spe-000      thru vis-tfu-spe-999        .
      *              *-------------------------------------------------*
      *              * Percentuale fissa                               *
      *              *-------------------------------------------------*
           perform   vis-per-spe-000      thru vis-per-spe-999        .
      *              *-------------------------------------------------*
      *              * Imponibile per la percentuale                   *
      *              *-------------------------------------------------*
           perform   vis-ibl-spe-000      thru vis-ibl-spe-999        .
      *              *-------------------------------------------------*
      *              * Importo fisso                                   *
      *              *-------------------------------------------------*
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
      *              *-------------------------------------------------*
      *              * Codice iva spesa                                *
      *              *-------------------------------------------------*
           perform   vis-civ-spe-000      thru vis-civ-spe-999        .
      *              *-------------------------------------------------*
      *              * Descrizione codice iva spesa                    *
      *              *-------------------------------------------------*
           perform   vis-des-civ-000      thru vis-des-civ-999        .
      *              *-------------------------------------------------*
      *              * Contropartita spesa                             *
      *              *-------------------------------------------------*
           perform   vis-ccp-spe-000      thru vis-ccp-spe-999        .
      *              *-------------------------------------------------*
      *              * Descrizione contropartita spesa                 *
      *              *-------------------------------------------------*
           perform   vis-des-ccp-000      thru vis-des-ccp-999        .
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Descrizione estesa                              *
      *              *-------------------------------------------------*
           perform   pmt-des-ves-000      thru pmt-des-ves-999        .
      *              *-------------------------------------------------*
      *              * Descrizione ridotta                             *
      *              *-------------------------------------------------*
           perform   pmt-des-vri-000      thru pmt-des-vri-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per stampa                          *
      *              *-------------------------------------------------*
           perform   pmt-des-stp-000      thru pmt-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento                              *
      *              *-------------------------------------------------*
           perform   pmt-tfu-spe-000      thru pmt-tfu-spe-999        .
      *              *-------------------------------------------------*
      *              * Percentuale fissa                               *
      *              *-------------------------------------------------*
           perform   pmt-per-spe-000      thru pmt-per-spe-999        .
      *              *-------------------------------------------------*
      *              * Imponibile per la percentuale                   *
      *              *-------------------------------------------------*
           perform   pmt-ibl-spe-000      thru pmt-ibl-spe-999        .
      *              *-------------------------------------------------*
      *              * Importo fisso                                   *
      *              *-------------------------------------------------*
           perform   pmt-imp-spe-000      thru pmt-imp-spe-999        .
      *              *-------------------------------------------------*
      *              * Codice iva spesa                                *
      *              *-------------------------------------------------*
           perform   pmt-civ-spe-000      thru pmt-civ-spe-999        .
      *              *-------------------------------------------------*
      *              * Contropartita spesa                             *
      *              *-------------------------------------------------*
           perform   pmt-ccp-spe-000      thru pmt-ccp-spe-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione estesa               *
      *    *-----------------------------------------------------------*
       pmt-des-ves-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione video estesa   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-ves-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione ridotta              *
      *    *-----------------------------------------------------------*
       pmt-des-vri-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione video ridotta  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-vri-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per la stampa        *
      *    *-----------------------------------------------------------*
       pmt-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione per la stampa  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo funzionamento spesa         *
      *    *-----------------------------------------------------------*
       pmt-tfu-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo funzionamento spesa   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tfu-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Percentuale fissa                *
      *    *-----------------------------------------------------------*
       pmt-per-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "% per la spesa             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-per-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Imponibile per la percentuale    *
      *    *-----------------------------------------------------------*
       pmt-ibl-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Imponibile per % spesa     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ibl-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Importo fisso                    *
      *    *-----------------------------------------------------------*
       pmt-imp-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo per la spesa       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-imp-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice iva per la spesa          *
      *    *-----------------------------------------------------------*
       pmt-civ-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice iva per la spesa    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-civ-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contropartita per la spesa       *
      *    *-----------------------------------------------------------*
       pmt-ccp-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Contropartita per la spesa :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ccp-spe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione estesa           *
      *    *-----------------------------------------------------------*
       acc-des-ves-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-des-ves-999.
       acc-des-ves-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-ves (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-ves-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-ves-999.
       acc-des-ves-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-ves (1)      .
       acc-des-ves-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-des-ves (1)    =    spaces
                     go to acc-des-ves-100.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-ves (1)
                    (01 : 01)             =    spaces
                     go to acc-des-ves-100.
       acc-des-ves-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-ves-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-ves-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-ves-100.
       acc-des-ves-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione estesa        *
      *    *-----------------------------------------------------------*
       vis-des-ves-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-ves (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ves-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione ridotta          *
      *    *-----------------------------------------------------------*
       acc-des-vri-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-des-vri-999.
       acc-des-vri-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-vri (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-vri-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-vri-999.
       acc-des-vri-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-vri (1)      .
       acc-des-vri-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-vri (1)    not  = spaces
                     go to acc-des-vri-500.
           if        v-key                =    "UP  "
                     go to acc-des-vri-600
           else      go to acc-des-vri-100.
       acc-des-vri-500.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-vri (1)
                    (01 : 01)             =    spaces
                     go to acc-des-vri-100.
       acc-des-vri-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-vri-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-vri-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-vri-100.
       acc-des-vri-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione ridotta       *
      *    *-----------------------------------------------------------*
       vis-des-vri-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-vri (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-vri-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione per stampa       *
      *    *-----------------------------------------------------------*
       acc-des-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-stp (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
       acc-des-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-stp (1)      .
       acc-des-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-stp (1)    not  = spaces
                     go to acc-des-stp-500.
           if        v-key                =    "UP  "
                     go to acc-des-stp-600
           else      go to acc-des-stp-100.
       acc-des-stp-500.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-stp (1)
                    (01 : 01)             =    spaces
                     go to acc-des-stp-100.
       acc-des-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-stp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-stp-100.
       acc-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per stampa    *
      *    *-----------------------------------------------------------*
       vis-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-stp (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo funzionamento         *
      *    *-----------------------------------------------------------*
       acc-tfu-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-tfu-spe-999.
       acc-tfu-spe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tfu-spe-lun    to   v-car                  .
           move      w-exp-tfu-spe-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tfu-spe-tbl    to   v-txt                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-tfu-spe (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tfu-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tfu-spe-999.
       acc-tfu-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tfu-spe (1)      .
       acc-tfu-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    =    zero
                     go to acc-tfu-spe-100.
       acc-tfu-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuali normalizzazioni                   *
      *                  *---------------------------------------------*
           if        w-tes-tfu-spe (1)    =    01 or
                     w-tes-tfu-spe (1)    =    02
                     go to acc-tfu-spe-620
           else if   w-tes-tfu-spe (1)    =    03 or
                     w-tes-tfu-spe (1)    =    04
                     go to acc-tfu-spe-640
           else      go to acc-tfu-spe-660.
       acc-tfu-spe-620.
      *                      *-----------------------------------------*
      *                      * Se percentuale                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-imp-spe (1)      .
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
           go to     acc-tfu-spe-800.
       acc-tfu-spe-640.
      *                      *-----------------------------------------*
      *                      * Se importo                              *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-per-spe (1)      .
           move      zero                 to   w-tes-ibl-spe (1)      .
           perform   vis-per-spe-000      thru vis-per-spe-999        .
           perform   vis-ibl-spe-000      thru vis-ibl-spe-999        .
           go to     acc-tfu-spe-800.
       acc-tfu-spe-660.
      *                      *-----------------------------------------*
      *                      * Se a seconda del fornitore              *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-per-spe (1)      .
           move      zero                 to   w-tes-imp-spe (1)      .
           perform   vis-per-spe-000      thru vis-per-spe-999        .
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
           go to     acc-tfu-spe-800.
       acc-tfu-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tfu-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tfu-spe-100.
       acc-tfu-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo funzionamento        *
      *    *-----------------------------------------------------------*
       vis-tfu-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tfu-spe-lun    to   v-car                  .
           move      w-exp-tfu-spe-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tfu-spe-tbl    to   v-txt                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tfu-spe (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tfu-spe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Percentuale fissa            *
      *    *-----------------------------------------------------------*
       acc-per-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-per-spe-999.
           if        w-tes-tfu-spe (1)    not  = 1 and
                     w-tes-tfu-spe (1)    not  = 2
                     go to acc-per-spe-999.
       acc-per-spe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-per-spe (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-per-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-per-spe-999.
       acc-per-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-per-spe (1)      .
       acc-per-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se percentuale variabile : nessun controllo *
      *                  *---------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 1
                     go to acc-per-spe-600.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-per-spe (1)    not  = zero
                     go to acc-per-spe-600.
           if        v-key                =    "UP  "
                     go to acc-per-spe-600
           else      go to acc-per-spe-100.
       acc-per-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-per-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-per-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-per-spe-100.
       acc-per-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Percentuale fissa         *
      *    *-----------------------------------------------------------*
       vis-per-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-per-spe (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-per-spe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Imponibile per la percent. *
      *    *-----------------------------------------------------------*
       acc-ibl-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-ibl-spe-999.
           if        w-tes-tfu-spe (1)    not  = 1 and
                     w-tes-tfu-spe (1)    not  = 2 and
                     w-tes-tfu-spe (1)    not  = 5
                     go to acc-ibl-spe-999.
       acc-ibl-spe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ibl-spe-lun    to   v-car                  .
           move      w-exp-ibl-spe-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ibl-spe-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ibl-spe (1)    to   v-num                  .
           if        v-num                >    10
                     subtract 07          from v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ibl-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ibl-spe-999.
       acc-ibl-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                >    04
                     add   07             to   v-num                  .
           move      v-num                to   w-tes-ibl-spe (1)      .
       acc-ibl-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-ibl-spe (1)    =    zero
                     go to acc-ibl-spe-100.
       acc-ibl-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore             *
      *                  *---------------------------------------------*
           if        w-tes-ibl-spe (1)    =    01 or
                     w-tes-ibl-spe (1)    =    02 or
                     w-tes-ibl-spe (1)    =    11 or
                     w-tes-ibl-spe (1)    =    12 or
                     w-tes-ibl-spe (1)    =    13 or
                     w-tes-ibl-spe (1)    =    14 or
                     w-tes-ibl-spe (1)    =    15 or
                     w-tes-ibl-spe (1)    =    16 or
                     w-tes-ibl-spe (1)    =    17 or
                     w-tes-ibl-spe (1)    =    18 or
                     w-tes-ibl-spe (1)    =    19
                     go to acc-ibl-spe-615
           else if   w-tes-ibl-spe (1)    =    03
                     go to acc-ibl-spe-625
           else      go to acc-ibl-spe-100.
       acc-ibl-spe-615.
      *                  *---------------------------------------------*
      *                  * Se su :                                     *
      *                  * - Totale lordo                              *
      *                  * - Totale netto                              *
      *                  * - Totale merci                              *
      *                  * - Totale servizi                            *
      *                  * - Totale imballi                            *
      *                  * - Totale libero 4                           *
      *                  * - Totale libero 5                           *
      *                  * - Totale libero 6                           *
      *                  * - Totale libero 7                           *
      *                  * - Totale libero 8                           *
      *                  * - Totale extra                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-ibl-spe-800.
       acc-ibl-spe-625.
      *                  *---------------------------------------------*
      *                  * Se su di una combinazione delle voci tra :  *
      *                  * - Totale merci                              *
      *                  * - Totale servizi                            *
      *                  * - Totale imballi                            *
      *                  * - Totale libero 4                           *
      *                  * - Totale libero 5                           *
      *                  * - Totale libero 6                           *
      *                  * - Totale libero 7                           *
      *                  * - Totale libero 8                           *
      *                  * - Totale extra                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non si e' in Up o Down o Do si passa *
      *                      * sicuramente alle accettazioni           *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  " and
                     v-key                not  = "DOWN" and
                     v-key                not  = "DO  "
                     go to acc-ibl-spe-630.
      *                      *-----------------------------------------*
      *                      * Se non esiste nessuna voce a 'S' si re- *
      *                      * imposta                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-exp-ibx-spe-c03      .
           inspect   w-tes-ibt-spe (1)
                                      tallying w-exp-ibx-spe-c03
                     for                  all  "S"                    .
           if        w-exp-ibx-spe-c03    =    zero
                     go to acc-ibl-spe-100.
      *                      *-----------------------------------------*
      *                      * Altrimenti : a controllo tasto Do       *
      *                      *-----------------------------------------*
           go to     acc-ibl-spe-800.
       acc-ibl-spe-630.
      *                      *-----------------------------------------*
      *                      * Accettazione voci da includere          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio immagine video          *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Costruzione box accettazione        *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riempimento box e normalizzazione   *
      *                          * eventuale preliminare dei valori di *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      zero                 to   w-exp-ibx-spe-c01      .
           move      11                   to   w-exp-ibx-spe-c02      .
       acc-ibl-spe-635.
           add       1                    to   w-exp-ibx-spe-c01      .
           if        w-exp-ibx-spe-c01    >    9
                     go to acc-ibl-spe-640.
           add       1                    to   w-exp-ibx-spe-c02      .
           if        w-tes-ibx-spe
                    (1, w-exp-ibx-spe-c01)
                                          not  = "S"
                     move  "N"            to   w-tes-ibx-spe
                                              (1, w-exp-ibx-spe-c01)  .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-exp-ibx-spe-c02    to   v-lin                  .
           move      57                   to   v-pos                  .
           move      w-exp-ibx-spe-tbx
                    (w-exp-ibx-spe-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-exp-ibx-spe-c02    to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tes-ibx-spe
                    (1, w-exp-ibx-spe-c01)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ibl-spe-635.
       acc-ibl-spe-640.
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ibl-spe-645.
      *                          *-------------------------------------*
      *                          * Accettazioni                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Inizializzazione contatori      *
      *                              *---------------------------------*
           move      01                   to   w-exp-ibx-spe-c01      .
           move      12                   to   w-exp-ibx-spe-c02      .
       acc-ibl-spe-650.
      *                              *---------------------------------*
      *                              * Accettazione scelta Si/No       *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-exp-ibx-spe-c02    to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ibx-spe
                    (1, w-exp-ibx-spe-c01)
                                          to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ibl-spe-655.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
           if        v-key                not  = "EXIT"
                     go to acc-ibl-spe-660.
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Uscita con Exit             *
      *                                  *-----------------------------*
           move      "E"                  to   w-cnt-tus-acc-tes      .
           go to     acc-ibl-spe-999.
       acc-ibl-spe-660.
      *                              *---------------------------------*
      *                              * Se Delt                         *
      *                              *---------------------------------*
           if        v-key                not  = "DELT"
                     go to acc-ibl-spe-670.
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Uscita con Delt             *
      *                                  *-----------------------------*
           move      "X"                  to   w-cnt-tus-acc-tes      .
           go to     acc-ibl-spe-999.
       acc-ibl-spe-670.
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-alf                to   w-tes-ibx-spe
                                              (1, w-exp-ibx-spe-c01)  .
       acc-ibl-spe-675.
      *                              *---------------------------------*
      *                              * Controllo valore                *
      *                              *---------------------------------*
           if        w-tes-ibx-spe
                    (1, w-exp-ibx-spe-c01)
                                          not  = "S" and
                     w-tes-ibx-spe
                    (1, w-exp-ibx-spe-c01)
                                          not  = "N"
                     go to acc-ibl-spe-650.
       acc-ibl-spe-680.
      *                              *---------------------------------*
      *                              * Se Do                           *
      *                              *---------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-ibl-spe-685.
      *                                  *-----------------------------*
      *                                  * Controllo globale su Do     *
      *                                  *-----------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                                  *-----------------------------*
      *                                  * Se non superato : reimpo-   *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-cnt-tdo-nok-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-nok-flg
                     go to acc-ibl-spe-650.
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Uscita con Do               *
      *                                  *-----------------------------*
           move      "S"                  to   w-cnt-tus-acc-tes      .
           go to     acc-ibl-spe-999.
       acc-ibl-spe-685.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-ibl-spe-695.
      *                                  *-----------------------------*
      *                                  * Se su prima riga            *
      *                                  *-----------------------------*
           if        w-exp-ibx-spe-c01    not  = 01
                     go to acc-ibl-spe-690.
      *                                      *-------------------------*
      *                                      * Test che esista almeno  *
      *                                      * una voce a 'S'          *
      *                                      *-------------------------*
           move      zero                 to   w-exp-ibx-spe-c03      .
           inspect   w-tes-ibt-spe (1)
                                      tallying w-exp-ibx-spe-c03
                     for                  all  "S"                    .
      *                                      *-------------------------*
      *                                      * Se no : reimpostazione  *
      *                                      *-------------------------*
           if        w-exp-ibx-spe-c03    =    zero
                     go to acc-ibl-spe-650.
      *                                      *-------------------------*
      *                                      * Se si                   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Ripristino immagine *
      *                                          * video               *
      *                                          *---------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Uscita con Up       *
      *                                          *---------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-ibl-spe-999.
       acc-ibl-spe-690.
      *                                  *-----------------------------*
      *                                  * Se su riga successiva alla  *
      *                                  * prima                       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Decremento contatori    *
      *                                      *-------------------------*
           subtract  1                    from w-exp-ibx-spe-c01      .
           subtract  1                    from w-exp-ibx-spe-c02      .
      *                                      *-------------------------*
      *                                      * Riciclo                 *
      *                                      *-------------------------*
           go to     acc-ibl-spe-650.
       acc-ibl-spe-695.
      *                              *---------------------------------*
      *                              * Se Down o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se su ultima riga           *
      *                                  *-----------------------------*
           if        w-exp-ibx-spe-c01    not  = 09
                     go to acc-ibl-spe-700.
      *                                      *-------------------------*
      *                                      * Test che esista almeno  *
      *                                      * una voce a 'S'          *
      *                                      *-------------------------*
           move      zero                 to   w-exp-ibx-spe-c03      .
           inspect   w-tes-ibt-spe (1)
                                      tallying w-exp-ibx-spe-c03
                     for                  all  "S"                    .
      *                                      *-------------------------*
      *                                      * Se no : reimpostazione  *
      *                                      *-------------------------*
           if        w-exp-ibx-spe-c03    =    zero
                     go to acc-ibl-spe-650.
      *                                      *-------------------------*
      *                                      * Se si                   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Ripristino immagine *
      *                                          * video               *
      *                                          *---------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Uscita con Down     *
      *                                          *---------------------*
           move      "DOWN"               to   v-key                  .
           go to     acc-ibl-spe-999.
       acc-ibl-spe-700.
      *                                  *-----------------------------*
      *                                  * Se su riga precedente l'ul- *
      *                                  * tima                        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Incremento contatori    *
      *                                      *-------------------------*
           add       1                    to   w-exp-ibx-spe-c01      .
           add       1                    to   w-exp-ibx-spe-c02      .
      *                                      *-------------------------*
      *                                      * Riciclo                 *
      *                                      *-------------------------*
           go to     acc-ibl-spe-650.
       acc-ibl-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ibl-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ibl-spe-100.
       acc-ibl-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Imponibile per percent.   *
      *    *-----------------------------------------------------------*
       vis-ibl-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ibl-spe-lun    to   v-car                  .
           move      w-exp-ibl-spe-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ibl-spe-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-ibl-spe (1)    to   v-num                  .
           if        v-num                >    10
                     subtract 07          from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ibl-spe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Importo fisso                *
      *    *-----------------------------------------------------------*
       acc-imp-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-imp-spe-999.
           if        w-tes-tfu-spe (1)    not  = 3 and
                     w-tes-tfu-spe (1)    not  = 4
                     go to acc-imp-spe-999.
       acc-imp-spe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-imp-spe (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-imp-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-imp-spe-999.
       acc-imp-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-imp-spe (1)      .
       acc-imp-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se importo variabile : nessun controllo     *
      *                  *---------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 3
                     go to acc-imp-spe-600.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-imp-spe (1)    not  = zero
                     go to acc-imp-spe-600.
           if        v-key                =    "UP  "
                     go to acc-imp-spe-600
           else      go to acc-imp-spe-100.
       acc-imp-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-imp-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-imp-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-imp-spe-100.
       acc-imp-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Importo fisso             *
      *    *-----------------------------------------------------------*
       vis-imp-spe-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-imp-spe (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-spe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice iva per la spesa      *
      *    *-----------------------------------------------------------*
       acc-civ-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-civ-spe-999.
       acc-civ-spe-100.
      *              *-------------------------------------------------*
      *              * Editing preliminare per l'accettazione          *
      *              *-------------------------------------------------*
           move      w-tes-civ-spe (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zci-ope      .
           move      w-edt-iva-cie        to   w-cod-mne-zci-cod      .
           move      20                   to   w-cod-mne-zci-lin      .
           move      30                   to   w-cod-mne-zci-pos      .
           move      20                   to   w-cod-mne-zci-dln      .
           move      41                   to   w-cod-mne-zci-dps      .
           move      "<BD"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
       acc-civ-spe-110.
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           if        w-cod-mne-zci-ope    =    "F+"
                     go to acc-civ-spe-115.
           if        w-cod-mne-zci-ope    =    "AC"
                     go to acc-civ-spe-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-civ-spe-115.
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
           go to     acc-civ-spe-110.
       acc-civ-spe-120.
      *              *-------------------------------------------------*
      *              * Editing valore impostato                        *
      *              *-------------------------------------------------*
           move      w-cod-mne-zci-cod    to   w-edt-iva-cie          .
           perform   edt-iva-cod-000      thru edt-iva-cod-999        .
           move      w-edt-iva-cod        to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-civ-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-civ-spe-999.
       acc-civ-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-civ-spe (1)      .
       acc-civ-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-civ-spe (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zci-des    to   w-tes-civ-spe-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-civ-000      thru vis-des-civ-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zci-flg    not  = spaces
                     go to acc-civ-spe-100.
       acc-civ-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-civ-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-civ-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-civ-spe-100.
       acc-civ-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice iva per lo sconto  *
      *    *-----------------------------------------------------------*
       vis-civ-spe-000.
      *              *-------------------------------------------------*
      *              * Editing preliminare                             *
      *              *-------------------------------------------------*
           move      w-tes-civ-spe (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-civ-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice iva    *
      *    *                                 per la spesa              *
      *    *-----------------------------------------------------------*
       vis-des-civ-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-civ-spe-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-civ-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice contropartita per la  *
      *    *                              spesa                        *
      *    *-----------------------------------------------------------*
       acc-ccp-spe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-ccp-spe-999.
       acc-ccp-spe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ccp-spe (1)    to   w-cod-mne-pdc-cod      .
           move      21                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      21                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ccp-spe-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ccp-spe-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ccp-spe-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ccp-spe-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ccp-spe-110.
       acc-ccp-spe-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ccp-spe-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ccp-spe-999.
       acc-ccp-spe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ccp-spe (1)      .
       acc-ccp-spe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ccp-spe (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ccp-spe-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-ccp-000      thru vis-des-ccp-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ccp-spe-100.
       acc-ccp-spe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ccp-spe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ccp-spe-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ccp-spe-100.
       acc-ccp-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice contropartita per  *
      *    *                                 la spesa                  *
      *    *-----------------------------------------------------------*
       vis-ccp-spe-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ccp-spe (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ccp-spe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice con-   *
      *    *                                 tropartita per la spesa   *
      *    *-----------------------------------------------------------*
       vis-des-ccp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ccp-spe-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ccp-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Controllo se chiave vuota                                 *
      *    *-----------------------------------------------------------*
       cnt-key-vuo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-key-vuo-flg      .
      *              *-------------------------------------------------*
      *              * Test su Codice spesa                            *
      *              *-------------------------------------------------*
           if        w-tes-num-spf        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg      .
       cnt-key-vuo-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi non chiave       *
      *    *-----------------------------------------------------------*
       cnt-tdo-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-nok-flg      .
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su Descrizione estesa                 *
      *              *-------------------------------------------------*
           if        w-tes-des-ves (1)    not  = spaces
                     go to cnt-tdo-nok-150.
           move      "Manca la descrizione per video estesa           "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo su Descrizione ridotta                *
      *              *-------------------------------------------------*
           if        w-tes-des-vri (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Manca la descrizione per video ridotta          "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su Descrizione per la stampa          *
      *              *-------------------------------------------------*
           if        w-tes-des-stp (1)    not  = spaces
                     go to cnt-tdo-nok-250.
           move      "Manca la descrizione per la stampa              "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo funzionamento                 *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = zero
                     go to cnt-tdo-nok-300.
           move      "Manca il tipo funzionamento spesa               "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su Percentuale fissa                  *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 01
                     go to cnt-tdo-nok-350.
           if        w-tes-per-spe (1)    not  = zero
                     go to cnt-tdo-nok-350.
           move      "Manca la percentuale per la spesa               "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-350.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo imponibile                    *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 01 and
                     w-tes-tfu-spe (1)    not  = 02 and
                     w-tes-tfu-spe (1)    not  = 05
                     go to cnt-tdo-nok-400.
           if        w-tes-ibl-spe (1)    not  = zero
                     go to cnt-tdo-nok-400.
           move      "Manca il tipo di imponibile per la spesa        "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo su totalizzatori di fattura           *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 01 and
                     w-tes-tfu-spe (1)    not  = 02 and
                     w-tes-tfu-spe (1)    not  = 05
                     go to cnt-tdo-nok-450.
           if        w-tes-ibl-spe (1)    not  = 03
                     go to cnt-tdo-nok-450.
           move      zero                 to   w-exp-ibx-spe-c03      .
           inspect   w-tes-ibt-spe (1)
                                      tallying w-exp-ibx-spe-c03
                     for                  all  "S"                    .
           if        w-exp-ibx-spe-c03    >    zero
                     go to cnt-tdo-nok-450.
           move      "Manca la specifica dei totalizzatori            "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-450.
      *              *-------------------------------------------------*
      *              * Controllo su Importo fisso                      *
      *              *-------------------------------------------------*
           if        w-tes-tfu-spe (1)    not  = 03
                     go to cnt-tdo-nok-600.
           if        w-tes-imp-spe (1)    not  = zero
                     go to cnt-tdo-nok-600.
           move      "Manca l'importo per la spesa                    "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totalizzatori                               *
      *                  *---------------------------------------------*
           if        w-tes-ibl-spe (1)    =    03
                     go to cnt-tdo-nok-800.
           move      spaces               to   w-tes-ibt-spe (1)      .
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-999.
           exit.
                      
      *    *===========================================================*
      *    * Normalizzazione dati chiave e non chiave                  *
      *    *-----------------------------------------------------------*
       nor-key-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati chiave                     *
      *              *-------------------------------------------------*
           perform   nor-key-reg-000      thru nor-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave testata         *
      *              *-------------------------------------------------*
           perform   nor-nok-tes-000      thru nor-nok-tes-999        .
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      zero                 to   w-tes-num-spf          .
           move      spaces               to   w-tes-cod-lng          .
           move      spaces               to   w-tes-cod-lng-des      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-ves (1)      .
           move      spaces               to   w-tes-des-vri (1)      .
           move      spaces               to   w-tes-des-stp (1)      .
           move      zero                 to   w-tes-tfu-spe (1)      .
           move      zero                 to   w-tes-per-spe (1)      .
           move      zero                 to   w-tes-ibl-spe (1)      .
           move      spaces               to   w-tes-ibx-spe (1, 1)   .
           move      spaces               to   w-tes-ibx-spe (1, 2)   .
           move      spaces               to   w-tes-ibx-spe (1, 3)   .
           move      spaces               to   w-tes-ibx-spe (1, 4)   .
           move      spaces               to   w-tes-ibx-spe (1, 5)   .
           move      spaces               to   w-tes-ibx-spe (1, 6)   .
           move      spaces               to   w-tes-ibx-spe (1, 7)   .
           move      spaces               to   w-tes-ibx-spe (1, 8)   .
           move      spaces               to   w-tes-ibx-spe (1, 9)   .
           move      zero                 to   w-tes-imp-spe (1)      .
           move      zero                 to   w-tes-civ-spe (1)      .
           move      spaces               to   w-tes-civ-spe-des (1)  .
           move      zero                 to   w-tes-ccp-spe (1)      .
           move      spaces               to   w-tes-ccp-spe-des (1)  .
           move      spaces               to   w-tes-alx-exp (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *-----------------------------------------------------------*
       rou-let-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-let-reg      .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-tes-num-spf        to   rf-ysf-num-spf         .
           move      w-tes-cod-lng        to   rf-ysf-cod-lng         .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se movimento non trovato                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-060.
      *                          *-------------------------------------*
      *                          * Flag di uscita                      *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                          *-------------------------------------*
      *                          * Messaggio                           *
      *                          *-------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Box di errore                       *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-060.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se anagrafica trovata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [ysf]                        *
      *                          *-------------------------------------*
           move      rf-ysf-des-ves       to   w-tes-des-ves (1)      .
           move      rf-ysf-des-vri       to   w-tes-des-vri (1)      .
           move      rf-ysf-des-stp       to   w-tes-des-stp (1)      .
           move      rf-ysf-tfu-spe       to   w-tes-tfu-spe (1)      .
           move      rf-ysf-per-spe       to   w-tes-per-spe (1)      .
           move      rf-ysf-ibl-spe       to   w-tes-ibl-spe (1)      .
           move      rf-ysf-ibx-spe (1)   to   w-tes-ibx-spe (1, 1)   .
           move      rf-ysf-ibx-spe (2)   to   w-tes-ibx-spe (1, 2)   .
           move      rf-ysf-ibx-spe (3)   to   w-tes-ibx-spe (1, 3)   .
           move      rf-ysf-ibx-spe (4)   to   w-tes-ibx-spe (1, 4)   .
           move      rf-ysf-ibx-spe (5)   to   w-tes-ibx-spe (1, 5)   .
           move      rf-ysf-ibx-spe (6)   to   w-tes-ibx-spe (1, 6)   .
           move      rf-ysf-ibx-spe (7)   to   w-tes-ibx-spe (1, 7)   .
           move      rf-ysf-ibx-spe (8)   to   w-tes-ibx-spe (1, 8)   .
           move      rf-ysf-ibx-spe (9)   to   w-tes-ibx-spe (1, 9)   .
           move      rf-ysf-imp-spe       to   w-tes-imp-spe (1)      .
           move      rf-ysf-civ-spe       to   w-tes-civ-spe (1)      .
           move      rf-ysf-ccp-spe       to   w-tes-ccp-spe (1)      .
           move      rf-ysf-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [ysf]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * iva per la spesa                *
      *                              *---------------------------------*
           move      w-tes-civ-spe (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-civ-spe-des (1)  .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * contropartita per la spesa      *
      *                              *---------------------------------*
           move      w-tes-ccp-spe (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ccp-spe-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Test per visualizzazione                        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-ins      .
      *              *-------------------------------------------------*
      *              * Se lingua con codice 'I' : nessun controllo     *
      *              *-------------------------------------------------*
           if        w-tes-cod-lng        =    "I  "
                     go to pre-acc-ins-999.
      *              *-------------------------------------------------*
      *              * Ricerca del corrispondente nella tabella con    *
      *              * codice 'I'                                      *
      *              *-------------------------------------------------*
           move      w-tes-num-spf        to   w-ric-cod-ita-spf      .
           perform   ric-cod-ita-000      thru ric-cod-ita-999        .
      *              *-------------------------------------------------*
      *              * Test su flag di uscita                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se uscita con errore                        *
      *                  *---------------------------------------------*
           if        w-ric-cod-ita-flg    =    spaces
                     go to pre-acc-ins-500.
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio di errore        *
      *                      *-----------------------------------------*
           move      "Manca la corrispondente tabella in italiano     "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Flag di errore in uscita                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-pre-acc-ins      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-500.
      *                  *---------------------------------------------*
      *                  * Se uscita senza errore                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti             *
      *                      *-----------------------------------------*
           move      w-ric-cod-ita-ves    to   w-tes-des-ves (1)      .
           move      w-ric-cod-ita-vri    to   w-tes-des-vri (1)      .
           move      w-ric-cod-ita-tfu    to   w-tes-tfu-spe (1)      .
           move      w-ric-cod-ita-per    to   w-tes-per-spe (1)      .
           move      w-ric-cod-ita-ibl    to   w-tes-ibl-spe (1)      .
           move      w-ric-cod-ita-ibx (1)
                                          to   w-tes-ibx-spe (1, 1)   .
           move      w-ric-cod-ita-ibx (2)
                                          to   w-tes-ibx-spe (1, 2)   .
           move      w-ric-cod-ita-ibx (3)
                                          to   w-tes-ibx-spe (1, 3)   .
           move      w-ric-cod-ita-ibx (4)
                                          to   w-tes-ibx-spe (1, 4)   .
           move      w-ric-cod-ita-ibx (5)
                                          to   w-tes-ibx-spe (1, 5)   .
           move      w-ric-cod-ita-ibx (6)
                                          to   w-tes-ibx-spe (1, 6)   .
           move      w-ric-cod-ita-ibx (7)
                                          to   w-tes-ibx-spe (1, 7)   .
           move      w-ric-cod-ita-ibx (8)
                                          to   w-tes-ibx-spe (1, 8)   .
           move      w-ric-cod-ita-ibx (9)
                                          to   w-tes-ibx-spe (1, 9)   .
           move      w-ric-cod-ita-imp    to   w-tes-imp-spe (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori letti            *
      *                      *-----------------------------------------*
           perform   vis-des-ves-000      thru vis-des-ves-999        .
           perform   vis-des-vri-000      thru vis-des-vri-999        .
           perform   vis-tfu-spe-000      thru vis-tfu-spe-999        .
           perform   vis-per-spe-000      thru vis-per-spe-999        .
           perform   vis-ibl-spe-000      thru vis-ibl-spe-999        .
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-mod      .
      *              *-------------------------------------------------*
      *              * Se lingua con codice 'I' : nessun controllo     *
      *              *-------------------------------------------------*
           if        w-tes-cod-lng        =    "I  "
                     go to pre-acc-mod-999.
      *              *-------------------------------------------------*
      *              * Ricerca del corrispondente nella tabella con    *
      *              * codice 'I'                                      *
      *              *-------------------------------------------------*
           move      w-tes-num-spf        to   w-ric-cod-ita-spf      .
           perform   ric-cod-ita-000      thru ric-cod-ita-999        .
      *              *-------------------------------------------------*
      *              * Test su flag di uscita                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se uscita con errore                        *
      *                  *---------------------------------------------*
           if        w-ric-cod-ita-flg    =    spaces
                     go to pre-acc-mod-500.
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio di errore        *
      *                      *-----------------------------------------*
           move      "Manca la corrispondente tabella in italiano     "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Flag di errore in uscita                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-pre-acc-mod      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-mod-999.
       pre-acc-mod-500.
      *                  *---------------------------------------------*
      *                  * Se uscita senza errore                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti             *
      *                      *-----------------------------------------*
           move      w-ric-cod-ita-ves    to   w-tes-des-ves (1)      .
           move      w-ric-cod-ita-vri    to   w-tes-des-vri (1)      .
           move      w-ric-cod-ita-tfu    to   w-tes-tfu-spe (1)      .
           move      w-ric-cod-ita-per    to   w-tes-per-spe (1)      .
           move      w-ric-cod-ita-ibl    to   w-tes-ibl-spe (1)      .
           move      w-ric-cod-ita-ibx (1)
                                          to   w-tes-ibx-spe (1, 1)   .
           move      w-ric-cod-ita-ibx (2)
                                          to   w-tes-ibx-spe (1, 2)   .
           move      w-ric-cod-ita-ibx (3)
                                          to   w-tes-ibx-spe (1, 3)   .
           move      w-ric-cod-ita-ibx (4)
                                          to   w-tes-ibx-spe (1, 4)   .
           move      w-ric-cod-ita-ibx (5)
                                          to   w-tes-ibx-spe (1, 5)   .
           move      w-ric-cod-ita-ibx (6)
                                          to   w-tes-ibx-spe (1, 6)   .
           move      w-ric-cod-ita-ibx (7)
                                          to   w-tes-ibx-spe (1, 7)   .
           move      w-ric-cod-ita-ibx (8)
                                          to   w-tes-ibx-spe (1, 8)   .
           move      w-ric-cod-ita-ibx (9)
                                          to   w-tes-ibx-spe (1, 9)   .
           move      w-ric-cod-ita-imp    to   w-tes-imp-spe (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori letti            *
      *                      *-----------------------------------------*
           perform   vis-des-ves-000      thru vis-des-ves-999        .
           perform   vis-des-vri-000      thru vis-des-vri-999        .
           perform   vis-tfu-spe-000      thru vis-tfu-spe-999        .
           perform   vis-per-spe-000      thru vis-per-spe-999        .
           perform   vis-ibl-spe-000      thru vis-ibl-spe-999        .
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-mod-999.
       pre-acc-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per visualizzazione              *
      *    *-----------------------------------------------------------*
       pre-acc-vis-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-vis      .
      *              *-------------------------------------------------*
      *              * Se lingua con codice 'I' : nessun controllo     *
      *              *-------------------------------------------------*
           if        w-tes-cod-lng        =    "I  "
                     go to pre-acc-vis-999.
      *              *-------------------------------------------------*
      *              * Ricerca del corrispondente nella tabella con    *
      *              * codice 'I'                                      *
      *              *-------------------------------------------------*
           move      w-tes-num-spf        to   w-ric-cod-ita-spf      .
           perform   ric-cod-ita-000      thru ric-cod-ita-999        .
      *              *-------------------------------------------------*
      *              * Test su flag di uscita                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se uscita con errore                        *
      *                  *---------------------------------------------*
           if        w-ric-cod-ita-flg    =    spaces
                     go to pre-acc-vis-500.
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio di errore        *
      *                      *-----------------------------------------*
           move      "Manca la corrispondente tabella in italiano     "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Flag di errore in uscita                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-pre-acc-vis      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-vis-999.
       pre-acc-vis-500.
      *                  *---------------------------------------------*
      *                  * Se uscita senza errore                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti             *
      *                      *-----------------------------------------*
           move      w-ric-cod-ita-ves    to   w-tes-des-ves (1)      .
           move      w-ric-cod-ita-vri    to   w-tes-des-vri (1)      .
           move      w-ric-cod-ita-tfu    to   w-tes-tfu-spe (1)      .
           move      w-ric-cod-ita-per    to   w-tes-per-spe (1)      .
           move      w-ric-cod-ita-ibl    to   w-tes-ibl-spe (1)      .
           move      w-ric-cod-ita-ibx (1)
                                          to   w-tes-ibx-spe (1, 1)   .
           move      w-ric-cod-ita-ibx (2)
                                          to   w-tes-ibx-spe (1, 2)   .
           move      w-ric-cod-ita-ibx (3)
                                          to   w-tes-ibx-spe (1, 3)   .
           move      w-ric-cod-ita-ibx (4)
                                          to   w-tes-ibx-spe (1, 4)   .
           move      w-ric-cod-ita-ibx (5)
                                          to   w-tes-ibx-spe (1, 5)   .
           move      w-ric-cod-ita-ibx (6)
                                          to   w-tes-ibx-spe (1, 6)   .
           move      w-ric-cod-ita-ibx (7)
                                          to   w-tes-ibx-spe (1, 7)   .
           move      w-ric-cod-ita-ibx (8)
                                          to   w-tes-ibx-spe (1, 8)   .
           move      w-ric-cod-ita-ibx (9)
                                          to   w-tes-ibx-spe (1, 9)   .
           move      w-ric-cod-ita-imp    to   w-tes-imp-spe (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori letti            *
      *                      *-----------------------------------------*
           perform   vis-des-ves-000      thru vis-des-ves-999        .
           perform   vis-des-vri-000      thru vis-des-vri-999        .
           perform   vis-tfu-spe-000      thru vis-tfu-spe-999        .
           perform   vis-per-spe-000      thru vis-per-spe-999        .
           perform   vis-ibl-spe-000      thru vis-ibl-spe-999        .
           perform   vis-imp-spe-000      thru vis-imp-spe-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-vis-999.
       pre-acc-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
       pos-exi-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su modifica                             *
      *    *-----------------------------------------------------------*
       pos-exi-mod-000.
       pos-exi-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su visualizzazione                      *
      *    *-----------------------------------------------------------*
       pos-exi-vis-000.
       pos-exi-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di inserimento                      *
      *    *-----------------------------------------------------------*
       pos-cnf-ins-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
      *              *-------------------------------------------------*
      *              * Delete movimento da files                       *
      *              *-------------------------------------------------*
           perform   del-mov-fil-000      thru del-mov-fil-999        .
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [ysf]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [ysf]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-ysf-000      thru wrt-rec-ysf-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [ysf]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-ysf-000      thru rew-rec-ysf-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [ysf]                             *
      *              *-------------------------------------------------*
           perform   del-rec-ysf-000      thru del-rec-ysf-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [ysf]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-ysf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-num-spf        to   rf-ysf-num-spf         .
           move      w-tes-cod-lng        to   rf-ysf-cod-lng         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        =    "I  "
                     go to cmp-rec-ysf-200.
           move      spaces               to   rf-ysf-des-ves         .
           move      spaces               to   rf-ysf-des-vri         .
           move      zero                 to   rf-ysf-tfu-spe         .
           move      zero                 to   rf-ysf-per-spe         .
           move      zero                 to   rf-ysf-ibl-spe         .
           move      zero                 to   rf-ysf-imp-spe         .
           move      zero                 to   rf-ysf-civ-spe         .
           move      zero                 to   rf-ysf-ccp-spe         .
           go to     cmp-rec-ysf-500.
       cmp-rec-ysf-200.
           move      w-tes-des-ves (1)    to   rf-ysf-des-ves         .
           move      w-tes-des-vri (1)    to   rf-ysf-des-vri         .
           move      w-tes-tfu-spe (1)    to   rf-ysf-tfu-spe         .
           move      w-tes-per-spe (1)    to   rf-ysf-per-spe         .
           move      w-tes-ibl-spe (1)    to   rf-ysf-ibl-spe         .
           move      w-tes-ibx-spe (1, 1) to   rf-ysf-ibx-spe (1)     .
           move      w-tes-ibx-spe (1, 2) to   rf-ysf-ibx-spe (2)     .
           move      w-tes-ibx-spe (1, 3) to   rf-ysf-ibx-spe (3)     .
           move      w-tes-ibx-spe (1, 4) to   rf-ysf-ibx-spe (4)     .
           move      w-tes-ibx-spe (1, 5) to   rf-ysf-ibx-spe (5)     .
           move      w-tes-ibx-spe (1, 6) to   rf-ysf-ibx-spe (6)     .
           move      w-tes-ibx-spe (1, 7) to   rf-ysf-ibx-spe (7)     .
           move      w-tes-ibx-spe (1, 8) to   rf-ysf-ibx-spe (8)     .
           move      w-tes-ibx-spe (1, 9) to   rf-ysf-ibx-spe (9)     .
           move      w-tes-imp-spe (1)    to   rf-ysf-imp-spe         .
           move      w-tes-civ-spe (1)    to   rf-ysf-civ-spe         .
           move      w-tes-ccp-spe (1)    to   rf-ysf-ccp-spe         .
       cmp-rec-ysf-500.
           move      w-tes-des-stp (1)    to   rf-ysf-des-stp         .
           move      w-tes-alx-exp (1)    to   rf-ysf-alx-exp         .
       cmp-rec-ysf-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [ysf]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-ysf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ysf-000      thru cmp-rec-ysf-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
       wrt-rec-ysf-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [ysf]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-ysf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ysf-000      thru cmp-rec-ysf-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
       rew-rec-ysf-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [ysf]                                *
      *    *-----------------------------------------------------------*
       del-rec-ysf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ysf-000      thru cmp-rec-ysf-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
       del-rec-ysf-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [ysf]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-ysf-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcf0310"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-ysf-sel
                     go to  fnd-arc-ysf-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per possibili- *
      *              * ta' di function-key "SLCT" durante l'interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/prg/obj/pdcf0310"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-spf"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-ysf-sel
                     move  s-num          to   w-fnd-arc-ysf-spf
           else      move  "#"            to   w-fnd-arc-ysf-sel      .
           move      "CV"                 to   s-ope                  .
           move      "cod-lng"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-ysf-sel
                     move  s-alf          to   w-fnd-arc-ysf-lng
           else      move  "#"            to   w-fnd-arc-ysf-sel      .
       fnd-arc-ysf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zln]                         *
      *    *-----------------------------------------------------------*
       let-arc-zln-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zln-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice lingua a spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zln-cod    =    spaces
                     go to let-arc-zln-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODLNG"             to   f-key                  .
           move      w-let-arc-zln-cod    to   rf-zln-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzln"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zln                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zln-400.
       let-arc-zln-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zln-des-lng       to   w-let-arc-zln-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zln-999.
       let-arc-zln-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zln-flg      .
           move      all   "."            to   w-let-arc-zln-des      .
           go to     let-arc-zln-999.
       let-arc-zln-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zln-des      .
       let-arc-zln-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
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
           move      13                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      15                   to   v-pos                  .
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
           move      64                   to   v-pos                  .
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
           move      65                   to   v-pos                  .
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
      *    * Box per messaggio di errore in pre-esecuzione programma   *
      *    *-----------------------------------------------------------*
       box-pre-exe-000.
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
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Attenzione : Le personalizzazioni non consentono l
      -              "'utilizzo di questo"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "programma."         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per presa visione                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione 'OK' di presa visione              *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       box-pre-exe-100.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(20)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT"
                     go to box-pre-exe-900.
           if        v-alf                not  = "OK"
                     go to box-pre-exe-100.
       box-pre-exe-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-pre-exe-999.
           exit.

      *    *===========================================================*
      *    * Ricerca del corrispondente nella tabella con codice 'I'   *
      *    *-----------------------------------------------------------*
       ric-cod-ita-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di ricerca                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-ric-cod-ita-flg      .
      *              *-------------------------------------------------*
      *              * Lettura della tabella [ysf]                     *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-ric-cod-ita-spf    to   rf-ysf-num-spf         .
           move      "I  "                to   rf-ysf-cod-lng         .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato : uscita con flag *
      *                      * di errore                               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-ric-cod-ita-flg
                     go to ric-cod-ita-999.
      *                      *-----------------------------------------*
      *                      * Se record trovato : preparazione dati   *
      *                      * da visualizzare                         *
      *                      *-----------------------------------------*
           move      rf-ysf-des-ves       to   w-ric-cod-ita-ves      .
           move      rf-ysf-des-vri       to   w-ric-cod-ita-vri      .
           move      rf-ysf-tfu-spe       to   w-ric-cod-ita-tfu      .
           move      rf-ysf-per-spe       to   w-ric-cod-ita-per      .
           move      rf-ysf-ibl-spe       to   w-ric-cod-ita-ibl      .
           move      rf-ysf-ibx-spe (1)   to   w-ric-cod-ita-ibx (1)  .
           move      rf-ysf-ibx-spe (2)   to   w-ric-cod-ita-ibx (2)  .
           move      rf-ysf-ibx-spe (3)   to   w-ric-cod-ita-ibx (3)  .
           move      rf-ysf-ibx-spe (4)   to   w-ric-cod-ita-ibx (4)  .
           move      rf-ysf-ibx-spe (5)   to   w-ric-cod-ita-ibx (5)  .
           move      rf-ysf-ibx-spe (6)   to   w-ric-cod-ita-ibx (6)  .
           move      rf-ysf-ibx-spe (7)   to   w-ric-cod-ita-ibx (7)  .
           move      rf-ysf-ibx-spe (8)   to   w-ric-cod-ita-ibx (8)  .
           move      rf-ysf-ibx-spe (9)   to   w-ric-cod-ita-ibx (9)  .
           move      rf-ysf-imp-spe       to   w-ric-cod-ita-imp      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     ric-cod-ita-999.
       ric-cod-ita-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice lingua          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzln0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

