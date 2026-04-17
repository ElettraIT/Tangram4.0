       Identification Division.
       Program-Id.                                 pdcc4800           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcc480              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/01/01    *
      *                       Ultima revisione:    NdK del 12/02/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione annotazioni clienti per documenti  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Nota importante:   Per le fatture elettroniche viene trattato  *
      *                    l'apposito record [cse]                     *
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
                     "dcc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "com"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcc480"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcc4800"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "           ANNOTAZIONI CLIENTI          "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcx"                          .
      *        *-------------------------------------------------------*
      *        * [cse]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfcse"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-cli          pic  9(07)                  .
               10  w-tes-cod-cli-rag      pic  x(40)                  .
               10  w-tes-dpz-cli          pic  x(04)                  .
               10  w-tes-dpz-cli-rag      pic  x(40)                  .
               10  w-tes-cod-ann          pic  9(03)                  .
               10  w-tes-cod-ann-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ann-cli.
                   15  w-tes-rig-cli occurs 10
                                          pic  x(40)                  .
               10  w-tes-cod-prg.
                   15  w-tes-cod-prg-dcc  pic  x(01)                  .
                   15  w-tes-cod-prg-orc  pic  x(01)                  .
                   15  w-tes-cod-prg-ods  pic  x(01)                  .
                   15  w-tes-cod-prg-bol  pic  x(01)                  .
                   15  w-tes-cod-prg-fat  pic  x(01)                  .
                   15  w-tes-cod-prg-xml  pic  x(01)                  .
                   15  w-tes-cod-prg-gep  pic  x(01)                  .
                   15  w-tes-cod-prg-tel  pic  x(01)                  .
               10  w-tes-dat-ini          pic  9(07)                  .
               10  w-tes-dat-fin          pic  9(07)                  .
               10  w-tes-cod-ang          pic  9(03)                  .
               10  w-tes-snx-stp          pic  x(01)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 76   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice cliente                   *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cli              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti dipendenze per il cliente    *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcc.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il cliente   commerciale ha dipendenze  *
      *            * - N : No, il cliente   commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente   commerciale                      *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il cliente   *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-dpz      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Work per record [dcx]                                 *
      *        *-------------------------------------------------------*
           05  w-det-rec-dcx.
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-rec-dcx-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice programma di comodo                        *
      *            *---------------------------------------------------*
               10  w-det-rec-dcx-prg      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Work per record [cse]                                 *
      *        *-------------------------------------------------------*
           05  w-det-rec-cse.
      *            *---------------------------------------------------*
      *            * Codice 'tag' di comodo                            *
      *            *---------------------------------------------------*
               10  w-det-rec-cse-tag      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Si/no di comodo                                   *
      *            *---------------------------------------------------*
               10  w-det-rec-cse-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Nota di comodo                                    *
      *            *---------------------------------------------------*
               10  w-det-rec-cse-not      pic  x(80)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Programma                                  *
      *        *-------------------------------------------------------*
           05  w-exp-cod-prg.
               10  w-exp-cod-prg-num      pic  9(02)       value 8    .
               10  w-exp-cod-prg-lun      pic  9(02)       value 30   .
               10  w-exp-cod-prg-tbl.
                   15  filler             pic  x(30) value
                            "[ ] Anagrafica commerciale    "          .
                   15  filler             pic  x(30) value
                            "[ ] Ordini                    "          .
                   15  filler             pic  x(30) value
                            "[ ] Spedizioni                "          .
                   15  filler             pic  x(30) value
                            "[ ] Bolle                     "          .
                   15  filler             pic  x(30) value
                            "[ ] Fatture                   "          .
                   15  filler             pic  x(30) value
                            "[ ] Fatture elettroniche      "          .
                   15  filler             pic  x(30) value
                            "[ ] Portafoglio               "          .
                   15  filler             pic  x(30) value
                            "[ ] Rubrica telefonica        "          .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No in stampa                            *
      *        *-------------------------------------------------------*
           05  w-exp-snx-stp.
               10  w-exp-snx-stp-num      pic  9(02)       value 03   .
               10  w-exp-snx-stp-lun      pic  9(02)       value 05   .
               10  w-exp-snx-stp-tbl.
                   15  filler             pic  x(05) value "No   "    .
                   15  filler             pic  x(05) value "Anche"    .
                   15  filler             pic  x(05) value "Solo "    .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [dcx]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-dcx.
               10  w-fnd-arc-dcx-gen      pic  x(01)                  .
               10  w-fnd-arc-dcx-sel      pic  x(01)                  .
               10  w-fnd-arc-dcx-cod      pic  9(03)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcx.
               10  w-let-arc-dcx-flg      pic  x(01)                  .
               10  w-let-arc-dcx-cod      pic  9(03)                  .
               10  w-let-arc-dcx-ann.
                   15  w-let-arc-dcx-rig occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(56)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-dcx.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-dcx-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-dcx-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-dcx-val-pos          pic  9(07)                  .

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
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
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
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              * [dcx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *              *-------------------------------------------------*
      *              * [cse]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * [dcx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *              *-------------------------------------------------*
      *              * [cse]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
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
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza cliente                   *
      *                  *---------------------------------------------*
           perform   acc-dpz-cli-000      thru acc-dpz-cli-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice annotazione                          *
      *                  *---------------------------------------------*
           perform   acc-cod-ann-000      thru acc-cod-ann-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-200.
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
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza cliente                       *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice annotazione                              *
      *              *-------------------------------------------------*
           perform   vis-cod-ann-000      thru vis-cod-ann-999        .
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
           move      07                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza cliente                       *
      *              *-------------------------------------------------*
           perform   pmt-dpz-cli-000      thru pmt-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice annotazione                              *
      *              *-------------------------------------------------*
           perform   pmt-cod-ann-000      thru pmt-cod-ann-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice cliente                *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice dipendenza cliente     *
      *    *-----------------------------------------------------------*
       pmt-dpz-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice dipendenza cliente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice annotazione            *
      *    *-----------------------------------------------------------*
       pmt-cod-ann-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice annotazione         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-ann-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice cliente                *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-sav-cod-cli          .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Codice cliente a zero per annotazioni generiche"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-cod-cli        to   w-cod-mne-dcc-cod      .
           move      04                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      04                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
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
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cli          .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record da file [dcc]                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-cod-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-cod-cli-410
           else      go to acc-cod-cli-450.
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore del   *
      *                      * codice dipendenza                       *
      *                      *-----------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to acc-cod-cli-415
           else      go to acc-cod-cli-420.
       acc-cod-cli-415.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a spaces           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione forzata dipendenza  *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-dpz-cli-rag      .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     acc-cod-cli-800.
       acc-cod-cli-420.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a non-spaces       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura codice dipendenza a Spa-  *
      *                          * ces con visualizzazione             *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-dpz-cli          .
           move      spaces               to   w-tes-dpz-cli-rag      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice dipendenza   *
      *                          *-------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-450.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero   *
      *                  *---------------------------------------------*
       acc-cod-cli-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso da quello   *
      *                      * precedente si forza il codice dipenden- *
      *                      * za a Spaces e lo si visualizza          *
      *                      *-----------------------------------------*
           if        w-tes-cod-cli        =    w-sav-cod-cli
                     go to acc-cod-cli-460.
           if        w-tes-dpz-cli        =    spaces
                     go to acc-cod-cli-460.
           move      spaces               to   w-tes-dpz-cli          .
           move      spaces               to   w-tes-dpz-cli-rag      .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
       acc-cod-cli-460.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * lettura anagrafica contabile clienti    *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-465
           else      go to acc-cod-cli-470.
       acc-cod-cli-465.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-470.
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il cliente commerciale                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-det-snd-dcc-cli      .
           perform   det-snd-dcc-000      thru det-snd-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale segnale di cliente con dipendenze *
      *                  *---------------------------------------------*
           perform   vis-snd-dcc-000      thru vis-snd-dcc-999        .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-cli-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-cli-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice cliente             *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale cliente a  *
      *    *                                fianco del codice cliente  *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cli-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Segnale di cliente con di- *
      *    *                                dipendenze                 *
      *    *-----------------------------------------------------------*
       vis-snd-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      80                   to   v-pos                  .
           if        w-det-snd-dcc-snx    =    "S"
                     move  "*"            to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice dipendenza             *
      *    *-----------------------------------------------------------*
       acc-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-dpz-cli-999.
           if        w-det-snd-dcc-snx    not  = "S"
                     go to acc-dpz-cli-999.
       acc-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "'*' per annotazioni relative a tutte le dipendenze
      -              ""                   to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "A*"                 to   w-cod-cod-dcc-ope      .
           move      w-tes-cod-cli        to   w-cod-cod-dcc-cli      .
           move      w-tes-dpz-cli        to   w-cod-cod-dcc-cod      .
           move      05                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      09                   to   w-cod-cod-dcc-rln      .
           move      30                   to   w-cod-cod-dcc-rps      .
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
       acc-dpz-cli-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-cli-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-cli-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-cli-110.
       acc-dpz-cli-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
       acc-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-dpz-cli          .
       acc-dpz-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se '*'                                      *
      *                  *---------------------------------------------*
           if        w-tes-dpz-cli        not  = "*   "
                     go to acc-dpz-cli-500.
           move      "Tutte le dipendenze"
                                          to   w-tes-dpz-cli-rag      .
           go to     acc-dpz-cli-560.
       acc-dpz-cli-500.
      *                  *---------------------------------------------*
      *                  * Lettura record da file [dcc]                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-dcc-cli      .
           move      w-tes-dpz-cli        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-cli-rag      .
       acc-dpz-cli-560.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
       acc-dpz-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dpz-cli-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dpz-cli-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
       acc-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice dipendenza          *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpz-cli        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale dipendenza *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-cli-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice annotazione            *
      *    *-----------------------------------------------------------*
       acc-cod-ann-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-ann-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      w-tes-cod-ann        to   v-num                  .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-ann-999.
       acc-cod-ann-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-ann          .
       acc-cod-ann-200.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-ann-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [dcx]                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-fnd-arc-dcx-gen      .
           perform   fnd-arc-dcx-000      thru fnd-arc-dcx-999        .
           if        w-fnd-arc-dcx-sel    not  = spaces
                     go to acc-cod-ann-100.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione e visualizzazione valore    *
      *                  *---------------------------------------------*
           move      w-fnd-arc-dcx-cod    to   w-tes-cod-ann          .
           perform   vis-cod-ann-000      thru vis-cod-ann-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-cod-ann-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-ann        =    zero
                     go to acc-cod-ann-410
           else      go to acc-cod-ann-500.
       acc-cod-ann-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se codice cliente a zero           *
      *                      *-----------------------------------------*
           if        w-tes-cod-cli        not  = zero
                     go to acc-cod-ann-600.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      999                  to   w-enc-dcx-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-dcx-val-pos    to   w-tes-cod-ann          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-ann-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-ann-000      thru vis-cod-ann-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-ann-600.
       acc-cod-ann-500.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna azione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-ann-600.
       acc-cod-ann-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-ann-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-ann-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-ann-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-ann-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-ann-999.
       acc-cod-ann-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice annotazione         *
      *    *-----------------------------------------------------------*
       vis-cod-ann-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-ann        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-ann-999.
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
      *                  * Codice annotazione generica                 *
      *                  *---------------------------------------------*
           perform   acc-cod-ang-000      thru acc-cod-ang-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Annotazioni                                 *
      *                  *---------------------------------------------*
           perform   acc-ann-cli-000      thru acc-ann-cli-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           perform   acc-cod-prg-000      thru acc-cod-prg-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Si/no in stampa                             *
      *                  *---------------------------------------------*
           perform   acc-snx-stp-000      thru acc-snx-stp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Data finale                                 *
      *                  *---------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
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
      *              * Codice annotazione generica                     *
      *              *-------------------------------------------------*
           perform   vis-cod-ang-000      thru vis-cod-ang-999        .
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   vis-ann-cli-000      thru vis-ann-cli-999        .
      *              *-------------------------------------------------*
      *              * Programma                                       *
      *              *-------------------------------------------------*
           perform   vis-cod-prg-000      thru vis-cod-prg-999        .
      *              *-------------------------------------------------*
      *              * Si/no in stampa                                 *
      *              *-------------------------------------------------*
           perform   vis-snx-stp-000      thru vis-snx-stp-999        .
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   vis-dat-ini-000      thru vis-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   vis-dat-fin-000      thru vis-dat-fin-999        .
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
           move      08                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice annotazione generica                     *
      *              *-------------------------------------------------*
           perform   pmt-cod-ang-000      thru pmt-cod-ang-999        .
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   pmt-ann-cli-000      thru pmt-ann-cli-999        .
      *              *-------------------------------------------------*
      *              * Programma                                       *
      *              *-------------------------------------------------*
           perform   pmt-cod-prg-000      thru pmt-cod-prg-999        .
      *              *-------------------------------------------------*
      *              * Si/no in stampa                                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-stp-000      thru pmt-snx-stp-999        .
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice annotazione generica      *
      *    *-----------------------------------------------------------*
       pmt-cod-ang-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Annotazione codificata     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-ang-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Annotazioni                      *
      *    *-----------------------------------------------------------*
       pmt-ann-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Testo dell'annotazione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ann-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Programma                     *
      *    *-----------------------------------------------------------*
       pmt-cod-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "In trattamento             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-prg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/no in stampa               *
      *    *-----------------------------------------------------------*
       pmt-snx-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "In stampa :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-snx-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data iniziale                    *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "A partire              dal :"
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
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   fino al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice annotazione generica   *
      *    *-----------------------------------------------------------*
       acc-cod-ang-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-cod-ang-999.
       acc-cod-ang-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      w-tes-cod-ang (1)    to   v-num                  .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-ang-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-ang-999.
       acc-cod-ang-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-ang (1)      .
       acc-cod-ang-200.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-ang-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [dcx]                      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-dcx-gen      .
           perform   fnd-arc-dcx-000      thru fnd-arc-dcx-999        .
           if        w-fnd-arc-dcx-sel    not  = spaces
                     go to acc-cod-ang-100.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione e visualizzazione valore    *
      *                  *---------------------------------------------*
           move      w-fnd-arc-dcx-cod    to   w-tes-cod-ang (1)      .
           perform   vis-cod-ang-000      thru vis-cod-ang-999        .
       acc-cod-ang-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-ang (1)    =    zero
                     go to acc-cod-ang-410
           else      go to acc-cod-ang-500.
       acc-cod-ang-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna azione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-ang-600.
       acc-cod-ang-500.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura tabella [dcx]                   *
      *                      *-----------------------------------------*
           move      w-tes-cod-ang (1)    to   w-let-arc-dcx-cod      .
           perform   let-arc-dcx-000      thru let-arc-dcx-999        .
           move      w-let-arc-dcx-ann    to   w-tes-ann-cli (1)      .
           perform   vis-ann-cli-000      thru vis-ann-cli-999        .
      *                      *-----------------------------------------*
      *                      * Nessuna azione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-ang-600.
       acc-cod-ang-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-ang-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-ang-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-ang-100.
       acc-cod-ang-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice annotaz. generica   *
      *    *-----------------------------------------------------------*
       vis-cod-ang-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-ang (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-ang-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Annotazioni                  *
      *    *-----------------------------------------------------------*
       acc-ann-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-ang (1)    not  = zero
                     go to acc-ann-cli-999.
       acc-ann-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      "X"                  to   v-edm                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ann-cli (1)    to   v-txt                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ann-cli-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ann-cli-999.
       acc-ann-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-tes-ann-cli (1)      .
       acc-ann-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-ann-cli (1)    =    spaces
                     go to acc-ann-cli-100.
       acc-ann-cli-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-ann-cli (1)
                    (01 : 01)             =    spaces
                     go to acc-ann-cli-100.
       acc-ann-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ann-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ann-cli-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ann-cli-100.
       acc-ann-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Annotazioni               *
      *    *-----------------------------------------------------------*
       vis-ann-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 1) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 2) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 3) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 4) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 5) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 6) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 7) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 8) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 9) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-cli (1, 10)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Programma                     *
      *    *-----------------------------------------------------------*
       acc-cod-prg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-cod-prg-999.
       acc-cod-prg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-cod-prg-lun    to   v-car                  .
           move      w-exp-cod-prg-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      "X"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-cod-prg-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-prg (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-prg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-prg-999.
       acc-cod-prg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-prg (1)      .
       acc-cod-prg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso                     *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg (1)    =    spaces
                     go to acc-cod-prg-100.
       acc-cod-prg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-prg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-prg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-prg-100.
       acc-cod-prg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Programma                  *
      *    *-----------------------------------------------------------*
       vis-cod-prg-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to vis-cod-prg-999.
      *
           move      "DS"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-cod-prg-lun    to   v-car                  .
           move      w-exp-cod-prg-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-cod-prg-tbl    to   v-txt                  .
           move      w-tes-cod-prg (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-prg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no in stampa                      *
      *    *-----------------------------------------------------------*
       acc-snx-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-snx-stp-999.
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-gep (1)
                                          not  = spaces or
                     w-tes-cod-prg-gep (1)
                                          not  = spaces or
                     w-tes-cod-prg-tel (1)
                                          not  = spaces
                     move     "N"         to   w-tes-snx-stp (1)
                     perform  vis-snx-stp-000
                                          thru vis-snx-stp-999
                     go to acc-snx-stp-999.
      *
           if        w-tes-cod-prg-xml (1)
                                          not  = spaces
                     move     "P"         to   w-tes-snx-stp (1)
                     perform  vis-snx-stp-000
                                          thru vis-snx-stp-999
                     go to acc-snx-stp-999.
       acc-snx-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-stp-lun    to   v-car                  .
           move      w-exp-snx-stp-num    to   v-ldt                  .
           move      "NAS#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-exp-snx-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        w-tes-snx-stp (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-stp (1)    =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-stp (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-snx-stp (1)    =    "P"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-stp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-stp-999.
       acc-snx-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-tes-snx-stp (1)
           else if   v-num                =    02
                     move  "S"            to   w-tes-snx-stp (1)
           else if   v-num                =    03
                     move  "P"            to   w-tes-snx-stp (1)
           else      move  "N"            to   w-tes-snx-stp (1)      .
       acc-snx-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-stp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-stp-100.
       acc-snx-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no in stampa                   *
      *    *-----------------------------------------------------------*
       vis-snx-stp-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to vis-snx-stp-999.
      *
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-stp-lun    to   v-car                  .
           move      w-exp-snx-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-exp-snx-stp-tbl    to   v-txt                  .
      *
           if        w-tes-snx-stp (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-stp (1)    =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-stp (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-snx-stp (1)    =    "P"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data iniziale                *
      *    *-----------------------------------------------------------*
       acc-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-dat-ini-999.
       acc-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dat-ini (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-ini-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-ini-999.
       acc-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-ini (1)      .
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
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-ini-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-ini-100.
       acc-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data iniziale             *
      *    *-----------------------------------------------------------*
       vis-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dat-ini (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data finale                  *
      *    *-----------------------------------------------------------*
       acc-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-dat-fin-999.
       acc-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dat-fin (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-fin-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-fin-999.
       acc-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-fin (1)      .
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
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-fin-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-fin-100.
       acc-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data finale               *
      *    *-----------------------------------------------------------*
       vis-dat-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dat-fin (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Controllo codice cliente                        *
      *              *-------------------------------------------------*
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Controllo codice annotazione                    *
      *              *-------------------------------------------------*
           if        w-tes-cod-ann        not  = zero
                     go to cnt-tdo-key-800.
           move      "Manca il codice annotazione !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-key-999.
       cnt-tdo-key-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-key-flg      .
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
      *              * Controllo su annotazioni                        *
      *              *-------------------------------------------------*
           if        w-tes-ann-cli (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Mancano le annotazioni !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su data iniziale - finale             *
      *              *-------------------------------------------------*
           if        w-tes-dat-ini (1)       =    zero or
                     w-tes-dat-fin (1)       =    zero
                     go to cnt-tdo-nok-800.
           if        w-tes-dat-ini (1)       not  > w-tes-dat-fin (1)
                     go to cnt-tdo-nok-800.
           move      "Data iniziale maggiore di quella finale !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
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
           move      zero                 to   w-tes-cod-cli          .
           move      spaces               to   w-tes-cod-cli-rag      .
           move      spaces               to   w-tes-dpz-cli          .
           move      spaces               to   w-tes-dpz-cli-rag      .
           move      zero                 to   w-tes-cod-ann          .
           move      spaces               to   w-tes-cod-ann-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-ann-cli (1)      .
           move      all spaces           to   w-tes-cod-prg (1)      .
           move      zero                 to   w-tes-dat-ini (1)      .
           move      zero                 to   w-tes-dat-fin (1)      .
           move      zero                 to   w-tes-cod-ang (1)      .
           move      spaces               to   w-tes-snx-stp (1)      .
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
      *              * Normalizzazione contatore record letti          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rec-dcx-ctr      .
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Start su [dcx]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-cli        to   rf-dcx-cod-cli         .
           move      w-tes-dpz-cli        to   rf-dcx-dpz-cli         .
           move      w-tes-cod-ann        to   rf-dcx-cod-ann         .
           move      spaces               to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-600.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dcx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-600.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Test max su [dcx]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-cli       not  = w-tes-cod-cli
                     go to  rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza cliente           *
      *                  *---------------------------------------------*
           if        rf-dcx-dpz-cli       not  = w-tes-dpz-cli
                     go to  rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Test su codice annotazione                  *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-ann       not  = w-tes-cod-ann
                     go to  rou-let-reg-600.
       rou-let-reg-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dcx]                              *
      *              *-------------------------------------------------*
       rou-let-reg-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-rec-dcx-ctr      .
       rou-let-reg-520.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-prg       =    "pdcc4000"
                     move  "X"            to   w-tes-cod-prg-dcc (1)
           else if   rf-dcx-cod-prg       =    "porc3000"
                     move  "X"            to   w-tes-cod-prg-orc (1)
           else if   rf-dcx-cod-prg       =    "pods3000"
                     move  "X"            to   w-tes-cod-prg-ods (1)
           else if   rf-dcx-cod-prg       =    "pbol3000"
                     move  "X"            to   w-tes-cod-prg-bol (1)
           else if   rf-dcx-cod-prg       =    "pfat3000"
                     move  "X"            to   w-tes-cod-prg-fat (1)
           else if   rf-dcx-cod-prg       =    "mfatxml0"
                     move  "X"            to   w-tes-cod-prg-xml (1)
           else if   rf-dcx-cod-prg       =    "pgep3000"
                     move  "X"            to   w-tes-cod-prg-gep (1)
           else if   rf-dcx-cod-prg       =    "pxpg0012"
                     move  "X"            to   w-tes-cod-prg-tel (1)  .
      *                  *---------------------------------------------*
      *                  * Altri dati                                  *
      *                  *---------------------------------------------*
           move      rf-dcx-ann-cli       to   w-tes-ann-cli (1)      .
           move      rf-dcx-dat-ini       to   w-tes-dat-ini (1)      .
           move      rf-dcx-dat-fin       to   w-tes-dat-fin (1)      .
           move      rf-dcx-cod-ang       to   w-tes-cod-ang (1)      .
      *
           if        rf-dcx-cod-ang       not  numeric
                     move  zero           to   w-tes-cod-ang (1)      .
      *
           move      rf-dcx-snx-stp       to   w-tes-snx-stp (1)      .
           move      rf-dcx-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-580.
      *              *-------------------------------------------------*
      *              * Riciclo su [dcx]                                *
      *              *-------------------------------------------------*
           go to     rou-let-reg-200.
       rou-let-reg-600.
      *              *-------------------------------------------------*
      *              * Test su contatore record letti                  *
      *              *-------------------------------------------------*
           if        w-det-rec-dcx-ctr    >    zero
                     go to rou-let-reg-840.
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Se non trovati records                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se inserimento consentito              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-810.
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-810.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Inserimento            *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-840.
      *              *-------------------------------------------------*
      *              * Se non trovati records                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori contenuti indirettamente             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura tabella [dcx]                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-ang (1)    =    zero
                     go to rou-let-reg-860.
           move      w-tes-cod-ang (1)    to   w-let-arc-dcx-cod      .
           perform   let-arc-dcx-000      thru let-arc-dcx-999        .
           move      w-let-arc-dcx-ann    to   w-tes-ann-cli (1)      .
       rou-let-reg-860.
      *                  *---------------------------------------------*
      *                  * Valori precedenti                           *
      *                  *---------------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Test per visualizzazione                    *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-let-reg-900.
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
       pre-acc-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
      *              *-------------------------------------------------*
      *              * Se e' stata eseguita l'attribuzione del codice  *
      *              * in automatico, si ripristina, se possibile, il  *
      *              * codice al valore precedente l'incremento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non attribuzione automatica : uscita     *
      *                  *---------------------------------------------*
           if        w-tes-cod-ann-aut    =    spaces
                     go to pos-exi-ins-999.
      *                  *---------------------------------------------*
      *                  * Ripristino codice automatico progressivo    *
      *                  *---------------------------------------------*
           perform   rip-cod-aut-000      thru rip-cod-aut-999        .
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
      *              * Trattamento file [dcx]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [dcx]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-dcx-000      thru wrt-rec-dcx-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [dcx]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-dcx-000      thru rew-rec-dcx-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [dcx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dcx-000      thru del-rec-dcx-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record [dcx]                              *
      *    *-----------------------------------------------------------*
       nor-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
       nor-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dcx]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   rf-dcx-cod-cli         .
           move      w-tes-dpz-cli        to   rf-dcx-dpz-cli         .
           move      w-tes-cod-ann        to   rf-dcx-cod-ann         .
      *                  *---------------------------------------------*
      *                  * Data, utente, fase, di ultimo inserimento o *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dcx-ide-dat         .
           move      s-ute                to   rf-dcx-ide-ute         .
           move      s-fas                to   rf-dcx-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-ann-cli (1)    to   rf-dcx-ann-cli         .
           move      w-tes-dat-ini (1)    to   rf-dcx-dat-ini         .
           move      w-tes-dat-fin (1)    to   rf-dcx-dat-fin         .
           move      w-tes-cod-ang (1)    to   rf-dcx-cod-ang         .
           move      w-tes-snx-stp (1)    to   rf-dcx-snx-stp         .
           move      w-tes-alx-exp (1)    to   rf-dcx-alx-exp         .
       cmp-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dcx]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Delete record [dcx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dcx-000      thru del-rec-dcx-999        .
      *              *-------------------------------------------------*
      *              * Scrittura record [dcx]                          *
      *              *-------------------------------------------------*
           perform   scr-rec-dcx-000      thru scr-rec-dcx-999        .
       wrt-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dcx]                                    *
      *    *-----------------------------------------------------------*
       scr-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Composizione record tabella codificata          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        not  = zero
                     go to scr-rec-dcx-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     scr-rec-dcx-900.
       scr-rec-dcx-050.
      *              *-------------------------------------------------*
      *              * Composizione record - pdcc4000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-dcc (1)
                                          =    spaces
                     go to scr-rec-dcx-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pdcc4000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-100.
      *              *-------------------------------------------------*
      *              * Composizione record - porc3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-orc (1)
                                          =    spaces
                     go to scr-rec-dcx-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "porc3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-200.
      *              *-------------------------------------------------*
      *              * Composizione record - pods3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-ods (1)
                                          =    spaces
                     go to scr-rec-dcx-300.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pods3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-300.
      *              *-------------------------------------------------*
      *              * Composizione record - pbol3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-bol (1)
                                          =    spaces
                     go to scr-rec-dcx-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbol3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-400.
      *              *-------------------------------------------------*
      *              * Composizione record - pfat3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-fat (1)
                                          =    spaces
                     go to scr-rec-dcx-450.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pfat3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-450.
      *              *-------------------------------------------------*
      *              * Composizione record - mfatxml0                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-xml (1)
                                          =    spaces
                     go to scr-rec-dcx-500.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "mfatxml0"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento 'tag' per fattura elettronica   *
      *                  *---------------------------------------------*
           perform   wrt-rec-cse-000      thru wrt-rec-cse-999        .
       scr-rec-dcx-500.
      *              *-------------------------------------------------*
      *              * Composizione record - pgep3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-gep (1)
                                          =    spaces
                     go to scr-rec-dcx-600.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pgep3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-600.
      *              *-------------------------------------------------*
      *              * Composizione record - pxpg0012                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-tel (1)
                                          =    spaces
                     go to scr-rec-dcx-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pxpg0012"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dcx-000      thru put-rec-dcx-999        .
       scr-rec-dcx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-rec-dcx-999.
       scr-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Put record [dcx]                                          *
      *    *-----------------------------------------------------------*
       put-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
       put-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [dcx]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Delete record [dcx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dcx-000      thru del-rec-dcx-999        .
      *              *-------------------------------------------------*
      *              * Write record [dcx]                              *
      *              *-------------------------------------------------*
           perform   wrt-rec-dcx-000      thru wrt-rec-dcx-999        .
       rew-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [dcx]                                *
      *    *-----------------------------------------------------------*
       del-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Composizione record tabella codificata          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        not  = zero
                     go to del-rec-dcx-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     del-rec-dcx-900.
       del-rec-dcx-050.
      *              *-------------------------------------------------*
      *              * Composizione record - pdcc4000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pdcc4000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-100.
      *              *-------------------------------------------------*
      *              * Composizione record - porc3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "porc3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-200.
      *              *-------------------------------------------------*
      *              * Composizione record - pods3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pods3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-300.
      *              *-------------------------------------------------*
      *              * Composizione record - pbol3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbol3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-400.
      *              *-------------------------------------------------*
      *              * Composizione record - pfat3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pfat3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-450.
      *              *-------------------------------------------------*
      *              * Composizione record - mfatxml0                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "mfatxml0"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record [dcx]                         *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record [cse]                         *
      *                  *---------------------------------------------*
           perform   del-rec-cse-000      thru del-rec-cse-999        .
       del-rec-dcx-500.
      *              *-------------------------------------------------*
      *              * Composizione record - pgep3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pgep3000"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-600.
      *              *-------------------------------------------------*
      *              * Composizione record - pxpg0012                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dcx-000      thru nor-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pxpg0012"           to   rf-dcx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dcx-000      thru cmp-rec-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dcx-000      thru dlt-rec-dcx-999        .
       del-rec-dcx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-rec-dcx-999.
       del-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [dcx]                                *
      *    *-----------------------------------------------------------*
       dlt-rec-dcx-000.
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
       dlt-rec-dcx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione 'tag' fattura elettronica                   *
      *    *-----------------------------------------------------------*
       del-rec-cse-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Altri dati gestionali'              *
      *              *-------------------------------------------------*
           move      "2.2.1.16            "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      spaces               to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       del-rec-cse-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Tipo dato'                          *
      *              *-------------------------------------------------*
           move      "2.2.1.16.1          "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      "NOTA"               to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       del-rec-cse-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Riferimento Testo'                  *
      *              *-------------------------------------------------*
           move      "2.2.1.16.2          "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      w-tes-ann-cli (1)    to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       del-rec-cse-999.
           exit.

      *    *===========================================================*
      *    * Scrittura 'tag' fattura elettronica                       *
      *    *-----------------------------------------------------------*
       wrt-rec-cse-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Altri dati gestionali'              *
      *              *-------------------------------------------------*
           move      "2.2.1.16            "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      spaces               to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       wrt-rec-cse-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Tipo dato'                          *
      *              *-------------------------------------------------*
           move      "2.2.1.16.1          "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      "NOTA"               to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       wrt-rec-cse-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cse]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Put record 'Riferimento Testo'                  *
      *              *-------------------------------------------------*
           move      "2.2.1.16.2          "
                                          to   w-det-rec-cse-tag      .
           move      "S"                  to   w-det-rec-cse-snx      .
           move      w-tes-ann-cli (1)    to   w-det-rec-cse-not      .
           perform   cmp-rec-cse-000      thru cmp-rec-cse-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
       wrt-rec-cse-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [cse]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-cse-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofcse"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cse                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   rf-cse-cod-cli         .
           move      w-tes-dpz-cli        to   rf-cse-dpz-cli         .
           move      w-det-rec-cse-tag    to   rf-cse-cod-tag         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-det-rec-cse-snx    to   rf-cse-snx-tag         .
           move      w-det-rec-cse-not    to   rf-cse-pma-alf         .
       cmp-rec-cse-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se presenti dipendenze per il cliente      *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcc-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcc-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcc-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-det-snd-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-999.
       det-snd-dcc-100.
      *              *-------------------------------------------------*
      *              * Next su [dcc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Max su [dcc], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcc-cod-cli       not  = w-det-snd-dcc-cli
                     go to det-snd-dcc-800.
       det-snd-dcc-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcc-dpz-cli       =    spaces
                     go to det-snd-dcc-100.
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcc-ctr      .
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcc-ctr    >    1
                     go to det-snd-dcc-500.
           move      rf-dcc-dpz-cli       to   w-det-snd-dcc-dpz      .
       det-snd-dcc-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcc] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcc-100.
       det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcc-ctr    >    zero
                     go to det-snd-dcc-900
           else      go to det-snd-dcc-999.
       det-snd-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcc-snx      .
       det-snd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [dcx]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-dcx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-dcx-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcx4810"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-dcx-sel
                     go to  fnd-arc-dcx-999.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice     *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
      *
           if        w-fnd-arc-dcx-gen    =    spaces
                     move  w-tes-cod-cli  to   s-num
           else      move  zero           to   s-num                  .
      *
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza del cliente                            *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dpz-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
      *
           if        w-fnd-arc-dcx-gen    =    spaces
                     move  w-tes-dpz-cli  to   s-alf
           else      move  spaces         to   s-alf                  .
      *
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/dcc/prg/obj/pdcc4810"
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
      *                  *---------------------------------------------*
      *                  * Codice annotazione                          *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-ann"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to fnd-arc-dcx-900.
      *                  *---------------------------------------------*
      *                  * Valore letto                                *
      *                  *---------------------------------------------*
           move      s-num                to   w-fnd-arc-dcx-cod      .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-dcx-999.
       fnd-arc-dcx-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-dcx-sel      .
       fnd-arc-dcx-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
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
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all"."               to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-999.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcx]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-dcx-cod    =    zero
                     go to let-arc-dcx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      zero                 to   rf-dcx-cod-cli         .
           move      spaces               to   rf-dcx-dpz-cli         .
           move      w-let-arc-dcx-cod    to   rf-dcx-cod-ann         .
           move      spaces               to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcx-400.
       let-arc-dcx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcx-ann-cli       to   w-let-arc-dcx-ann      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcx-999.
       let-arc-dcx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcx-flg      .
           move      spaces               to   w-let-arc-dcx-ann      .
           go to     let-arc-dcx-999.
       let-arc-dcx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcx-ann      .
       let-arc-dcx-999.
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
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [dcx]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to att-cod-aut-400.
       att-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "Ep"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      s-num                to   w-enc-dcx-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-dcx-val-pre    to   w-enc-dcx-val-pos      .
           add       1                    to   w-enc-dcx-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-dcx-val-pos    =    zero
                     move  1              to   w-enc-dcx-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-dcx-val-pos    >    w-enc-dcx-val-max
                     move  1              to   w-enc-dcx-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione contatore record letti  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-rec-dcx-ctr      .
       att-cod-aut-510.
      *                      *-----------------------------------------*
      *                      * Start su [dcx]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-cli        to   rf-dcx-cod-cli         .
           move      w-tes-dpz-cli        to   rf-dcx-dpz-cli         .
           move      w-enc-dcx-val-pos    to   rf-dcx-cod-ann         .
           move      spaces               to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                          *-------------------------------------*
      *                          * Test su esito start                 *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-cod-aut-560.
       att-cod-aut-520.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale [dcx]               *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                          *-------------------------------------*
      *                          * Test su esito lettura               *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-cod-aut-560.
       att-cod-aut-530.
      *                      *-----------------------------------------*
      *                      * Test max su [dcx]                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su codice cliente              *
      *                          *-------------------------------------*
           if        rf-dcx-cod-cli       not  = w-tes-cod-cli
                     go to  att-cod-aut-560.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza cliente   *
      *                          *-------------------------------------*
           if        rf-dcx-dpz-cli       not  = w-tes-dpz-cli
                     go to  att-cod-aut-560.
      *                          *-------------------------------------*
      *                          * Test su codice annotazione          *
      *                          *-------------------------------------*
           if        rf-dcx-cod-ann       not  = w-tes-cod-ann
                     go to  att-cod-aut-560.
       att-cod-aut-540.
      *                      *-----------------------------------------*
      *                      * Selezioni su [dcx]                      *
      *                      *-----------------------------------------*
       att-cod-aut-550.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-det-rec-dcx-ctr      .
       att-cod-aut-560.
      *                      *-----------------------------------------*
      *                      * Test su contatore                       *
      *                      *-----------------------------------------*
           if        w-det-rec-dcx-ctr    > zero
                     go to att-cod-aut-600
           else      go to att-cod-aut-700.
       att-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-dcx-val-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-cod-aut-500.
       att-cod-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           move      w-enc-dcx-val-pos    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to att-cod-aut-000.
       att-cod-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice automatico progressivo       *
      *    *-----------------------------------------------------------*
       rip-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [dcx]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to rip-cod-aut-400.
       rip-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        s-num                =    w-enc-dcx-val-pos
                     go to rip-cod-aut-600.
       rip-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "Er"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento del codice automatico con *
      *                      * il valore pre incremento                *
      *                      *-----------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "dcx "               to   s-nam                  .
           move      w-enc-dcx-val-pre    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to rip-cod-aut-000.
       rip-cod-aut-999.
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
      *    * Subroutines per l'accettazione cliente commerciale        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

