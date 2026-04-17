       Identification Division.
       Program-Id.                                 pcge0400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    tab                 *
      *                                   Fase:    cge040              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/12/90    *
      *                       Ultima revisione:    NdK del 31/10/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella codici iva                 *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * CONVENZIONE    :   Per introdurre la gestione dei Codici Iva   *
      * IMPORTANTE         con i decimali, si e' scelto che gli even-  *
      * ===========        tuali decimali (massimo 2) vengano posizio- *
      *                    nati nei primi due caratteri del codice.    *
      *                                                                *
      *                    Alcuni esempi:                              *
      *                    --------------                              *
      *                                                                *
      *                    Aliquota 11,5%  - codice 50011 - 1 decimale *
      *                    Aliquota 23,4%  - codice 40023 - 1 decimale *
      *                    Aliquota 23,45% - codice 45023 - 2 decimali *
      *                                                                *
      *                    Aliquota 23,4% - non detraibile - 40723     *
      *                    Aliquota 23,4% - omaggio        - 40923     *
      *                                                                *
      *                    Questo si e' reso necessario per evitare di *
      *                    confondere le aliquote con decimali con i   *
      *                    codici di esenzione Iva (>100 900<)         *
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
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge040"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge0400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      GESTIONE TABELLA CODICI IVA       "       .

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
      *            *---------------------------------------------------*
      *            * Flag di uscita per il tasto 'Exit'                *
      *            *---------------------------------------------------*
               10  w-cnt-acc-flg-exi      pic  x(01)                  .
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
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [zno]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzno"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-iva          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-doc          pic  x(15)                  .
               10  w-tes-com-int          pic  x(40)                  .
               10  w-tes-cod-mne          pic  x(05)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-leg          pic  x(40)                  .
               10  w-tes-des-int          pic  x(40)                  .
               10  w-tes-dec-ali          pic  9(01)                  .
               10  w-tes-ali-iva          pic  9(05)                  .
               10  w-tes-cod-nop          pic  x(05)                  .
               10  w-tes-cod-nop-des      pic  x(80)                  .
               10  w-tes-flg-ecf          pic  x(01)                  .
               10  w-tes-snx-lqp          pic  x(01)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 73  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per accettazioni                                *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione codice Iva                      *
      *        *-------------------------------------------------------*
           05  w-acc-cod-iva              pic  9(03)v9(02)            .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zno]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zno.
               10  w-let-arc-zno-flg      pic  x(01)                  .
               10  w-let-arc-zno-cod      pic  x(05)                  .
               10  w-let-arc-zno-des      pic  x(80)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/no in liquidazione periodica            *
      *        *-------------------------------------------------------*
           05  w-exp-snx-lqp.
               10  w-exp-snx-lqp-num      pic  9(02)       value 2    .
               10  w-exp-snx-lqp-lun      pic  9(02)       value 02   .
               10  w-exp-snx-lqp-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no imposta di bollo virtuale            *
      *        *-------------------------------------------------------*
           05  w-exp-snx-ibv.
               10  w-exp-snx-ibv-num      pic  9(02)       value 2    .
               10  w-exp-snx-ibv-lun      pic  9(02)       value 02   .
               10  w-exp-snx-ibv-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione natura operazione              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acdezno0.acl"                   .

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
                     go to exe-acc-cmp-600.
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
       exe-acc-cmp-600.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-850.
      *                  *---------------------------------------------*
      *                  * Subroutine di controllo per Exit            *
      *                  *---------------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
      *                  *---------------------------------------------*
      *                  * Se Exit non attivato : a ripristino para-   *
      *                  * metri di impostazione                       *
      *                  *---------------------------------------------*
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to exe-acc-cmp-400.
      *                  *---------------------------------------------*
      *                  * Altrimenti : ad uscita                      *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-900.
       exe-acc-cmp-850.
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *                                                           *
      *    * Subroutine di controllo tasto 'Exit'                      *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-exi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-exi      .
       exe-acc-cmp-exi-050.
      *              *-------------------------------------------------*
      *              * Se impostazione campi chiave : si ignora        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se in visualizzazione : si ignora               *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se non avvenuta alcuna modifica : si ignora     *
      *              *-------------------------------------------------*
           if        w-cnt-acc-flg-aum    =    spaces
                     go to exe-acc-cmp-exi-920.
       exe-acc-cmp-exi-100.
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
      *              * Richiesta conferma di uscita                    *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#EXI"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "EXIT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se non function key EXIT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Altrimenti : ad Exit attivato                   *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-800.
       exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Se uscita senza Exit                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di Exit non attivato                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-acc-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-800.
      *              *-------------------------------------------------*
      *              * Se uscita con Exit                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-900.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-999.
       exe-acc-cmp-exi-999.
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
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione natura operazione      *
      *              *-------------------------------------------------*
           perform   cod-des-zno-opn-000  thru cod-des-zno-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione natura operazione     *
      *              *-------------------------------------------------*
           perform   cod-des-zno-cls-000  thru cod-des-zno-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              *-------------------------------------------------*
      *              * [zno]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzno"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zno                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              *-------------------------------------------------*
      *              * [zno]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzno"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zno                 .
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
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-iva-000      thru acc-cod-iva-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
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
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
           perform   vis-cod-iva-000      thru vis-cod-iva-999        .
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
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
           perform   pmt-cod-iva-000      thru pmt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice iva                    *
      *    *-----------------------------------------------------------*
       pmt-cod-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice iva                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice iva                    *
      *    *-----------------------------------------------------------*
       acc-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Comodo di accettazione                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-acc-cod-iva          .
       acc-cod-iva-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zci-ope      .
           move      w-acc-cod-iva        to   w-cod-mne-zci-cod      .
           move      04                   to   w-cod-mne-zci-lin      .
           move      30                   to   w-cod-mne-zci-pos      .
           move      07                   to   w-cod-mne-zci-dln      .
           move      30                   to   w-cod-mne-zci-dps      .
           move      "<BD"                to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
       acc-cod-iva-110.
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           if        w-cod-mne-zci-ope    =    "F+"
                     go to acc-cod-iva-115.
           if        w-cod-mne-zci-ope    =    "AC"
                     go to acc-cod-iva-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-iva-115.
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
           go to     acc-cod-iva-110.
       acc-cod-iva-120.
           move      w-cod-mne-zci-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-iva-999.
       acc-cod-iva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-cod-iva          .
       acc-cod-iva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-iva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-iva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-iva-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-iva-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-iva-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-iva-999.
       acc-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice iva                 *
      *    *-----------------------------------------------------------*
       vis-cod-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-acc-cod-iva        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-iva-999.
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
      *                      *-----------------------------------------*
      *                      * Subroutine di controllo per Exit        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Exit non attivato : come 'Up'    *
      *                          *-------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to acc-nok-reg-870.
      *                          *-------------------------------------*
      *                          * Se Exit attivato                    *
      *                          *-------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Abilitazione tasto Do                           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Descrizione per documenti                   *
      *                  *---------------------------------------------*
           perform   acc-des-doc-000      thru acc-des-doc-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Commento ad uso interno                     *
      *                  *---------------------------------------------*
           perform   acc-com-int-000      thru acc-com-int-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Natura operazione                           *
      *                  *---------------------------------------------*
           perform   acc-cod-nop-000      thru acc-cod-nop-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Si/no in liquidazione periodica             *
      *                  *---------------------------------------------*
           perform   acc-snx-lqp-000      thru acc-snx-lqp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Si/no imposta di bollo virtuale             *
      *                  *---------------------------------------------*
           perform   acc-snx-ibv-000      thru acc-snx-ibv-999        .
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
      *              * Descrizione per documenti                       *
      *              *-------------------------------------------------*
           perform   vis-des-doc-000      thru vis-des-doc-999        .
      *              *-------------------------------------------------*
      *              * Aliquota                                        *
      *              *-------------------------------------------------*
           perform   vis-ali-iva-000      thru vis-ali-iva-999        .
      *              *-------------------------------------------------*
      *              * Decimali aliquota                               *
      *              *-------------------------------------------------*
           perform   vis-dec-ali-000      thru vis-dec-ali-999        .
      *              *-------------------------------------------------*
      *              * Commento ad uso interno                         *
      *              *-------------------------------------------------*
           perform   vis-com-int-000      thru vis-com-int-999        .
      *              *-------------------------------------------------*
      *              * Natura operazione                               *
      *              *-------------------------------------------------*
           perform   vis-cod-nop-000      thru vis-cod-nop-999        .
           perform   vis-cod-nop-des-000  thru vis-cod-nop-des-999    .
      *              *-------------------------------------------------*
      *              * Si/no in liquidazione periodica                 *
      *              *-------------------------------------------------*
           perform   vis-snx-lqp-000      thru vis-snx-lqp-999        .
      *              *-------------------------------------------------*
      *              * Si/no imposta di bollo virtuale                 *
      *              *-------------------------------------------------*
           perform   vis-snx-ibv-000      thru vis-snx-ibv-999        .
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
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Legenda                                         *
      *              *-------------------------------------------------*
           perform   pmt-leg-end-000      thru pmt-leg-end-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per documenti                       *
      *              *-------------------------------------------------*
           perform   pmt-des-doc-000      thru pmt-des-doc-999        .
      *              *-------------------------------------------------*
      *              * Aliquota                                        *
      *              *-------------------------------------------------*
           perform   pmt-ali-iva-000      thru pmt-ali-iva-999        .
      *              *-------------------------------------------------*
      *              * Decimali aliquota                               *
      *              *-------------------------------------------------*
           perform   pmt-dec-ali-000      thru pmt-dec-ali-999        .
      *              *-------------------------------------------------*
      *              * Commento ad uso interno                         *
      *              *-------------------------------------------------*
           perform   pmt-com-int-000      thru pmt-com-int-999        .
      *              *-------------------------------------------------*
      *              * Natura operazione                               *
      *              *-------------------------------------------------*
           perform   pmt-cod-nop-000      thru pmt-cod-nop-999        .
      *              *-------------------------------------------------*
      *              * Si/no in liquidazione periodica                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-lqp-000      thru pmt-snx-lqp-999        .
      *              *-------------------------------------------------*
      *              * Si/no imposta di bollo virtuale                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-ibv-000      thru pmt-snx-ibv-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Legenda                          *
      *    *-----------------------------------------------------------*
       pmt-leg-end-000.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------------------------------+---------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea legenda 1.                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "da   1 a  99 - % imponibili             | I seguen
      -              "ti codici Iva non esistono in "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea legenda 2.                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "da 301 a 399 - Non imponibili           | tabella 
      -              "ma possono essere utilizzati  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea legenda 3.                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "da 401 a 499 - Esenti                   | --------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea legenda 4.                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "da 501 a 599 - Esclusi                  | 7xx = no
      -              "n detraibile per aliquota xx  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea legenda 5.                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "da 601 a 699 - Fuori campo iva          | 9xx = so
      -              "la IVA senza imponibile per xx"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-leg-end-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per documenti        *
      *    *-----------------------------------------------------------*
       pmt-des-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione per documenti  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Aliquota                         *
      *    *-----------------------------------------------------------*
       pmt-ali-iva-000.
       pmt-ali-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Decimali aliquota                *
      *    *-----------------------------------------------------------*
       pmt-dec-ali-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Decimali                   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dec-ali-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Commento ad uso interno          *
      *    *-----------------------------------------------------------*
       pmt-com-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Commento ad uso interno    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-com-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Natura operazione                *
      *    *-----------------------------------------------------------*
       pmt-cod-nop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Natura operazione IVA      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-nop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no in liquidazione periodica  *
      *    *-----------------------------------------------------------*
       pmt-snx-lqp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "In liquidazione periodica  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-lqp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no imposta di bollo virtuale  *
      *    *-----------------------------------------------------------*
       pmt-snx-ibv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Bollo virtuale :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ibv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Descrizione per documenti            *
      *    *-----------------------------------------------------------*
       acc-des-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se % imponibile                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    not  = zero
                     go to acc-des-doc-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione descrizione                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-des-doc (1)      .
      *                  *---------------------------------------------*
      *                  * Editing codice Iva                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .    
           move      03                   to   w-all-str-num          .    
           move      "Aliquota"           to   w-all-str-cat (1)      .    
           move      v-edt                to   w-all-str-cat (2)      .    
           move      "%"                  to   w-all-str-cat (3)      .    
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-tes-des-doc (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-doc-000      thru vis-des-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-des-doc-999.
       acc-des-doc-050.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-tes-des-doc (1)    not  = spaces
                     go to acc-des-doc-070.
           if        w-edt-iva-cod-003    =    6
                     move   "Fuori campo iva"
                                          to   w-tes-des-doc (1)      .
       acc-des-doc-070.
      *                  *---------------------------------------------*
      *                  * Visualizzazione note operative              *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           if        w-edt-iva-cod-003    =    3
                     move   "Impostare titolo di non imponibilita'"
                                          to   v-not
           else if   w-edt-iva-cod-003    =    4
                     move   "Impostare titolo di esenzione"
                                          to   v-not
           else if   w-edt-iva-cod-003    =    5
                     move   "Impostare titolo di esclusione"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-des-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-doc (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-doc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-doc-999.
       acc-des-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-doc (1)      .
       acc-des-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-doc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-doc-100.
       acc-des-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per documenti *
      *    *-----------------------------------------------------------*
       vis-des-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-doc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Aliquota                          *
      *    *-----------------------------------------------------------*
       vis-ali-iva-000.
       vis-ali-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decimali aliquota         *
      *    *-----------------------------------------------------------*
       vis-dec-ali-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    not  = zero
                     go to vis-dec-ali-999.
       vis-dec-ali-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dec-ali (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dec-ali-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Commento ad uso interno      *
      *    *-----------------------------------------------------------*
       acc-com-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se % imponibile                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    zero
                     go to acc-com-int-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione note operative              *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Indicare l'eventuale Riferimento Normativo per le 
      -              "fatture elettroniche"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-com-int-100.
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
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-com-int (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
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
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-com-int-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-com-int-999.
       acc-com-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-com-int (1)      .
       acc-com-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-com-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-com-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-com-int-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-com-int-100.
       acc-com-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Commento ad uso interno   *
      *    *-----------------------------------------------------------*
       vis-com-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-com-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-com-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Natura operazione                    *
      *    *-----------------------------------------------------------*
       acc-cod-nop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    zero
                     go to acc-cod-nop-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-tes-cod-nop (1)    not  = spaces
                     go to acc-cod-nop-100.
      *                  *---------------------------------------------*
      *                  * Ridefinizione codice Iva                    *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    zero
                     move  spaces         to   w-tes-cod-nop (1)
           else if   w-tes-cod-iva        =    3
                     move  "N3"           to   w-tes-cod-nop (1)
           else if   w-tes-cod-iva        =    4
                     move  "N4"           to   w-tes-cod-nop (1)
           else if   w-tes-cod-iva        =    5
                     move  "N1"           to   w-tes-cod-nop (1)
           else if   w-tes-cod-iva        =    6
                     move  "N2"           to   w-tes-cod-nop (1)
           else      move  spaces         to   w-tes-cod-nop (1)      .
       acc-cod-nop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zno-ope      .
           move      w-tes-cod-nop (1)    to   w-cod-des-zno-cod      .
           move      13                   to   w-cod-des-zno-lin      .
           move      30                   to   w-cod-des-zno-pos      .
           move      13                   to   w-cod-des-zno-dln      .
           move      36                   to   w-cod-des-zno-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zno-cll-000  thru cod-des-zno-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zno-foi-000  thru cod-des-zno-foi-999    .
       acc-cod-nop-110.
           perform   cod-des-zno-cll-000  thru cod-des-zno-cll-999    .
           if        w-cod-des-zno-ope    =    "F+"
                     go to acc-cod-nop-115.
           if        w-cod-des-zno-ope    =    "AC"
                     go to acc-cod-nop-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-nop-115.
           perform   cod-des-zno-foi-000  thru cod-des-zno-foi-999    .
           go to     acc-cod-nop-110.
       acc-cod-nop-120.
           move      w-cod-des-zno-cod    to   v-alf                  .
       acc-cod-nop-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-nop-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-nop-999.
       acc-cod-nop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-nop (1)      .
       acc-cod-nop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura descrizione                         *
      *                  *---------------------------------------------*
           move      w-tes-cod-nop (1)    to   w-let-arc-zno-cod      .
           perform   let-arc-zno-000      thru let-arc-zno-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zno-des    to   w-tes-cod-nop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-nop-des-000  thru vis-cod-nop-des-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente o  *
      *                  * non esistente                               *
      *                  *---------------------------------------------*
           if        w-let-arc-zno-flg    =    spaces
                     go to acc-cod-nop-450.
       acc-cod-nop-425.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-nop-100.
       acc-cod-nop-450.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
       acc-cod-nop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-nop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-nop-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-nop-100.
       acc-cod-nop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Natura operazione                 *
      *    *-----------------------------------------------------------*
       vis-cod-nop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-nop (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-nop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Natura operazione, descrizione    *
      *    *-----------------------------------------------------------*
       vis-cod-nop-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-nop-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-nop-des (1)
                    (41 : 40)             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-nop-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no in liquidazione periodica      *
      *    *-----------------------------------------------------------*
       acc-snx-lqp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    zero
                     move "S"             to   w-tes-snx-lqp (1)
                     perform   vis-snx-lqp-000
                                          thru vis-snx-lqp-999
                     go to acc-snx-lqp-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-tes-cod-nop (1)    not  = spaces
                     go to acc-snx-lqp-100.
           move      "S"                  to   w-tes-snx-lqp (1)      .
       acc-snx-lqp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-lqp-lun    to   v-car                  .
           move      w-exp-snx-lqp-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-lqp-tbl    to   v-txt                  .
      *
           if        w-tes-snx-lqp (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-lqp (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lqp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lqp-999.
       acc-snx-lqp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-lqp (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-lqp (1)
           else      move  spaces         to   w-tes-snx-lqp (1)      .
       acc-snx-lqp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-lqp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-lqp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-lqp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-lqp-100.
       acc-snx-lqp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no in liquidazione periodica   *
      *    *-----------------------------------------------------------*
       vis-snx-lqp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-lqp-lun    to   v-car                  .
           move      w-exp-snx-lqp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-lqp-tbl    to   v-txt                  .
      *
           if        w-tes-snx-lqp (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-lqp (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-lqp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no imposta di bollo virtuale      *
      *    *-----------------------------------------------------------*
       acc-snx-ibv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    zero
                     move "N"             to   w-tes-flg-ecf (1)
                     perform   vis-snx-ibv-000
                                          thru vis-snx-ibv-999
                     go to acc-snx-ibv-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-tes-flg-ecf (1)    not  = spaces
                     go to acc-snx-ibv-100.
           if        w-tes-cod-nop (1)    = "N2.2 " or
                     w-tes-cod-nop (1)    = "N3.1 " or
                     w-tes-cod-nop (1)    = "N3.2 "
                     move  "N"            to   w-tes-flg-ecf (1)      .
       acc-snx-ibv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ibv-lun    to   v-car                  .
           move      w-exp-snx-ibv-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-exp-snx-ibv-tbl    to   v-txt                  .
      *
           if        w-tes-flg-ecf (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-flg-ecf (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ibv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ibv-999.
       acc-snx-ibv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-flg-ecf (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-flg-ecf (1)
           else      move  spaces         to   w-tes-flg-ecf (1)      .
       acc-snx-ibv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-ibv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ibv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ibv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ibv-100.
       acc-snx-ibv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no imposta di bollo virtuale   *
      *    *-----------------------------------------------------------*
       vis-snx-ibv-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ibv-lun    to   v-car                  .
           move      w-exp-snx-ibv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-exp-snx-ibv-tbl    to   v-txt                  .
      *
           if        w-tes-flg-ecf (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-flg-ecf (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ibv-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cps"                   .

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
      *              * Test su congruenza codice Iva                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero: nessun controllo                   *
      *                  *---------------------------------------------*
           if        w-acc-cod-iva        =    zero
                     go to  cnt-tdo-key-800.
      *                  *---------------------------------------------*
      *                  * Ridefinizione codice Iva                    *
      *                  *---------------------------------------------*
           move      w-acc-cod-iva        to   w-edt-iva-cie          .
      *                  *---------------------------------------------*
      *                  * Se aliquota imponibile                      *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-i01    =    zero
                     go to  cnt-tdo-key-800.
      *                  *---------------------------------------------*
      *                  * Se aliquota non imponibile                  *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-i01    =    3
                     go to  cnt-tdo-key-800.
      *                  *---------------------------------------------*
      *                  * Se aliquota esente                          *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-i01    =    4
                     go to  cnt-tdo-key-800.
      *                  *---------------------------------------------*
      *                  * Se aliquota esclusa                         *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-i01    =    5
                     go to  cnt-tdo-key-800.
      *                  *---------------------------------------------*
      *                  * Se aliquota fuori campo iva                 *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-i01    =    6
                     go to  cnt-tdo-key-800.
       cnt-tdo-key-100.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Codice iva formalmente scorretto !                
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Box di errore                               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-key-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-key-999.
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
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-acc-cod-iva        =    zero
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
      *              * Controllo su descrizione per documenti          *
      *              *-------------------------------------------------*
           if        w-tes-des-doc (1)    not  = spaces
                     go to cnt-tdo-nok-600.
           if        w-acc-cod-iva        <    100
                     go to cnt-tdo-nok-600.
           move      "Manca la descrizione per i documenti !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione interna            *
      *                  *---------------------------------------------*
           move      w-tes-des-doc (1)    to   w-tes-des-int (1)      .
      *                  *---------------------------------------------*
      *                  * Preparazione denominazione in uppercase per *
      *                  * l'ordinamento                               *
      *                  *---------------------------------------------*
           move      w-tes-des-doc (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
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
      *                  *---------------------------------------------*
      *                  * Box di errore                               *
      *                  *---------------------------------------------*
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
           move      zero                 to   w-tes-cod-iva          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-doc (1)      .
           move      spaces               to   w-tes-com-int (1)      .
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-leg (1)      .
           move      spaces               to   w-tes-des-int (1)      .
           move      zero                 to   w-tes-dec-ali (1)      .
           move      zero                 to   w-tes-ali-iva (1)      .
           move      spaces               to   w-tes-cod-nop (1)      .
           move      spaces               to   w-tes-cod-nop-des (1)  .
           move      spaces               to   w-tes-flg-ecf (1)      .
           move      spaces               to   w-tes-snx-lqp (1)      .
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
      *              * Trattamento preliminare codice                  *
      *              *-------------------------------------------------*
           move      w-acc-cod-iva        to   w-edt-iva-cie          .
           perform   edt-iva-cod-000      thru edt-iva-cod-999        .
           move      w-edt-iva-cod        to   w-tes-cod-iva          .
       rou-let-reg-020.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODIVA    "         to   f-key                  .
           move      w-tes-cod-iva        to   rf-zci-cod-iva         .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
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
      *                      * Trattamento aliquota, decimali e mne-   *
      *                      * monico                                  *
      *                      *-----------------------------------------*
           perform   rou-let-reg-adm-000  thru rou-let-reg-adm-999    .
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
      *                      * Trattamento aliquota, decimali e mne-   *
      *                      * monico                                  *
      *                      *-----------------------------------------*
           perform   rou-let-reg-adm-000  thru rou-let-reg-adm-999    .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [zci]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Gestiti                         *
      *                              *---------------------------------*
           move      rf-zci-des-doc       to   w-tes-des-doc (1)      .
           move      rf-zci-com-int       to   w-tes-com-int (1)      .
           move      rf-zci-cod-nop       to   w-tes-cod-nop (1)      .
           move      rf-zci-flg-ecf       to   w-tes-flg-ecf (1)      .
           move      rf-zci-snx-lqp       to   w-tes-snx-lqp (1)      .
      *                              *---------------------------------*
      *                              * Non gestiti                     *
      *                              *---------------------------------*
           move      rf-zci-des-key       to   w-tes-des-key (1)      .
           move      rf-zci-des-leg       to   w-tes-des-leg (1)      .
           move      rf-zci-des-int       to   w-tes-des-int (1)      .
           move      rf-zci-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zci]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura natura operazione       *
      *                              *---------------------------------*
           move      w-tes-cod-nop (1)    to   w-let-arc-zno-cod      .
           perform   let-arc-zno-000      thru let-arc-zno-999        .
           move      w-let-arc-zno-des    to   w-tes-cod-nop-des (1)  .
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
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Subroutine per trattamento aliquota, decimali e mnemonico *
      *    *-----------------------------------------------------------*
       rou-let-reg-adm-000.
      *              *-------------------------------------------------*
      *              * Ridefinizione codice Iva                        *
      *              *-------------------------------------------------*
           move      w-tes-cod-iva        to   w-edt-iva-cod          .
       rou-let-reg-adm-100.
      *              *-------------------------------------------------*
      *              * Test se codice di esenzione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulla 3 cifra                          *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-003    =    zero
                     go to rou-let-reg-adm-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-dec-ali (1)      .
           move      zero                 to   w-tes-ali-iva (1)      .
      *                  *---------------------------------------------*
      *                  * Mnemonico: si pone la natura operazione     *
      *                  *---------------------------------------------*
           move      rf-zci-cod-nop       to   w-tes-cod-mne (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-let-reg-adm-900.
       rou-let-reg-adm-200.
      *              *-------------------------------------------------*
      *              * Trattamento decimali aliquota                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulle prime 2 cifre                    *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-001    not  = zero
                     add   1              to   w-tes-dec-ali (1)      .
           if        w-edt-iva-cod-002    not  = zero
                     add   1              to   w-tes-dec-ali (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione decimali aliquota           *
      *                  *---------------------------------------------*
           perform   vis-dec-ali-000      thru vis-dec-ali-999        .
       rou-let-reg-adm-400.
      *              *-------------------------------------------------*
      *              * Trattamento aliquota                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interi                                      *
      *                  *---------------------------------------------*
           move      w-edt-iva-cod-045    to   w-tes-ali-iva (1)      .   
      *                  *---------------------------------------------*
      *                  * Eventuali decimali                          *
      *                  *---------------------------------------------*
           if        w-tes-dec-ali (1)    =    1
                     multiply 10          by   w-tes-ali-iva (1)
                     move     w-edt-iva-cod-001
                                          to   w-tes-ali-iva (1)
                                              (05 : 01)
           else if   w-tes-dec-ali (1)    =    2
                     multiply 100         by   w-tes-ali-iva (1)
                     move     w-edt-iva-cod-001
                                          to   w-tes-ali-iva (1)
                                              (04 : 01)
                     move     w-edt-iva-cod-002
                                          to   w-tes-ali-iva (1)
                                              (05 : 01)               .
       rou-let-reg-adm-480.
      *                  *---------------------------------------------*
      *                  * Visualizzazione aliquota                    *
      *                  *---------------------------------------------*
           perform   vis-ali-iva-000      thru vis-ali-iva-999        .
       rou-let-reg-adm-600.
      *              *-------------------------------------------------*
      *              * Trattamento mnemonico                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing aliquota                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      05                   to   v-car                  .
           subtract  w-tes-dec-ali (1)    from v-car                  .
           move      w-tes-dec-ali (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-ali-iva (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      v-edt                to   w-tes-cod-mne (1)      .
       rou-let-reg-adm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-adm-999.
       rou-let-reg-adm-999.
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
      *              * Trattamento file [zci]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zci]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zci-000      thru wrt-rec-zci-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zci]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zci-000      thru rew-rec-zci-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zci]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zci-000      thru del-rec-zci-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zci]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zci-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva        to   rf-zci-cod-iva         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-doc (1)    to   rf-zci-des-doc         .
           move      w-tes-com-int (1)    to   rf-zci-com-int         .
           move      w-tes-cod-mne (1)    to   rf-zci-cod-mne         .
           move      w-tes-des-key (1)    to   rf-zci-des-key         .
           move      w-tes-des-leg (1)    to   rf-zci-des-leg         .
           move      w-tes-des-int (1)    to   rf-zci-des-int         .
           move      w-tes-dec-ali (1)    to   rf-zci-dec-ali         .
           move      w-tes-ali-iva (1)    to   rf-zci-ali-iva         .
           move      w-tes-flg-ecf (1)    to   rf-zci-flg-ecf         .
           move      w-tes-snx-lqp (1)    to   rf-zci-snx-lqp         .
           move      w-tes-cod-nop (1)    to   rf-zci-cod-nop         .
           move      w-tes-alx-exp (1)    to   rf-zci-alx-exp         .
       cmp-rec-zci-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zci]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zci-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zci-000      thru cmp-rec-zci-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       wrt-rec-zci-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zci]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zci-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zci-000      thru cmp-rec-zci-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       rew-rec-zci-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zci]                                *
      *    *-----------------------------------------------------------*
       del-rec-zci-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zci-000      thru cmp-rec-zci-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       del-rec-zci-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zno]                             *
      *    *-----------------------------------------------------------*
       let-arc-zno-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zno-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a Spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zno-cod    =    spaces
                     go to let-arc-zno-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNOP    "         to   f-key                  .
           move      w-let-arc-zno-cod    to   rf-zno-cod-nop         .
           move      "pgm/cge/fls/ioc/obj/iofzno"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zno                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zno-400.
       let-arc-zno-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zno-des-nop       to   w-let-arc-zno-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zno-999.
       let-arc-zno-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zno-flg      .
           move      all   "."            to   w-let-arc-zno-des      .
           go to     let-arc-zno-999.
       let-arc-zno-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zno-des      .
       let-arc-zno-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione natura operazione          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acdezno0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

