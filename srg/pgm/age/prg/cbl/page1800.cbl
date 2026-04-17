       Identification Division.
       Program-Id.                                 page1800           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    tab                 *
      *                                   Fase:    age180              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/04/10    *
      *                       Ultima revisione:    NdK del 20/03/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella provvigioni a scaglioni su *
      *                    fatturato mensile                           *
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
                     "age"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "age180"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page1800"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "GESTIONE TABELLA PROVVIGIONI - FATTURATO"       .

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
      *            *---------------------------------------------------*
      *            * Per routine pre-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-snx-del      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pos-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-snx-del      pic  x(01)                  .
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
      *            * Numero pagina testata in corso di trattamento     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-npt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio per il campo precedente               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-svp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Si/No pagina testata in corso di trattamento      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-snp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagine componenti la testata               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-mpt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione pagine di testata                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-pte.
                   15  w-cnt-sts-imp-ptx  occurs 9
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ingresso in pagine di testata                     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-ing-pte.
                   15  w-cnt-sts-ing-ptx  occurs 9
                                          pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts pagine di testata         *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-pte.
                   15  w-cnt-sts-pmt-ptx  occurs 9
                                          pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Visualizzazione dati pagine di testata            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-pte.
                   15  w-cnt-sts-vis-ptx  occurs 9
                                          pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zpf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpf"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-age          pic  9(07)                  .
               10  w-tes-cod-age-nom      pic  x(20)                  .
               10  w-tes-cod-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-tip-fat          pic  9(02)                  .
               10  w-tes-tot-snx occurs 9 pic  x(01)                  .
               10  w-tes-scp-tbs.
                   15  w-tes-scp-tbe occurs 6.
                       20  w-tes-scp-tot  pic  9(11)       comp-3     .
                       20  w-tes-scp-per  pic  9(02)v9(01) comp-3     .
               10  w-tes-alx-exp          pic  x(80)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
               10  w-let-arc-age-sup      pic  9(07)                  .
               10  w-let-arc-age-cat      pic  9(05)                  .
               10  w-let-arc-age-per occurs 03 
                                          pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Work area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per trattamento codice agente                    *
      *        *-------------------------------------------------------*
           05  w-wrk-cod-age              pic  9(02) value zero       .

      *    *===========================================================*
      *    * Work area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore generico di comodo                          *
      *        *-------------------------------------------------------*
           05  w-cix-gen-001              pic  9(03)                  .
           05  w-cix-gen-002              pic  9(03)                  .

      *    *===========================================================*
      *    * Work area per accettazione tabella scaglioni              *
      *    *-----------------------------------------------------------*
       01  w-cst.
           05  w-cst-flg-exi              pic  x(01)                  .
           05  w-cst-ctr-rig              pic  9(02)                  .
           05  w-cst-cts-rig              pic  9(02)                  .
           05  w-cst-ctx-rig              pic  9(02)                  .
           05  w-cst-cty-rig              pic  9(02)                  .
           05  w-cst-ctz-rig              pic  9(02)                  .
           05  w-cst-sav-scg              pic  9(11)                  .
           05  w-cst-cln-scg              pic  x(80) value
               "                 |                           |          
      -        "       |                "                             .

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
      *              * Tasto di funzione Prsc :                        *
      *              * - se in tipo impostazione testata e su pagina   *
      *              *   maggiore di 1 : abilitato                     *
      *              * - altrimenti    : invariato                     *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T" and
                     w-cnt-sts-imp-npt    >    1
                     move  "PRSC"         to   v-pfk (07)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Nxsc :                        *
      *              * - se in tipo impostazione testata e se pagina   *
      *              *   gia' completamente impostata o testata gia'   *
      *              *   completamente impostata : abilitato           *
      *              *  - altrimenti             : invariato           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    not  = "T"
                     go to exe-acc-cmp-050.
           if        w-cnt-sts-imp-tes    not  = spaces or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     move  "NXSC"         to   v-pfk (08)             .
       exe-acc-cmp-050.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Pf4 :                         *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Impostazione corpo     : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-imp    =    "C"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (17)             .
       exe-acc-cmp-080.
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
      *              * Routine pre-richiesta di ratifica tasto Delete  *
      *              *-------------------------------------------------*
           perform   pre-snx-del-000      thru pre-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pre-snx-del    not  = spaces
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
      *              * Se non function key DELT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Routine post-richiesta di ratifica tasto Delete *
      *              *-------------------------------------------------*
           perform   pos-snx-del-000      thru pos-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pre-snx-del    not  = spaces
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Altrimenti : uscita                             *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-300.
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
      *              *-------------------------------------------------*
      *              * Set del flag di tasto Pf4                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-acc-cmp-900.
      *                  *---------------------------------------------*
      *                  * Attivazione segnale di duplicazione         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto 'Exit'                      *
      *                  *---------------------------------------------*
           move      "EXIT"               to   v-key                  .
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
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [zpf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zpf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
           move      1                    to   w-cnt-sts-imp-npt      .
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes
                                               w-cnt-sts-imp-pte
                                               w-cnt-sts-ing-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes
                                               w-cnt-sts-pmt-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes
                                               w-cnt-sts-vis-pte      .
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
      *                  * Prompts per prima pagina testata            *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
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
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Anno di competenza                          *
      *                  *---------------------------------------------*
           perform   acc-ann-fis-000      thru acc-ann-fis-999        .
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
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *              *-------------------------------------------------*
      *              * Anno di competenza                              *
      *              *-------------------------------------------------*
           perform   vis-ann-fis-000      thru vis-ann-fis-999        .
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
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Anno di competenza                              *
      *              *-------------------------------------------------*
           perform   pmt-ann-fis-000      thru pmt-ann-fis-999        .
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
      *    * Visualizzazione prompts per Codice agente                 *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Anno di competenza            *
      *    *-----------------------------------------------------------*
       pmt-ann-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Anno di competenza         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-ann-fis-999.
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
           move      w-tes-cod-age        to   w-cod-mne-age-cod      .
           move      04                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      04                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
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
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
       acc-cod-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-age          .
       acc-cod-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-age        to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati agente                  *
      *                  *---------------------------------------------*
           move      w-let-arc-age-nom    to   w-tes-cod-age-nom      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati agente                 *
      *                  *---------------------------------------------*
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-age-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-age-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
       acc-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice agente              *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-age        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Nominativo agente         *
      *    *-----------------------------------------------------------*
       vis-cod-age-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-age-nom    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Anno fiscale               *
      *    *-----------------------------------------------------------*
       acc-ann-fis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ann-fis-100.
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
           if        w-tes-cod-cli        =    zero
                     move  "B"            to   v-edm
           else      move  "9"            to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-cli        to   v-num                  .
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
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-ann-fis-999.
       acc-ann-fis-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cli          .
       acc-ann-fis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione secolo                      *
      *                  *---------------------------------------------*
           move      "NS"                 to   s-ope                  .
           move      zero                 to   s-dat                  .
           move      w-tes-cod-cli        to   s-ann                  .
           move      01                   to   s-mes                  .
           move      01                   to   s-gio                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-tes-cod-cli          .
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        not  = zero
                     go to acc-ann-fis-500.
           if        v-key                =    "UP  "
                     go to acc-ann-fis-600
           else      go to acc-ann-fis-100.
       acc-ann-fis-500.
      *                  *---------------------------------------------*
      *                  * Visualizzazione anno                        *
      *                  *---------------------------------------------*
           perform   vis-ann-fis-000      thru vis-ann-fis-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione anno per esteso             *
      *                  *---------------------------------------------*
           perform   vis-ann-fis-ext-000  thru vis-ann-fis-ext-999    .
       acc-ann-fis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ann-fis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-ann-fis-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-ann-fis-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-ann-fis-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-ann-fis-999.
       acc-ann-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Anno fiscale            *
      *    *-----------------------------------------------------------*
       vis-ann-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        w-tes-cod-cli        =    zero
                     move  "B"            to   v-edm
           else      move  "9"            to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo richieste : Anno per esteso         *
      *    *-----------------------------------------------------------*
       vis-ann-fis-ext-000.
      *              *-------------------------------------------------*
      *              * Editing anno                                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
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
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-fis-ext-999.
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
                     move  "#"            to   w-cnt-sts-imp-tes
                     move  all "#"        to   w-cnt-sts-imp-pte
                     move  all "#"        to   w-cnt-sts-ing-pte      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di pagine che     *
      *                  * compongono la testata                       *
      *                  *---------------------------------------------*
           perform   dmp-tes-reg-000      thru dmp-tes-reg-999        .
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
      *                  * Prompts per pagina di testata               *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-ptx
                    (w-cnt-sts-imp-npt)   =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati pagina testata         *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"      or
                     w-cnt-mfu-tip-fun    =    "V"      or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces or
                     w-cnt-sts-ing-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     if    w-cnt-sts-vis-ptx
                          (w-cnt-sts-imp-npt)
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Flag di ingresso in pagina di testata       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-ing-ptx
                                              (w-cnt-sts-imp-npt)     .
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
      *                  *---------------------------------------------*
      *                  * Se spostamento di pagina                    *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "-"
                     go to acc-nok-reg-300
           else      go to acc-nok-reg-400.
       acc-nok-reg-300.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina precedente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' 1 si ignora *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  > 1
                     go to acc-nok-reg-200.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina attuale a : no    *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
       acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : decremento ulteriore        *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces and
                     w-cnt-sts-imp-npt    >    1
                     go to acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio numero pagina attuale   *
      *                          *-------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di impostazione dati della   *
      *                          * della pagina attuale a : si'        *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si pone lo status di impostazione   *
      *                          * dati generale testata a si'         *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  "#"            to   w-cnt-sts-imp-tes      .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si va' a conferma impostazioni      *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-nok-reg-800.
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : a re-incremento             *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina impostata a : no  *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-svp)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-svp)     .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
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
      *    * Determinazione numero pagine che compongono la testata    *
      *    *-----------------------------------------------------------*
       dmp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * La testata e' composta di nr. 1 pagine          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Flag di uscita a Si                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Test per pagina 1                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-999.
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
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero pagina    *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tabella scaglioni                           *
      *                  *---------------------------------------------*
           perform   acc-scp-tbs-000      thru acc-scp-tbs-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 1                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Tabella scaglioni                               *
      *              *-------------------------------------------------*
           perform   vis-tbs-spb-000      thru vis-tbs-spb-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
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
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Tabella scaglioni                               *
      *              *-------------------------------------------------*
           perform   pmt-tbs-spb-000      thru pmt-tbs-spb-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tabella scaglioni                *
      *    *-----------------------------------------------------------*
       pmt-tbs-spb-000.
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      64                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      " Fatturato annuale agente  |  % provvigione  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tbs-spb-100.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-gen-001          .
       pmt-tbs-spb-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-cix-gen-001          .
      *              *-------------------------------------------------*
      *              * Test sul contatore                              *
      *              *-------------------------------------------------*
           if        w-cix-gen-001        >    6
                     go to pmt-tbs-spb-999.
      *              *-------------------------------------------------*
      *              * Linea di colonne                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           add       w-cix-gen-001        to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cst-cln-scg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pmt-tbs-spb-200.
       pmt-tbs-spb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Se non esiste alcuna pagina attiva successiva   *
      *              * alla pagina attuale : uscita                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio numero pagina attuale           *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se all'ultima pagina : ripristino pagina    *
      *                  * salvata ed uscita                           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-pre-vpg-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se no : a re-incremento                     *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se si : ripristino pagina salvata e accet-  *
      *                  * tazione presa visione                       *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-svp    to   w-cnt-sts-imp-npt      .
       acc-pre-vpg-200.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-alf                  .
           move      spaces               to   v-not                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pre-vpg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pre-vpg-000.
       acc-pre-vpg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione tabella scaglioni                            *
      *    *-----------------------------------------------------------*
       acc-scp-tbs-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 1..10                *
      *              *-------------------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
       acc-scp-tbs-020.
      *              *-------------------------------------------------*
      *              * Scaglione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-scp-tot 
                    (1, w-cst-ctr-rig)    to   w-cst-sav-scg          .
       acc-scp-tbs-040.
      *                  *---------------------------------------------*
      *                  * Prompt per scaglione                        *
      *                  *---------------------------------------------*
           perform   pmt-acc-scg-000      thru pmt-acc-scg-999        .
      *                  *---------------------------------------------*
      *                  * Accettazione scaglione                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      30                   to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tasti funzione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se oltre 10                    *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        >    10
                     go to acc-scp-tbs-620.
      *                          *-------------------------------------*
      *                          * Determinazione numero riga + 1      *
      *                          *-------------------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                          *-------------------------------------*
      *                          * Up   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Down : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Insr : sempre ammesso, a meno che   *
      *                          *        non si sia in una riga new   *
      *                          *        oppure manchi spazio per     *
      *                          *        l'inserimento                *
      *                          *-------------------------------------*
           if        w-tes-scp-tot (1, 6)
                                          not  = zero
                     go to acc-scp-tbs-060.
           if        w-cst-ctr-rig        =    6
                     go to acc-scp-tbs-060.
           if        w-cst-sav-scg        =    zero
                     go to acc-scp-tbs-060.
           move      "INSR"               to   v-pfk (04)             .
       acc-scp-tbs-060.
      *                          *-------------------------------------*
      *                          * Do   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Remv : sempre ammesso               *
      *                          *-------------------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                          *-------------------------------------*
      *                          * Back : sempre ammesso, a meno che   *
      *                          *        si sia gia' sulla prima riga *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        not  = 1
                     move  "BACK"         to   v-pfk (09)             .
      *                          *-------------------------------------*
      *                          * Tab  : sempre ammesso               *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-tes-scp-tot 
                    (1, w-cst-ctr-rig)    to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-scp-tbs-300.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-scp-tbs-999.
      *                  *---------------------------------------------*
      *                  * Se Delt                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-scp-tbs-999.
      *                  *---------------------------------------------*
      *                  * Se premuto un altro tasto funzione non deve *
      *                  * essere avvenuta variazione del campo        *
      *                  *---------------------------------------------*
           if        v-num                not  = w-cst-sav-scg
                     go to acc-scp-tbs-020.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-scp-tbs-080.
      *                      *-----------------------------------------*
      *                      * Eventuale eliminazione prompt scaglione *
      *                      *-----------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    not  = zero
                     go to acc-scp-tbs-070.
           if        w-cst-ctr-rig        >    10
                     go to acc-scp-tbs-070.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      20                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-scp-tbs-070.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Se su riga > 1 : a riga precedente      *
      *                      *-----------------------------------------*
           if        w-cst-ctr-rig        >    1
                     subtract  1          from w-cst-ctr-rig
                     go to     acc-scp-tbs-020.
      *                      *-----------------------------------------*
      *                      * ALtrimenti : uscita con Up              *
      *                      *-----------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-scp-tbs-999.
       acc-scp-tbs-080.
      *                  *---------------------------------------------*
      *                  * Se Down                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-scp-tbs-160.
       acc-scp-tbs-100.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Se lo scaglione precedentemente salvato *
      *                      * era a zero                              *
      *                      *-----------------------------------------*
           if        w-cst-sav-scg        not  = zero
                     go to acc-scp-tbs-120.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di uno scaglione a ze-  *
      *                          * ro : uscita                         *
      *                          *-------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-scp-tbs-600.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla sulla stessa  *
      *                          * riga, in pratica sulla successiva   *
      *                          *-------------------------------------*
           go to     acc-scp-tbs-020.
       acc-scp-tbs-120.
      *                      *-----------------------------------------*
      *                      * Se lo scaglione precedentemente salvato *
      *                      * era diverso da zero                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se si e' all'ultima riga : uscita   *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        =    10
                     go to acc-scp-tbs-600.
      *                      *-----------------------------------------*
      *                      * Altrimenti : a riga successiva          *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
           go to     acc-scp-tbs-020.
       acc-scp-tbs-160.
      *                  *---------------------------------------------*
      *                  * Se Insr                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-scp-tbs-180.
      *                      *-----------------------------------------*
      *                      * Inserimento riga in castelletto         *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-820      thru acc-scp-tbs-829        .
      *                      *-----------------------------------------*
      *                      * Riciclo sulla stessa riga               *
      *                      *-----------------------------------------*
           go to     acc-scp-tbs-020.
       acc-scp-tbs-180.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-scp-tbs-200.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella scaglioni             *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-980      thru acc-scp-tbs-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-scp-tbs-040.
      *                      *-----------------------------------------*
      *                      * Controllo globali                       *
      *                      *-----------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito controllo                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se positivo : uscita                *
      *                          *-------------------------------------*
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-tes
                     go to acc-scp-tbs-999.
      *                          *-------------------------------------*
      *                          * Altrimenti si torna alla reimposta- *
      *                          * zione della stessa riga             *
      *                          *-------------------------------------*
           go to     acc-scp-tbs-040.
       acc-scp-tbs-200.
      *                  *---------------------------------------------*
      *                  * Se Remv                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-scp-tbs-220.
       acc-scp-tbs-210.
      *                      *-----------------------------------------*
      *                      * Compattamento non controllato           *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-810      thru acc-scp-tbs-819        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione della stessa riga        *
      *                      *-----------------------------------------*
           go to     acc-scp-tbs-020.
       acc-scp-tbs-220.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-scp-tbs-240.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella scaglioni             *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-980      thru acc-scp-tbs-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-scp-tbs-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-scp-tbs-999.
       acc-scp-tbs-240.
      *                  *---------------------------------------------*
      *                  * Se Back                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-scp-tbs-260.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Ad impostazione della prima riga        *
      *                      *-----------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
           go to     acc-scp-tbs-020.
       acc-scp-tbs-260.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-scp-tbs-340.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-scp-tbs-800      thru acc-scp-tbs-809        .
      *                      *-----------------------------------------*
      *                      * Se lo scaglione precedentemente salvato *
      *                      * era a zero                              *
      *                      *-----------------------------------------*
           if        w-cst-sav-scg        not  = zero
                     go to acc-scp-tbs-280.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di uno scaglione a ze-  *
      *                          * ro : uscita                         *
      *                          *-------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-scp-tbs-600.
       acc-scp-tbs-280.
      *                      *-----------------------------------------*
      *                      * Se lo scaglione precedentemente salvato *
      *                      * era diverso da zero ci si posiziona     *
      *                      * dopo l'ultima riga                      *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                          *-------------------------------------*
      *                          * Se sono presenti tutte le righe :   *
      *                          * uscita                              *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        >    10
                     go to acc-scp-tbs-600.
      *                          *-------------------------------------*
      *                          * Se la riga e' vuota si va ad impo-  *
      *                          * starla                              *
      *                          *-------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-scp-tbs-020.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla per trovare   *
      *                          * una riga vuota                      *
      *                          *-------------------------------------*
           go to     acc-scp-tbs-280.
       acc-scp-tbs-300.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valore impostato         *
      *                      *-----------------------------------------*
           move      v-num                to   w-tes-scp-tot
                                              (1, w-cst-ctr-rig)      .
      *                      *-----------------------------------------*
      *                      * Se campo vuoto : come Down              *
      *                      *-----------------------------------------*
           if        v-num                =    zero
                     go to acc-scp-tbs-100.
      *                  *---------------------------------------------*
      *                  * Controlli impostazione scaglione : nessuno  *
      *                  *---------------------------------------------*
       acc-scp-tbs-340.
       acc-scp-tbs-380.
      *                          *-------------------------------------*
      *                          * Accettazione percentuale            *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      58                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Up   : sempre ammesso       *
      *                                  *-----------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-tes-scp-per 
                    (1, w-cst-ctr-rig)    to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
           if        v-key                =    spaces
                     go to acc-scp-tbs-400.
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-scp-tbs-210.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-scp-tbs-340.
       acc-scp-tbs-400.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-tes-scp-per
                                              (1, w-cst-ctr-rig)      .
      *                              *---------------------------------*
      *                              * Percentuale a zero non ammessa  *
      *                              * a meno che non sia la prima     *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-cst-ctr-rig        =    1
                     go to acc-scp-tbs-580.
           if        w-tes-scp-per
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-scp-tbs-380.
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-scp-tbs-580.
       acc-scp-tbs-580.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Se fine righe : uscita                      *
      *                  *---------------------------------------------*
           if        w-cst-ctr-rig        >    10
                     go to acc-scp-tbs-620.
      *                  *---------------------------------------------*
      *                  * Altrimenti : a prossimo scaglione           *
      *                  *---------------------------------------------*
           go to     acc-scp-tbs-020.
       acc-scp-tbs-600.
      *              *-------------------------------------------------*
      *              * Eventuale eliminazione prompt per scaglione     *
      *              *-------------------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    not  = zero
                     go to acc-scp-tbs-620.
           if        w-cst-ctr-rig        >    10
                     go to acc-scp-tbs-620.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      20                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-scp-tbs-620.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-scp-tbs-999.
       acc-scp-tbs-800.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento controllato *
      *              * tabella : solo se lo scaglione e' a zero        *
      *              *-------------------------------------------------*
           if        w-tes-scp-tot
                    (1, w-cst-ctr-rig)    not  = zero
                     go to acc-scp-tbs-809.
           if        w-cst-ctr-rig        =    10
                     go to acc-scp-tbs-809.
           if        w-tes-scp-tot
                    (1, w-cst-cts-rig)    =    zero
                     go to acc-scp-tbs-809.
           perform   acc-scp-tbs-810      thru acc-scp-tbs-819        .
       acc-scp-tbs-809.
           exit.
       acc-scp-tbs-810.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento righe della *
      *              * tabella escludendo la riga w-cst-ctr-rig        *
      *              *-------------------------------------------------*
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-scp-tbs-811.
           if        w-cst-ctx-rig        =    6
                     move  zero           to   w-tes-scp-tot (1, 6)
                     move  zero           to   w-tes-scp-per (1, 6)
                     go to acc-scp-tbs-812.
           move      w-tes-scp-tbe
                    (1, w-cst-cty-rig)    to   w-tes-scp-tbe
                                              (1, w-cst-ctx-rig)      .
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-scp-tbs-811.
       acc-scp-tbs-812.
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       09                   to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-scp-tbs-813.
           if        w-cst-ctx-rig        =    19
                     go to acc-scp-tbs-814.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           move      w-cst-cty-rig        to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-cst-ctx-rig        to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           if        v-alf                =    w-cst-cln-scg
                     go to acc-scp-tbs-815.
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-scp-tbs-813.
       acc-scp-tbs-814.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cst-cln-scg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-scp-tbs-815.
       acc-scp-tbs-819.
           exit.
       acc-scp-tbs-820.
      *              *-------------------------------------------------*
      *              * Subroutine interna di inserimento della riga    *
      *              * numero w-cst-ctr-rig nella tabella scaglioni    *
      *              *-------------------------------------------------*
           move      9                    to   w-cst-cty-rig          .
       acc-scp-tbs-821.
           if        w-tes-scp-tot
                    (1, w-cst-cty-rig)    =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-scp-tbs-821.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-scp-tbs-822.
           move      w-tes-scp-tbe
                    (1, w-cst-ctx-rig)    to   w-tes-scp-tbe
                                              (1, w-cst-cty-rig)      .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-scp-tbs-822.
           move      zero                 to   w-tes-scp-tot
                                              (1, w-cst-ctr-rig)      .
           move      zero                 to   w-tes-scp-per
                                              (1, w-cst-ctr-rig)      .
           move      9                    to   w-cst-cty-rig          .
       acc-scp-tbs-823.
           if        w-cst-cty-rig        =    w-cst-ctr-rig
                     go to     acc-scp-tbs-824.
           if        w-tes-scp-tot
                    (1, w-cst-cty-rig)    =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-scp-tbs-823.
       acc-scp-tbs-824.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-scp-tbs-825.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           add       09
                     w-cst-ctx-rig      giving v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       09
                     w-cst-cty-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-scp-tbs-825.
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cst-cln-scg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-scp-tbs-829.
           exit.
       acc-scp-tbs-980.
      *              *-------------------------------------------------*
      *              * Subroutine interna di esecuzione operazioni per *
      *              * fine impostazione tabella scaglioni             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-cst-flg-exi          .
      *                  *---------------------------------------------*
      *                  * Controllo che almeno una riga sia presente  *
      *                  *---------------------------------------------*
           if        w-tes-scp-tot (1, 1) =    zero
                     move  "#"            to   w-cst-flg-exi
                     go to acc-scp-tbs-989.
       acc-scp-tbs-989.
           exit.
       acc-scp-tbs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione tabella scaglioni                         *
      *    *-----------------------------------------------------------*
       vis-tbs-spb-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 1..6             *
      *                  *---------------------------------------------*
           move      zero                 to   w-cst-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Ciclo 1..6                                  *
      *                  *---------------------------------------------*
       vis-tbs-spb-100.
           add       1                    to   w-cst-ctr-rig          .
           if        w-cst-ctr-rig        >    6
                     go to  vis-tbs-spb-999.
           if        w-tes-scp-tot 
                    (1, w-cst-ctr-rig)    =    zero and
                     w-tes-scp-per 
                    (1, w-cst-ctr-rig)    =    zero
                     go to  vis-tbs-spb-999.
      *                      *-----------------------------------------*
      *                      * Prompt per scaglione                    *
      *                      *-----------------------------------------*
           perform   pmt-acc-scg-000      thru pmt-acc-scg-999        .
      *                      *-----------------------------------------*
      *                      * Scaglione                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-scp-tot
                    (1, w-cst-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tbs-spb-200.
      *                      *-----------------------------------------*
      *                      * Percentuale                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        w-tes-scp-per
                    (1, w-cst-ctr-rig)    =    zero
                     move  "9"            to   v-edm
           else      move  "BD"           to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      58                   to   v-pos                  .
           move      w-tes-scp-per
                    (1, w-cst-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tbs-spb-300.
      *                      *-----------------------------------------*
      *                      * Riciclo a riga castelletto successiva   *
      *                      *-----------------------------------------*
           go to     vis-tbs-spb-100.
       vis-tbs-spb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per scaglioni della tabella        *
      *    *-----------------------------------------------------------*
       pmt-acc-scg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      20                   to   v-pos                  .
           move      "fino a   "          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-acc-scg-999.
           exit.

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
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-cod-cli        =    spaces
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
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controlli su tabella scaglioni                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che almeno un elemento sia pre-   *
      *                  * sente in tabella                            *
      *                  *---------------------------------------------*
           if        w-tes-scp-tot
                    (1, 1)         not  = zero
                     go to cnt-tdo-nok-410.
           move      "Tabella scaglioni vuota non accettabile         "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-410.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Uscita per controlli non superati               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
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
           move      zero                 to   w-tes-cod-age          .
           move      spaces               to   w-tes-cod-age-nom      .
           move      zero                 to   w-tes-cod-cli          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-tip-fat (1)      .
           move      spaces               to   w-tes-tot-snx (1, 1)   .
           move      spaces               to   w-tes-tot-snx (1, 2)   .
           move      spaces               to   w-tes-tot-snx (1, 3)   .
           move      spaces               to   w-tes-tot-snx (1, 4)   .
           move      spaces               to   w-tes-tot-snx (1, 5)   .
           move      spaces               to   w-tes-tot-snx (1, 6)   .
           move      spaces               to   w-tes-tot-snx (1, 7)   .
           move      spaces               to   w-tes-tot-snx (1, 8)   .
           move      spaces               to   w-tes-tot-snx (1, 9)   .
           perform   nor-scp-tbs-000      thru nor-scp-tbs-999        .
           move      spaces               to   w-tes-alx-exp (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati tabella scaglioni                    *
      *    *-----------------------------------------------------------*
       nor-scp-tbs-000.
           move      zero                 to   w-tes-scp-tot (1, 1)   .
           move      zero                 to   w-tes-scp-tot (1, 2)   .
           move      zero                 to   w-tes-scp-tot (1, 3)   .
           move      zero                 to   w-tes-scp-tot (1, 4)   .
           move      zero                 to   w-tes-scp-tot (1, 5)   .
           move      zero                 to   w-tes-scp-tot (1, 6)   .
      *
           move      zero                 to   w-tes-scp-per (1, 1)   .
           move      zero                 to   w-tes-scp-per (1, 2)   .
           move      zero                 to   w-tes-scp-per (1, 3)   .
           move      zero                 to   w-tes-scp-per (1, 4)   .
           move      zero                 to   w-tes-scp-per (1, 5)   .
           move      zero                 to   w-tes-scp-per (1, 6)   .
       nor-scp-tbs-999.
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
           move      "AGECLI    "         to   f-key                  .
           move      w-tes-cod-age        to   rf-zpf-cod-age         .
           move      w-tes-cod-cli        to   rf-zpf-cod-cli         .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
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
      *                      * Normalizzazione tipo fatturato          *
      *                      *-----------------------------------------*
           move      01                   to   w-tes-tip-fat (1)      .
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
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [zpf]                        *
      *                          *-------------------------------------*
           move      rf-zpf-tip-fat       to   w-tes-tip-fat (1)      .
           move      rf-zpf-tot-snx (1)   to   w-tes-tot-snx (1, 1)   .
           move      rf-zpf-tot-snx (2)   to   w-tes-tot-snx (1, 2)   .
           move      rf-zpf-tot-snx (3)   to   w-tes-tot-snx (1, 3)   .
           move      rf-zpf-tot-snx (4)   to   w-tes-tot-snx (1, 4)   .
           move      rf-zpf-tot-snx (5)   to   w-tes-tot-snx (1, 5)   .
           move      rf-zpf-tot-snx (6)   to   w-tes-tot-snx (1, 6)   .
           move      rf-zpf-tot-snx (7)   to   w-tes-tot-snx (1, 7)   .
           move      rf-zpf-tot-snx (8)   to   w-tes-tot-snx (1, 8)   .
           move      rf-zpf-tot-snx (9)   to   w-tes-tot-snx (1, 9)   .
      *
           move      zero                 to   w-cix-gen-001          .
       rou-let-reg-200.
           add       1                    to   w-cix-gen-001          .
           if        w-cix-gen-001        >    6
                     go to rou-let-reg-220.
           move      rf-zpf-scp-tot
                    (w-cix-gen-001)       to   w-tes-scp-tot
                                              (1, w-cix-gen-001)      .
           move      rf-zpf-scp-per
                    (w-cix-gen-001)       to   w-tes-scp-per
                                              (1, w-cix-gen-001)      .
           go to     rou-let-reg-200.
       rou-let-reg-220.
           move      rf-zpf-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zpf]                        *
      *                          *-------------------------------------*
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
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-500.
       pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Se richiesta la duplicazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Da buffer a 'w-tes'                     *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di controllo      *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
           move      all "#"              to   w-cnt-sts-imp-pte      .
           move      all "#"              to   w-cnt-sts-ing-pte      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di visualizzazio- *
      *                      * ne per le pagine                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione segnale di duplicazione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagine di testata           *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-500.
      *                  *---------------------------------------------*
      *                  * Se non richiesta la duplicazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *-----------------------------------------------------------*
       pre-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-snx-del      .
       pre-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Routine post-richiesta di ratifica tasto Delete           *
      *    *-----------------------------------------------------------*
       pos-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-snx-del      .
       pos-snx-del-999.
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
      *              * Trattamento file [zpf]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zpf]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zpf-000      thru wrt-rec-zpf-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zpf]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zpf-000      thru rew-rec-zpf-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zpf]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zpf-000      thru del-rec-zpf-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zpf]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zpf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-age        to   rf-zpf-cod-age         .
           move      w-tes-cod-cli        to   rf-zpf-cod-cli         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-tip-fat (1)    to   rf-zpf-tip-fat         .
           move      w-tes-tot-snx (1, 1) to   rf-zpf-tot-snx (1)     .
           move      w-tes-tot-snx (1, 2) to   rf-zpf-tot-snx (2)     .
           move      w-tes-tot-snx (1, 3) to   rf-zpf-tot-snx (3)     .
           move      w-tes-tot-snx (1, 4) to   rf-zpf-tot-snx (4)     .
           move      w-tes-tot-snx (1, 5) to   rf-zpf-tot-snx (5)     .
           move      w-tes-tot-snx (1, 6) to   rf-zpf-tot-snx (6)     .
           move      w-tes-tot-snx (1, 7) to   rf-zpf-tot-snx (7)     .
           move      w-tes-tot-snx (1, 8) to   rf-zpf-tot-snx (8)     .
           move      w-tes-tot-snx (1, 9) to   rf-zpf-tot-snx (9)     .
      *
           move      zero                 to   w-cix-gen-001          .
       cmp-rec-zpf-100.
           add       1                    to   w-cix-gen-001          .
           if        w-cix-gen-001        >    6
                     go to cmp-rec-zpf-120.
           move      w-tes-scp-tot
                    (1, w-cix-gen-001)    to   rf-zpf-scp-tot
                                              (w-cix-gen-001)         .
           move      w-tes-scp-per
                    (1, w-cix-gen-001)    to   rf-zpf-scp-per
                                              (w-cix-gen-001)         .
           go to     cmp-rec-zpf-100.
       cmp-rec-zpf-120.
           move      w-tes-alx-exp (1)    to   rf-zpf-alx-exp         .
       cmp-rec-zpf-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zpf]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zpf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpf-000      thru cmp-rec-zpf-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
       wrt-rec-zpf-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zpf]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zpf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpf-000      thru cmp-rec-zpf-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
       rew-rec-zpf-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zpf]                                *
      *    *-----------------------------------------------------------*
       del-rec-zpf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpf-000      thru cmp-rec-zpf-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpf                 .
       del-rec-zpf-999.
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
           move      rf-age-sup-age       to   w-let-arc-age-sup      .
           move      rf-age-cat-pvg       to   w-let-arc-age-cat      .
           move      rf-age-per-pvg (1)   to   w-let-arc-age-per (1)  .
           move      rf-age-per-pvg (2)   to   w-let-arc-age-per (2)  .
           move      rf-age-per-pvg (3)   to   w-let-arc-age-per (3)  .
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
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-600.
           move      zero                 to   w-let-arc-age-sup      .
           move      zero                 to   w-let-arc-age-cat      .
           move      zero                 to   w-let-arc-age-per (1)  .
           move      zero                 to   w-let-arc-age-per (2)  .
           move      zero                 to   w-let-arc-age-per (3)  .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice agente              *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

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
