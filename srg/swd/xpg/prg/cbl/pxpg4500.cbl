       Identification Division.
       Program-Id.                                 pxpg4500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    sds                 *
      *                                   Fase:    xpg450              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Versione attuale:    NdK del 27/01/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione status di impegno delle stampanti  *
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
                     "swd"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "xpg"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "sds"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg450"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg4500"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  GESTIONE STATUS DI IMPEGNO STAMPANTI  "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mpslct"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mppssx"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work per records di [pss] 'stp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssstp0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'imp'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssimp0.cpw"                   .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione stato di impegno stampante        *
      *        *-------------------------------------------------------*
           05  w-clo-imp.
      *            *---------------------------------------------------*
      *            * Canale di uscita per salvataggio                  *
      *            *---------------------------------------------------*
               10  w-clo-imp-can-stp      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Record di impegno stampante                       *
      *            *---------------------------------------------------*
               10  w-clo-imp-rec-imp.
                   15  filler occurs 2048 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su parametri stampa di [pss]            *
      *        *-------------------------------------------------------*
           05  w-fnd-pss-imp.
               10  w-fnd-pss-imp-sel      pic  x(01)                  .
               10  w-fnd-pss-imp-can      pic  x(40)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
           05  filler                     pic  x(01)                  .

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
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazione campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Status di impegno della stampante          *
      *        *-------------------------------------------------------*
           05  w-exp-sts-imp.
               10  w-exp-sts-imp-num      pic  9(02)       value 6    .
               10  w-exp-sts-imp-lun      pic  9(02)       value 40   .
               10  w-exp-sts-imp-tbl.
                   15  filler             pic  x(40) value
                            "Non impegnata                           ".
                   15  filler             pic  x(40) value
                            "Pre-impegnata per selezione stampante   ".
                   15  filler             pic  x(40) value
                            "Impegnata per operazioni di manutenzione".
                   15  filler             pic  x(40) value
                            "Impegnata per montaggio modulo carta    ".
                   15  filler             pic  x(40) value
                            "Impegnata per servizio di spool         ".
                   15  filler             pic  x(40) value
                            "Impegnata per fase di stampa            ".

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
      *              * Tasto di funzione Pf4 :                         *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Modifica               : abilitato solo   *
      *              *                                se nessuna mo-   *
      *              *                                difica           *
      *              *  - se Visualizzazione        : abilitato solo   *
      *              *                                se nessuna mo-   *
      *              *                                difica           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    not  = "K"    and
                    (w-cnt-mfu-tip-fun    =    "M"   or
                     w-cnt-mfu-tip-fun    =    "V"    ) and
                     w-cnt-acc-flg-aum    =    spaces
                     move  "[4] "         to   v-pfk (18)             .
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
           move      "Conferma operazione di annullamento (S/N) ?"
                                          to   v-not                  .
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
           if        w-cnt-pos-snx-del    not  = spaces
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
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Se Pf4                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-acc-cmp-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio record                          *
      *                  *---------------------------------------------*
           move      w-imp                to   w-clo-imp-rec-imp      .
      *                  *---------------------------------------------*
      *                  * Set del flag                                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Forzatura del tasto Exit                    *
      *                  *---------------------------------------------*
           move      "EXIT"               to   v-key                  .
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      66                   to   v-pos                  .
           if        w-cnt-mfu-tip-fun    =    "I"
                     move  "    Inserimento"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "M"
                     move  "       Modifica"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "V"
                     move  "Visualizzazione"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Open  per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Close per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "mppssf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                not  = spaces
                     go to rou-cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "mppssf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mppssf"                         .
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
       acc-key-reg-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Canale di uscita                            *
      *                  *---------------------------------------------*
           perform   acc-can-stp-000        thru acc-can-stp-999      .
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
      *              * Canale di uscita                                *
      *              *-------------------------------------------------*
           perform   vis-can-stp-000      thru vis-can-stp-999        .
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
      *              * Canale di uscita                                *
      *              *-------------------------------------------------*
           perform   pmt-can-stp-000      thru pmt-can-stp-999        .
      *              *-------------------------------------------------*
      *              * Lineette di separazione a linea 05              *
      *              *-------------------------------------------------*
           perform   pmt-lds-l05-000      thru pmt-lds-l05-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per canale di uscita                               *
      *    *-----------------------------------------------------------*
       pmt-can-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Canale di uscita           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-can-stp-999.
           exit.

      *    *===========================================================*
      *    * Prompt per lineette di separazione a linea 05             *
      *    *-----------------------------------------------------------*
       pmt-lds-l05-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lds-l05-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Canale di uscita                           *
      *    *-----------------------------------------------------------*
       acc-can-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-can-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-imp-can-stp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-can-stp-999.
       acc-can-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-can-stp          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-can-stp-400.
      *                  *---------------------------------------------*
      *                  * Find su canali di stampa impegnati          *
      *                  *---------------------------------------------*
           move      w-imp-can-stp        to   w-fnd-pss-imp-can      .
           perform   fnd-pss-imp-000      thru fnd-pss-imp-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-pss-imp-sel    not  = spaces
                     go to acc-can-stp-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-pss-imp-can    to   w-imp-can-stp          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-can-stp-000      thru vis-can-stp-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Se canale di stampa selezionato a spaces :  *
      *                  * riciclo a impostazione                      *
      *                  *---------------------------------------------*
           if        w-imp-can-stp        =    spaces
                     go to acc-can-stp-100.
       acc-can-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-can-stp-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-imp-can-stp        to   w-bla-emb-str          .
           perform   tst-bla-emb-000      thru tst-bla-emb-999        .
           if        w-bla-emb-flg        not  = spaces
                     go to acc-can-stp-100.
       acc-can-stp-450.
      *                  *---------------------------------------------*
      *                  * Valore 'spool' non ammesso                  *
      *                  *---------------------------------------------*
           if        w-imp-can-stp
                    (01 : 05)             =    "spool"
                     go to acc-can-stp-100.
      *                  *---------------------------------------------*
      *                  * Valore 'rcp' non ammesso                    *
      *                  *---------------------------------------------*
           if        w-imp-can-stp
                    (01 : 03)             =    "rcp"
                     go to acc-can-stp-100.
      *                  *---------------------------------------------*
      *                  * Valore 'ucp' non ammesso                    *
      *                  *---------------------------------------------*
           if        w-imp-can-stp
                    (01 : 03)             =    "ucp"
                     go to acc-can-stp-100.
       acc-can-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-can-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-can-stp-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-can-stp-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-can-stp-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-can-stp-999.
       acc-can-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Canale di uscita                        *
      *    *-----------------------------------------------------------*
       vis-can-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-can-stp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-can-stp-999.
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
           move      "[Exit] per fine visualizzazione :"
                                          to   v-not                  .
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
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
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
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Status di impegno                           *
      *                  *---------------------------------------------*
           perform   acc-sts-imp-000      thru acc-sts-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Azienda che impegna                         *
      *                  *---------------------------------------------*
           perform   acc-azi-imp-000      thru acc-azi-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Terminale che impegna                       *
      *                  *---------------------------------------------*
           perform   acc-ter-imp-000      thru acc-ter-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Utente che impegna                          *
      *                  *---------------------------------------------*
           perform   acc-ute-imp-000      thru acc-ute-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Sistema applicativo che impegna             *
      *                  *---------------------------------------------*
           perform   acc-sap-imp-000      thru acc-sap-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-450.
      *                  *---------------------------------------------*
      *                  * Area gestionale che impegna                 *
      *                  *---------------------------------------------*
           perform   acc-arg-imp-000      thru acc-arg-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Settore gestionale che impegna              *
      *                  *---------------------------------------------*
           perform   acc-set-imp-000      thru acc-set-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-450.
       acc-tes-reg-550.
      *                  *---------------------------------------------*
      *                  * Fase gestionale che impegna                 *
      *                  *---------------------------------------------*
           perform   acc-fas-imp-000      thru acc-fas-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
       acc-tes-reg-600.
      *                  *---------------------------------------------*
      *                  * Data di impegno                             *
      *                  *---------------------------------------------*
           perform   acc-dat-imp-000      thru acc-dat-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-550.
       acc-tes-reg-650.
      *                  *---------------------------------------------*
      *                  * Ora di impegno                              *
      *                  *---------------------------------------------*
           perform   acc-ora-imp-000      thru acc-ora-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-600.
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
      *              * Status di impegno                               *
      *              *-------------------------------------------------*
           perform   vis-sts-imp-000      thru vis-sts-imp-999        .
      *              *-------------------------------------------------*
      *              * Azienda che impegna                             *
      *              *-------------------------------------------------*
           perform   vis-azi-imp-000      thru vis-azi-imp-999        .
      *              *-------------------------------------------------*
      *              * Terminale che impegna                           *
      *              *-------------------------------------------------*
           perform   vis-ter-imp-000      thru vis-ter-imp-999        .
      *              *-------------------------------------------------*
      *              * Utente che impegna                              *
      *              *-------------------------------------------------*
           perform   vis-ute-imp-000      thru vis-ute-imp-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo che impegna                 *
      *              *-------------------------------------------------*
           perform   vis-sap-imp-000      thru vis-sap-imp-999        .
      *              *-------------------------------------------------*
      *              * Area gestionale che impegna                     *
      *              *-------------------------------------------------*
           perform   vis-arg-imp-000      thru vis-arg-imp-999        .
      *              *-------------------------------------------------*
      *              * Settore gestionale che impegna                  *
      *              *-------------------------------------------------*
           perform   vis-set-imp-000      thru vis-set-imp-999        .
      *              *-------------------------------------------------*
      *              * Fase gestionale che impegna                     *
      *              *-------------------------------------------------*
           perform   vis-fas-imp-000      thru vis-fas-imp-999        .
      *              *-------------------------------------------------*
      *              * Data di impegno                                 *
      *              *-------------------------------------------------*
           perform   vis-dat-imp-000      thru vis-dat-imp-999        .
      *              *-------------------------------------------------*
      *              * Ora di impegno                                  *
      *              *-------------------------------------------------*
           perform   vis-ora-imp-000      thru vis-ora-imp-999        .
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
      *              * Status di impegno                               *
      *              *-------------------------------------------------*
           perform   pmt-sts-imp-000      thru pmt-sts-imp-999        .
      *              *-------------------------------------------------*
      *              * Azienda che impegna                             *
      *              *-------------------------------------------------*
           perform   pmt-azi-imp-000      thru pmt-azi-imp-999        .
      *              *-------------------------------------------------*
      *              * Terminale che impegna                           *
      *              *-------------------------------------------------*
           perform   pmt-ter-imp-000      thru pmt-ter-imp-999        .
      *              *-------------------------------------------------*
      *              * Utente che impegna                              *
      *              *-------------------------------------------------*
           perform   pmt-ute-imp-000      thru pmt-ute-imp-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo che impegna                 *
      *              *-------------------------------------------------*
           perform   pmt-sap-imp-000      thru pmt-sap-imp-999        .
      *              *-------------------------------------------------*
      *              * Area gestionale che impegna                     *
      *              *-------------------------------------------------*
           perform   pmt-arg-imp-000      thru pmt-arg-imp-999        .
      *              *-------------------------------------------------*
      *              * Settore gestionale che impegna                  *
      *              *-------------------------------------------------*
           perform   pmt-set-imp-000      thru pmt-set-imp-999        .
      *              *-------------------------------------------------*
      *              * Fase gestionale che impegna                     *
      *              *-------------------------------------------------*
           perform   pmt-fas-imp-000      thru pmt-fas-imp-999        .
      *              *-------------------------------------------------*
      *              * Data di impegno                                 *
      *              *-------------------------------------------------*
           perform   pmt-dat-imp-000      thru pmt-dat-imp-999        .
      *              *-------------------------------------------------*
      *              * Ora di impegno                                  *
      *              *-------------------------------------------------*
           perform   pmt-ora-imp-000      thru pmt-ora-imp-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Status di impegno                                *
      *    *-----------------------------------------------------------*
       pmt-sts-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Status di impegno          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sts-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Azienda che impegna                              *
      *    *-----------------------------------------------------------*
       pmt-azi-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice azienda             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-azi-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Terminale che impegna                            *
      *    *-----------------------------------------------------------*
       pmt-ter-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice terminale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ter-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Utente che impegna                               *
      *    *-----------------------------------------------------------*
       pmt-ute-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice utente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ute-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Sistema applicativo che impegna                  *
      *    *-----------------------------------------------------------*
       pmt-sap-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sistema applicativo        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sap-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Area gestionale che impegna                      *
      *    *-----------------------------------------------------------*
       pmt-arg-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Area gestionale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-arg-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Settore gestionale che impegna                   *
      *    *-----------------------------------------------------------*
       pmt-set-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Settore gestionale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-set-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Fase gestionale che impegna                      *
      *    *-----------------------------------------------------------*
       pmt-fas-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fase gestionale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fas-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data di impegno                                  *
      *    *-----------------------------------------------------------*
       pmt-dat-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data gg/mm/aa              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-imp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Ora di impegno                                   *
      *    *-----------------------------------------------------------*
       pmt-ora-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ora  hh:mm:ss:xx           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ora-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Status di impegno                          *
      *    *-----------------------------------------------------------*
       acc-sts-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sts-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-imp-lun    to   v-car                  .
           move      w-exp-sts-imp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-sts-imp-tbl    to   v-txt                  .
           move      w-imp-sts-imp        to   v-num                  .
           add       1                    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sts-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sts-imp-999.
       acc-sts-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-imp-sts-imp          .
           subtract  1                    from w-imp-sts-imp          .
       acc-sts-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sts-imp-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammissibile               *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero or
                     w-imp-sts-imp        =    01   or
                     w-imp-sts-imp        =    02   or
                     w-imp-sts-imp        =    03   or
                     w-imp-sts-imp        =    04   or
                     w-imp-sts-imp        =    05
                     go to acc-sts-imp-600
           else      go to acc-sts-imp-100.
       acc-sts-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sts-imp-625.
      *                  *---------------------------------------------*
      *                  * Se lo status indica che la stampante non e' *
      *                  * impegnata : si normalizzano e si visualiz-  *
      *                  * zano tutti gli altri valori                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-imp-sts-imp        not  = zero
                     go to acc-sts-imp-800.
      *                      *-----------------------------------------*
      *                      * Azienda che impegna                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-azi-imp          .
           perform   vis-azi-imp-000      thru vis-azi-imp-999        .
      *                      *-----------------------------------------*
      *                      * Terminale che impegna                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-ter-imp          .
           perform   vis-ter-imp-000      thru vis-ter-imp-999        .
      *                      *-----------------------------------------*
      *                      * Utente che impegna                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-ute-imp          .
           perform   vis-ute-imp-000      thru vis-ute-imp-999        .
      *                      *-----------------------------------------*
      *                      * Sistema applicativo che impegna         *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-sap-imp          .
           perform   vis-sap-imp-000      thru vis-sap-imp-999        .
      *                      *-----------------------------------------*
      *                      * Area gestionale che impegna             *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-arg-imp          .
           perform   vis-arg-imp-000      thru vis-arg-imp-999        .
      *                      *-----------------------------------------*
      *                      * Settore gestionale che impegna          *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-set-imp          .
           perform   vis-set-imp-000      thru vis-set-imp-999        .
      *                      *-----------------------------------------*
      *                      * Fase gestionale che impegna             *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-fas-imp          .
           perform   vis-fas-imp-000      thru vis-fas-imp-999        .
      *                      *-----------------------------------------*
      *                      * Data ed ora di impegno                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-sdt-imp          .
           perform   vis-dat-imp-000      thru vis-dat-imp-999        .
           perform   vis-dat-imp-000      thru vis-dat-imp-999        .
       acc-sts-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sts-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sts-imp-100.
       acc-sts-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Status di impegno                       *
      *    *-----------------------------------------------------------*
       vis-sts-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-imp-lun    to   v-car                  .
           move      w-exp-sts-imp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sts-imp-tbl    to   v-txt                  .
           move      w-imp-sts-imp        to   v-num                  .
           add       1                    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sts-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Azienda che impegna                        *
      *    *-----------------------------------------------------------*
       acc-azi-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-azi-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-azi-imp-999.
       acc-azi-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-azi-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-azi-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-azi-imp-999.
       acc-azi-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-azi-imp          .
       acc-azi-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-azi-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-azi-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-azi-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-azi-imp-100.
       acc-azi-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Azienda che impegna                     *
      *    *-----------------------------------------------------------*
       vis-azi-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-azi-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-azi-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Terminale che impegna                      *
      *    *-----------------------------------------------------------*
       acc-ter-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ter-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-ter-imp-999.
       acc-ter-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-ter-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ter-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ter-imp-999.
       acc-ter-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-ter-imp          .
       acc-ter-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ter-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ter-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ter-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ter-imp-100.
       acc-ter-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Terminale che impegna                   *
      *    *-----------------------------------------------------------*
       vis-ter-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-ter-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ter-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Utente che impegna                         *
      *    *-----------------------------------------------------------*
       acc-ute-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ute-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-ute-imp-999.
       acc-ute-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-ute-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ute-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ute-imp-999.
       acc-ute-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-ute-imp          .
       acc-ute-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ute-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ute-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ute-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ute-imp-100.
       acc-ute-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Utente che impegna                      *
      *    *-----------------------------------------------------------*
       vis-ute-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-ute-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ute-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sistema applicativo che impegna            *
      *    *-----------------------------------------------------------*
       acc-sap-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sap-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-sap-imp-999.
       acc-sap-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-sap-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sap-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sap-imp-999.
       acc-sap-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-sap-imp          .
       acc-sap-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sap-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sap-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sap-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sap-imp-100.
       acc-sap-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Sistema applicativo che impegna         *
      *    *-----------------------------------------------------------*
       vis-sap-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-sap-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sap-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area gestionale che impegna                *
      *    *-----------------------------------------------------------*
       acc-arg-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-arg-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-arg-imp-999.
       acc-arg-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-arg-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-arg-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-arg-imp-999.
       acc-arg-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-arg-imp          .
       acc-arg-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-arg-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-arg-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-arg-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-arg-imp-100.
       acc-arg-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area gestionale che impegna             *
      *    *-----------------------------------------------------------*
       vis-arg-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-arg-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-arg-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Settore gestionale che impegna             *
      *    *-----------------------------------------------------------*
       acc-set-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-set-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-set-imp-999.
       acc-set-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-set-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-set-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-set-imp-999.
       acc-set-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-set-imp          .
       acc-set-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-set-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-set-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-set-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-set-imp-100.
       acc-set-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Settore gestionale che impegna          *
      *    *-----------------------------------------------------------*
       vis-set-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-set-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-set-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Fase gestionale che impegna                *
      *    *-----------------------------------------------------------*
       acc-fas-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fas-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-fas-imp-999.
       acc-fas-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-fas-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-fas-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-fas-imp-999.
       acc-fas-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-imp-fas-imp          .
       acc-fas-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fas-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fas-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-fas-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-fas-imp-100.
       acc-fas-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Fase gestionale che impegna             *
      *    *-----------------------------------------------------------*
       vis-fas-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-fas-imp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fas-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di impegno                            *
      *    *-----------------------------------------------------------*
       acc-dat-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-dat-imp-999.
       acc-dat-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      s-dat                to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-imp-999.
       acc-dat-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      v-dat                to   s-dat                  .
           move      s-sdt                to   w-imp-sdt-imp          .
       acc-dat-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-imp-100.
       acc-dat-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data di impegno                         *
      *    *-----------------------------------------------------------*
       vis-dat-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      s-dat                to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Ora di impegno                             *
      *    *-----------------------------------------------------------*
       acc-ora-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ora-imp-025.
      *                  *---------------------------------------------*
      *                  * Se stampante non impegnata : non si esegue  *
      *                  * l'accettazione                              *
      *                  *---------------------------------------------*
           if        w-imp-sts-imp        =    zero
                     go to acc-ora-imp-999.
       acc-ora-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BM"                 to   v-edm                  .
           move      "XX:XX:XX:XX"        to   v-msk                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      s-tim                to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ora-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ora-imp-999.
       acc-ora-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      v-num                to   s-tim                  .
           move      s-sdt                to   w-imp-sdt-imp          .
       acc-ora-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ora-imp-425.
      *                  *---------------------------------------------*
      *                  * Controllo formale su formato hh:mm:ss:cc    *
      *                  *---------------------------------------------*
           move      w-imp-sdt-imp        to   s-sdt                  .
           if        s-ora                >    23 or
                     s-min                >    59 or
                     s-scd                >    59
                     go to acc-ora-imp-100.
       acc-ora-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ora-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ora-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ora-imp-100.
       acc-ora-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Ora di impegno                          *
      *    *-----------------------------------------------------------*
       vis-ora-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BM"                 to   v-edm                  .
           move      "XX:XX:XX:XX"        to   v-msk                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-imp-sdt-imp        to   s-sdt                  .
           move      s-tim                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ora-imp-999.
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
           if        w-imp-can-stp        =    spaces
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
      *              *-------------------------------------------------*
      *              * Stato di impegno stampante                      *
      *              *-------------------------------------------------*
           move      "NO"                 to   j-ope                  .
           move      "IMP"                to   j-tre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           move      j-dat                to   w-imp                  .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
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
           move      "RD"                 to   j-ope                  .
           move      "IMP"                to   j-tre                  .
           move      w-imp-can-stp        to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        j-rsc                =    e-not-fnd
                     go to rou-let-reg-100
           else if   j-rsc                =    spaces
                     go to rou-let-reg-600
           else      go to rou-let-reg-900.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
       rou-let-reg-200.
      *                  *---------------------------------------------*
      *                  * Ricerca se esiste almeno una stampante con  *
      *                  * il canale di stampa impostato, e deviazio-  *
      *                  * ne secondo l'esito                          *
      *                  *---------------------------------------------*
       rou-let-reg-210.
      *                      *-----------------------------------------*
      *                      * Start su codici stampante               *
      *                      *-----------------------------------------*
           move      "ST"                 to   j-ope                  .
           move      "STP "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad errore             *
      *                      *-----------------------------------------*
           if        j-rsc                not  = spaces
                     go to rou-let-reg-300.
       rou-let-reg-220.
      *                      *-----------------------------------------*
      *                      * Next su codici stampante                *
      *                      *-----------------------------------------*
           move      "RN"                 to   j-ope                  .
           move      "STP "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *                      *-----------------------------------------*
      *                      * Se At end : ad errore                   *
      *                      *-----------------------------------------*
           if        j-rsc                not  = spaces
                     go to rou-let-reg-300.
      *                      *-----------------------------------------*
      *                      * Record in area di ridefinizione         *
      *                      *-----------------------------------------*
           move      j-dat                to   w-stp                  .
      *                      *-----------------------------------------*
      *                      * Se stampante locale : la si ignora      *
      *                      *-----------------------------------------*
           if        w-stp-cod-stp
                    (01 : 01)             =    "#"
                     go to rou-let-reg-220.
      *                      *-----------------------------------------*
      *                      * Se stampante in spool : la si ignora    *
      *                      *-----------------------------------------*
           if        w-stp-can-stp
                    (01 : 05)             =    "spool"
                     go to rou-let-reg-220.
      *                      *-----------------------------------------*
      *                      * Se stampante in remote copy : la si i-  *
      *                      * gnora                                   *
      *                      *-----------------------------------------*
           if        w-stp-can-stp
                    (01 : 03)             =    "rcp"
                     go to rou-let-reg-220.
      *                      *-----------------------------------------*
      *                      * Se stampante tipo 'ucp' : la si ignora  *
      *                      *-----------------------------------------*
           if        w-stp-can-stp
                    (01 : 03)             =    "ucp"
                     go to rou-let-reg-220.
      *                      *-----------------------------------------*
      *                      * Se canale di stampa pari a quello impo- *
      *                      * stato la selezione e' superata, altri-  *
      *                      * menti si prova con il codice stampante  *
      *                      * successivo memorizzato                  *
      *                      *-----------------------------------------*
           if        w-stp-can-stp        =    w-imp-can-stp
                     go to rou-let-reg-400
           else      go to rou-let-reg-220.
       rou-let-reg-300.
      *                  *---------------------------------------------*
      *                  * Se non esiste nemmeno una stampante con il  *
      *                  * canale di stampa impostato                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           move      "Non esiste alcuna stampante con questo canale di s
      -              "tampa !        "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Flag di errore                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-400.
      *                  *---------------------------------------------*
      *                  * Se esiste almeno una stampante con il cana- *
      *                  * le di stampa impostato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-600.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Record in area di lavoro                    *
      *                  *---------------------------------------------*
           move      j-dat                to   w-imp                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
       pre-acc-ins-100.
      *              *-------------------------------------------------*
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
       pre-acc-ins-110.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-200.
       pre-acc-ins-120.
      *                  *---------------------------------------------*
      *                  * Normalizzazione segnale di duplicazione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
       pre-acc-ins-130.
      *                  *---------------------------------------------*
      *                  * Copia del valore precedente, preservando    *
      *                  * il canale di stampa                         *
      *                  *---------------------------------------------*
           move      w-imp-can-stp        to   w-clo-imp-can-stp      .
           move      w-clo-imp-rec-imp    to   w-imp                  .
           move      w-clo-imp-can-stp    to   w-imp-can-stp          .
       pre-acc-ins-140.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flags di controllo          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di impostazione pagine di testata  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
       pre-acc-ins-150.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina di testata           *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
       pre-acc-ins-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              * Se inserimento                                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      "PT"                 to   j-ope                  .
           move      "IMP"                to   j-tre                  .
           move      w-imp-can-stp        to   j-kre                  .
           move      w-imp                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                =    spaces
                     go to scr-mov-fil-999.
           move      j-msg                to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Se modifica                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riscrittura record                          *
      *                  *---------------------------------------------*
           move      "UP"                 to   j-ope                  .
           move      "IMP"                to   j-tre                  .
           move      w-imp-can-stp        to   j-kre                  .
           move      w-imp                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                =    spaces
                     go to scr-mov-fil-999.
           move      j-msg                to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   j-ope                  .
           move      "IMP"                to   j-tre                  .
           move      w-imp-can-stp        to   j-kre                  .
           move      w-imp                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Find su stato impegno stampanti di [pss]                  *
      *    *-----------------------------------------------------------*
       fnd-pss-imp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-pss-imp-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg4510"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-pss-imp-sel
                     go to  fnd-pss-imp-999.
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
      *              * Preparazione variabile di i.p.c. 'cod-stp'      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-stp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      08                   to   s-car                  .
           move      w-fnd-pss-imp-can    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg4510"                       .
           cancel    "swd/xpg/prg/obj/pxpg4510"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "pss-imp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-pss-imp-sel
                     move  s-alf          to   w-fnd-pss-imp-can
           else      move  "#"            to   w-fnd-pss-imp-sel      .
       fnd-pss-imp-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-bla-emb-str                  *
      *    *-----------------------------------------------------------*
       tst-bla-emb-000.
           move      spaces               to   w-bla-emb-flg          .
           if        w-bla-emb-str        =    spaces
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr (1)    =    spaces
                     move  "#"            to   w-bla-emb-flg
                     go to tst-bla-emb-999.
           move      1                    to   w-bla-emb-ctr          .
       tst-bla-emb-100.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       not  = spaces
                     go to tst-bla-emb-100.
       tst-bla-emb-200.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       =    spaces
                     go to tst-bla-emb-200.
           move      "#"                  to   w-bla-emb-flg          .
       tst-bla-emb-999.
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

