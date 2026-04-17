       Identification Division.
       Program-Id.                                 pdcc1550           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    tab                 *
      *                                   Fase:    dcc155              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/04/05    *
      *                       Ultima revisione:    NdK del 14/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione descrizione forme di pagamento in  *
      *                    lingua straniera                            *
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
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcc155"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcc1550"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "DESCRIZIONE FORME DI PAGAMENTO IN LINGUA"       .

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
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfx"                          .
      *        *-------------------------------------------------------*
      *        * [zln]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzln"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-tip-rec          pic  9(02)                  .
               10  w-tes-cod-fop          pic  9(07)                  .
               10  w-tes-cod-fop-des      pic  x(40)                  .
               10  w-tes-cod-lng          pic  x(03)                  .
               10  w-tes-cod-lng-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-alt.
                   15  w-tes-ded-rig occurs 10
                                          pic  x(40)                  .

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
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per compattamento righe descrizione prodotto     *
      *        *-------------------------------------------------------*
           05  w-cmp-rig-dep.
               10  w-cmp-rig-dep-flg      pic  x(01)                  .
               10  w-cmp-rig-dep-nri      pic  9(02)                  .
               10  w-cmp-rig-dep-nrd      pic  9(02)                  .
               10  w-cmp-rig-dep-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice lingua                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzln0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzfp0.acl"                   .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore per percentuali                             *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per valori di default                           *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Codice lingua                                         *
      *        *-------------------------------------------------------*
           05  w-def-cod-lng              pic  x(03)                  .
           05  w-def-cod-lng-tfu          pic  x(03)                  .

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
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-zfp-opn-000  thru cod-mne-zfp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice lingua          *
      *              *-------------------------------------------------*
           perform   cod-cod-zln-opn-000  thru cod-cod-zln-opn-999    .
      *              *-------------------------------------------------*
      *              * Inizializzazione valori di default generale     *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-cod-lng          .
           move      spaces               to   w-def-cod-lng-tfu      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-zfp-cls-000  thru cod-mne-zfp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice lingua         *
      *              *-------------------------------------------------*
           perform   cod-cod-zln-cls-000  thru cod-cod-zln-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              * [zfx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * [zfx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
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
      *                  * Codice lingua                               *
      *                  *---------------------------------------------*
           perform   acc-cod-lng-000      thru acc-cod-lng-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice forma di pagamento                   *
      *                  *---------------------------------------------*
           perform   acc-cod-fop-000      thru acc-cod-fop-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     move  spaces         to   w-def-cod-lng-tfu
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
      *              * Codice lingua                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-lng-000      thru vis-cod-lng-999        .
      *              *-------------------------------------------------*
      *              * Codice forma di pagamento                       *
      *              *-------------------------------------------------*
           perform   vis-cod-fop-000      thru vis-cod-fop-999        .
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
      *              * Codice lingua                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-lng-000      thru pmt-cod-lng-999        .
      *              *-------------------------------------------------*
      *              * Codice forma di pagamento                       *
      *              *-------------------------------------------------*
           perform   pmt-cod-fop-000      thru pmt-cod-fop-999        .
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
      *    * Visualizzazione prompts per Codice lingua                 *
      *    *-----------------------------------------------------------*
       pmt-cod-lng-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice lingua              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-lng-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice forma di pagamento        *
      *    *-----------------------------------------------------------*
       pmt-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice lingua                 *
      *    *-----------------------------------------------------------*
       acc-cod-lng-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = spaces
                     go to acc-cod-lng-100.
           if        w-def-cod-lng        =    spaces
                     go to acc-cod-lng-100.
           move      w-def-cod-lng        to   w-tes-cod-lng          .
           move      w-tes-cod-lng        to   w-let-arc-zln-cod      .
           perform   let-arc-zln-000      thru let-arc-zln-999        .
           move      w-let-arc-zln-des    to   w-tes-cod-lng-des      .
           perform   vis-cod-lng-000      thru vis-cod-lng-999        .
           perform   vis-cod-lng-des-000  thru vis-cod-lng-des-999    .
           if        w-def-cod-lng-tfu    not  = spaces
                     go to acc-cod-lng-600.
       acc-cod-lng-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zln-ope      .
           move      w-tes-cod-lng        to   w-cod-cod-zln-cod      .
           move      04                   to   w-cod-cod-zln-lin      .
           move      30                   to   w-cod-cod-zln-pos      .
           move      spaces               to   v-edm                  .
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
           perform   vis-cod-lng-des-000  thru vis-cod-lng-des-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag                               *
      *                  *---------------------------------------------*
           if        w-let-arc-zln-flg    not  = spaces
                     go to acc-cod-lng-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = spaces
                     go to acc-cod-lng-500.
           go to     acc-cod-lng-100.
       acc-cod-lng-500.
      *              *-------------------------------------------------*
      *              * Lingua Italiana non permessa                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul codice lingua                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-lng        not  = "I  "
                     go to acc-cod-lng-600.
      *                      *-----------------------------------------*
      *                      * Se valore non trovato in tabella : mes- *
      *                      * saggio di errore e reimpostazione       *
      *                      *-----------------------------------------*
           move      "Descrizione in Italiano non gestibile !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-lng-100.
       acc-cod-lng-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento default                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-lng        to   w-def-cod-lng          .
           move      "#"                  to   w-def-cod-lng-tfu      .
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
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-lng        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lng-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione lingua        *
      *    *-----------------------------------------------------------*
       vis-cod-lng-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-lng-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lng-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Forma di pagamento                   *
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
           move      w-tes-cod-fop        to   w-cod-mne-zfp-cod      .
           move      05                   to   w-cod-mne-zfp-lin      .
           move      30                   to   w-cod-mne-zfp-pos      .
           move      05                   to   w-cod-mne-zfp-dln      .
           move      41                   to   w-cod-mne-zfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
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
       acc-cod-fop-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-fop-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-fop          .
       acc-cod-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-fop-450.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfp]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop        to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zfp-des    to   w-tes-cod-fop-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fop-000      thru vis-des-fop-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zfp-flg    not  = spaces
                     go to acc-cod-fop-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione a meno    *
      *                  * non si sia premuto il tasto 'Up'            *
      *                  *---------------------------------------------*
           if        w-tes-cod-fop        not  = zero
                     go to acc-cod-fop-600.
           if        v-key                =    "UP  "
                     go to acc-cod-fop-800
           else      go to acc-cod-fop-100.
       acc-cod-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-fop-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-fop-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-fop-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-fop-999.
       acc-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice forma di pagamento         *
      *    *-----------------------------------------------------------*
       vis-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fop        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione forma di pagamento    *
      *    *-----------------------------------------------------------*
       vis-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-fop-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fop-999.
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
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Descrizione in lingua                       *
      *                  *---------------------------------------------*
           perform   acc-des-alt-000      thru acc-des-alt-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
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
      *              * Descrizione in lingua                           *
      *              *-------------------------------------------------*
           perform   vis-des-alt-000      thru vis-des-alt-999        .
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
      *              * Descrizione in lingua                           *
      *              *-------------------------------------------------*
           perform   pmt-des-alt-000      thru pmt-des-alt-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione in lingua            *
      *    *-----------------------------------------------------------*
       pmt-des-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione in lingua      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-alt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione in lingua        *
      *    *-----------------------------------------------------------*
       acc-des-alt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-alt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-alt (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-alt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-alt-999.
       acc-des-alt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-alt (1)      .
       acc-des-alt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-alt (1)    not  = spaces
                     go to acc-des-alt-450.
           if        v-key                =    "UP  "
                     go to acc-des-alt-600
           else      go to acc-des-alt-100.
       acc-des-alt-450.
      *                  *---------------------------------------------*
      *                  * Compattamento verso l'alto delle righe di   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           perform   cmp-rig-dep-000      thru cmp-rig-dep-999        .
      *                  *---------------------------------------------*
      *                  * Se compattamento avvenuto : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-cmp-rig-dep-flg    not  = spaces
                     go to acc-des-alt-100.
       acc-des-alt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-alt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-alt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-alt-100.
       acc-des-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione in lingua     *
      *    *-----------------------------------------------------------*
       vis-des-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-alt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-alt-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento righe della descrizione prodotto *
      *    *-----------------------------------------------------------*
       cmp-rig-dep-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di rivisualizzazione de-   *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cmp-rig-dep-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione numero righe attuale             *
      *              *-------------------------------------------------*
           move      11                   to   w-cmp-rig-dep-nri      .
       cmp-rig-dep-100.
           subtract  1                    from w-cmp-rig-dep-nri      .
           if        w-tes-ded-rig
                    (1, w-cmp-rig-dep-nri)
                                          =    spaces
                     go to cmp-rig-dep-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero righe descrizione       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-dep-nrd      .
      *              *-------------------------------------------------*
      *              * Ciclo per compattamento                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-dep-ctr      .
       cmp-rig-dep-200.
           add       1                    to   w-cmp-rig-dep-ctr      .
           if        w-cmp-rig-dep-ctr    >    w-cmp-rig-dep-nri
                     go to cmp-rig-dep-300.
      *                  *---------------------------------------------*
      *                  * Se riga di descrizione a spazi : riciclo    *
      *                  *---------------------------------------------*
           if        w-tes-ded-rig
                    (1, w-cmp-rig-dep-ctr)
                                          =    spaces
                     go to cmp-rig-dep-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero righe           *
      *                  *---------------------------------------------*
           add       1                    to   w-cmp-rig-dep-nrd      .
      *                  *---------------------------------------------*
      *                  * Spostamento riga di descrizione             *
      *                  *---------------------------------------------*
           move      w-tes-ded-rig
                    (1, w-cmp-rig-dep-ctr)
                                          to   w-tes-ded-rig
                                              (1, w-cmp-rig-dep-nrd)  .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cmp-rig-dep-200.
       cmp-rig-dep-300.
      *              *-------------------------------------------------*
      *              * Abblencamento righe residue                     *
      *              *-------------------------------------------------*
           move      w-cmp-rig-dep-nrd    to   w-cmp-rig-dep-ctr      .
       cmp-rig-dep-320.
           add       1                    to   w-cmp-rig-dep-ctr      .
           if        w-cmp-rig-dep-ctr    >    w-cmp-rig-dep-nri
                     go to cmp-rig-dep-900.
           move      spaces               to   w-tes-ded-rig
                                              (1, w-cmp-rig-dep-ctr)  .
           go to     cmp-rig-dep-320.
       cmp-rig-dep-900.
      *              *-------------------------------------------------*
      *              * Determinazione del flag di rivisualizzazione    *
      *              *-------------------------------------------------*
           if        w-cmp-rig-dep-nri    not  = w-cmp-rig-dep-nrd
                     move  "#"            to   w-cmp-rig-dep-flg      .
       cmp-rig-dep-999.
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
      *              * Test su Codice lingua                           *
      *              *-------------------------------------------------*
           if        w-tes-cod-lng        =    spaces
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
      *              * Controllo su Descrizione in lingua              *
      *              *-------------------------------------------------*
           if        w-tes-des-alt (1)    not  = spaces
                     go to cnt-tdo-nok-800.
           move      "Manca la descrizione in lingua                  "
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
           move      zero                 to   w-tes-tip-rec          .
           move      zero                 to   w-tes-cod-fop          .
           move      spaces               to   w-tes-cod-fop-des      .
           move      spaces               to   w-tes-cod-lng          .
           move      spaces               to   w-tes-cod-lng-des      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-alt (1)      .
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
      *              * Preparazione tipo record                        *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-tip-rec          .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG    "         to   f-key                  .
           move      w-tes-tip-rec        to   rf-zfx-tip-rec         .
           move      w-tes-cod-fop        to   rf-zfx-cod-fop         .
           move      w-tes-cod-lng        to   rf-zfx-cod-lng         .
           move      01                   to   rf-zfx-num-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
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
      *                          * Start su file [zfx]                 *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      02                   to   rf-zfx-tip-rec         .
           move      w-tes-cod-fop        to   rf-zfx-cod-fop         .
           move      w-tes-cod-lng        to   rf-zfx-cod-lng         .
           move      zero                 to   rf-zfx-num-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                              *---------------------------------*
      *                              * Test su esito operazione        *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-340.
       rou-let-reg-320.
      *                                  *-----------------------------*
      *                                  * Lettura sequenziale file    *
      *                                  * [zfx]                       *
      *                                  *-----------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                                  *-----------------------------*
      *                                  * Test se 'at end'            *
      *                                  *-----------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-340.
      *                                  *-----------------------------*
      *                                  * Test sul massimo            *
      *                                  *-----------------------------*
           if        rf-zfx-tip-rec       not  = 02            or
                     rf-zfx-cod-fop       not  = w-tes-cod-fop or
                     rf-zfx-cod-lng       not  = w-tes-cod-lng
                     go to rou-let-reg-340.
      *                                  *-----------------------------*
      *                                  * Bufferizzazione descrizione *
      *                                  *-----------------------------*
           move      rf-zfx-des-alt       to   w-tes-ded-rig
                                              (1, rf-zfx-num-prg)     .
      *                                  *-----------------------------*
      *                                  * Riciclo su lettura          *
      *                                  *-----------------------------*
           go to     rou-let-reg-320.
       rou-let-reg-340.
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
      *              * Trattamento file [zfx]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zfx]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zfx-000      thru wrt-rec-zfx-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zfx]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zfx-000      thru rew-rec-zfx-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zfx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zfx-000      thru del-rec-zfx-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zfx]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zfx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      02                   to   rf-zfx-tip-rec         .
           move      w-tes-cod-fop        to   rf-zfx-cod-fop         .
           move      w-tes-cod-lng        to   rf-zfx-cod-lng         .
           move      w-cix-ctr-001        to   rf-zfx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-ded-rig
                    (1, w-cix-ctr-001)    to   rf-zfx-des-alt         .
       cmp-rec-zfx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zfx]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zfx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per scrittura fino a 10 righe di descri-  *
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       wrt-rec-zfx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-zfx-999.
           if        w-tes-ded-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to wrt-rec-zfx-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [zfx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-zfx-000      thru cmp-rec-zfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     wrt-rec-zfx-100.
       wrt-rec-zfx-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       wrt-rec-zfx-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-zfx-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           move      02                   to   rf-zfx-tip-rec         .
           move      w-tes-cod-fop        to   rf-zfx-cod-fop         .
           move      w-tes-cod-lng        to   rf-zfx-cod-lng         .
           move      w-cix-ctr-001        to   rf-zfx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-rec-zfx-520.
       wrt-rec-zfx-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zfx]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zfx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per riscrittura fino a 10 righe di descri-*
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       rew-rec-zfx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-zfx-999.
           if        w-tes-ded-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to rew-rec-zfx-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [zfx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-zfx-000      thru cmp-rec-zfx-999        .
      *                  *---------------------------------------------*
      *                  * Forced put record                           *
      *                  *---------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-zfx-100.
       rew-rec-zfx-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       rew-rec-zfx-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-zfx-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           move      02                   to   rf-zfx-tip-rec         .
           move      w-tes-cod-fop        to   rf-zfx-cod-fop         .
           move      w-tes-cod-lng        to   rf-zfx-cod-lng         .
           move      w-cix-ctr-001        to   rf-zfx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-zfx-520.
       rew-rec-zfx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zfx]                                *
      *    *-----------------------------------------------------------*
       del-rec-zfx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione fino a 10 righe di de-  *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       del-rec-zfx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to del-rec-zfx-999.
      *                  *---------------------------------------------*
      *                  * Composizione record [zfx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-zfx-000      thru cmp-rec-zfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     del-rec-zfx-100.
       del-rec-zfx-999.
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
      *    * Subroutines per l'accettazione del codice lingua          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzln0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzfp0.acs"                   .

