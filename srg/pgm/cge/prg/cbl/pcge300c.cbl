       Identification Division.
       Program-Id.                                 pcge300c           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/12/89    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Primanota di contabilita' generale          *
      *                                                                *
      *                    Pareggiamento acconti cliente               *
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
      *            * Per routine rou-let-reg-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-let-reg      pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Per tasto Do su saldaconto                        *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-sdc-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            * - S : Saldaconto                                  *
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

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-imp-sdc          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det partite aperte cliente                   *
      *        *-------------------------------------------------------*
           05  w-det-pap-cli.
               10  w-det-pap-cli-cod      pic  9(07)                  .
               10  w-det-pap-cli-flg      pic  x(01)                  .
               10  w-det-pap-cli-msg      pic  x(76)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-imp-ass              pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per tabella partite                             *
      *    *-----------------------------------------------------------*
       01  w-par.
      *        *-------------------------------------------------------*
      *        * Comodo per rottura data partita                       *
      *        *-------------------------------------------------------*
           05  w-par-rot-dat              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura numero partita                     *
      *        *-------------------------------------------------------*
           05  w-par-rot-num              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-par-flg-uno              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per funzione saldaconto                         *
      *    *-----------------------------------------------------------*
       01  w-sdc.
           05  w-sdc-npg-max              pic  9(03)                  .
           05  w-sdc-npg-vis              pic  9(03)                  .
           05  w-sdc-npg-dat              pic  9(03)                  .
           05  w-sdc-ctr-rig              pic  9(03)                  .
           05  w-sdc-rig-ini              pic  9(03)                  .
           05  w-sdc-tot-ass              pic s9(11)                  .
           05  w-sdc-tot-prg              pic s9(11)                  .
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-s11              pic s9(11)                  .
           05  w-sdc-flg-imr              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatore generico 'I'                                *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work-area per linkage programma gestione pareggiamento au-*
      *    * tomatico acconti                                          *
      *    *-----------------------------------------------------------*
       01  w-lpa.
           05  w-lpa-exi-sts              pic  x(01)                  .
           05  w-lpa-tip-arc              pic  x(01)                  .
           05  w-lpa-cod-arc              pic  9(07)                  .
           05  w-lpa-rag-arc              pic  x(40)                  .
           05  w-lpa-dri-doc              pic  9(07)                  .
           05  w-lpa-nri-doc              pic  x(10)                  .
           05  w-lpa-tot-doc              pic s9(13)                  .
           05  w-lpa-max-ele              pic  9(03)                  .
           05  w-lpa-num-ele              pic  9(03)                  .
           05  w-lpa-tbl-par.
               10  w-lpa-tbp-ele occurs 501.
                   15  w-lpa-dat-rif      pic  9(07)       comp-3     .
                   15  w-lpa-num-rif      pic  x(10)                  .
                   15  w-lpa-res-par      pic s9(13)       comp-3     .
                   15  w-lpa-imp-ass      pic s9(13)       comp-3     .
                   15  w-lpa-prg-rig      pic s9(13)       comp-3     .

      ******************************************************************
       Procedure Division                using w-lpa                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
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
      *                  * Se esito negativo : uscita                  *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione campi non chiave       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pre-acc-ins-000
                                          thru pre-acc-ins-999
      *                  *---------------------------------------------*
      *                  * Se Modifica                                 *
      *                  *---------------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pre-acc-mod-000
                                          thru pre-acc-mod-999
      *                  *---------------------------------------------*
      *                  * Se Visualizzazione                          *
      *                  *---------------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pre-acc-vis-000
                                          thru pre-acc-vis-999
      *                  *---------------------------------------------*
      *                  * Se tipo funzionamento errato : uscita       *
      *                  *---------------------------------------------*
           else      go to main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *              * Tasto di funzione Pf3 : sempre abilitato        *
      *              *-------------------------------------------------*
           move      "[3] "               to   v-pfk (17)             .
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
      *              * Se Pf3                                          *
      *              *-------------------------------------------------*
           if        v-key                not  = "[3] "
                     go to exe-acc-cmp-800.
           perform   pf3-cge-300-000      thru pf3-cge-300-999        .
           go to     exe-acc-cmp-400.
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
      *    * Tasto funzione "Pf3" disponibile ad ogni impostazione     *
      *    *-----------------------------------------------------------*
       pf3-cge-300-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  pf3-cge-300-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       pf3-cge-300-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
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
      *              * Altre operazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione partite aperte cliente       *
      *                  *---------------------------------------------*
           move      w-lpa-cod-arc        to   w-det-pap-cli-cod      .
           perform   det-pap-cli-000      thru det-pap-cli-999        .
           if        w-det-pap-cli-flg    =    spaces
                     go to pre-exe-pgm-500.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      w-det-pap-cli-msg    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita con errore                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-500.
      *              *-------------------------------------------------*
      *              * Intestazione cliente                            *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Partite aperte del cliente   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpa-cod-arc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    v-edt
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-lpa-rag-arc
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 05                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
                     go to acc-key-reg-000.
           move      "S"                  to   w-cnt-tus-acc-key      .
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campi chiave della registrazione          *
      *    *-----------------------------------------------------------*
       vis-key-reg-000.
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
       pmt-key-reg-999.
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
       acc-nok-reg-300.
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
           perform   cnt-tdo-sdc-000      thru cnt-tdo-sdc-999        .
           if        w-cnt-tdo-sdc-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
           else      move  spaces         to   w-cnt-tdo-sdc-flg
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
           go to     acc-nok-reg-300.
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
      *              * Se tipo impostazione saldaconto ad accettazione *
      *              * saldaconto                                      *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "S"
                     go to acc-tes-reg-800.
      *              *-------------------------------------------------*
      *              * Tipo impostazione : testata                     *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-mfu-tip-imp      .
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : saldaconto                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-mfu-tip-imp      .
       acc-tes-reg-800.
      *              *-------------------------------------------------*
      *              * Accettazione funzione saldaconto                *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
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
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Funzione saldaconto              *
      *    *-----------------------------------------------------------*
       pmt-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Intestazione colonne                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Data   |   Numero   |  Saldo partita  | Importo 
      -              "da assegnare |   Progressivo"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Colonne per saldaconto                          *
      *              *-------------------------------------------------*
           move      zero                 to   I                      .
       pmt-fun-sdc-100.
           add       1                    to   I                      .
           if        I                    >    13
                     go to pmt-fun-sdc-200.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       6
                     I                  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "         |            |                 |         
      -              "             |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-fun-sdc-100.
       pmt-fun-sdc-200.
      *              *-------------------------------------------------*
      *              * Trattini a linea 20                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 21                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal 'Totale documento :'                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Totale documento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo documento                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G<"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-lpa-tot-doc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fine colonne                                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazioni per funzione saldaconto                      *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Se impostazione saldaconto gia' avvenuta        *
      *              *-------------------------------------------------*
           if        w-tes-imp-sdc (1)    =    spaces
                     go to  acc-fun-sdc-100.
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  11                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-100.
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-lpa-num-ele        to   w-sdc-npg-max          .
           divide    12                   into w-sdc-npg-max
                                        giving w-sdc-npg-max
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-max          .
      *              *-------------------------------------------------*
      *              * Selezione preventiva : solo se in inserimento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform sel-pre-sdc-000
                                          thru sel-pre-sdc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           perform   pmt-fun-sdc-000      thru pmt-fun-sdc-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina visualizzata                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      w-sdc-rig-ini        to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-lpa-num-ele
                     go to  acc-fun-sdc-900.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    12                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
      *              *-------------------------------------------------*
      *              * Visualizzazione totale assegnato e totale pro-  *
      *              * gressivo                                        *
      *              *-------------------------------------------------*
           if        w-sdc-npg-vis        =    zero
                     perform vis-tot-ass-000
                                          thru vis-tot-ass-999
                     perform vis-tot-prg-000
                                          thru vis-tot-prg-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina da trattare              *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        not  = w-sdc-npg-vis
                     perform vis-pag-sdc-000
                                          thru vis-pag-sdc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-sdc-npg-dat
                                        giving w-sdc-wrk-rig          .
           multiply  12                   by   w-sdc-wrk-rig          .
           subtract  w-sdc-wrk-rig        from w-sdc-ctr-rig
                                        giving w-sdc-wrk-lin          .
           add       7                    to   w-sdc-wrk-lin          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di impostazione riga       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-imr          .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Importo assegnato                           *
      *                  *---------------------------------------------*
           perform   acc-imp-ris-000      thru acc-imp-ris-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-fun-sdc-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-500.
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-500.
      *                      *-----------------------------------------*
      *                      * Se Prev Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to  acc-fun-sdc-600.
           subtract  1                    from w-sdc-npg-dat          .
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  11                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-600.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                =    "TAB "
                     move   w-lpa-num-ele to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "BACK"
                     move   1             to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Attivazione flag di impostazione riga           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sdc-flg-imr          .
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-200.
       acc-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Attivazione flag di saldaconto impostato        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-imp-sdc (1)      .
       acc-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Selezione preventiva per funzione saldaconto              *
      *    *-----------------------------------------------------------*
       sel-pre-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione totale riscosso                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-tot-ass          .
      *              *-------------------------------------------------*
      *              * Normalizzazione totale progressivo              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-tot-prg          .
       sel-pre-sdc-100.
      *              *-------------------------------------------------*
      *              * Selezione su partite fino al totale documento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga iniziale : zero                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-rig-ini          .
      *                  *---------------------------------------------*
      *                  * Ciclo di preparazione colonna progressivi   *
      *                  *---------------------------------------------*
           move      zero                 to   I                      .
       sel-pre-sdc-110.
           add       1                    to   I                      .
           if        I                    >    w-lpa-num-ele
                     go to  sel-pre-sdc-200.
           add       w-lpa-res-par (I)    to   w-sdc-tot-prg          .
           move      w-sdc-tot-prg        to   w-lpa-prg-rig (I)      .
           go to     sel-pre-sdc-110.
       sel-pre-sdc-200.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per selezione partite *
      *                  *---------------------------------------------*
           move      zero                 to   I                      .
       sel-pre-sdc-210.
           add       1                    to   I                      .
           if        I                    >    w-lpa-num-ele
                     go to  sel-pre-sdc-215.
           add       w-lpa-res-par (I)    to   w-sdc-tot-ass          .
           if        w-sdc-tot-ass        =    w-lpa-tot-doc
                     move   I             to   w-sdc-wrk-rig
                     go to  sel-pre-sdc-220.
           if        w-sdc-tot-ass        <    w-lpa-tot-doc
                     go to sel-pre-sdc-210.
       sel-pre-sdc-215.
           move      zero                 to   w-sdc-wrk-rig          .
       sel-pre-sdc-220.
      *                  *---------------------------------------------*
      *                  * Ciclo di selezione partite                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-tot-ass          .
           move      zero                 to   I                      .
       sel-pre-sdc-230.
           add       1                    to   I                      .
           if        I                    >    w-sdc-wrk-rig
                     go to  sel-pre-sdc-240.
           add       w-lpa-res-par (I)    to   w-sdc-tot-ass          .
           move      w-lpa-res-par (I)    to   w-lpa-imp-ass (I)      .
           go to     sel-pre-sdc-230.
       sel-pre-sdc-240.
       sel-pre-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina saldaconto                         *
      *    *-----------------------------------------------------------*
       vis-pag-sdc-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento numero pagina visualizzata        *
      *              *-------------------------------------------------*
           move      w-sdc-npg-dat        to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Determinazione contatore righe iniziale         *
      *              *-------------------------------------------------*
           multiply  12                   by   w-sdc-npg-dat
                                      giving   w-sdc-wrk-rig          .
           subtract  12                   from w-sdc-wrk-rig          .
           move      zero                 to   I                      .
       vis-pag-sdc-100.
           add       1                    to   I                      .
           if        I                    >    12
                     go to  vis-pag-sdc-900.
           add       1                    to   w-sdc-wrk-rig          .
           if        w-sdc-wrk-rig        >    w-lpa-num-ele
                     go to  vis-pag-sdc-120
           else      go to  vis-pag-sdc-140.
       vis-pag-sdc-120.
           if        w-sdc-npg-dat        =    1
                     go to  vis-pag-sdc-900.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "         |            |                 |         
      -              "             |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-pag-sdc-100.
       vis-pag-sdc-140.
      *              *-------------------------------------------------*
      *              * Visualizzazione riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data riferimento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           add       7
                     I                  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-lpa-dat-rif
                    (w-sdc-wrk-rig)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero riferimento                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      12                   to   v-pos                  .
      *
           move      w-lpa-num-rif
                    (w-sdc-wrk-rig)       to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Residuo partita                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      25                   to   v-pos                  .
           move      w-lpa-res-par
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo assegnato                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      46                   to   v-pos                  .
           move      w-lpa-imp-ass
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Progressivo di riga                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-lpa-prg-rig
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su riga successiva                  *
      *                  *---------------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Importo assegnato            *
      *    *-----------------------------------------------------------*
       acc-imp-ris-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valori riga precedenti          *
      *                  *---------------------------------------------*
           move      w-lpa-imp-ass
                    (w-sdc-ctr-rig)       to   w-sav-imp-ass          .
       acc-imp-ris-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      46                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Assegnazione tasti funzione                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Up   : ammesso se numero riga superiore *
      *                      *        a 1                              *
      *                      *-----------------------------------------*
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
      *                      *-----------------------------------------*
      *                      * Down : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                      *-----------------------------------------*
      *                      * Do   : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Nxsc : ammesso solo se pagina da tratta-*
      *                      *        re minore di numero pagine mas-  *
      *                      *        simo                             *
      *                      *-----------------------------------------*
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
      *                      *-----------------------------------------*
      *                      * Prsc : ammesso solo se su pagina succes-*
      *                      *        siva alla prima                  *
      *                      *-----------------------------------------*
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
      *                      *-----------------------------------------*
      *                      * Tab  : ammesso se non si e' gia' sul-   *
      *                      *        l'ultima riga                    *
      *                      *-----------------------------------------*
           if        w-sdc-ctr-rig        <    w-lpa-num-ele
                     move   "TAB "        to   v-pfk (08)             .
      *                      *-----------------------------------------*
      *                      * Back : ammesso se non si e' gia' sul-   *
      *                      *        la prima riga                    *
      *                      *-----------------------------------------*
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
      *                      *-----------------------------------------*
      *                      * Slct : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "SLCT"               to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Remv : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "REMV"               to   v-pfk (11)             .
           move      w-lpa-imp-ass
                    (w-sdc-ctr-rig)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-imp-ris-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-imp-ris-999.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-imp-ris-200.
      *                  *---------------------------------------------*
      *                  * Controaggiornamento totale assegnato        *
      *                  *---------------------------------------------*
           subtract  w-lpa-imp-ass
                    (w-sdc-ctr-rig)        from w-sdc-tot-ass         .
           perform   vis-tot-ass-000      thru vis-tot-ass-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori riga                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)         .
           perform vis-imp-ass-000        thru vis-imp-ass-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-imp-ris-999.
       acc-imp-ris-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Up, Down, Prsc, Nxsc, Tab e Back             *
      *              *-------------------------------------------------*
           if        v-key                not  = "UP  " and
                     v-key                not  = "DOWN" and
                     v-key                not  = "PRSC" and
                     v-key                not  = "NXSC" and
                     v-key                not  = "TAB " and
                     v-key                not  = "BACK"
                     go to  acc-imp-ris-300.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-lpa-imp-ass
                    (w-sdc-ctr-rig)       not  = w-sav-imp-ass
                     move   w-sav-imp-ass to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)
                     go to  acc-imp-ris-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita                         *
      *                  *---------------------------------------------*
           go to     acc-imp-ris-999.
       acc-imp-ris-300.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-imp-ris-400.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-lpa-imp-ass
                    (w-sdc-ctr-rig)       not  = w-sav-imp-ass
                     move   w-sav-imp-ass to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)
                     go to  acc-imp-ris-100.
      *                  *---------------------------------------------*
      *                  * Azioni per Slct                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se importo gia' esistente               *
      *                      *-----------------------------------------*
           if        w-lpa-imp-ass
                    (w-sdc-ctr-rig)       =    zero
                     go to  acc-imp-ris-320.
      *                          *-------------------------------------*
      *                          * Aggiornamento totale riscosso       *
      *                          *-------------------------------------*
           subtract  w-lpa-imp-ass
                    (w-sdc-ctr-rig)       from w-sdc-tot-ass          .
      *                          *-------------------------------------*
      *                          * Aggiornamento elementi riga         *
      *                          *-------------------------------------*
           move      zero                 to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)         .
           go to     acc-imp-ris-340.
       acc-imp-ris-320.
      *                      *-----------------------------------------*
      *                      * Se importo non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento elementi riga         *
      *                          *-------------------------------------*
           move      w-lpa-res-par
                    (w-sdc-ctr-rig)       to   w-lpa-imp-ass
                                              (w-sdc-ctr-rig)         .
      *                          *-------------------------------------*
      *                          * Aggiornamento totale assegnato      *
      *                          *-------------------------------------*
           add       w-lpa-imp-ass
                    (w-sdc-ctr-rig)       to   w-sdc-tot-ass          .
       acc-imp-ris-340.
      *                      *-----------------------------------------*
      *                      * Visualizzazione importo assegnato       *
      *                      *-----------------------------------------*
           perform   vis-imp-ass-000      thru vis-imp-ass-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione totale assegnato        *
      *                      *-----------------------------------------*
           perform   vis-tot-ass-000      thru vis-tot-ass-999        .
      *                      *-----------------------------------------*
      *                      * Uscita come per 'Down'                  *
      *                      *-----------------------------------------*
           move      "DOWN"               to   v-key                  .
           go to     acc-imp-ris-999.
       acc-imp-ris-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-imp-ris-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale assegnato              *
      *                  *---------------------------------------------*
           subtract  w-sav-imp-ass        from w-sdc-tot-ass          .
           add       w-lpa-imp-ass
                    (w-sdc-ctr-rig)       to   w-sdc-tot-ass          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione totale assegnato            *
      *                  *---------------------------------------------*
           perform   vis-tot-ass-000      thru vis-tot-ass-999        .
       acc-imp-ris-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-sdc-000
                                          thru cnt-tdo-sdc-999
                     if      w-cnt-tdo-sdc-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-imp-ris-999
                     else    move  spaces to   w-cnt-tdo-sdc-flg
                             go to acc-imp-ris-100.
       acc-imp-ris-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Importo assegnato         *
      *    *-----------------------------------------------------------*
       vis-imp-ass-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      46                   to   v-pos                  .
           move      w-lpa-imp-ass
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-ass-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Totale assegnato          *
      *    *-----------------------------------------------------------*
       vis-tot-ass-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      w-sdc-tot-ass        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-ass-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Totale progressivo        *
      *    *-----------------------------------------------------------*
       vis-tot-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-sdc-tot-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-prg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione partite aperte cliente                     *
      *    *-----------------------------------------------------------*
       det-pap-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-pap-cli-flg      .
      *              *-------------------------------------------------*
      *              * Flag di primo passaggio                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-par-flg-uno          .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore numero partite        *
      *              *-------------------------------------------------*
           move      zero                 to   w-lpa-num-ele          .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di comodo per rottura par- *
      *              * tita                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-par-rot-dat          .
           move      spaces               to   w-par-rot-num          .
      *              *-------------------------------------------------*
      *              * Start su file [mgr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCRIF"             to   f-key                  .
           move      "C"                  to   rf-mgr-tip-arc         .
           move      w-det-pap-cli-cod    to   rf-mgr-cod-arc         .
           move      zero                 to   rf-mgr-dat-rif         .
           move      spaces               to   rf-mgr-num-rif         .
           move      zero                 to   rf-mgr-dat-reg         .
           move      zero                 to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to  det-pap-cli-300.
       det-pap-cli-200.
      *                  *---------------------------------------------*
      *                  * Se errore                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione messaggio                  *
      *                      *-----------------------------------------*
           move      "Attenzione : nessuna partita aperta per il cliente
      -              " !"                 to   w-det-pap-cli-msg      .
      *                      *-----------------------------------------*
      *                      * Uscita con segnale di errore            *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-pap-cli-flg      .
           go to     det-pap-cli-999.
       det-pap-cli-300.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mgr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-mgr-tip-arc       not  = "C"            or
                     rf-mgr-cod-arc       not  = w-det-pap-cli-cod
                     go to  det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Se rottura data e numero partita                *
      *              *-------------------------------------------------*
           if        rf-mgr-dat-rif       =    w-par-rot-dat and
                     rf-mgr-num-rif       =    w-par-rot-num
                     go to  det-pap-cli-500.
      *                  *---------------------------------------------*
      *                  * Fine partita precedente                     *
      *                  *---------------------------------------------*
           perform   det-pap-cli-910      thru det-pap-cli-919        .
           if        w-lpa-num-ele        not  > w-lpa-max-ele
                     go to  det-pap-cli-400.
       det-pap-cli-350.
      *                      *-----------------------------------------*
      *                      * Se esubero numero partite per cliente   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Attenzione : cliente con partite aperte oltre il m
      -              "assimo consentito !"
                                          to   w-det-pap-cli-msg      .
      *                          *-------------------------------------*
      *                          * Uscita con segnale di errore        *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-pap-cli-flg      .
           go to     det-pap-cli-999.
       det-pap-cli-400.
      *                  *---------------------------------------------*
      *                  * Inizio partita successiva                   *
      *                  *---------------------------------------------*
           perform   det-pap-cli-900      thru det-pap-cli-909        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento area rottura data e numero    *
      *                  * partita                                     *
      *                  *---------------------------------------------*
           move      rf-mgr-dat-rif       to   w-par-rot-dat          .
           move      rf-mgr-num-rif       to   w-par-rot-num          .
       det-pap-cli-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento residuo partita                   *
      *              *-------------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     subtract rf-mgr-imp-mov
                                          from w-lpa-res-par (501)
           else      add      rf-mgr-imp-mov
                                          to   w-lpa-res-par (501)    .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura file [mgr]                   *
      *              *-------------------------------------------------*
           go to     det-pap-cli-300.
       det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Fine partita precedente                         *
      *              *-------------------------------------------------*
           perform   det-pap-cli-910      thru det-pap-cli-919        .
           if        w-lpa-num-ele        >    w-lpa-max-ele
                     go to  det-pap-cli-350.
      *              *-------------------------------------------------*
      *              * Test se esiste almeno una partita per il clien- *
      *              * te                                              *
      *              *-------------------------------------------------*
           if        w-lpa-num-ele        =    zero
                     go to  det-pap-cli-200.
           go to     det-pap-cli-999.
       det-pap-cli-900.
      *              *-------------------------------------------------*
      *              * Subroutine interna di inizio partita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati partita nell'elemento   *
      *                  * 501esimo                                    *
      *                  *---------------------------------------------*
           move      rf-mgr-dat-rif       to   w-lpa-dat-rif (501)    .
           move      rf-mgr-num-rif       to   w-lpa-num-rif (501)    .
           move      zero                 to   w-lpa-res-par (501)    .
           move      zero                 to   w-lpa-imp-ass (501)    .
           move      zero                 to   w-lpa-prg-rig (501)    .
       det-pap-cli-909.
           exit.
       det-pap-cli-910.
      *              *-------------------------------------------------*
      *              * Subroutine interna di fine partita              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se primo passaggio : uscita                 *
      *                  *---------------------------------------------*
           if        w-par-flg-uno        not  = spaces
                     move   spaces        to   w-par-flg-uno
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Se saldo partita zero : uscita              *
      *                  *---------------------------------------------*
           if        w-lpa-res-par (501)  =    zero
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero partite         *
      *                  *---------------------------------------------*
           add       1                    to   w-lpa-num-ele          .
           if        w-lpa-num-ele        >    w-lpa-max-ele
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Aggiornamento tabella partite               *
      *                  *---------------------------------------------*
           move      w-lpa-tbp-ele (501)  to   w-lpa-tbp-ele
                                              (w-lpa-num-ele)         .
       det-pap-cli-919.
           exit.
       det-pap-cli-999.
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
       cnt-tdo-nok-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do saldaconto             *
      *    *-----------------------------------------------------------*
       cnt-tdo-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-sdc-flg      .
      *              *-------------------------------------------------*
      *              * Test su totale assegnato                        *
      *              *-------------------------------------------------*
           if        w-sdc-tot-ass        =    zero
                     move   "#"           to   w-cnt-tdo-sdc-flg      .
       cnt-tdo-sdc-999.
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
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-imp-sdc (1)      .
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
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : impostazione           *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
       pre-acc-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per visualizzazione              *
      *    *-----------------------------------------------------------*
       pre-acc-vis-000.
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
      *              * Determinazione status uscita da saldaconto      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-lpa-exi-sts          .
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
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
