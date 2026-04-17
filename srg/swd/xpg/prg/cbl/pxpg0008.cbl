       Identification Division.
       Program-Id.                                 pxpg0008           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/07/24    *
      *                       Ultima revisione:    NdK del 29/07/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Cambio password utente (.w)                 *
      *                                                                *
      *                    ___ DA IMPLEMENTARE ___                     *
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
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "utente"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg0008"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "         CAMBIO PASSWORD UTENTE         "       .

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

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
      *            *---------------------------------------------------*
      *            * Mese                                              *
      *            *---------------------------------------------------*
               10  w-tes-mes-cal          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Secolo/anno                                       *
      *            *---------------------------------------------------*
               10  w-tes-saa-cal          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Giorno della settimana del primo giorno del mese  *
      *            *---------------------------------------------------*
               10  w-tes-gds-pgm          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  filler                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per trattamento variabili di i.p.c.             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per variabile di i.p.c. 'pxpg2200'                    *
      *        *-------------------------------------------------------*
           05  w-ipc-xpg-220.
      *            *---------------------------------------------------*
      *            * Si/No richiamo programma per modifica della pro-  *
      *            * pria password                                     *
      *            *  - S : Si                                         *
      *            *  - N : No                                         *
      *            *---------------------------------------------------*
               10  w-ipc-xpg-220-pwd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice utente eventualmente passato dal chiamante *
      *            *---------------------------------------------------*
               10  w-ipc-xpg-220-ute      pic  x(08)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Mese                                                  *
      *        *-------------------------------------------------------*
           05  w-sav-mes-cal              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Secolo/anno                                           *
      *        *-------------------------------------------------------*
           05  w-sav-saa-cal              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Mese                                                  *
      *        *-------------------------------------------------------*
           05  w-def-mes-cal              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Secolo/anno                                           *
      *        *-------------------------------------------------------*
           05  w-def-saa-cal              pic  9(03)                  .
           05  w-def-saa-max              pic  9(03)                  .
           05  w-def-saa-min              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Giorno della settimana del primo giorno del mese      *
      *        *-------------------------------------------------------*
           05  w-def-gds-pgm              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Giorno attuale                                        *
      *        *-------------------------------------------------------*
           05  w-def-gio-cal              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per visualizzazione                             *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Valori di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-vis-gds-flg              pic  x(01)                  .
           05  w-vis-gds-saa              pic  9(03)                  .
           05  w-vis-gds-mes              pic  9(02)                  .
           05  w-vis-gds-gio              pic  9(02)                  .
           05  w-vis-gds-lin              pic  9(02)                  .
           05  w-vis-gds-pos              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per 7 giorni                             *
      *        *-------------------------------------------------------*
           05  w-vis-gds-ctr              pic  9(02)                  .
           05  w-vis-gds-1g7              pic  9(01)                  .
           05  w-vis-gds-edt.
               10  w-vis-gds-ele occurs 07.
                   15  w-vis-gds-apa      pic  x(01)                  .
                   15  w-vis-gds-gds      pic  x(02)                  .
                   15  w-vis-gds-cpa      pic  x(01)                  .
                   15  filler             pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Mese                                       *
      *        *-------------------------------------------------------*
           05  w-exp-mes-cal.
               10  w-exp-mes-cal-num      pic  9(02)       value 12   .
               10  w-exp-mes-cal-lun      pic  9(02)       value 10   .
               10  w-exp-mes-cal-tbl.
                   15  filler             pic  x(10) value
                            "Gennaio   "                              .
                   15  filler             pic  x(10) value
                            "Febbraio  "                              .
                   15  filler             pic  x(10) value
                            "Marzo     "                              .
                   15  filler             pic  x(10) value
                            "Aprile    "                              .
                   15  filler             pic  x(10) value
                            "Maggio    "                              .
                   15  filler             pic  x(10) value
                            "Giugno    "                              .
                   15  filler             pic  x(10) value
                            "Luglio    "                              .
                   15  filler             pic  x(10) value
                            "Agosto    "                              .
                   15  filler             pic  x(10) value
                            "Settembre "                              .
                   15  filler             pic  x(10) value
                            "Ottobre   "                              .
                   15  filler             pic  x(10) value
                            "Novembre  "                              .
                   15  filler             pic  x(10) value
                            "Dicembre  "                              .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-max              pic  9(02)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura eventuale variabile di i.p.c. di nome   *
      *              * 'pxpg2200'                                      *
      *              *-------------------------------------------------*
           perform   ipc-xpg-220-000      thru ipc-xpg-220-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di visualizzazione titolo  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-tit      .
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
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
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
      *              * Tasto di funzione Prsc : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Nxsc : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
       exe-acc-cmp-050.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Delt : non abilitato          *
      *              *-------------------------------------------------*
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
      *              * Routine pre-richiesta di ratifica tasto Delete  *
      *              *-------------------------------------------------*
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
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *                                                 *
      *              * Nota : Se il programma e' stato richiamato per  *
      *              *        modificare la propria password, non vie- *
      *              *        ne eseguito il controllo di riservatezza *
      *              *-------------------------------------------------*
           if        w-ipc-xpg-220-pwd    =    "S"
                     move  "Q+"           to   s-ope
           else      move  "P+"           to   s-ope                  .
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
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      70                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione tipo funzionamento                        *
      *    *-----------------------------------------------------------*
       vis-tip-fun-000.
       vis-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura eventuale variabile di i.p.c. di nome 'pxpg2200'  *
      *    *-----------------------------------------------------------*
       ipc-xpg-220-000.
      *              *-------------------------------------------------*
      *              * Lettura variabile tramite segreteria            *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-ute"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-xpg-220-200
           else      go to ipc-xpg-220-400.
       ipc-xpg-220-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore letto in area di ridefinizione       *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-xpg-220-pwd      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore relativo a 'Si/No    *
      *                  * richiamo programma per modifica della       *
      *                  * propria password'                           *
      *                  *---------------------------------------------*
           if        w-ipc-xpg-220-pwd    not  = "S"
                     move  "N"            to   w-ipc-xpg-220-pwd
                     move  spaces         to   w-ipc-xpg-220-ute      .
      *                  *---------------------------------------------*
      *                  * Lettura variabile relativa al codice utente *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-ute"            to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se letta                                    *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-ipc-xpg-220-ute      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-xpg-220-999.
       ipc-xpg-220-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore relativo a 'Si/No    *
      *                  * richiamo programma per modifica della       *
      *                  * propria password'                           *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-xpg-220-pwd      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-xpg-220-999.
       ipc-xpg-220-999.
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
       acc-key-reg-020.
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
           move      "#"                  to   w-cnt-sts-vis-tit      .
      *                  *---------------------------------------------*
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per prima pagina testata            *
      *                  *---------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Visualizzazione chiave                          *
      *              *-------------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Nuova password utente                       *
      *                  *---------------------------------------------*
______*    perform   acc-pwd-ute-000      thru acc-pwd-ute-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Conferma password utente                    *
      *                  *---------------------------------------------*
______*    perform   acc-pwd-ute-000      thru acc-pwd-ute-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
      *                  *---------------------------------------------*
      *                  * Presa visione                               *
      *                  *---------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
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
      *              * Mese                                            *
      *              *-------------------------------------------------*
           perform   vis-mes-cal-000      thru vis-mes-cal-999        .
      *              *-------------------------------------------------*
      *              * Secolo/anno                                     *
      *              *-------------------------------------------------*
           perform   vis-saa-cal-000      thru vis-saa-cal-999        .
      *              *-------------------------------------------------*
      *              * Giorni della settimana                          *
      *              *-------------------------------------------------*
           perform   vis-gds-mes-000      thru vis-gds-mes-999        .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Sottolineatura                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "            CAMBIO PASSWORD UTENTE 'Xxxxxxxx'     
      -              "       "            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "            CAMBIO PASSWORD UTENTE 'Xxxxxxxx'     
      -              "       "            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Presa visione                 *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "<-  ->  = cambio mese                      Freccia
      -              " su o giu' = cambio anno      "
                                          to   v-nt1                  .
           move      "Ricerca = Mese in corso                    { } [ ]
      -              " < > = Festivita'     * = Oggi"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "LEFT"               to   v-pfk (03)             .
           move      "RGHT"               to   v-pfk (04)             .
           move      "FIND"               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-pre-vis-999.
       acc-pre-vis-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-pre-vis-400.
      *                  *---------------------------------------------*
      *                  * Valori di default                           *
      *                  *---------------------------------------------*
           move      w-def-mes-cal        to   w-tes-mes-cal          .
           move      w-def-saa-cal        to   w-tes-saa-cal          .
           move      w-def-gds-pgm        to   w-tes-gds-pgm          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione calendario                  *
      *                  *---------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-100.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-pre-vis-500.
      *                  *---------------------------------------------*
      *                  * Decremento dell'anno                        *
      *                  *---------------------------------------------*
           if        w-tes-saa-cal        >    w-def-saa-min
                     subtract 1           from w-tes-saa-cal          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione calendario                  *
      *                  *---------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-100.
       acc-pre-vis-500.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-pre-vis-600.
      *                  *---------------------------------------------*
      *                  * Incremento dell'anno                        *
      *                  *---------------------------------------------*
           if        w-tes-saa-cal        <    w-def-saa-max
                     add  1               to   w-tes-saa-cal          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione calendario                  *
      *                  *---------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-100.
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Se Left o Previous Screen                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "LEFT" and
                     v-key                not  = "PRSC"
                     go to acc-pre-vis-700.
      *                  *---------------------------------------------*
      *                  * Decremento del mese                         *
      *                  *---------------------------------------------*
           if        w-tes-mes-cal        >    1
                     subtract  1          from w-tes-mes-cal
           else      move      12         to   w-tes-mes-cal
                     subtract  1          from w-tes-saa-cal          .
           if        w-tes-saa-cal        <    w-def-saa-min
                     move  w-def-saa-min  to   w-tes-saa-cal          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione calendario                  *
      *                  *---------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-100.
       acc-pre-vis-700.
      *              *-------------------------------------------------*
      *              * Se Right o Next Screen                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "RGHT" and
                     v-key                not  = "NXSC"
                     go to acc-pre-vis-800.
      *                  *---------------------------------------------*
      *                  * Incremento del mese                         *
      *                  *---------------------------------------------*
           if        w-tes-mes-cal        not  > 11
                     add       1          to   w-tes-mes-cal
           else      move      01         to   w-tes-mes-cal
                     add       1          to   w-tes-saa-cal          .
           if        w-tes-saa-cal        >    w-def-saa-max
                     move  w-def-saa-max  to   w-tes-saa-cal          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione calendario                  *
      *                  *---------------------------------------------*
           perform   vis-key-reg-000      thru vis-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-100.
       acc-pre-vis-800.
      *              *-------------------------------------------------*
      *              * Se Do , come Exit                               *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-pre-vis-999.
       acc-pre-vis-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Mese                      *
      *    *-----------------------------------------------------------*
       vis-mes-cal-000.
      *              *-------------------------------------------------*
      *              * Literal del mese, da segreteria                 *
      *              *-------------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-tes-mes-cal        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      s-alf                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mes-cal-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Secolo/anno                       *
      *    *-----------------------------------------------------------*
       vis-saa-cal-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      06                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-tes-saa-cal        to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-saa-cal-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Giorni della settimana            *
      *    *-----------------------------------------------------------*
       vis-gds-mes-000.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare del video                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ciclo di pulizia                            *
      *                  *---------------------------------------------*
           move      zero                 to   w-vis-gds-ctr          .
       vis-gds-mes-020.
           add       1                    to   w-vis-gds-ctr          .
           if        w-vis-gds-ctr        >    11
                     go to vis-gds-mes-040.
      *                  *---------------------------------------------*
      *                  * Linea vuota                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      09                   to   v-lin                  .
           add       w-vis-gds-ctr        to   v-lin                  .
           move      45                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-020.
       vis-gds-mes-040.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gds-mes-050.
      *              *-------------------------------------------------*
      *              * Primo giorno dell'anno                          *
      *              *-------------------------------------------------*
           move      "GS"                 to   s-ope                  .
           move      01                   to   s-gio                  .
           move      w-tes-mes-cal        to   s-mes                  .
           move      w-tes-saa-cal        to   s-saa                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-num                to   w-tes-gds-pgm          .
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-vis-gds-flg          .
           move      w-tes-mes-cal        to   w-vis-gds-mes          .
           move      w-tes-saa-cal        to   w-vis-gds-saa          .
           move      zero                 to   w-vis-gds-gio          .
           move      zero                 to   w-vis-gds-lin          .
           move      w-tes-gds-pgm        to   w-vis-gds-pos          .
           move      spaces               to   w-vis-gds-edt          .
           move      w-vis-gds-pos        to   w-vis-gds-1g7          .
           subtract  1                    from w-vis-gds-1g7          .
       vis-gds-mes-200.
      *              *-------------------------------------------------*
      *              * Incremento del contatore giorno 1 .. 42         *
      *              *-------------------------------------------------*
           add       1                    to   w-vis-gds-gio          .
      *              *-------------------------------------------------*
      *              * Test sul giorno                                 *
      *              *-------------------------------------------------*
           if        w-vis-gds-gio        >    42
                     go to vis-gds-mes-900.
       vis-gds-mes-300.
      *              *-------------------------------------------------*
      *              * Test se superato l'ultimo giorno del mese       *
      *              *-------------------------------------------------*
           move      w-vis-gds-gio        to   s-gio                  .
           move      w-vis-gds-mes        to   s-mes                  .
           move      w-vis-gds-saa        to   s-saa                  .
      *
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     move  "#"            to   w-vis-gds-flg          .
       vis-gds-mes-400.
      *              *-------------------------------------------------*
      *              * Incremento posizionatore                        *
      *              *-------------------------------------------------*
           add       1                    to   w-vis-gds-1g7          .
      *              *-------------------------------------------------*
      *              * Test se elemento da emettere vuoto              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di controllo giorno            *
      *                  *---------------------------------------------*
           if        w-vis-gds-flg        =    spaces
                     go to  vis-gds-mes-420.
      *                  *---------------------------------------------*
      *                  * Elemento vuoto                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-vis-gds-apa
                                              (w-vis-gds-1g7)         .
           move      spaces               to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)         .
           move      spaces               to   w-vis-gds-gds
                                              (w-vis-gds-1g7)         .
      *                  *---------------------------------------------*
      *                  * A incremento posizionatore                  *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-490.
       vis-gds-mes-420.
      *              *-------------------------------------------------*
      *              * Trattamento parentesi                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se data attuale                        *
      *                  *---------------------------------------------*
           if        w-vis-gds-saa        not  = w-def-saa-cal
                     go to  vis-gds-mes-440.
           if        w-vis-gds-mes        not  = w-def-mes-cal
                     go to  vis-gds-mes-440.
           if        w-vis-gds-gio        not  = w-def-gio-cal
                     go to  vis-gds-mes-440.
           move      " "                  to   w-vis-gds-apa
                                              (w-vis-gds-1g7)         .
           move      "*"                  to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)         .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-440.
      *              *-------------------------------------------------*
      *              * Festivita'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non sia domenica                   *
      *                  *---------------------------------------------*
           if        w-vis-gds-1g7        =    7
                     go to  vis-gds-mes-470.
       vis-gds-mes-442.
      *                  *---------------------------------------------*
      *                  * Test se Capodanno                           *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 01 or
                     w-vis-gds-gio        not  = 01
                     go to  vis-gds-mes-444.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-444.
      *                  *---------------------------------------------*
      *                  * Test se Epifania                            *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 01 or
                     w-vis-gds-gio        not  = 06
                     go to  vis-gds-mes-446.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-446.
      *                  *---------------------------------------------*
      *                  * Test se 25 aprile                           *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 04 or
                     w-vis-gds-gio        not  = 25
                     go to  vis-gds-mes-447.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-447.
      *                  *---------------------------------------------*
      *                  * Test se Pasqua (2004 > 2020)                *
      *                  *---------------------------------------------*
           if       (w-vis-gds-gio        =    12  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    104)    or
                    (w-vis-gds-gio        =    28  and
                     w-vis-gds-mes        =    03  and
                     w-vis-gds-saa        =    105)    or
                    (w-vis-gds-gio        =    17  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    106)    or
                    (w-vis-gds-gio        =    09  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    107)    or
                    (w-vis-gds-gio        =    24  and
                     w-vis-gds-mes        =    03  and
                     w-vis-gds-saa        =    108)    or
                    (w-vis-gds-gio        =    13  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    109)    or
                    (w-vis-gds-gio        =    05  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    110)    or
                    (w-vis-gds-gio        =    25  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    111)    or
                    (w-vis-gds-gio        =    09  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    112)    or
                    (w-vis-gds-gio        =    01  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    113)    or
                    (w-vis-gds-gio        =    21  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    114)    or
                    (w-vis-gds-gio        =    06  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    115)    or
                    (w-vis-gds-gio        =    28  and
                     w-vis-gds-mes        =    03  and
                     w-vis-gds-saa        =    116)    or
                    (w-vis-gds-gio        =    17  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    117)    or
                    (w-vis-gds-gio        =    02  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    118)    or
                    (w-vis-gds-gio        =    22  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    119)    or
                    (w-vis-gds-gio        =    13  and
                     w-vis-gds-mes        =    04  and
                     w-vis-gds-saa        =    120)
      *                  *---------------------------------------------*
      *                  * Trattamento Pasqua                          *
      *                  *---------------------------------------------*
                     move  "<"            to   w-vis-gds-apa
                                              (w-vis-gds-1g7)
                     move  ">"            to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
                     go to vis-gds-mes-480
           else      go to vis-gds-mes-448.
       vis-gds-mes-448.
      *                  *---------------------------------------------*
      *                  * Test se I maggio                            *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 05 or
                     w-vis-gds-gio        not  = 01
                     go to  vis-gds-mes-449.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-449.
      *                  *---------------------------------------------*
      *                  * Test se 2 giugno                            *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 06 or
                     w-vis-gds-gio        not  = 02
                     go to  vis-gds-mes-450.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-450.
      *                  *---------------------------------------------*
      *                  * Test se Ferragosto                          *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 08 or
                     w-vis-gds-gio        not  = 15
                     go to  vis-gds-mes-452.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-452.
      *                  *---------------------------------------------*
      *                  * Test se Ognissanti                          *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 11 or
                     w-vis-gds-gio        not  = 01
                     go to  vis-gds-mes-454.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-454.
      *                  *---------------------------------------------*
      *                  * Test se 8 dicembre                          *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 12 or
                     w-vis-gds-gio        not  = 08
                     go to  vis-gds-mes-456.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-456.
      *                  *---------------------------------------------*
      *                  * Test se Natale                              *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 12 or
                     w-vis-gds-gio        not  = 25
                     go to  vis-gds-mes-458.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-458.
      *                  *---------------------------------------------*
      *                  * Test se Santo Stefano                       *
      *                  *---------------------------------------------*
           if        w-vis-gds-mes        not  = 12 or
                     w-vis-gds-gio        not  = 26
                     go to  vis-gds-mes-460.
      *                  *---------------------------------------------*
      *                  * Trattamento festivita'                      *
      *                  *---------------------------------------------*
           perform   vis-gds-mes-820      thru vis-gds-mes-829        .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-460.
      *                  *---------------------------------------------*
      *                  * Altre festivita'                            *
      *                  *---------------------------------------------*
       vis-gds-mes-470.
      *              *-------------------------------------------------*
      *              * Domenica                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-vis-gds-1g7        =    7
                     move  "["            to   w-vis-gds-apa
                                              (w-vis-gds-1g7)
                     move  "]"            to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)
           else      move  spaces         to   w-vis-gds-apa
                                              (w-vis-gds-1g7)
                     move  spaces         to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)         .
      *                  *---------------------------------------------*
      *                  * A editing                                   *
      *                  *---------------------------------------------*
           go to     vis-gds-mes-480.
       vis-gds-mes-480.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-vis-gds-gio        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-vis-gds-gds
                                              (w-vis-gds-1g7)         .
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           move      02                   to   w-all-str-lun          .
           move      w-vis-gds-gds
                    (w-vis-gds-1g7)       to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
           move      w-all-str-alf        to   w-vis-gds-gds
                                              (w-vis-gds-1g7)         .
       vis-gds-mes-490.
      *              *-------------------------------------------------*
      *              * Incremento posizionatore                        *
      *              *-------------------------------------------------*
           if        w-vis-gds-1g7        =    7
                     add   2              to   w-vis-gds-lin
                     move  zero           to   w-vis-gds-1g7
                     go to vis-gds-mes-600.
       vis-gds-mes-500.
      *              *-------------------------------------------------*
      *              * A riciclo                                       *
      *              *-------------------------------------------------*
           go to     vis-gds-mes-700.
       vis-gds-mes-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-gds-mes-800      thru vis-gds-mes-819        .
           if        w-vis-gds-flg        not  = spaces
                     go to vis-gds-mes-900.
       vis-gds-mes-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vis-gds-mes-200.
       vis-gds-mes-800.
      *              *=================================================*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      w-vis-gds-lin        to   v-lin                  .
           add       08                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-vis-gds-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-gds-mes-819.
           exit.
       vis-gds-mes-820.
      *              *=================================================*
      *              * Trattamento festivita'                          *
      *              *-------------------------------------------------*
           move      "{"                  to   w-vis-gds-apa
                                              (w-vis-gds-1g7)         .
           move      "}"                  to   w-vis-gds-cpa
                                              (w-vis-gds-1g7)         .
       vis-gds-mes-829.
           exit.
       vis-gds-mes-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-gds-mes-999.
       vis-gds-mes-999.
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
           if        w-bla-emb-ctr        >    w-bla-emb-max
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       not  = spaces
                     go to tst-bla-emb-100.
       tst-bla-emb-200.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    w-bla-emb-max
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

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
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
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
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
       box-msg-e02-999.
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
           if        w-tes-saa-cal        =    zero
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
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      w-def-mes-cal        to   w-tes-mes-cal          .
           move      w-def-saa-cal        to   w-tes-saa-cal          .
           move      w-def-gds-pgm        to   w-tes-gds-pgm          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


