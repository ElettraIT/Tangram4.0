       Identification Division.
       Program-Id.                                 pxpg001v           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/06/10    *
      *                       Ultima revisione:    NdK del 01/02/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Rubrica telefonica Vettori (.tv)            *
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
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "telvet"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg001v"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "       RUBRICA TELEFONICA VETTORI       "       .

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
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
      *            *---------------------------------------------------*
      *            * Tipo archivio                                     *
      *            *---------------------------------------------------*
               10  w-tes-tip-arc          pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Commerciale o amministrativo                      *
      *            *---------------------------------------------------*
               10  w-tes-com-amm          pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Codice archivio                                   *
      *            *---------------------------------------------------*
               10  w-tes-cod-arc          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza archivio                        *
      *            *---------------------------------------------------*
               10  w-tes-dpz-arc          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale archivio                          *
      *            *---------------------------------------------------*
               10  w-tes-cod-arc-rag.
                   15  w-tes-cod-arc-rs1  pic  x(40)                  .
                   15  w-tes-cod-arc-rs2  pic  x(40)                  .
               10  w-tes-cod-arc-rgk      pic  x(40)                  .
               10  w-tes-cod-arc-mne      pic  x(40)                  .
               10  w-tes-cod-arc-via      pic  x(40)                  .
               10  w-tes-cod-arc-loc      pic  x(40)                  .
               10  w-tes-cod-arc-nom      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Utenze ed interlocutori archivio                  *
      *            *---------------------------------------------------*
               10  w-tes-tip-sel          pic  x(03)                  .
               10  w-tes-con-ele          pic  9(02)                  .
               10  w-tes-con-elm          pic  9(02)                  .
               10  w-tes-con-ctr          pic  9(02)                  .
               10  w-tes-con-ctw          pic  9(02)                  .
               10  w-tes-con-spg          pic  9(02)                  .
               10  w-tes-con-ctl          pic  9(02)                  .
               10  w-tes-con-lpp          pic  9(02) value  8         .
               10  w-tes-con-max          pic  9(02) value 45         .
               10  w-tes-con-arc occurs 45.
                   15  w-tes-tip-con      pic  x(03)                  .
                   15  w-tes-num-con      pic  x(80)                  .
                   15  w-tes-int-con      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  filler                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Eventuali limitazioni per dipendenza                  *
      *        *-------------------------------------------------------*
           05  w-ref-lim-dpz.
               10  w-ref-lim-dpz-ndp      pic  9(02)                  .
               10  w-ref-lim-dpz-dpz      pic  9(02)                  .
               10  w-ref-lim-dpz-ute      pic  x(08)                  .
               10  w-ref-lim-dpz-alf.
                   15  w-ref-lim-dpz-snc  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-lim-dpz-snf  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-lim-dpz-snv  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-lim-dpz-sna  pic  x(01)                  .
                   15  filler             pic  x(03)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice vettore                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo archivio                                         *
      *        *-------------------------------------------------------*
           05  w-sav-tip-arc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Commerciale o amministrativo                          *
      *        *-------------------------------------------------------*
           05  w-sav-com-amm              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice archivio                                       *
      *        *-------------------------------------------------------*
           05  w-sav-cod-arc              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Tipo archivio                                         *
      *        *-------------------------------------------------------*
           05  w-def-tip-arc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Commerciale o amministrativo                          *
      *        *-------------------------------------------------------*
           05  w-def-com-amm              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento per Next Screen - Previous Screen    *
      *        *-------------------------------------------------------*
           05  w-def-tip-ord              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo richiesta per Next Screen - Previous Screen      *
      *        *-------------------------------------------------------*
           05  w-def-tip-ric              pic  9(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Commerciale o amministrativo               *
      *        *-------------------------------------------------------*
           05  w-exp-com-amm.
               10  w-exp-com-amm-num      pic  9(02)       value 2    .
               10  w-exp-com-amm-lun      pic  9(02)       value 20   .
               10  w-exp-com-amm-tbl.
                   15  filler             pic  x(20) value
                            "Commerciale         "                    .
                   15  filler             pic  x(20) value
                            "Amministrativo      "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 3    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 20   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(20) value
                            "Per Ragione sociale "                    .
                   15  filler             pic  x(20) value
                            "Per Codice          "                    .
                   15  filler             pic  x(20) value
                            "Per Mnemonico       "                    .

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
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
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
      *              *-------------------------------------------------*
      *              * Inizializzazione valori di defaults generali    *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-tip-arc          .
           move      1                    to   w-def-com-amm          .
           move      1                    to   w-def-tip-ord          .
           move      zero                 to   w-def-tip-ric          .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore di comodo            *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-wrk-ctr-001      .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuali limitazioni per dipendenza        *
      *                  *---------------------------------------------*
           perform   ref-lim-dpz-000      thru ref-lim-dpz-999        .
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Open moduli accettazione                        *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Eventuali range di codici attribuibile per dipendenza     *
      *    *-----------------------------------------------------------*
       ref-lim-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "S"                  to   w-ref-lim-dpz-snc      .
           move      "S"                  to   w-ref-lim-dpz-snf      .
           move      "S"                  to   w-ref-lim-dpz-snv      .
           move      "S"                  to   w-ref-lim-dpz-sna      .
       ref-lim-dpz-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta a segreteria del codice utente    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-ute                to   w-ref-lim-dpz-ute      .
      *                  *---------------------------------------------*
      *                  * Determinazione se utente con dipendenza     *
      *                  * unica collegata                             *
      *                  *---------------------------------------------*
           move      "D?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-num                to   w-ref-lim-dpz-ndp      .
           move      s-alf                to   w-ref-lim-dpz-dpz      .
      *                  *---------------------------------------------*
      *                  * Test se sola una dipendenza attiva          *
      *                  *---------------------------------------------*
           if        w-ref-lim-dpz-ndp    not  = 01
                     go to ref-lim-dpz-900.
      *                  *---------------------------------------------*
      *                  * Test sul codice dipendenza                  *
      *                  *---------------------------------------------*
           if        w-ref-lim-dpz-dpz    =    00
                     go to ref-lim-dpz-900.
       ref-lim-dpz-200.
      *              *-------------------------------------------------*
      *              * Lettura referenza multipla                      *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      w-ref-lim-dpz-dpz    to   s-num                  .
           move      "pgm/azi[num-tel]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to ref-lim-dpz-900.
           if        s-alf                =    spaces
                     go to ref-lim-dpz-900.
       ref-lim-dpz-400.
      *              *-------------------------------------------------*
      *              * Valori letti                                    *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ref-lim-dpz-alf      .
      *              *-------------------------------------------------*
      *              * Normalizzazione eventuali                       *
      *              *-------------------------------------------------*
           if        w-ref-lim-dpz-snc    not  = "N"
                     move  "S"            to   w-ref-lim-dpz-snc      .
           if        w-ref-lim-dpz-snf    not  = "N"            
                     move  "S"            to   w-ref-lim-dpz-snf      .
           if        w-ref-lim-dpz-snv    not  = "N"            
                     move  "S"            to   w-ref-lim-dpz-snv      .
           if        w-ref-lim-dpz-sna    not  = "N"            
                     move  "S"            to   w-ref-lim-dpz-sna      .
       ref-lim-dpz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ref-lim-dpz-999.
       ref-lim-dpz-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli accettazione                       *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-000      thru cls-mdl-acc-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione vettore                *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-opn-000  thru cod-mne-vet-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione vettore               *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-cls-000  thru cod-mne-vet-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              * Open modulo di determinazione contatti          *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
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
      *              * Close modulo di determinazione contatti         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
           if        d-con-arc-exi-sts    not  = spaces
                     go to rou-cls-fls-999.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
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
      *              * Incremento contatore di comodo                  *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-wrk-ctr-001      .
           if        w-cnt-wrk-ctr-001    not  > 1
                     go to acc-key-reg-020.
      *                  *---------------------------------------------*
      *                  * Si forza il valore di default generale per  *
      *                  * il tipo archivio e si visualizza            *
      *                  *---------------------------------------------*
           move      w-def-tip-arc        to   w-tes-tip-arc          .
           perform   vis-tip-arc-000      thru vis-tip-arc-999        .
           go to     acc-key-reg-300.
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
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           perform   acc-tip-arc-000      thru acc-tip-arc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Commerciale o Amministrativo                *
      *                  *---------------------------------------------*
           perform   acc-com-amm-000      thru acc-com-amm-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-400.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           perform   acc-dpz-arc-000      thru acc-dpz-arc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-300.
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
      *              * Tipo archivio                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-arc-000      thru vis-tip-arc-999        .
      *              *-------------------------------------------------*
      *              * Commerciale o Amministrativo                    *
      *              *-------------------------------------------------*
           perform   vis-com-amm-000      thru vis-com-amm-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio                      *
      *              *-------------------------------------------------*
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale archivio                        *
      *              *-------------------------------------------------*
           perform   vis-rag-arc-000      thru vis-rag-arc-999        .
      *              *-------------------------------------------------*
      *              * Utenze ed interlocutori archivio                *
      *              *-------------------------------------------------*
           perform   vis-uei-arc-000      thru vis-uei-arc-999        .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Tipo archivio                                   *
      *              *-------------------------------------------------*
           perform   pmt-tip-arc-000      thru pmt-tip-arc-999        .
      *              *-------------------------------------------------*
      *              * Commerciale o Amministrativo                    *
      *              *-------------------------------------------------*
           perform   pmt-com-amm-000      thru pmt-com-amm-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio                      *
      *              *-------------------------------------------------*
           perform   pmt-dpz-arc-000      thru pmt-dpz-arc-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Utenze ed interlocutori archivio                *
      *              *-------------------------------------------------*
           perform   pmt-uei-arc-000      thru pmt-uei-arc-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo archivio                 *
      *    *-----------------------------------------------------------*
       pmt-tip-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Vettore    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Commerciale o Amministrativo  *
      *    *-----------------------------------------------------------*
       pmt-com-amm-000.
       pmt-com-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice archivio               *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice dipendenza archivio    *
      *    *-----------------------------------------------------------*
       pmt-dpz-arc-000.
       pmt-dpz-arc-300.
      *              *-------------------------------------------------*
      *              * Vettore                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-dpz-arc-900.
       pmt-dpz-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-dpz-arc-999.
       pmt-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Utenze ed interlocutori archivio *
      *    *-----------------------------------------------------------*
       pmt-uei-arc-000.
       pmt-uei-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-uei-arc-999.
       pmt-uei-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo archivio                *
      *    *-----------------------------------------------------------*
       acc-tip-arc-000.
       acc-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo archivio             *
      *    *-----------------------------------------------------------*
       vis-tip-arc-000.
       vis-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Commerciale o amministrativo         *
      *    *-----------------------------------------------------------*
       acc-com-amm-000.
       acc-com-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Commerciale o amministrativo      *
      *    *-----------------------------------------------------------*
       vis-com-amm-000.
       vis-com-amm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice archivio               *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
       acc-cod-arc-300.
      *              *-------------------------------------------------*
      *              * Vettore                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           perform   acc-cod-vet-000      thru acc-cod-vet-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-cod-arc-900.
       acc-cod-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice dipendenza archivio    *
      *    *-----------------------------------------------------------*
       acc-dpz-arc-000.
       acc-dpz-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dpz-arc-999.
       acc-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice archivio            *
      *    *-----------------------------------------------------------*
       vis-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-tes-cod-arc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice dipendenza archivio *
      *    *-----------------------------------------------------------*
       vis-dpz-arc-000.
       vis-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale archivio   *
      *    *-----------------------------------------------------------*
       vis-rag-arc-000.
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-tes-cod-arc-rs1    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-tes-cod-arc-rs2    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-tes-cod-arc-via    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Localita'                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-tes-cod-arc-loc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Utenze ed interlocutori archivio *
      *    *-----------------------------------------------------------*
       vis-uei-arc-000.
      *              *-------------------------------------------------*
      *              * Ciclo di visualizzazione                        *
      *              *-------------------------------------------------*
           move      w-tes-con-spg        to   w-tes-con-ctr          .
           move      zero                 to   w-tes-con-ctl          .
       vis-uei-arc-200.
           add       1                    to   w-tes-con-ctl          .
           add       1                    to   w-tes-con-ctr          .
           if        w-tes-con-ctl        >    w-tes-con-lpp
                     go to vis-uei-arc-800.
      *              *-------------------------------------------------*
      *              * Tipo utenza                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      11                   to   v-lin                  .
           add       w-tes-con-ctl        to   v-lin                  .
           move      03                   to   v-pos                  .
      *
           if        w-tes-tip-con
                    (w-tes-con-ctr)       =    "SEP"
                     move  all "-"        to   v-alf
           else      move  w-tes-tip-con
                          (w-tes-con-ctr) to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlocutore utenza                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           add       w-tes-con-ctl        to   v-lin                  .
           move      07                   to   v-pos                  .
      *
           if        w-tes-tip-con
                    (w-tes-con-ctr)       =    "SEP"
                     move  all "-"        to   v-alf
           else      move  w-tes-int-con
                          (w-tes-con-ctr) to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero utenza                                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           add       w-tes-con-ctl        to   v-lin                  .
           move      39                   to   v-pos                  .
      *
           if        w-tes-tip-con
                    (w-tes-con-ctr)       =    "SEP"
                     move  all "-"        to   v-alf
           else      move  w-tes-num-con
                          (w-tes-con-ctr) to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uei-arc-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vis-uei-arc-200.
       vis-uei-arc-800.
      *              *-------------------------------------------------*
      *              * Eventuale indicazione per scorrimento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           subtract  1                    from w-tes-con-ele
                                        giving w-tes-con-ctw          .
           if        w-tes-con-spg        =    w-tes-con-ctw
                     go to vis-uei-arc-820.
           if        w-tes-con-ele        not  > w-tes-con-lpp
                     go to vis-uei-arc-820.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "\/ segue /\"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-uei-arc-900.
       vis-uei-arc-820.
      *                  *---------------------------------------------*
      *                  * Visualizzazione linea normalizzata          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uei-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-uei-arc-999.
       vis-uei-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice vettore                *
      *    *-----------------------------------------------------------*
       acc-cod-vet-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-arc        to   w-sav-cod-arc          .
       acc-cod-vet-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-vet-ope      .
           move      w-tes-cod-arc        to   w-cod-mne-vet-cod      .
           move      07                   to   w-cod-mne-vet-lin      .
           move      16                   to   w-cod-mne-vet-pos      .
           move      07                   to   w-cod-mne-vet-rln      .
           move      29                   to   w-cod-mne-vet-rps      .
           move      09                   to   w-cod-mne-vet-vln      .
           move      29                   to   w-cod-mne-vet-vps      .
           move      10                   to   w-cod-mne-vet-lln      .
           move      29                   to   w-cod-mne-vet-lps      .
           move      "<B"                 to   v-edm                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXPD"               to   v-pfk (13)             .
      *
           move      "[1] "               to   v-pfk (14)             .
           move      "[2] "               to   v-pfk (15)             .
           move      "[3] "               to   v-pfk (16)             .
           move      "[4] "               to   v-pfk (17)             .
      *
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
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-vet-999.
       acc-cod-vet-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-arc          .
       acc-cod-vet-300.
      *              *-------------------------------------------------*
      *              * Se 'Expand'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXPD"
                     go to acc-cod-vet-320.
      *                  *---------------------------------------------*
      *                  * Trattamento subroutine di expand            *
      *                  *---------------------------------------------*
           perform   rou-trt-exp-000      thru rou-trt-exp-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-100.
       acc-cod-vet-320.
      *              *-------------------------------------------------*
      *              * Trattamento Previous Screen                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cod-vet-340.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento                   *
      *                  *---------------------------------------------*
           move      "-"                  to   w-def-tip-ric          .
           perform   rou-trt-nxs-vet-000  thru rou-trt-nxs-vet-999    .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-100.
       acc-cod-vet-340.
      *              *-------------------------------------------------*
      *              * Trattamento Next Screen                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cod-vet-360.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento                   *
      *                  *---------------------------------------------*
           move      "+"                  to   w-def-tip-ric          .
           perform   rou-trt-nxs-vet-000  thru rou-trt-nxs-vet-999    .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-100.
       acc-cod-vet-360.
      *              *-------------------------------------------------*
      *              * Trattamento Pf1                                 *
      *              *-------------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-cod-vet-365.
      *                  *---------------------------------------------*
      *                  * Forzatura selettore tipo contatto           *
      *                  *---------------------------------------------*
           if        w-tes-tip-sel        =    spaces
                     move  "TEL"          to   w-tes-tip-sel
           else if   w-tes-tip-sel        =    "TEL"
                     move  spaces         to   w-tes-tip-sel
           else      move  "TEL"          to   w-tes-tip-sel          .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-400.
       acc-cod-vet-365.
      *              *-------------------------------------------------*
      *              * Trattamento Pf2                                 *
      *              *-------------------------------------------------*
           if        v-key                not  = "[2] "
                     go to acc-cod-vet-370.
      *                  *---------------------------------------------*
      *                  * Forzatura selettore tipo contatto           *
      *                  *---------------------------------------------*
           if        w-tes-tip-sel        =    spaces
                     move  "FAX"          to   w-tes-tip-sel
           else if   w-tes-tip-sel        =    "FAX"
                     move  spaces         to   w-tes-tip-sel
           else      move  "FAX"          to   w-tes-tip-sel          .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-400.
       acc-cod-vet-370.
      *              *-------------------------------------------------*
      *              * Trattamento Pf3                                 *
      *              *-------------------------------------------------*
           if        v-key                not  = "[3] "
                     go to acc-cod-vet-375.
      *                  *---------------------------------------------*
      *                  * Forzatura selettore tipo contatto           *
      *                  *---------------------------------------------*
           if        w-tes-tip-sel        =    spaces
                     move  "CEL"          to   w-tes-tip-sel
           else if   w-tes-tip-sel        =    "CEL"
                     move  spaces         to   w-tes-tip-sel
           else      move  "CEL"          to   w-tes-tip-sel          .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-400.
       acc-cod-vet-375.
      *              *-------------------------------------------------*
      *              * Trattamento Pf4                                 *
      *              *-------------------------------------------------*
           if        v-key                not  = "[4] "
                     go to acc-cod-vet-400.
      *                  *---------------------------------------------*
      *                  * Forzatura selettore tipo contatto           *
      *                  *---------------------------------------------*
           if        w-tes-tip-sel        =    spaces
                     move  "EML"          to   w-tes-tip-sel
           else if   w-tes-tip-sel        =    "EML"
                     move  spaces         to   w-tes-tip-sel
           else      move  "EML"          to   w-tes-tip-sel          .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-cod-vet-400.
       acc-cod-vet-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice impostato a zero             *
      *                  *---------------------------------------------*
           if        w-tes-cod-arc        not  = zero
                     go to acc-cod-vet-420.
           if        v-key                =    "UP  "
                     go to acc-cod-vet-800
           else      go to acc-cod-vet-100.
       acc-cod-vet-420.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [vet]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Lettura record da file [vet]                *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET    "         to   f-key                  .
           move      w-tes-cod-arc        to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                      *-----------------------------------------*
      *                      * Test su esito della lettura             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-cod-vet-460.
       acc-cod-vet-450.
      *                      *-----------------------------------------*
      *                      * Se record non trovato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Anagrafica vettore non trovata !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-vet-100.
       acc-cod-vet-460.
      *                      *-----------------------------------------*
      *                      * Se record trovato                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Bufferizzazione dati vettore        *
      *                          *-------------------------------------*
           perform   buf-rec-vet-000      thru buf-rec-vet-999        .
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-cod-vet-600.
       acc-cod-vet-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati letti                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale archivio                *
      *                      *-----------------------------------------*
           perform   vis-rag-arc-000      thru vis-rag-arc-999        .
      *                      *-----------------------------------------*
      *                      * Utenze ed interlocutori archivio        *
      *                      *-----------------------------------------*
           perform   vis-uei-arc-000      thru vis-uei-arc-999        .
       acc-cod-vet-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-vet-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-vet-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-vet-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-vet-999.
       acc-cod-vet-999.
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
           if        w-tes-cod-arc        =    zero
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
           move      zero                 to   w-tes-tip-arc          .
           move      zero                 to   w-tes-com-amm          .
           move      zero                 to   w-tes-cod-arc          .
           move      spaces               to   w-tes-dpz-arc          .
           move      spaces               to   w-tes-cod-arc-rag      .
           move      spaces               to   w-tes-cod-arc-via      .
           move      spaces               to   w-tes-cod-arc-loc      .
           move      spaces               to   w-tes-cod-arc-nom      .
      *              *-------------------------------------------------*
      *              * Contatti                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-tes-tip-sel          .
           move      zero                 to   w-tes-con-ele          .
           move      zero                 to   w-tes-con-spg          .
           move      zero                 to   w-tes-con-ctr          .
       nor-key-reg-600.
           add       1                    to   w-tes-con-ctr          .
           if        w-tes-con-ctr        >    w-tes-con-max
                     go to nor-key-reg-620.
           move      spaces               to   w-tes-tip-con
                                              (w-tes-con-ctr)         .
           move      spaces               to   w-tes-num-con
                                              (w-tes-con-ctr)         .
           move      spaces               to   w-tes-int-con
                                              (w-tes-con-ctr)         .
           go to     nor-key-reg-600.
       nor-key-reg-620.
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di expand da codici archivio                   *
      *    *-----------------------------------------------------------*
       rou-trt-exp-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione tipo ordinamento di default        *
      *              *-------------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-trt-exp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-trt-exp-999.
       rou-trt-exp-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di expand da codici archivio                   *
      *    *                                                           *
      *    * Accettazione campo : Tipo ordinamento                     *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Preparazioni pre-accettazione                   *
      *              *-------------------------------------------------*
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      "RCM#"               to   v-msk                  .
           move      07                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-def-tip-ord        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ord-999.
       acc-tip-ord-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-def-tip-ord          .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        w-def-tip-ord        =    zero
                     go to acc-tip-ord-100.
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di Next Screen o Previous Screen               *
      *    *                                                           *
      *    * Vettore                                                   *
      *    *-----------------------------------------------------------*
       rou-trt-nxs-vet-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        w-def-tip-ord        =    zero
                     go to rou-trt-nxs-vet-800
           else if   w-def-tip-ord        =    1
                     go to rou-trt-nxs-vet-100
           else if   w-def-tip-ord        =    2
                     go to rou-trt-nxs-vet-200
           else if   w-def-tip-ord        =    3
                     go to rou-trt-nxs-vet-300
           else      go to rou-trt-nxs-vet-800.
       rou-trt-nxs-vet-100.
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [vet]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           if        w-def-tip-ric        =    "+"
                     move  "NL"           to   f-cfr
           else      move  "NG"           to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      w-tes-cod-arc-rgk    to   rf-vet-rag-key         .
           move      w-tes-cod-arc        to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad uscita             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-trt-nxs-vet-900.
      *                  *---------------------------------------------*
      *                  * A Read-Next per [vet]                       *
      *                  *---------------------------------------------*
           go to     rou-trt-nxs-vet-500.
       rou-trt-nxs-vet-200.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [vet]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           if        w-def-tip-ric        =    "+"
                     move  "NL"           to   f-cfr
           else      move  "NG"           to   f-cfr                  .
           move      "CODVET    "         to   f-key                  .
           move      w-tes-cod-arc        to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad uscita             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-trt-nxs-vet-900.
      *                  *---------------------------------------------*
      *                  * A Read-Next per [vet]                       *
      *                  *---------------------------------------------*
           go to     rou-trt-nxs-vet-500.
       rou-trt-nxs-vet-300.
      *              *-------------------------------------------------*
      *              * Mnemonico                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [vet]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           if        w-def-tip-ric        =    "+"
                     move  "NL"           to   f-cfr
           else      move  "NG"           to   f-cfr                  .
           move      "CODMNE    "         to   f-key                  .
           move      w-tes-cod-arc-mne    to   rf-vet-cod-mne         .
           move      w-tes-cod-arc        to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : ad uscita             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-trt-nxs-vet-900.
      *                  *---------------------------------------------*
      *                  * A Read-Next per [vet]                       *
      *                  *---------------------------------------------*
           go to     rou-trt-nxs-vet-500.
       rou-trt-nxs-vet-500.
      *              *-------------------------------------------------*
      *              * Next su [vet] di posizionamento                 *
      *              *-------------------------------------------------*
           if        w-def-tip-ric        =    "+"
                     move  "RN"           to   f-ope
           else      move  "RP"           to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : uscita                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-trt-nxs-vet-900.
      *              *-------------------------------------------------*
      *              * Next su [vet]                                   *
      *              *-------------------------------------------------*
           if        w-def-tip-ric        =    "+"
                     move  "RN"           to   f-ope
           else      move  "RP"           to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : uscita                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-trt-nxs-vet-900.
      *                  *---------------------------------------------*
      *                  * A bufferizzazione                           *
      *                  *---------------------------------------------*
           go to     rou-trt-nxs-vet-600.
       rou-trt-nxs-vet-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione dati letti                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      rf-vet-cod-vet       to   w-tes-cod-arc          .
           move      spaces               to   w-tes-dpz-arc          .
           perform   buf-rec-vet-000      thru buf-rec-vet-999        .
       rou-trt-nxs-vet-620.
      *              *-------------------------------------------------*
      *              * Visualizzazione dati letti                      *
      *              *-------------------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
           perform   vis-rag-arc-000      thru vis-rag-arc-999        .
           perform   vis-uei-arc-000      thru vis-uei-arc-999        .
       rou-trt-nxs-vet-680.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     rou-trt-nxs-vet-900.
       rou-trt-nxs-vet-800.
      *              *-------------------------------------------------*
      *              * Se non specificato il tipo ordinamento          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-trt-nxs-vet-900.
       rou-trt-nxs-vet-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-trt-nxs-vet-999.
       rou-trt-nxs-vet-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di bufferizzazione dati Vettore                *
      *    *-----------------------------------------------------------*
       buf-rec-vet-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
           move      rf-vet-rag-soc       to   w-tes-cod-arc-rag      .
           move      rf-vet-rag-key       to   w-tes-cod-arc-rgk      .
           move      rf-vet-cod-mne       to   w-tes-cod-arc-mne      .
           move      rf-vet-via-vet       to   w-tes-cod-arc-via      .
           move      rf-vet-loc-vet       to   w-tes-cod-arc-loc      .
           move      spaces               to   w-tes-cod-arc-nom      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatti                    *
      *                  *---------------------------------------------*
           perform   nor-con-arc-000      thru nor-con-arc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione contatti                     *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      41                   to   d-con-arc-tip-arc      .
           move      rf-vet-cod-vet       to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      w-tes-tip-sel        to   d-con-arc-tip-sel      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *
           move      d-con-arc-num-ele    to   w-tes-con-ele          .
           move      zero                 to   w-tes-con-ctr          .
       buf-rec-vet-200.
           add       1                    to   w-tes-con-ctr          .
           if        w-tes-con-ctr        >    d-con-arc-num-ele
                     go to buf-rec-vet-900.
           move      d-con-arc-tip-con
                    (w-tes-con-ctr)       to   w-tes-tip-con
                                              (w-tes-con-ctr)         .
           move      d-con-arc-int-con
                    (w-tes-con-ctr)       to   w-tes-int-con
                                              (w-tes-con-ctr)         .
           move      d-con-arc-num-con
                    (w-tes-con-ctr)       to   w-tes-num-con
                                              (w-tes-con-ctr)         .
           go to     buf-rec-vet-200.
       buf-rec-vet-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rec-vet-999.
       buf-rec-vet-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di normalizzazione contatti                    *
      *    *-----------------------------------------------------------*
       nor-con-arc-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-con-ctr          .
       nor-con-arc-200.
           add       1                    to   w-tes-con-ctr          .
           if        w-tes-con-ctr        >    w-tes-con-max
                     go to nor-con-arc-900.
           move      spaces               to   w-tes-tip-con
                                              (w-tes-con-ctr)         .
           move      spaces               to   w-tes-num-con
                                              (w-tes-con-ctr)         .
           move      spaces               to   w-tes-int-con
                                              (w-tes-con-ctr)         .
           go to     nor-con-arc-200.
       nor-con-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     nor-con-arc-999.
       nor-con-arc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice vettore             *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acs"                   .

