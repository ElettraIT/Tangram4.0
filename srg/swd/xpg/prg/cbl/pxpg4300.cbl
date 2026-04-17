       Identification Division.
       Program-Id.                                 pxpg4300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    sds                 *
      *                                   Fase:    xpg430              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Versione attuale:    NdK del 10/12/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione codici modulo                      *
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
                     "xpg430"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg4300"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "         GESTIONE CODICI MODULO         "       .

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
      *    * Work per records di [pss] 'mod'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssmod0.cpw"                   .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione codice modulo                     *
      *        *-------------------------------------------------------*
           05  w-clo-mod.
      *            *---------------------------------------------------*
      *            * Codice modulo per salvataggio                     *
      *            *---------------------------------------------------*
               10  w-clo-mod-cod-mod      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Record modulo                                     *
      *            *---------------------------------------------------*
               10  w-clo-mod-rec-mod.
                   15  filler occurs 2048 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per comodi di lavoro                            *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-00a              pic  9(03)                  .
           05  w-wrk-ctr-00b              pic  9(03)                  .
           05  w-wrk-ctr-00c              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per 16 interlinee possibili              *
      *        *-------------------------------------------------------*
           05  w-edt-ilp-are.
               10  w-edt-ilp-1o2 occurs 2.
                   15  w-edt-ilp-sub occurs 8.
                       20  w-edt-ilp-ele  pic  9(02),9(02)            .
                       20  filler         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per calcolo                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-dim-plc              pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su codici modulo di [pss]               *
      *        *-------------------------------------------------------*
           05  w-fnd-pss-mod.
               10  w-fnd-pss-mod-sel      pic  x(01)                  .
               10  w-fnd-pss-mod-cod      pic  x(08)                  .
               
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
      *    * Work per accettazione campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo modulo                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mod.
               10  w-exp-tip-mod-num      pic  9(02)       value 4    .
               10  w-exp-tip-mod-lun      pic  9(02)       value 30   .
               10  w-exp-tip-mod-tbl.
                   15  filler             pic  x(30) value
                            "Libero                        "          .
                   15  filler             pic  x(30) value
                            "Tipografico                   "          .
                   15  filler             pic  x(30) value
                            "Specifico non tipografico     "          .
                   15  filler             pic  x(30) value
                            "Postscript incapsulato        "          .
      *        *-------------------------------------------------------*
      *        * Work per : Classe di appartenenza                     *
      *        *-------------------------------------------------------*
           05  w-exp-cla-mod.
               10  w-exp-cla-mod-num      pic  9(02)       value 2    .
               10  w-exp-cla-mod-lun      pic  9(02)       value 20   .
               10  w-exp-cla-mod-tbl.
                   15  filler             pic  x(20) value
                            "Standard            "                    .
                   15  filler             pic  x(20) value
                            "su Misura           "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no PostScript                           *
      *        *-------------------------------------------------------*
           05  w-exp-snx-psc.
               10  w-exp-snx-psc-num      pic  9(02)       value 2    .
               10  w-exp-snx-psc-lun      pic  9(02)       value 02   .
               10  w-exp-snx-psc-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no lettura facilitata                   *
      *        *-------------------------------------------------------*
           05  w-exp-snx-lfc.
               10  w-exp-snx-lfc-num      pic  9(02)       value 2    .
               10  w-exp-snx-lfc-lun      pic  9(02)       value 02   .
               10  w-exp-snx-lfc-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Orientamento                               *
      *        *-------------------------------------------------------*
           05  w-exp-ori-mod.
               10  w-exp-ori-mod-num      pic  9(02)       value 3    .
               10  w-exp-ori-mod-lun      pic  9(02)       value 12   .
               10  w-exp-ori-mod-tbl.
                   15  filler             pic  x(12) value
                            "Automatico  "                            .
                   15  filler             pic  x(12) value
                            "Orizzontale "                            .
                   15  filler             pic  x(12) value
                            "Verticale   "                            .

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
           move      w-mod                to   w-clo-mod-rec-mod      .
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
      *                  * Codice modulo                               *
      *                  *---------------------------------------------*
           perform   acc-cod-mod-000      thru acc-cod-mod-999        .
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
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-mod-000      thru vis-cod-mod-999        .
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
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-mod-000      thru pmt-cod-mod-999        .
      *              *-------------------------------------------------*
      *              * Lineette di separazione a linea 05              *
      *              *-------------------------------------------------*
           perform   pmt-lds-l05-000      thru pmt-lds-l05-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per codice modulo                                  *
      *    *-----------------------------------------------------------*
       pmt-cod-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice modulo              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mod-999.
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
      *    * Accettazione : Codice modulo                              *
      *    *-----------------------------------------------------------*
       acc-cod-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-mod-cod-mod        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mod-999.
       acc-cod-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mod-cod-mod          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-mod-400.
      *                  *---------------------------------------------*
      *                  * Find su codici modulo                       *
      *                  *---------------------------------------------*
           move      w-mod-cod-mod        to   w-fnd-pss-mod-cod      .
           perform   fnd-pss-mod-000      thru fnd-pss-mod-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-pss-mod-sel    not  = spaces
                     go to acc-cod-mod-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-pss-mod-cod    to   w-mod-cod-mod          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-cod-mod-000      thru vis-cod-mod-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-cod-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-mod-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-mod-cod-mod        to   w-all-str-alf          .
           move      08                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-mod-100.
       acc-cod-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-mod-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mod-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-mod-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-mod-999.
       acc-cod-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice modulo                           *
      *    *-----------------------------------------------------------*
       vis-cod-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mod-cod-mod        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mod-999.
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
      *                  * Descrizione modulo                          *
      *                  *---------------------------------------------*
           perform   acc-des-mod-000      thru acc-des-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Tipo modulo                                 *
      *                  *---------------------------------------------*
           perform   acc-tip-mod-000      thru acc-tip-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Larghezza modulo                            *
      *                  *---------------------------------------------*
           perform   acc-l72-mod-000      thru acc-l72-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Altezza modulo                              *
      *                  *---------------------------------------------*
           perform   acc-a72-mod-000      thru acc-a72-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Interlinee possibili per il modulo          *
      *                  *---------------------------------------------*
           perform   acc-int-mod-000      thru acc-int-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-450.
      *                  *---------------------------------------------*
      *                  * Si/no PostScript                            *
      *                  *---------------------------------------------*
           perform   acc-snx-psc-000      thru acc-snx-psc-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Si/no lettura facilitata                    *
      *                  *---------------------------------------------*
           perform   acc-snx-lfc-000      thru acc-snx-lfc-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-450.
       acc-tes-reg-550.
      *                  *---------------------------------------------*
      *                  * Orientamento                                *
      *                  *---------------------------------------------*
           perform   acc-ori-mod-000      thru acc-ori-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
       acc-tes-reg-600.
      *                  *---------------------------------------------*
      *                  * Classe di appartenenza                      *
      *                  *---------------------------------------------*
           perform   acc-cla-mod-000      thru acc-cla-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-450.
       acc-tes-reg-650.
      *                  *---------------------------------------------*
      *                  * Sistema applicativo di appartenenza         *
      *                  *---------------------------------------------*
           perform   acc-sap-mod-000      thru acc-sap-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-600.
       acc-tes-reg-700.
      *                  *---------------------------------------------*
      *                  * Codice modulo compatibile                   *
      *                  *---------------------------------------------*
           perform   acc-cmp-mod-000      thru acc-cmp-mod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-650.
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
      *              * Descrizione modulo                              *
      *              *-------------------------------------------------*
           perform   vis-des-mod-000      thru vis-des-mod-999        .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *-------------------------------------------------*
           perform   vis-tip-mod-000      thru vis-tip-mod-999        .
      *              *-------------------------------------------------*
      *              * Larghezza modulo                                *
      *              *-------------------------------------------------*
           perform   vis-l72-mod-000      thru vis-l72-mod-999        .
           perform   vis-l72-mod-plc-000  thru vis-l72-mod-plc-999    .
      *              *-------------------------------------------------*
      *              * Altezza modulo                                  *
      *              *-------------------------------------------------*
           perform   vis-a72-mod-000      thru vis-a72-mod-999        .
           perform   vis-a72-mod-plc-000  thru vis-a72-mod-plc-999    .
      *              *-------------------------------------------------*
      *              * Interlinee possibili per il modulo              *
      *              *-------------------------------------------------*
           perform   vis-int-mod-000      thru vis-int-mod-999        .
      *              *-------------------------------------------------*
      *              * Si/no PostScript                                *
      *              *-------------------------------------------------*
           perform   vis-snx-psc-000      thru vis-snx-psc-999        .
      *              *-------------------------------------------------*
      *              * Si/no lettura facilitata                        *
      *              *-------------------------------------------------*
           perform   vis-snx-lfc-000      thru vis-snx-lfc-999        .
      *              *-------------------------------------------------*
      *              * Orientamento                                    *
      *              *-------------------------------------------------*
           perform   vis-ori-mod-000      thru vis-ori-mod-999        .
      *              *-------------------------------------------------*
      *              * Classe di appartenenza                          *
      *              *-------------------------------------------------*
           perform   vis-cla-mod-000      thru vis-cla-mod-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo di appartenenza             *
      *              *-------------------------------------------------*
           perform   vis-sap-mod-000      thru vis-sap-mod-999        .
      *              *-------------------------------------------------*
      *              * Codice modulo compatibile                       *
      *              *-------------------------------------------------*
           perform   vis-cmp-mod-000      thru vis-cmp-mod-999        .
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
      *              * Descrizione modulo                              *
      *              *-------------------------------------------------*
           perform   pmt-des-mod-000      thru pmt-des-mod-999        .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-mod-000      thru pmt-tip-mod-999        .
      *              *-------------------------------------------------*
      *              * Larghezza modulo                                *
      *              *-------------------------------------------------*
           perform   pmt-l72-mod-000      thru pmt-l72-mod-999        .
      *              *-------------------------------------------------*
      *              * Altezza modulo                                  *
      *              *-------------------------------------------------*
           perform   pmt-a72-mod-000      thru pmt-a72-mod-999        .
      *              *-------------------------------------------------*
      *              * Interlinee possibili per il modulo              *
      *              *-------------------------------------------------*
           perform   pmt-int-mod-000      thru pmt-int-mod-999        .
      *              *-------------------------------------------------*
      *              * Si/no PostScript                                *
      *              *-------------------------------------------------*
           perform   pmt-snx-psc-000      thru pmt-snx-psc-999        .
      *              *-------------------------------------------------*
      *              * Si/no lettura facilitata                        *
      *              *-------------------------------------------------*
           perform   pmt-snx-lfc-000      thru pmt-snx-lfc-999        .
      *              *-------------------------------------------------*
      *              * Orientamento                                    *
      *              *-------------------------------------------------*
           perform   pmt-ori-mod-000      thru pmt-ori-mod-999        .
      *              *-------------------------------------------------*
      *              * Classe di appartenenza                          *
      *              *-------------------------------------------------*
           perform   pmt-cla-mod-000      thru pmt-cla-mod-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo di appartenenza             *
      *              *-------------------------------------------------*
           perform   pmt-sap-mod-000      thru pmt-sap-mod-999        .
      *              *-------------------------------------------------*
      *              * Codice modulo compatibile                       *
      *              *-------------------------------------------------*
           perform   pmt-cmp-mod-000      thru pmt-cmp-mod-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Descrizione modulo                               *
      *    *-----------------------------------------------------------*
       pmt-des-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione modulo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo modulo                                      *
      *    *-----------------------------------------------------------*
       pmt-tip-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo modulo                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Larghezza modulo                                 *
      *    *-----------------------------------------------------------*
       pmt-l72-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Larghezza modulo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-l72-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Altezza modulo                                   *
      *    *-----------------------------------------------------------*
       pmt-a72-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Altezza   modulo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-a72-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Interlinee possibili per il modulo               *
      *    *-----------------------------------------------------------*
       pmt-int-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Interlinee possibili       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-int-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/no PostScript                                 *
      *    *-----------------------------------------------------------*
       pmt-snx-psc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Modulo PostScript          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-psc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/no lettura facilitata                         *
      *    *-----------------------------------------------------------*
       pmt-snx-lfc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      53                   to   v-pos                  .
      *
           if        w-mod-snx-psc        =    "S"
                     move  "Lettura facilitata  :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-lfc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Orientamento                                     *
      *    *-----------------------------------------------------------*
       pmt-ori-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      53                   to   v-pos                  .
      *
           if        w-mod-snx-psc        =    "S"
                     move  "Orientamento :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ori-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Classe di appartenenza                           *
      *    *-----------------------------------------------------------*
       pmt-cla-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Classe di appartenenza     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cla-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Sistema applicativo di appartenenza              *
      *    *-----------------------------------------------------------*
       pmt-sap-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      53                   to   v-pos                  .
      *
           if        w-mod-cla-mod        =    "M"
                     move  "Sistema applicativo :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sap-mod-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice modulo compatibile                        *
      *    *-----------------------------------------------------------*
       pmt-cmp-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Compatibile con modulo     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cmp-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Descrizione modulo                         *
      *    *-----------------------------------------------------------*
       acc-des-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-des-mod        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-mod-999.
       acc-des-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mod-des-mod          .
       acc-des-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-mod-100.
       acc-des-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione modulo                      *
      *    *-----------------------------------------------------------*
       vis-des-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mod-des-mod        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo modulo                                *
      *    *-----------------------------------------------------------*
       acc-tip-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mod-lun    to   v-car                  .
           move      w-exp-tip-mod-num    to   v-ldt                  .
           move      "LTSP#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-tip-mod-tbl    to   v-txt                  .
           if        w-mod-tip-mod        =    "L"
                     move  01             to   v-num
           else if   w-mod-tip-mod        =    "T"
                     move  02             to   v-num
           else if   w-mod-tip-mod        =    "S"
                     move  03             to   v-num
           else if   w-mod-tip-mod        =    "P"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-mod-999.
       acc-tip-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "L"            to   w-mod-tip-mod
           else if   v-num                =    02
                     move  "T"            to   w-mod-tip-mod
           else if   v-num                =    03
                     move  "S"            to   w-mod-tip-mod
           else if   v-num                =    04
                     move  "P"            to   w-mod-tip-mod
           else      move  spaces         to   w-mod-tip-mod          .
       acc-tip-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mod-tip-mod        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-tip-mod-600
                     else  go to acc-tip-mod-100.
       acc-tip-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-mod-100.
       acc-tip-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo modulo                             *
      *    *-----------------------------------------------------------*
       vis-tip-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mod-lun    to   v-car                  .
           move      w-exp-tip-mod-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-mod-tbl    to   v-txt                  .
           if        w-mod-tip-mod        =    "L"
                     move  01             to   v-num
           else if   w-mod-tip-mod        =    "T"
                     move  02             to   v-num
           else if   w-mod-tip-mod        =    "S"
                     move  03             to   v-num
           else if   w-mod-tip-mod        =    "P"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Larghezza modulo                           *
      *    *-----------------------------------------------------------*
       acc-l72-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-l72-mod-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "Espresso in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-l72-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-l72-mod        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-l72-mod-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-l72-mod-900.
       acc-l72-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-mod-l72-mod          .
       acc-l72-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-l72-mod-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mod-l72-mod        =    zero
                     if    v-key          =    "UP  "
                           go to acc-l72-mod-600
                     else  go to acc-l72-mod-100.
       acc-l72-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Larghezza modulo in pollici                 *
      *                  *---------------------------------------------*
           perform   vis-l72-mod-plc-000  thru vis-l72-mod-plc-999    .
       acc-l72-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-l72-mod-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-l72-mod-100.
       acc-l72-mod-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-l72-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Larghezza modulo                        *
      *    *-----------------------------------------------------------*
       vis-l72-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mod-l72-mod        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-l72-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Larghezza modulo in pollici             *
      *    *-----------------------------------------------------------*
       vis-l72-mod-plc-000.
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-mod-l72-mod        =    zero
                     go to vis-l72-mod-plc-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           divide    72                   into w-mod-l72-mod
                                        giving w-wrk-dim-plc          .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-wrk-dim-plc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "("                  to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      """)"                to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-l72-mod-plc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Altezza modulo                             *
      *    *-----------------------------------------------------------*
       acc-a72-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-a72-mod-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "Espresso in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-a72-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-a72-mod        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-a72-mod-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-a72-mod-900.
       acc-a72-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-mod-a72-mod          .
       acc-a72-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-a72-mod-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mod-a72-mod        =    zero
                     if    v-key          =    "UP  "
                           go to acc-a72-mod-600
                     else  go to acc-a72-mod-100.
       acc-a72-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Altezza modulo in pollici                   *
      *                  *---------------------------------------------*
           perform   vis-a72-mod-plc-000  thru vis-a72-mod-plc-999    .
       acc-a72-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-a72-mod-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-a72-mod-100.
       acc-a72-mod-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-a72-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Altezza modulo                          *
      *    *-----------------------------------------------------------*
       vis-a72-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mod-a72-mod        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-a72-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Altezza modulo in pollici               *
      *    *-----------------------------------------------------------*
       vis-a72-mod-plc-000.
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-mod-a72-mod        =    zero
                     go to vis-a72-mod-plc-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           divide    72                   into w-mod-a72-mod
                                        giving w-wrk-dim-plc          .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-wrk-dim-plc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "("                  to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      """)"                to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-a72-mod-plc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Interlinee possibili per il modulo         *
      *    *-----------------------------------------------------------*
       acc-int-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-int-mod-005.
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
       acc-int-mod-010.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        >    20
                     go to acc-int-mod-020.
           if        w-mod-int-mod
                    (w-wrk-ctr-00a)       =    zero
                     go to acc-int-mod-010
           else      go to acc-int-mod-050.
       acc-int-mod-020.
           move      06,00                to   w-mod-int-mod (1)      .
       acc-int-mod-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..16           *
      *                  *---------------------------------------------*
           move      01                   to   w-wrk-ctr-00a          .
       acc-int-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-wrk-ctr-00a        >    8
                     move  14             to   v-lin
           else      move  13             to   v-lin                  .
           move      w-wrk-ctr-00a        to   v-pos                  .
           if        w-wrk-ctr-00a        >    8
                     subtract  8          from v-pos                  .
           multiply  6                    by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "TAB "               to   v-pfk (10)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-int-mod
                    (w-wrk-ctr-00a)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-int-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-int-mod-999.
       acc-int-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-mod-int-mod
                                              (w-wrk-ctr-00a)         .
       acc-int-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-int-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-int-mod-625
           else if   v-key                =    spaces or
                     v-key                =    "DOWN"
                     go to acc-int-mod-800
           else if   v-key                =    "DO  "
                     go to acc-int-mod-800
           else      go to acc-int-mod-650.
       acc-int-mod-625.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        w-wrk-ctr-00a        =    1
                     go to acc-int-mod-800.
           subtract  1                    from w-wrk-ctr-00a          .
           go to     acc-int-mod-100.
       acc-int-mod-650.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *                                                 *
      *              * N.B.: ridotte a 8 (16/01/03)                    *
      *              *-------------------------------------------------*
           if        w-wrk-ctr-00a        =    08
                     go to acc-int-mod-800.
           add       1                    to   w-wrk-ctr-00a          .
           go to     acc-int-mod-100.
       acc-int-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-int-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-int-mod-100.
       acc-int-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Interlinee possibili per il modulo      *
      *    *-----------------------------------------------------------*
       vis-int-mod-000.
           move      spaces               to   w-edt-ilp-are          .
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       vis-int-mod-100.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    2
                     go to vis-int-mod-300.
           move      zero                 to   w-wrk-ctr-00c          .
       vis-int-mod-200.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    8
                     go to vis-int-mod-100.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-mod-int-mod
                    (w-wrk-ctr-00a)       =    zero
                     go to vis-int-mod-200.
           move      w-mod-int-mod
                    (w-wrk-ctr-00a)       to   w-edt-ilp-ele
                                              (w-wrk-ctr-00b,
                                               w-wrk-ctr-00c)         .
           go to     vis-int-mod-200.
       vis-int-mod-300.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-ilp-1o2 (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-ilp-1o2 (2)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-int-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/no PostScript                           *
      *    *-----------------------------------------------------------*
       acc-snx-psc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-psc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-psc-lun    to   v-car                  .
           move      w-exp-snx-psc-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-snx-psc-tbl    to   v-txt                  .
      *
           if        w-mod-snx-psc        =    "N"
                     move  01             to   v-num
           else if   w-mod-snx-psc        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-psc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-psc-999.
       acc-snx-psc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-mod-snx-psc
           else if   v-num                =    02
                     move  "S"            to   w-mod-snx-psc
           else      move  spaces         to   w-mod-snx-psc          .
       acc-snx-psc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-psc-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammesso                   *
      *                  *---------------------------------------------*
           if        w-mod-snx-psc        not  = "S" and
                     w-mod-snx-psc        not  = "N"
                     go to acc-snx-psc-100.
       acc-snx-psc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompts                                     *
      *                  *---------------------------------------------*
           perform   pmt-snx-lfc-000      thru pmt-snx-lfc-999        .
           perform   pmt-ori-mod-000      thru pmt-ori-mod-999        .
       acc-snx-psc-625.
      *                  *---------------------------------------------*
      *                  * Eventuali normalizzazioni                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-mod-snx-psc        =    "S"
                     go to acc-snx-psc-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazioni                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-snx-lfc          .
           move      spaces               to   w-mod-ori-mod          .
      *                      *-----------------------------------------*
      *                      * Visualizzazioni                         *
      *                      *-----------------------------------------*
           perform   vis-snx-lfc-000      thru vis-snx-lfc-999        .
           perform   vis-ori-mod-000      thru vis-ori-mod-999        .
       acc-snx-psc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-psc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-psc-100.
       acc-snx-psc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/no PostScript                        *
      *    *-----------------------------------------------------------*
       vis-snx-psc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-psc-lun    to   v-car                  .
           move      w-exp-snx-psc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-psc-tbl    to   v-txt                  .
      *
           if        w-mod-snx-psc        =    "N"
                     move  01             to   v-num
           else if   w-mod-snx-psc        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-psc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/no lettura facilitata                   *
      *    *-----------------------------------------------------------*
       acc-snx-lfc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mod-snx-psc        not  = "S"
                     go to acc-snx-lfc-999.
       acc-snx-lfc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-lfc-lun    to   v-car                  .
           move      w-exp-snx-lfc-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-snx-lfc-tbl    to   v-txt                  .
      *
           if        w-mod-snx-lfc        =    "N"
                     move  01             to   v-num
           else if   w-mod-snx-lfc        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lfc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lfc-999.
       acc-snx-lfc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-mod-snx-lfc
           else if   v-num                =    02
                     move  "S"            to   w-mod-snx-lfc
           else      move  spaces         to   w-mod-snx-lfc          .
       acc-snx-lfc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-lfc-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammesso                   *
      *                  *---------------------------------------------*
           if        w-mod-snx-lfc        not  = "S" and
                     w-mod-snx-lfc        not  = "N"
                     go to acc-snx-lfc-100.
       acc-snx-lfc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-lfc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-lfc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-lfc-100.
       acc-snx-lfc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/no lettura facilitata                *
      *    *-----------------------------------------------------------*
       vis-snx-lfc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-lfc-lun    to   v-car                  .
           move      w-exp-snx-lfc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-exp-snx-lfc-tbl    to   v-txt                  .
      *
           if        w-mod-snx-lfc        =    "N"
                     move  01             to   v-num
           else if   w-mod-snx-lfc        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-lfc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Orientamento                               *
      *    *-----------------------------------------------------------*
       acc-ori-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mod-snx-psc        not  = "S"
                     go to acc-ori-mod-999.
       acc-ori-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ori-mod-lun    to   v-car                  .
           move      w-exp-ori-mod-num    to   v-ldt                  .
           move      "AOV#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-ori-mod-tbl    to   v-txt                  .
      *
           if        w-mod-ori-mod        =    "A"
                     move  01             to   v-num
           else if   w-mod-ori-mod        =    "O"
                     move  02             to   v-num
           else if   w-mod-ori-mod        =    "V"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ori-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ori-mod-999.
       acc-ori-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A"            to   w-mod-ori-mod
           else if   v-num                =    02
                     move  "O"            to   w-mod-ori-mod
           else if   v-num                =    03
                     move  "V"            to   w-mod-ori-mod
           else      move  spaces         to   w-mod-ori-mod          .
       acc-ori-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ori-mod-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammesso                   *
      *                  *---------------------------------------------*
           if        w-mod-ori-mod        not  = "A" and
                     w-mod-ori-mod        not  = "O" and
                     w-mod-ori-mod        not  = "V"
                     go to acc-ori-mod-100.
       acc-ori-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ori-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ori-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ori-mod-100.
       acc-ori-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Orientamento                            *
      *    *-----------------------------------------------------------*
       vis-ori-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ori-mod-lun    to   v-car                  .
           move      w-exp-ori-mod-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-exp-ori-mod-tbl    to   v-txt                  .
      *
           if        w-mod-ori-mod        =    "A"
                     move  01             to   v-num
           else if   w-mod-ori-mod        =    "O"
                     move  02             to   v-num
           else if   w-mod-ori-mod        =    "V"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ori-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Classe di appartenenza                     *
      *    *-----------------------------------------------------------*
       acc-cla-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cla-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-mod-lun    to   v-car                  .
           move      w-exp-cla-mod-num    to   v-ldt                  .
           move      "SM#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-cla-mod-tbl    to   v-txt                  .
           if        w-mod-cla-mod        =    "S"
                     move  01             to   v-num
           else if   w-mod-cla-mod        =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-mod-999.
       acc-cla-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-mod-cla-mod
           else if   v-num                =    02
                     move  "M"            to   w-mod-cla-mod
           else      move  spaces         to   w-mod-cla-mod          .
       acc-cla-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cla-mod-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammesso                   *
      *                  *---------------------------------------------*
           if        w-mod-cla-mod        not  = "S" and
                     w-mod-cla-mod        not  = "M"
                     go to acc-cla-mod-100.
       acc-cla-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt per sistema applicativo              *
      *                  *---------------------------------------------*
           perform   pmt-sap-mod-000      thru pmt-sap-mod-999        .
       acc-cla-mod-625.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione sistema applica-  *
      *                  * tivo di appartenenza                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non si tratta di un modulo su misu-  *
      *                      * ra : nessuna normalizzazione            *
      *                      *-----------------------------------------*
           if        w-mod-cla-mod        =    "M"
                     go to acc-cla-mod-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-sap-mod          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-sap-mod-000      thru vis-sap-mod-999        .
       acc-cla-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-mod-100.
       acc-cla-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Classe di appartenenza                  *
      *    *-----------------------------------------------------------*
       vis-cla-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-mod-lun    to   v-car                  .
           move      w-exp-cla-mod-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-cla-mod-tbl    to   v-txt                  .
           if        w-mod-cla-mod        =    "S"
                     move  01             to   v-num
           else if   w-mod-cla-mod        =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sistema applicativo di appartenenza        *
      *    *-----------------------------------------------------------*
       acc-sap-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mod-cla-mod        not  = "M"
                     go to acc-sap-mod-999.
       acc-sap-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-sap-mod        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sap-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sap-mod-999.
       acc-sap-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mod-sap-mod          .
       acc-sap-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sap-mod-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-mod-sap-mod        to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-sap-mod-100.
       acc-sap-mod-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mod-sap-mod        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-sap-mod-600
                     else  go to acc-sap-mod-100.
       acc-sap-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sap-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sap-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sap-mod-100.
       acc-sap-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Sistema applicativo di appartenenza     *
      *    *-----------------------------------------------------------*
       vis-sap-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-mod-sap-mod        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sap-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice modulo compatibile                  *
      *    *-----------------------------------------------------------*
       acc-cmp-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cmp-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-mod-cmp-mod        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cmp-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cmp-mod-999.
       acc-cmp-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mod-cmp-mod          .
       acc-cmp-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmp-mod-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-mod-cmp-mod        to   w-all-str-alf          .
           move      08                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cmp-mod-100.
       acc-cmp-mod-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mod-cmp-mod        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-cmp-mod-600
                     else  go to acc-cmp-mod-100.
       acc-cmp-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmp-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cmp-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cmp-mod-100.
       acc-cmp-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice modulo compatibile               *
      *    *-----------------------------------------------------------*
       vis-cmp-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mod-cmp-mod        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmp-mod-999.
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
           if        w-mod-cod-mod        =    spaces
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
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione                        *
      *              *-------------------------------------------------*
           if        w-mod-des-mod        not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca la descrizione per il codice modulo !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su tipo modulo                        *
      *              *-------------------------------------------------*
           if        w-mod-tip-mod        =    "L" or
                     w-mod-tip-mod        =    "T" or
                     w-mod-tip-mod        =    "S" or
                     w-mod-tip-mod        =    "P"
                     go to cnt-tdo-nok-150.
           move      "Manca il tipo modulo !                            
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo su larghezza modulo                   *
      *              *-------------------------------------------------*
           if        w-mod-l72-mod        not  = zero
                     go to cnt-tdo-nok-200.
           move      "Manca la larghezza modulo !                       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su altezza modulo                     *
      *              *-------------------------------------------------*
           if        w-mod-a72-mod        not  = zero
                     go to cnt-tdo-nok-250.
           move      "Manca l'altezza modulo !                          
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo su interlinee possibili               *
      *              *-------------------------------------------------*
           if        w-mod-int-mod (01)   not  = zero or
                     w-mod-int-mod (02)   not  = zero or
                     w-mod-int-mod (03)   not  = zero or
                     w-mod-int-mod (04)   not  = zero or
                     w-mod-int-mod (05)   not  = zero or
                     w-mod-int-mod (06)   not  = zero or
                     w-mod-int-mod (07)   not  = zero or
                     w-mod-int-mod (08)   not  = zero or
                     w-mod-int-mod (09)   not  = zero or
                     w-mod-int-mod (10)   not  = zero or
                     w-mod-int-mod (11)   not  = zero or
                     w-mod-int-mod (12)   not  = zero or
                     w-mod-int-mod (13)   not  = zero or
                     w-mod-int-mod (14)   not  = zero or
                     w-mod-int-mod (15)   not  = zero or
                     w-mod-int-mod (16)   not  = zero or
                     w-mod-int-mod (17)   not  = zero or
                     w-mod-int-mod (18)   not  = zero or
                     w-mod-int-mod (19)   not  = zero or
                     w-mod-int-mod (20)   not  = zero
                     go to cnt-tdo-nok-300.
           move      "Mancano le interlinee possibili per il modulo !   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su classe di appartenenza             *
      *              *-------------------------------------------------*
           if        w-mod-cla-mod        =    "S" or
                     w-mod-cla-mod        =    "M"
                     go to cnt-tdo-nok-350.
           move      "Manca la classe di appartenenza !                 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-350.
      *              *-------------------------------------------------*
      *              * Controllo su sistema applicativo di appartenen- *
      *              * za                                              *
      *              *-------------------------------------------------*
           if        w-mod-cla-mod        =    "S"
                     go to cnt-tdo-nok-400.
           if        w-mod-sap-mod        not  = spaces
                     go to cnt-tdo-nok-400.
           move      "Manca il sistema applicativo di appartenenza !    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Compattamento interlinee possibili              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       cnt-tdo-nok-410.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    20
                     go to cnt-tdo-nok-420.
           if        w-mod-int-mod
                    (w-wrk-ctr-00b)       =    zero
                     go to cnt-tdo-nok-410.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        =    w-wrk-ctr-00b
                     go to cnt-tdo-nok-410.
           move      w-mod-int-mod
                    (w-wrk-ctr-00b)       to   w-mod-int-mod
                                              (w-wrk-ctr-00a)         .
           go to     cnt-tdo-nok-410.
       cnt-tdo-nok-420.
           move      w-wrk-ctr-00a        to   w-wrk-ctr-00c          .
       cnt-tdo-nok-430.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    20
                     go to cnt-tdo-nok-800.
           move      zero                 to   w-mod-int-mod
                                              (w-wrk-ctr-00c)         .
           go to     cnt-tdo-nok-430.
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
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      "NO"                 to   j-ope                  .
           move      "MOD"                to   j-tre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           move      j-dat                to   w-mod                  .
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
           move      "MOD"                to   j-tre                  .
           move      w-mod-cod-mod        to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        j-rsc                =    e-not-fnd
                     go to rou-let-reg-100
           else if   j-rsc                =    spaces
                     go to rou-let-reg-300
           else      go to rou-let-reg-900.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Inserimento            *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-300.
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
           move      j-dat                to   w-mod                  .
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
      *                  * il codice modulo                            *
      *                  *---------------------------------------------*
           move      w-mod-cod-mod        to   w-clo-mod-cod-mod      .
           move      w-clo-mod-rec-mod    to   w-mod                  .
           move      w-clo-mod-cod-mod    to   w-mod-cod-mod          .
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
      *              *-------------------------------------------------*
      *              * Sistema applicativo di appartenenza             *
      *              *-------------------------------------------------*
           perform   pmt-sap-mod-000      thru pmt-sap-mod-999        .
      *              *-------------------------------------------------*
      *              * Si/no lettura facilitata                        *
      *              *-------------------------------------------------*
           perform   pmt-snx-lfc-000      thru pmt-snx-lfc-999        .
      *              *-------------------------------------------------*
      *              * Orientamento                                    *
      *              *-------------------------------------------------*
           perform   pmt-ori-mod-000      thru pmt-ori-mod-999        .
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
           move      "MOD"                to   j-tre                  .
           move      w-mod-cod-mod        to   j-kre                  .
           move      w-mod                to   j-dat                  .
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
           move      "MOD"                to   j-tre                  .
           move      w-mod-cod-mod        to   j-kre                  .
           move      w-mod                to   j-dat                  .
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
           move      "MOD"                to   j-tre                  .
           move      w-mod-cod-mod        to   j-kre                  .
           move      w-mod                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Find su codici modulo di [pss]                            *
      *    *-----------------------------------------------------------*
       fnd-pss-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-pss-mod-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg4310"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-pss-mod-sel
                     go to  fnd-pss-mod-999.
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
      *              * Preparazione variabile di i.p.c. 'cod-mod'      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-mod"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      08                   to   s-car                  .
           move      w-fnd-pss-mod-cod    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg4310"                       .
           cancel    "swd/xpg/prg/obj/pxpg4310"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "pss-mod"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-pss-mod-sel
                     move  s-alf          to   w-fnd-pss-mod-cod
           else      move  "#"            to   w-fnd-pss-mod-sel      .
       fnd-pss-mod-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .



