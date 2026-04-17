       Identification Division.
       Program-Id.                                 pcge2000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    arc                 *
      *                                   Fase:    cge200              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/12/90    *
      *                       Ultima revisione:    NdK del 14/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio piano dei conti           *
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
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge200"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge2000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    GESTIONE ARCHIVIO PIANO DEI CONTI   "       .

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
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [cdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcdb"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [zma]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzma"                          .
      *        *-------------------------------------------------------*
      *        * [zcn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcn"                          .
      *        *-------------------------------------------------------*
      *        * [zsz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzsz"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-mas          pic  9(02)                  .
               10  w-tes-cod-mas-des      pic  x(40)                  .
               10  w-tes-cod-con          pic  9(02)                  .
               10  w-tes-cod-con-des      pic  x(40)                  .
               10  w-tes-cod-stc          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-mcs          pic  x(40)                  .
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-tip-cnt          pic  x(01)                  .
               10  w-tes-cod-sez          pic  9(02)                  .
               10  w-tes-cod-sez-des      pic  x(20)                  .
               10  w-tes-ccb-dar          pic  x(06)                  .
               10  w-tes-ccb-dar-des      pic  x(40)                  .
               10  w-tes-ccb-ave          pic  x(06)                  .
               10  w-tes-ccb-ave-des      pic  x(40)                  .
               10  w-tes-key-ref          pic  x(10)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per mastro-conto-sottoconto                     *
      *    *-----------------------------------------------------------*
       01  w-mcs.
      *        *-------------------------------------------------------*
      *        * Per tipo codice trattato                              *
      *        * - M : Mastro                                          *
      *        * - C : Conto                                           *
      *        * - S : Sottoconto                                      *
      *        *-------------------------------------------------------*
           05  w-mcs-cod-trt              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per codice piano dei conti                            *
      *        *-------------------------------------------------------*
           05  w-mcs-cod-pdc              pic  9(07)                  .
           05  w-mcs-cod-pdr redefines
               w-mcs-cod-pdc.
               10  w-mcs-cod-mas          pic  9(02)                  .
               10  w-mcs-cod-con          pic  9(02)                  .
               10  w-mcs-cod-stc          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per classificatori di bilancio                  *
      *    *-----------------------------------------------------------*
       01  w-cdb.
           05  w-cdb-cod-cdb              pic  x(06)                  .
           05  w-cdb-cod-red redefines
               w-cdb-cod-cdb.
               10  w-cdb-tip-cdb          pic  x(01)                  .
               10  w-cdb-cod-rgr          pic  9(05)                  .
               10  w-cdb-cod-rer redefines
                   w-cdb-cod-rgr.
                   15  w-cdb-gru-cdb      pic  9(02)                  .
                   15  w-cdb-sgr-cdb      pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su tabella [zma]                        *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-zma.
               10  w-fnd-arc-zma-sel      pic  x(01)                  .
               10  w-fnd-arc-zma-pdc      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su tabella [zcn]                        *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-zcn.
               10  w-fnd-arc-zcn-sel      pic  x(01)                  .
               10  w-fnd-arc-zcn-pdc      pic  9(07)                  .
               10  w-fnd-arc-zcn-pdr  redefines
                   w-fnd-arc-zcn-pdc.
                   15   w-fnd-arc-zcn-mas pic  9(02)                  .
                   15   w-fnd-arc-zcn-con pic  9(02)                  .
                   15   w-fnd-arc-zcn-stc pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [pdc]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-pdc.
               10  w-fnd-arc-pdc-sel      pic  x(01)                  .
               10  w-fnd-arc-pdc-mas      pic  9(02)                  .
               10  w-fnd-arc-pdc-con      pic  9(02)                  .
               10  w-fnd-arc-pdc-cod      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su tabella [zsz]                        *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-zsz.
               10  w-fnd-arc-zsz-sel      pic  x(01)                  .
               10  w-fnd-arc-zsz-cod      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [cdb]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-cdb.
               10  w-fnd-arc-cdb-sel      pic  x(01)                  .
               10  w-fnd-arc-cdb-cod      pic  x(06)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [zma]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-zma.
               10  w-let-arc-zma-flg      pic  x(01)                  .
               10  w-let-arc-zma-cod      pic  9(02)                  .
               10  w-let-arc-zma-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [zcn]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcn.
               10  w-let-arc-zcn-flg      pic  x(01)                  .
               10  w-let-arc-zcn-cod      pic  9(02)                  .
               10  w-let-arc-zcn-mas      pic  9(02)                  .
               10  w-let-arc-zcn-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [zsz]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-zsz.
               10  w-let-arc-zsz-flg      pic  x(01)                  .
               10  w-let-arc-zsz-cod      pic  9(02)                  .
               10  w-let-arc-zsz-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cdb]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cdb.
               10  w-let-arc-cdb-flg      pic  x(01)                  .
               10  w-let-arc-cdb-tip      pic  x(01)                  .
               10  w-let-arc-cdb-cod      pic  9(05)                  .
               10  w-let-arc-cdb-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Codice e descrizione mastro                           *
      *        *-------------------------------------------------------*
           05  w-def-cod-mas              pic  9(02)                  .
           05  w-def-cod-mas-des          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice e descrizione conto                            *
      *        *-------------------------------------------------------*
           05  w-def-cod-con              pic  9(02)                  .
           05  w-def-cod-con-des          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice mastro                                         *
      *        *-------------------------------------------------------*
           05  w-sav-cod-mas              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice conto                                          *
      *        *-------------------------------------------------------*
           05  w-sav-cod-con              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo conto                                            *
      *        *-------------------------------------------------------*
           05  w-sav-tip-cnt              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo conto                                 *
      *        *-------------------------------------------------------*
           05  w-exp-tip-cnt.
               10  w-exp-tip-cnt-num      pic  9(02)       value 2    .
               10  w-exp-tip-cnt-lun      pic  9(02)       value 20   .
               10  w-exp-tip-cnt-tbl.
                   15  filler             pic  x(20) value
                            "Patrimoniale        "                    .
                   15  filler             pic  x(20) value
                            "Economico           "                    .

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

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti schede saldo per il sotto-   *
      *        * conto contabile                                       *
      *        *-------------------------------------------------------*
           05  w-det-snx-mgs.
      *            *---------------------------------------------------*
      *            * Codice sottoconto                                 *
      *            *---------------------------------------------------*
               10  w-det-snx-mgs-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Anni di esercizio attuale e precedente            *
      *            *---------------------------------------------------*
               10  w-det-snx-mgs-sac      pic  9(03)                  .
               10  w-det-snx-mgs-sap      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatori schede trovate esercizio attuale e      *
      *            * precedente                                        *
      *            *---------------------------------------------------*
               10  w-det-snx-mgs-csc      pic  9(05)                  .
               10  w-det-snx-mgs-cvc      pic  9(05)                  .
               10  w-det-snx-mgs-csp      pic  9(05)                  .
               10  w-det-snx-mgs-cvp      pic  9(05)                  .

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
      *              * Lettura personalizzazione numero livelli del    *
      *              * piano dei conti                                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Lettura variabili di i.p.c. per preparazione    *
      *              * eventuali defaults generali                     *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to pre-exe-pgm-200.
      *                  *---------------------------------------------*
      *                  * Variabile "cod-mas" per codice mastro       *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-mas"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-def-cod-mas
           else      move  zero           to   w-def-cod-mas          .
      *                  *---------------------------------------------*
      *                  * Descrizione mastro a Spaces                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-def-cod-mas-des      .
       pre-exe-pgm-200.
      *                  *---------------------------------------------*
      *                  * Variabile "cod-con" per codice conto        *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-con"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-def-cod-con
           else      move  zero           to   w-def-cod-con          .
      *                  *---------------------------------------------*
      *                  * Descrizione conto a Spaces                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-def-cod-con-des      .
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
      *              * [cdb]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdb                 .
      *              *-------------------------------------------------*
      *              * [mgs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * [zma]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *              *-------------------------------------------------*
      *              * [zcn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *              *-------------------------------------------------*
      *              * [zsz]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzsz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsz                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * [cdb]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdb                 .
      *              *-------------------------------------------------*
      *              * [mgs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * [zma]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *              *-------------------------------------------------*
      *              * [zcn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *              *-------------------------------------------------*
      *              * [zsz]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzsz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsz                 .
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
      *                  * Codice mastro                               *
      *                  *---------------------------------------------*
           perform   acc-cod-mas-000      thru acc-cod-mas-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice conto                                *
      *                  *---------------------------------------------*
           perform   acc-cod-con-000      thru acc-cod-con-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
           perform   acc-cod-stc-000      thru acc-cod-stc-999        .
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
      *              * Codice mastro                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-mas-000      thru vis-cod-mas-999        .
      *              *-------------------------------------------------*
      *              * Codice conto                                    *
      *              *-------------------------------------------------*
           perform   vis-cod-con-000      thru vis-cod-con-999        .
      *              *-------------------------------------------------*
      *              * Codice sottoconto                               *
      *              *-------------------------------------------------*
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
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
           if        w-prs-liv-pdc        =    2
                     move  06             to   v-lto
           else      move  07             to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice mastro                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-mas-000      thru pmt-cod-mas-999        .
      *              *-------------------------------------------------*
      *              * Codice conto                                    *
      *              *-------------------------------------------------*
           perform   pmt-cod-con-000      thru pmt-cod-con-999        .
      *              *-------------------------------------------------*
      *              * Codice sottoconto                               *
      *              *-------------------------------------------------*
           perform   pmt-cod-stc-000      thru pmt-cod-stc-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           if        w-prs-liv-pdc        =    2
                     move  06             to   v-lin
           else      move  07             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice mastro                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mas-000.
           if        w-prs-liv-pdc        =    2
                     go to pmt-cod-mas-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mastro              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-mas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice conto                  *
      *    *-----------------------------------------------------------*
       pmt-cod-con-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        w-prs-liv-pdc        =    2
                     move  04             to   v-lin
           else      move  05             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice conto               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-con-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice sottoconto             *
      *    *-----------------------------------------------------------*
       pmt-cod-stc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        w-prs-liv-pdc        =    2
                     move  05             to   v-lin
           else      move  06             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice sottoconto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-stc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice mastro                 *
      *    *-----------------------------------------------------------*
       acc-cod-mas-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to acc-cod-mas-999.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mas-020.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-mas        not  = zero
                     go to acc-cod-mas-050.
           move      w-def-cod-mas        to   w-tes-cod-mas          .
           move      w-def-cod-mas-des    to   w-tes-cod-mas-des      .
           if        w-tes-cod-mas        =    zero
                     go to acc-cod-mas-050.
           if        w-tes-cod-mas-des    not  = spaces
                     go to acc-cod-mas-022.
           move      w-tes-cod-mas        to   w-let-arc-zma-cod      .
           perform   let-arc-zma-000      thru let-arc-zma-999        .
           move      w-let-arc-zma-des    to   w-tes-cod-mas-des      .
       acc-cod-mas-022.
           perform   vis-cod-mas-000      thru vis-cod-mas-999        .
           perform   vis-des-mas-000      thru vis-des-mas-999        .
       acc-cod-mas-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-mas        to   w-sav-cod-mas          .
       acc-cod-mas-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-mas        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mas-999.
       acc-cod-mas-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-mas          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-mas-400.
      *                  *---------------------------------------------*
      *                  * Find su tabella [zma]                       *
      *                  *---------------------------------------------*
           perform   fnd-arc-zma-000      thru fnd-arc-zma-999        .
           if        w-fnd-arc-zma-sel    not  = spaces
                     go to acc-cod-mas-100.
      *                  *---------------------------------------------*
      *                  * Scomposizione codice letto                  *
      *                  *---------------------------------------------*
           move      w-fnd-arc-zma-pdc    to   w-mcs-cod-pdc          .
           if        w-mcs-cod-mas        not  = zero and
                     w-mcs-cod-con        =    zero and
                     w-mcs-cod-stc        =    zero
                     go to acc-cod-mas-222
           else if   w-mcs-cod-mas        not  = zero and
                     w-mcs-cod-con        not  = zero and
                     w-mcs-cod-stc        =    zero
                     go to acc-cod-mas-224
           else if   w-mcs-cod-mas        not  = zero and
                     w-mcs-cod-con        not  = zero and
                     w-mcs-cod-stc        not  = zero
                     go to acc-cod-mas-226
           else      go to acc-cod-mas-100.
       acc-cod-mas-222.
      *                      *-----------------------------------------*
      *                      * Se selezionato solo mastro              *
      *                      *-----------------------------------------*
           move      w-mcs-cod-mas        to   w-tes-cod-mas          .
           perform   vis-cod-mas-000      thru vis-cod-mas-999        .
           move      spaces               to   v-key                  .
           go to     acc-cod-mas-400.
       acc-cod-mas-224.
      *                      *-----------------------------------------*
      *                      * Se selezionato mastro e conto           *
      *                      *-----------------------------------------*
           move      w-mcs-cod-mas        to   w-tes-cod-mas          .
           move      w-mcs-cod-con        to   w-tes-cod-con          .
      *                          *-------------------------------------*
      *                          * Lettura descrizione mastro          *
      *                          *-------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zma-cod      .
           perform   let-arc-zma-000      thru let-arc-zma-999        .
      *                          *-------------------------------------*
      *                          * Memorizzazione descrizione          *
      *                          *-------------------------------------*
           move      w-let-arc-zma-des    to   w-tes-cod-mas-des      .
      *                          *-------------------------------------*
      *                          * Visualizzazione mastro              *
      *                          *-------------------------------------*
           perform   vis-cod-mas-000      thru vis-cod-mas-999        .
           perform   vis-des-mas-000      thru vis-des-mas-999        .
      *                          *-------------------------------------*
      *                          * Lettura descrizione conto           *
      *                          *-------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zcn-mas      .
           move      w-tes-cod-con        to   w-let-arc-zcn-cod      .
           perform   let-arc-zcn-000      thru let-arc-zcn-999        .
      *                          *-------------------------------------*
      *                          * Memorizzazione descrizione          *
      *                          *-------------------------------------*
           move      w-let-arc-zcn-des    to   w-tes-cod-con-des      .
      *                          *-------------------------------------*
      *                          * Visualizzazione conto               *
      *                          *-------------------------------------*
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           perform   vis-des-con-000      thru vis-des-con-999        .
           move      spaces               to   v-key                  .
           go to     acc-cod-mas-800.
       acc-cod-mas-226.
      *                      *-----------------------------------------*
      *                      * Se selezionato mastro, conto e sotto-   *
      *                      * conto                                   *
      *                      *-----------------------------------------*
           move      w-mcs-cod-mas        to   w-tes-cod-mas          .
           move      w-mcs-cod-con        to   w-tes-cod-con          .
           move      w-mcs-cod-stc        to   w-tes-cod-stc          .
      *                          *-------------------------------------*
      *                          * Lettura descrizione mastro          *
      *                          *-------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zma-cod      .
           perform   let-arc-zma-000      thru let-arc-zma-999        .
      *                          *-------------------------------------*
      *                          * Memorizzazione descrizione          *
      *                          *-------------------------------------*
           move      w-let-arc-zma-des    to   w-tes-cod-mas-des      .
      *                          *-------------------------------------*
      *                          * Visualizzazione mastro              *
      *                          *-------------------------------------*
           perform   vis-cod-mas-000      thru vis-cod-mas-999        .
           perform   vis-des-mas-000      thru vis-des-mas-999        .
      *                          *-------------------------------------*
      *                          * Lettura descrizione conto           *
      *                          *-------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zcn-mas      .
           move      w-tes-cod-con        to   w-let-arc-zcn-cod      .
           perform   let-arc-zcn-000      thru let-arc-zcn-999        .
      *                          *-------------------------------------*
      *                          * Memorizzazione descrizione          *
      *                          *-------------------------------------*
           move      w-let-arc-zcn-des    to   w-tes-cod-con-des      .
      *                          *-------------------------------------*
      *                          * Visualizzazione conto               *
      *                          *-------------------------------------*
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           perform   vis-des-con-000      thru vis-des-con-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice sottoconto   *
      *                          *-------------------------------------*
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
           move      "DO  "               to   v-key                  .
           go to     acc-cod-mas-800.
       acc-cod-mas-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura descrizione mastro                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zma-cod      .
           perform   let-arc-zma-000      thru let-arc-zma-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zma-des    to   w-tes-cod-mas-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione mastro          *
      *                  *---------------------------------------------*
           perform   vis-des-mas-000      thru vis-des-mas-999        .
      *                  *---------------------------------------------*
      *                  * Se valore variato : normalizzazione del     *
      *                  * conto e del sottoconto                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-mas        =    w-sav-cod-mas
                     go to acc-cod-mas-460.
           if        w-sav-cod-mas        =    zero
                     go to acc-cod-mas-460.
      *                      *-----------------------------------------*
      *                      * Normalizzazione conto                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-con        =    zero
                     go to acc-cod-mas-420.
           move      zero                 to   w-tes-cod-con          .
           move      spaces               to   w-tes-cod-con-des      .
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           perform   vis-des-con-000      thru vis-des-con-999        .
       acc-cod-mas-420.
      *                      *-----------------------------------------*
      *                      * Normalizzazione sottoconto              *
      *                      *-----------------------------------------*
           if        w-tes-cod-stc        =    zero
                     go to acc-cod-mas-460.
           move      zero                 to   w-tes-cod-stc          .
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
       acc-cod-mas-460.
      *                  *---------------------------------------------*
      *                  * Se valore impostato a Zero si forza il tas- *
      *                  * to Exit                                     *
      *                  *---------------------------------------------*
           if        w-tes-cod-mas        =    zero
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mas-999.
       acc-cod-mas-480.
      *                  *---------------------------------------------*
      *                  * Se mastro non esistente : forzatura del     *
      *                  * tasto Do                                    *
      *                  *---------------------------------------------*
           if        w-let-arc-zma-flg    not  = spaces
                     move  "DO  "         to   v-key
                     go to acc-cod-mas-800.
       acc-cod-mas-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mas-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-mas-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mas-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-mas-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-mas-999.
       acc-cod-mas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice mastro              *
      *    *-----------------------------------------------------------*
       vis-cod-mas-000.
      *                  *---------------------------------------------*
      *                  * Test se campo da visualizzare               *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to vis-cod-mas-999.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mas        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione Mastro         *
      *    *-----------------------------------------------------------*
       vis-des-mas-000.
      *                  *---------------------------------------------*
      *                  * Test se campo da visualizzare               *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to vis-des-mas-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-cod-mas-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice conto                  *
      *    *-----------------------------------------------------------*
       acc-cod-con-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-con        not  = zero
                     go to acc-cod-con-050.
           move      w-def-cod-con        to   w-tes-cod-con          .
           move      w-def-cod-con-des    to   w-tes-cod-con-des      .
           if        w-tes-cod-con        =    zero
                     go to acc-cod-con-050.
           if        w-tes-cod-con-des    not  = spaces
                     go to acc-cod-con-022.
      *                  *---------------------------------------------*
      *                  * Test su livello pdc                         *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     move   zero          to   w-fnd-arc-zcn-mas
           else      move   w-tes-cod-mas to   w-fnd-arc-zcn-mas      .
           move      w-tes-cod-con        to   w-let-arc-zcn-cod      .
           perform   let-arc-zcn-000      thru let-arc-zcn-999        .
           move      w-let-arc-zcn-des    to   w-tes-cod-con-des      .
       acc-cod-con-022.
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           perform   vis-des-con-000      thru vis-des-con-999        .
       acc-cod-con-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-con        to   w-sav-cod-con          .
       acc-cod-con-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-prs-liv-pdc        =    2
                     move  04             to   v-lin
           else      move  05             to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-prs-liv-pdc        =    3
                     move  "UP  "         to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-con        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-con-999.
       acc-cod-con-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-con          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-con-400.
      *                  *---------------------------------------------*
      *                  * Find su tabella [zcn]                       *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     move   zero          to   w-fnd-arc-zcn-mas
           else      move   w-tes-cod-mas to   w-fnd-arc-zcn-mas      .
           move      zero                 to   w-fnd-arc-zcn-con      .
           move      zero                 to   w-fnd-arc-zcn-stc      .
           perform   fnd-arc-zcn-000      thru fnd-arc-zcn-999        .
           if        w-fnd-arc-zcn-sel    not  = spaces
                     go to acc-cod-con-100.
      *                  *---------------------------------------------*
      *                  * Scomposizione codice letto                  *
      *                  *---------------------------------------------*
           move      w-fnd-arc-zcn-pdc    to   w-mcs-cod-pdc          .
           if        w-mcs-cod-con        not  = zero and
                     w-mcs-cod-stc        =    zero
                     go to acc-cod-con-224
           else if   w-mcs-cod-con        not  = zero and
                     w-mcs-cod-stc        not  = zero
                     go to acc-cod-con-226
           else      go to acc-cod-con-100.
       acc-cod-con-224.
      *                      *-----------------------------------------*
      *                      * Se selezionato solo conto               *
      *                      *-----------------------------------------*
           move      w-mcs-cod-con        to   w-tes-cod-con          .
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           move      spaces               to   v-key                  .
           go to     acc-cod-con-400.
       acc-cod-con-226.
      *                      *-----------------------------------------*
      *                      * Se selezionato conto e sottoconto       *
      *                      *-----------------------------------------*
           move      w-mcs-cod-con        to   w-tes-cod-con          .
           move      w-mcs-cod-stc        to   w-tes-cod-stc          .
      *                          *-------------------------------------*
      *                          * Lettura descrizione conto           *
      *                          *-------------------------------------*
           move      w-tes-cod-mas        to   w-let-arc-zcn-mas      .
           move      w-tes-cod-con        to   w-let-arc-zcn-cod      .
           perform   let-arc-zcn-000      thru let-arc-zcn-999        .
      *                          *-------------------------------------*
      *                          * Memorizzazione descrizione          *
      *                          *-------------------------------------*
           move      w-let-arc-zcn-des    to   w-tes-cod-con-des      .
      *                          *-------------------------------------*
      *                          * Visualizzazione conto               *
      *                          *-------------------------------------*
           perform   vis-cod-con-000      thru vis-cod-con-999        .
           perform   vis-des-con-000      thru vis-des-con-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice sottoconto   *
      *                          *-------------------------------------*
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
           move      "DO  "               to   v-key                  .
           go to     acc-cod-con-800.
       acc-cod-con-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura descrizione conto                   *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     move   zero          to   w-let-arc-zcn-mas
           else      move   w-tes-cod-mas to   w-let-arc-zcn-mas      .
           move      w-tes-cod-con        to   w-let-arc-zcn-cod      .
           perform   let-arc-zcn-000      thru let-arc-zcn-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione conto            *
      *                  *---------------------------------------------*
           move      w-let-arc-zcn-des    to   w-tes-cod-con-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione conto           *
      *                  *---------------------------------------------*
           perform   vis-des-con-000      thru vis-des-con-999        .
      *                  *---------------------------------------------*
      *                  * Se valore variato : normalizzazione del     *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-con        =    w-sav-cod-con
                     go to acc-cod-con-460.
           if        w-sav-cod-con        =    zero
                     go to acc-cod-con-460.
      *                      *-----------------------------------------*
      *                      * Normalizzazione sottoconto              *
      *                      *-----------------------------------------*
           if        w-tes-cod-stc        =    zero
                     go to acc-cod-con-460.
           move      zero                 to   w-tes-cod-stc          .
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
       acc-cod-con-460.
      *                  *---------------------------------------------*
      *                  * Se valore impostato a Zero si forza il tas- *
      *                  * to Do, a meno che non si sia in Up          *
      *                  *---------------------------------------------*
           if        w-tes-cod-con        =    zero and
                     v-key                not  = "UP  "
                     move  "DO  "         to   v-key
                     go to acc-cod-con-800.
       acc-cod-con-500.
      *                  *---------------------------------------------*
      *                  * Se conto non esistente : forzatura del tas- *
      *                  * to Do, a meno che non si sia in Up          *
      *                  *---------------------------------------------*
           if        w-let-arc-zcn-flg    not  = spaces and
                     v-key                not  = "UP  "
                     move  "DO  "         to   v-key
                     go to acc-cod-con-800.
       acc-cod-con-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-con-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-con-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-con-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-con-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-con-999.
       acc-cod-con-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice conto               *
      *    *-----------------------------------------------------------*
       vis-cod-con-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-prs-liv-pdc        =    2
                     move  04             to   v-lin
           else      move  05             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-con        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-con-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione conto          *
      *    *-----------------------------------------------------------*
       vis-des-con-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           if        w-prs-liv-pdc        =    2
                     move  04             to   v-lin
           else      move  05             to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-cod-con-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-con-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice sottoconto             *
      *    *-----------------------------------------------------------*
       acc-cod-stc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-stc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-prs-liv-pdc        =    2
                     move  05             to   v-lin
           else      move  06             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-stc        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-stc-999.
       acc-cod-stc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-stc          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-stc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [pdc]                      *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     move   zero          to   w-fnd-arc-pdc-mas
           else      move   w-tes-cod-mas to   w-fnd-arc-pdc-mas      .
           move      w-tes-cod-con        to   w-fnd-arc-pdc-con      .
           perform   fnd-arc-pdc-000      thru fnd-arc-pdc-999        .
           if        w-fnd-arc-pdc-sel    not  = spaces
                     go to acc-cod-stc-100.
           move      w-fnd-arc-pdc-cod    to   w-tes-cod-stc          .
           perform   vis-cod-stc-000      thru vis-cod-stc-999        .
           move      "DO  "               to   v-key                  .
       acc-cod-stc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato a Zero si forza il      *
      *                  * tasto Do, a meno che non si sia in Up       *
      *                  *---------------------------------------------*
           if        w-tes-cod-stc        =    zero and
                     v-key                not  = "UP  "
                     move  "DO  "         to   v-key
                     go to acc-cod-stc-800.
       acc-cod-stc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-stc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-stc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-stc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-stc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-stc-999.
       acc-cod-stc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice sottoconto          *
      *    *-----------------------------------------------------------*
       vis-cod-stc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-prs-liv-pdc        =    2
                     move  05             to   v-lin
           else      move  06             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-stc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-stc-999.
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
      *                  * Descrizione mastro conto o sottoconto       *
      *                  *---------------------------------------------*
           perform   acc-des-mcs-000      thru acc-des-mcs-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo conto                                  *
      *                  *---------------------------------------------*
           perform   acc-tip-cnt-000      thru acc-tip-cnt-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice sezione                              *
      *                  *---------------------------------------------*
           perform   acc-cod-sez-000      thru acc-cod-sez-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Codice classificatore in dare               *
      *                  *---------------------------------------------*
           perform   acc-ccb-dar-000      thru acc-ccb-dar-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Codice classificatore in avere              *
      *                  *---------------------------------------------*
           perform   acc-ccb-ave-000      thru acc-ccb-ave-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-600.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
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
      *              * Descrizione mastro conto o sottoconto           *
      *              *-------------------------------------------------*
           perform   vis-des-mcs-000      thru vis-des-mcs-999        .
      *              *-------------------------------------------------*
      *              * Tipo conto                                      *
      *              *-------------------------------------------------*
           perform   vis-tip-cnt-000      thru vis-tip-cnt-999        .
      *              *-------------------------------------------------*
      *              * Codice sezione                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-sez-000      thru vis-cod-sez-999        .
      *              *-------------------------------------------------*
      *              * Descrizione sezione                             *
      *              *-------------------------------------------------*
           perform   vis-des-sez-000      thru vis-des-sez-999        .
      *              *-------------------------------------------------*
      *              * Codice classificatore in dare                   *
      *              *-------------------------------------------------*
           perform   vis-ccb-dar-000      thru vis-ccb-dar-999        .
      *              *-------------------------------------------------*
      *              * Descrizione classificatore in dare              *
      *              *-------------------------------------------------*
           perform   vis-des-dar-000      thru vis-des-dar-999        .
      *              *-------------------------------------------------*
      *              * Codice classificatore in avere                  *
      *              *-------------------------------------------------*
           perform   vis-ccb-ave-000      thru vis-ccb-ave-999        .
      *              *-------------------------------------------------*
      *              * Descrizione classificatore in avere             *
      *              *-------------------------------------------------*
           perform   vis-des-ave-000      thru vis-des-ave-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
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
           if        w-prs-liv-pdc        =    2
                     move  07             to   v-lin
           else      move  08             to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Descrizione mastro conto o sottoconto           *
      *              *-------------------------------------------------*
           perform   pmt-des-mcs-000      thru pmt-des-mcs-999        .
      *              *-------------------------------------------------*
      *              * Tipo conto                                      *
      *              *-------------------------------------------------*
           perform   pmt-tip-cnt-000      thru pmt-tip-cnt-999        .
      *              *-------------------------------------------------*
      *              * Codice sezione                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-sez-000      thru pmt-cod-sez-999        .
      *              *-------------------------------------------------*
      *              * Classificatori di bilancio                      *
      *              *-------------------------------------------------*
           perform   pmt-tcb-dav-000      thru pmt-tcb-dav-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione mastro conto o sot-  *
      *    *                          toconto                          *
      *    *-----------------------------------------------------------*
       pmt-des-mcs-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo impostazione    *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I" or
                     w-cnt-mfu-tip-fun    =    "M" or
                     w-cnt-mfu-tip-fun    =    "V"
                     go to pmt-des-mcs-200.
       pmt-des-mcs-100.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     pmt-des-mcs-999.
       pmt-des-mcs-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi testata                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del campo trattato   *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        =    "M"
                     go to pmt-des-mcs-210
           else if   w-mcs-cod-trt        =    "C"
                     go to pmt-des-mcs-220
           else      go to pmt-des-mcs-230.
       pmt-des-mcs-210.
      *                  *---------------------------------------------*
      *                  * Se trattamento mastro                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione mastro         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     pmt-des-mcs-999.
       pmt-des-mcs-220.
      *                  *---------------------------------------------*
      *                  * Se trattamento conto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione conto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     pmt-des-mcs-999.
       pmt-des-mcs-230.
      *                  *---------------------------------------------*
      *                  * Se trattamento sottoconto                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione sottoconto     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     pmt-des-mcs-999.
       pmt-des-mcs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo conto                       *
      *    *-----------------------------------------------------------*
       pmt-tip-cnt-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I" and
                     w-cnt-mfu-tip-fun    not  = "M" and
                     w-cnt-mfu-tip-fun    not  = "V"
                     go to pmt-tip-cnt-999.
           if        w-mcs-cod-trt        not  = "S"
                     go to pmt-tip-cnt-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conto                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-cnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice sezione                   *
      *    *-----------------------------------------------------------*
       pmt-cod-sez-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I" and
                     w-cnt-mfu-tip-fun    not  = "M" and
                     w-cnt-mfu-tip-fun    not  = "V"
                     go to pmt-cod-sez-999.
           if        w-mcs-cod-trt        not  = "S"
                     go to pmt-cod-sez-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice sezione             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-sez-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Classificatori di bilancio       *
      *    *-----------------------------------------------------------*
       pmt-tcb-dav-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I" and
                     w-cnt-mfu-tip-fun    not  = "M" and
                     w-cnt-mfu-tip-fun    not  = "V"
                     go to pmt-tcb-dav-999.
           if        w-mcs-cod-trt        not  = "S"
                     go to pmt-tcb-dav-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codici classificatori di bilancio :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Se saldo in Dare         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Se saldo in Avere        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tcb-dav-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I" and
                     w-cnt-mfu-tip-fun    not  = "M" and
                     w-cnt-mfu-tip-fun    not  = "V"
                     go to pmt-cod-mne-999.
           if        w-mcs-cod-trt        not  = "S"
                     go to pmt-cod-mne-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mnemonico           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione mastro conto o   *
      *    * sottoconto                                                *
      *    *-----------------------------------------------------------*
       acc-des-mcs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-mcs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-mcs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-mcs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-mcs-999.
       acc-des-mcs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-mcs (1)      .
       acc-des-mcs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-des-mcs (1)    =    spaces
                     go to acc-des-mcs-100.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-mcs (1)
                    (01 : 01)             =    spaces
                     go to acc-des-mcs-100.
       acc-des-mcs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-mcs (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-mcs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-mcs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-mcs-100.
       acc-des-mcs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione mastro conto  *
      *    *                                 o sottoconto              *
      *    *-----------------------------------------------------------*
       vis-des-mcs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-mcs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mcs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo conto                   *
      *    *-----------------------------------------------------------*
       acc-tip-cnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to acc-tip-cnt-999.
       acc-tip-cnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cnt-lun    to   v-car                  .
           move      w-exp-tip-cnt-num    to   v-ldt                  .
           move      "PE#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-cnt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
      *
           if        w-tes-tip-cnt (1)    =    "P"
                     move  01             to   v-num
           else if   w-tes-tip-cnt (1)    =    "E"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-cnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-cnt-999.
       acc-tip-cnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "P"            to   w-tes-tip-cnt (1)
           else if   v-num                =    02
                     move  "E"            to   w-tes-tip-cnt (1)
           else      move  spaces         to   w-tes-tip-cnt (1)      .
       acc-tip-cnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-cnt (1)    =    spaces
                     go to acc-tip-cnt-100.
       acc-tip-cnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-cnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-cnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-cnt-100.
       acc-tip-cnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo conto                *
      *    *-----------------------------------------------------------*
       vis-tip-cnt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cnt-lun    to   v-car                  .
           move      w-exp-tip-cnt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-cnt-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
      *
           if        w-tes-tip-cnt (1)    =    "P"
                     move  01             to   v-num
           else if   w-tes-tip-cnt (1)    =    "E"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-cnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice sezione               *
      *    *-----------------------------------------------------------*
       acc-cod-sez-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to acc-cod-sez-999.
       acc-cod-sez-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-sez (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-sez-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-sez-999.
       acc-cod-sez-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-sez (1)      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-sez-300.
      *                  *---------------------------------------------*
      *                  * Find su tabella [zsz]                       *
      *                  *---------------------------------------------*
           perform   fnd-arc-zsz-000      thru fnd-arc-zsz-999        .
           if        w-fnd-arc-zsz-sel    not  = spaces
                     go to acc-cod-sez-100.
           move      w-fnd-arc-zsz-cod    to   w-tes-cod-sez (1)      .
           perform   vis-cod-sez-000      thru vis-cod-sez-999        .
           move      spaces               to   v-key                  .
           go to     acc-cod-sez-400.
       acc-cod-sez-300.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-cod-sez-400.
           perform   ins-arc-zsz-000      thru ins-arc-zsz-999        .
           go to     acc-cod-sez-100.
       acc-cod-sez-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zsz]                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-sez (1)    to   w-let-arc-zsz-cod      .
           perform   let-arc-zsz-000      thru let-arc-zsz-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zsz-des    to   w-tes-cod-sez-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-sez-000      thru vis-des-sez-999        .
      *                  *---------------------------------------------*
      *                  * Se anagrafica non esistente : reimpostaz.   *
      *                  *---------------------------------------------*
           if        w-let-arc-zsz-flg    not  = spaces
                     go to acc-cod-sez-100.
       acc-cod-sez-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-sez-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-sez-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-sez-100.
       acc-cod-sez-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice sezione            *
      *    *-----------------------------------------------------------*
       vis-cod-sez-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-sez (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-sez-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sezione       *
      *    *-----------------------------------------------------------*
       vis-des-sez-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-cod-sez-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-sez-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice classificatore - dare *
      *    *-----------------------------------------------------------*
       acc-ccb-dar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to acc-ccb-dar-999.
       acc-ccb-dar-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Digitare un codice nella forma : X99999, dove X in
      -              "dica il tipo classificatore di"
                                          to   v-nt1                  .
           move      "bilancio (A=Attivita', P=Passivita', O=Conti d'ord
      -              "ine ecc.) e 99999 il codice."
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     move  spaces         to   v-alf
           else      move  w-tes-ccb-dar (1)
                                          to   v-alf                  .
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
                     go to acc-ccb-dar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ccb-dar-999.
       acc-ccb-dar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-ccb-dar (1)      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-ccb-dar-300.
      *                  *---------------------------------------------*
      *                  * Find su archivio [cdb]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-cdb-000      thru fnd-arc-cdb-999        .
           if        w-fnd-arc-cdb-sel    not  = spaces
                     go to acc-ccb-dar-100.
           move      w-fnd-arc-cdb-cod    to   w-tes-ccb-dar (1)      .
           perform   vis-ccb-dar-000      thru vis-ccb-dar-999        .
           move      spaces               to   v-key                  .
           go to     acc-ccb-dar-400.
       acc-ccb-dar-300.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-ccb-dar-400.
           perform   ins-arc-cdb-000      thru ins-arc-cdb-999        .
           go to     acc-ccb-dar-100.
       acc-ccb-dar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     go to acc-ccb-dar-420.
           if        w-cdb-tip-cdb        not  = "A"  and
                     w-cdb-tip-cdb        not  = "P"  and
                     w-cdb-tip-cdb        not  = "O"  and
                     w-cdb-tip-cdb        not  = "C"  and
                     w-cdb-tip-cdb        not  = "R"
                     go to acc-ccb-dar-100.
       acc-ccb-dar-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cdb]                      *
      *                  *---------------------------------------------*
           move      w-cdb-tip-cdb        to   w-let-arc-cdb-tip      .
           move      w-cdb-cod-rgr        to   w-let-arc-cdb-cod      .
           perform   let-arc-cdb-000      thru let-arc-cdb-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-cdb-des    to   w-tes-ccb-dar-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-dar-000      thru vis-des-dar-999        .
      *                  *---------------------------------------------*
      *                  * Se anagrafica non esistente : reimpostaz.   *
      *                  *---------------------------------------------*
           if        w-let-arc-cdb-flg    not  = spaces
                     go to acc-ccb-dar-100.
      *                  *---------------------------------------------*
      *                  * Controllo che il codice impostato non sia   *
      *                  * un raggruppamento                           *
      *                  *---------------------------------------------*
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     go to  acc-ccb-dar-600.
           if        w-cdb-sgr-cdb        not  = zero
                     go to  acc-ccb-dar-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Attenzione : il codice classicatore di bilancio no
      -              "n puo' essere un raggruppa-"
                                          to   v-nt1                  .
           move      "             mento !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ccb-dar-100.
       acc-ccb-dar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ccb-dar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ccb-dar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ccb-dar-100.
       acc-ccb-dar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice classific. - dare  *
      *    *-----------------------------------------------------------*
       vis-ccb-dar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     move  spaces         to   v-alf
           else      move  w-tes-ccb-dar (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ccb-dar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione class. - dare *
      *    *-----------------------------------------------------------*
       vis-des-dar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-tes-ccb-dar-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-dar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice classific. - avere    *
      *    *-----------------------------------------------------------*
       acc-ccb-ave-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to acc-ccb-ave-999.
       acc-ccb-ave-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Digitare un codice nella forma : X99999, dove X in
      -              "dica il tipo classificatore di"
                                          to   v-nt1                  .
           move      "bilancio (A=Attivita', P=Passivita', O=Conti d'ord
      -              "ine ecc.) e 99999 il codice."
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     move  spaces         to   v-alf
           else      move  w-tes-ccb-ave (1)
                                          to   v-alf                  .
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
                     go to acc-ccb-ave-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ccb-ave-999.
       acc-ccb-ave-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-ccb-ave (1)      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-ccb-ave-300.
      *                  *---------------------------------------------*
      *                  * Find su archivio [cdb]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-cdb-000      thru fnd-arc-cdb-999        .
           if        w-fnd-arc-cdb-sel    not  = spaces
                     go to acc-ccb-ave-100.
           move      w-fnd-arc-cdb-cod    to   w-tes-ccb-ave (1)      .
           perform   vis-ccb-ave-000      thru vis-ccb-ave-999        .
           move      spaces               to   v-key                  .
           go to     acc-ccb-ave-400.
       acc-ccb-ave-300.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-ccb-ave-400.
           perform   ins-arc-cdb-000      thru ins-arc-cdb-999        .
           go to     acc-ccb-ave-100.
       acc-ccb-ave-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     go to  acc-ccb-ave-420.
           if        w-cdb-tip-cdb        not  = "A"  and
                     w-cdb-tip-cdb        not  = "P"  and
                     w-cdb-tip-cdb        not  = "O"  and
                     w-cdb-tip-cdb        not  = "C"  and
                     w-cdb-tip-cdb        not  = "R"
                     go to acc-ccb-ave-100.
       acc-ccb-ave-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cdb]                      *
      *                  *---------------------------------------------*
           move      w-cdb-tip-cdb        to   w-let-arc-cdb-tip      .
           move      w-cdb-cod-rgr        to   w-let-arc-cdb-cod      .
           perform   let-arc-cdb-000      thru let-arc-cdb-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-cdb-des    to   w-tes-ccb-ave-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-ave-000      thru vis-des-ave-999        .
      *                  *---------------------------------------------*
      *                  * Se anagrafica non esistente : reimpostaz.   *
      *                  *---------------------------------------------*
           if        w-let-arc-cdb-flg    not  = spaces
                     go to acc-ccb-ave-100.
      *                  *---------------------------------------------*
      *                  * Controllo che il codice impostato non sia   *
      *                  * un raggruppamento                           *
      *                  *---------------------------------------------*
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     go to  acc-ccb-ave-600.
           if        w-cdb-sgr-cdb        not  = zero
                     go to  acc-ccb-ave-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Attenzione : il codice classicatore di bilancio no
      -              "n puo' essere un raggruppa-"
                                          to   v-nt1                  .
           move      "             mento !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ccb-ave-100.
       acc-ccb-ave-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ccb-ave-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ccb-ave-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ccb-ave-100.
       acc-ccb-ave-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice classific. - avere *
      *    *-----------------------------------------------------------*
       vis-ccb-ave-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           if        w-cdb-tip-cdb        =    spaces
                     move  spaces         to   v-alf
           else      move  w-tes-ccb-ave (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ccb-ave-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione class. - avere*
      *    *-----------------------------------------------------------*
       vis-des-ave-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-tes-ccb-ave-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ave-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice mnemonico             *
      *    *-----------------------------------------------------------*
       acc-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to acc-cod-mne-999.
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
       acc-cod-mne-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-mne (1)      .
       acc-cod-mne-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mne (1)    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-mne-100.
       acc-cod-mne-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mne-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mne-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mne-100.
       acc-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice mnemonico          *
      *    *-----------------------------------------------------------*
       vis-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione codice in corso di trattamento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del livello pdc      *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to cnt-tdo-key-200
           else      go to cnt-tdo-key-400.
       cnt-tdo-key-200.
      *                  *---------------------------------------------*
      *                  * Piano dei conti a due livelli               *
      *                  *---------------------------------------------*
           if        w-tes-cod-stc        not  = zero
                     move  "S"            to   w-mcs-cod-trt
           else      move  "C"            to   w-mcs-cod-trt          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-key-999.
       cnt-tdo-key-400.
      *                  *---------------------------------------------*
      *                  * Piano dei conti a tre livelli               *
      *                  *---------------------------------------------*
           if        w-tes-cod-stc        not  = zero and
                     w-tes-cod-con        not  = zero
                     move  "S"            to   w-mcs-cod-trt
           else if   w-tes-cod-con        not  = zero
                     move  "C"            to   w-mcs-cod-trt
           else      move  "M"            to   w-mcs-cod-trt          .
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
      *              * Deviazione in funzione del livello pdc          *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to cnt-key-vuo-200
           else      go to cnt-key-vuo-400.
       cnt-key-vuo-200.
      *                  *---------------------------------------------*
      *                  * Se piano dei conti a due livelli            *
      *                  *---------------------------------------------*
           if        w-tes-cod-con        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg      .
           go to cnt-key-vuo-999.
       cnt-key-vuo-400.
      *                  *---------------------------------------------*
      *                  * Se piano dei conti a tre livelli            *
      *                  *---------------------------------------------*
           if        w-tes-cod-mas        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg      .
           go to cnt-key-vuo-999.
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
      *              * Controllo su Descrizione                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che esista il valore                        *
      *                  *---------------------------------------------*
           if        w-tes-des-mcs (1)    not  = spaces
                     go to cnt-tdo-nok-120.
           move      "Manca la descrizione !                            
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-120.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo conto                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se elemento in corso di trattamento :  *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Che esista il valore                        *
      *                  *---------------------------------------------*
           if        w-tes-tip-cnt (1)    not  = spaces
                     go to cnt-tdo-nok-140.
           move      "Manca il tipo conto !                             
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-140.
      *              *-------------------------------------------------*
      *              * Controllo su Codice sezione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se elemento in corso di trattamento :  *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Nessun controllo                            *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-160.
       cnt-tdo-nok-160.
      *              *-------------------------------------------------*
      *              * Controllo su Codice classificatore in dare      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se elemento in corso di trattamento :  *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to cnt-tdo-nok-180.
      *                  *---------------------------------------------*
      *                  * Nessun controllo                            *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-180.
       cnt-tdo-nok-180.
      *              *-------------------------------------------------*
      *              * Controllo su Codice classificatore in avere     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se elemento in corso di trattamento :  *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Nessun controllo                            *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-200.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice mnemonico                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se elemento in corso di trattamento :  *
      *                  * sottoconto                                  *
      *                  *---------------------------------------------*
           if        w-mcs-cod-trt        not  = "S"
                     go to cnt-tdo-nok-800.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mne (1)    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        =    spaces
                     go to cnt-tdo-nok-800.
           move      "Mnemonico con spazi non accettabile !             
      -              "               "    to   w-err-box-err-msg      .
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
           move      zero                 to   w-tes-cod-mas          .
           move      spaces               to   w-tes-cod-mas-des      .
           move      zero                 to   w-tes-cod-con          .
           move      spaces               to   w-tes-cod-con-des      .
           move      zero                 to   w-tes-cod-stc          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-mcs (1)      .
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-tip-cnt (1)      .
           move      zero                 to   w-tes-cod-sez (1)      .
           move      spaces               to   w-tes-cod-sez-des (1)  .
           move      spaces               to   w-tes-ccb-dar (1)      .
           move      spaces               to   w-tes-ccb-dar-des (1)  .
           move      spaces               to   w-tes-ccb-ave (1)      .
           move      spaces               to   w-tes-ccb-ave-des (1)  .
           move      spaces               to   w-tes-key-ref (1)      .
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
      *              * Deviazione in funzione del campo da trattare    *
      *              *-------------------------------------------------*
           if        w-mcs-cod-trt        =    "M"
                     go to rou-let-reg-100
           else if   w-mcs-cod-trt        =    "C"
                     go to rou-let-reg-200
           else      go to rou-let-reg-300.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se trattamento codice mastro                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODMAS    "         to   f-key                  .
           move      w-tes-cod-mas        to   rf-zma-cod-mas         .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-120.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-110.
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
           go to     rou-let-reg-999.
       rou-let-reg-110.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-120.
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
      *                          * record [zma]                        *
      *                          *-------------------------------------*
           move      rf-zma-des-mas       to   w-tes-des-mcs (1)      .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Test per visualizzazione                *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Se trattamento codice conto                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCON    "         to   f-key                  .
           move      w-tes-cod-mas        to   rf-zcn-cod-mas         .
           move      w-tes-cod-con        to   rf-zcn-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-220.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-210.
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
           go to     rou-let-reg-999.
       rou-let-reg-210.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-220.
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
      *                          * record [zcn]                        *
      *                          *-------------------------------------*
           move      rf-zcn-des-con       to   w-tes-des-mcs (1)      .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Test per visualizzazione                *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Se trattamento codice sottoconto                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC    "         to   f-key                  .
           move      w-tes-cod-mas        to   w-mcs-cod-mas          .
           move      w-tes-cod-con        to   w-mcs-cod-con          .
           move      w-tes-cod-stc        to   w-mcs-cod-stc          .
           move      w-mcs-cod-pdc        to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-320.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-310.
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
           go to     rou-let-reg-999.
       rou-let-reg-310.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-320.
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
      *                          * record [pdc]                        *
      *                          *-------------------------------------*
           move      rf-pdc-des-pdc       to   w-tes-des-mcs (1)      .
           move      rf-pdc-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-pdc-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-pdc-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-pdc-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-pdc-des-key       to   w-tes-des-key (1)      .
           move      rf-pdc-tip-cnt       to   w-tes-tip-cnt (1)      .
           move      rf-pdc-cod-sez       to   w-tes-cod-sez (1)      .
           move      rf-pdc-tcb-dar       to   w-cdb-tip-cdb          .
           move      rf-pdc-ccb-dar       to   w-cdb-cod-rgr          .
           move      w-cdb-cod-cdb        to   w-tes-ccb-dar (1)      .
           move      rf-pdc-tcb-ave       to   w-cdb-tip-cdb          .
           move      rf-pdc-ccb-ave       to   w-cdb-cod-rgr          .
           move      w-cdb-cod-cdb        to   w-tes-ccb-ave (1)      .
           move      rf-pdc-key-ref       to   w-tes-key-ref (1)      .
           move      rf-pdc-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [pdc]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura tabella [cgesez]        *
      *                              *---------------------------------*
           move      w-tes-cod-sez (1)    to   w-let-arc-zsz-cod      .
           perform   let-arc-zsz-000      thru let-arc-zsz-999        .
           move      w-let-arc-zsz-des    to   w-tes-cod-sez-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [cdb]          *
      *                              *---------------------------------*
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           move      w-cdb-tip-cdb        to   w-let-arc-cdb-tip      .
           move      w-cdb-cod-rgr        to   w-let-arc-cdb-cod      .
           perform   let-arc-cdb-000      thru let-arc-cdb-999        .
           move      w-let-arc-cdb-des    to   w-tes-ccb-dar-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [cdb]          *
      *                              *---------------------------------*
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           move      w-cdb-tip-cdb        to   w-let-arc-cdb-tip      .
           move      w-cdb-cod-rgr        to   w-let-arc-cdb-cod      .
           perform   let-arc-cdb-000      thru let-arc-cdb-999        .
           move      w-let-arc-cdb-des    to   w-tes-ccb-ave-des (1)  .
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
      *              * Visualizzazione prompts testata                 *
      *              *-------------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
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
      *              * Visualizzazione prompts testata                 *
      *              *-------------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
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
      *              * Visualizzazione prompts testata                 *
      *              *-------------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
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
       pre-snx-del-100.
      *              *-------------------------------------------------*
      *              * Controllo se esistono schede saldi per il sot-  *
      *              * toconto                                         *
      *              *-------------------------------------------------*
           move      w-tes-cod-mas        to   w-mcs-cod-mas          .
           move      w-tes-cod-con        to   w-mcs-cod-con          .
           move      w-tes-cod-stc        to   w-mcs-cod-stc          .
           move      w-mcs-cod-pdc        to   w-det-snx-mgs-cod      .
           perform   det-snx-mgs-000      thru det-snx-mgs-999        .
       pre-snx-del-140.
      *                  *---------------------------------------------*
      *                  * Se contatori a zero : ad uscita             *
      *                  *---------------------------------------------*
           if        w-det-snx-mgs-csc    =    zero and
                     w-det-snx-mgs-cvc    =    zero and
                     w-det-snx-mgs-csp    =    zero and
                     w-det-snx-mgs-cvp    =    zero
                     go to pre-snx-del-900.
       pre-snx-del-160.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   box-msg-del-000      thru box-msg-del-999        .
      *                  *---------------------------------------------*
      *                  * Disabilitazione della cancellazione         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-snx-del      .
       pre-snx-del-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-snx-del-999.
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
      *              *-------------------------------------------------*
      *              * Aggiornamento dei valori di defaults generali : *
      *              * - Codice e descrizione mastro                   *
      *              * - Codice e descrizione conto                    *
      *              *-------------------------------------------------*
           move      w-tes-cod-mas        to   w-def-cod-mas          .
           move      w-tes-cod-mas-des    to   w-def-cod-mas-des      .
           move      w-tes-cod-con        to   w-def-cod-con          .
           move      w-tes-cod-con-des    to   w-def-cod-con-des      .
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
      *              * Deviazione in funzione del campo da trattare    *
      *              *-------------------------------------------------*
           if        w-mcs-cod-trt        =    "M"
                     go to scr-mov-fil-100
           else if   w-mcs-cod-trt        =    "C"
                     go to scr-mov-fil-200
           else      go to scr-mov-fil-300.
       scr-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Trattamento file [zma]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-120.
      *                      *-----------------------------------------*
      *                      * Write record [zma]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-mas-000      thru wrt-rec-mas-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-120.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zma]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-mas-000      thru rew-rec-mas-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Trattamento file [zcn]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-220.
      *                      *-----------------------------------------*
      *                      * Write record [zcn]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-con-000      thru wrt-rec-con-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-220.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zcn]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-con-000      thru rew-rec-con-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-300.
      *              *-------------------------------------------------*
      *              * Trattamento file [pdc]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-320.
      *                      *-----------------------------------------*
      *                      * Write record [pdc]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-pdc-000      thru wrt-rec-pdc-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-320.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [pdc]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-pdc-000      thru rew-rec-pdc-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del campo da trattare    *
      *              *-------------------------------------------------*
           if        w-mcs-cod-trt        =    "M"
                     go to del-mov-fil-100
           else if   w-mcs-cod-trt        =    "C"
                     go to del-mov-fil-200
           else      go to del-mov-fil-300.
       del-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Delete record [zma]                             *
      *              *-------------------------------------------------*
           perform   del-rec-mas-000      thru del-rec-mas-999        .
           go to     del-mov-fil-999.
       del-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Delete record [zcn]                             *
      *              *-------------------------------------------------*
           perform   del-rec-con-000      thru del-rec-con-999        .
           go to     del-mov-fil-999.
       del-mov-fil-300.
      *              *-------------------------------------------------*
      *              * Delete record [pdc]                             *
      *              *-------------------------------------------------*
           perform   del-rec-pdc-000      thru del-rec-pdc-999        .
           go to     del-mov-fil-999.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zma]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mas-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-mas        to   rf-zma-cod-mas         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-mcs (1)    to   rf-zma-des-mas         .
           move      w-tes-des-key (1)    to   rf-zma-des-key         .
           move      spaces               to   rf-zma-mne-mas         .
           move      spaces               to   rf-zma-alx-exp         .
       cmp-rec-mas-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zcn]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-mas        to   rf-zcn-cod-mas         .
           move      w-tes-cod-con        to   rf-zcn-cod-con         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-mcs (1)    to   rf-zcn-des-con         .
           move      w-tes-des-key (1)    to   rf-zcn-des-key         .
           move      spaces               to   rf-zcn-mne-con         .
           move      spaces               to   rf-zcn-alx-exp         .
       cmp-rec-con-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdc]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-mas        to   w-mcs-cod-mas          .
           move      w-tes-cod-con        to   w-mcs-cod-con          .
           move      w-tes-cod-stc        to   w-mcs-cod-stc          .
           move      w-mcs-cod-pdc        to   rf-pdc-cod-pdc         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-pdc-ide-dat         .
           move      s-ute                to   rf-pdc-ide-ute         .
           move      s-fas                to   rf-pdc-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-pdc-cod-mne         .
           move      w-tes-des-key (1)    to   rf-pdc-des-key         .
           move      w-tes-des-mcs (1)    to   rf-pdc-des-pdc         .
           move      w-tes-tip-cnt (1)    to   rf-pdc-tip-cnt         .
           move      w-tes-cod-sez (1)    to   rf-pdc-cod-sez         .
           move      w-tes-ccb-dar (1)    to   w-cdb-cod-cdb          .
           move      w-cdb-tip-cdb        to   rf-pdc-tcb-dar         .
           move      w-cdb-cod-rgr        to   rf-pdc-ccb-dar         .
           move      w-tes-ccb-ave (1)    to   w-cdb-cod-cdb          .
           move      w-cdb-tip-cdb        to   rf-pdc-tcb-ave         .
           move      w-cdb-cod-rgr        to   rf-pdc-ccb-ave         .
           move      w-tes-key-ref (1)    to   rf-pdc-key-ref         .
           move      w-tes-alx-exp (1)    to   rf-pdc-alx-exp         .
       cmp-rec-pdc-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zma]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mas-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mas-000      thru cmp-rec-mas-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
       wrt-rec-mas-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zcn]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-con-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-con-000      thru cmp-rec-con-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
       wrt-rec-con-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdc]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-pdc-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdc-000      thru cmp-rec-pdc-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       wrt-rec-pdc-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zma]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-mas-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mas-000      thru cmp-rec-mas-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
       rew-rec-mas-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zcn]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-con-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-con-000      thru cmp-rec-con-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
       rew-rec-con-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [pdc]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-pdc-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdc-000      thru cmp-rec-pdc-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       rew-rec-pdc-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zma]                                *
      *    *-----------------------------------------------------------*
       del-rec-mas-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mas-000      thru cmp-rec-mas-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
       del-rec-mas-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zcn]                                *
      *    *-----------------------------------------------------------*
       del-rec-con-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-con-000      thru cmp-rec-con-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
       del-rec-con-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdc]                                *
      *    *-----------------------------------------------------------*
       del-rec-pdc-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdc-000      thru cmp-rec-pdc-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       del-rec-pdc-999.
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
      *    * Box per messaggio di errore in fase di cancellazione      *
      *    *-----------------------------------------------------------*
       box-msg-del-000.
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
           move      06                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-del-200.
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "                         A T T E N Z I O N E      
      -              "                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "Sono state rilevate le seguenti schede saldo per i
      -              "l sottoconto in    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "corso di cancellazione :                          
      -              "                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "Scheda saldo anno precedente [ ]  con valori [ ]  
      -              "                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "Scheda saldo anno in corso   [ ]  con valori [ ]  
      -              "                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "Prima di eseguire la cancellazione e' necessario r
      -              "imuovere o control-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "lare suddette schede con l'apposito programma (cge
      -              "950).              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-del-300.
      *              *-------------------------------------------------*
      *              * Indicatori desunti da contatori                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Schede anno precedente vuote                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
      *
           if        w-det-snx-mgs-csp    =    zero
                     move  spaces         to   v-alf
           else      move  "X"            to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Schede anno precedente con valori           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      52                   to   v-pos                  .
      *
           if        w-det-snx-mgs-cvp    =    zero
                     move  spaces         to   v-alf
           else      move  "X"            to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Schede anno in corso vuote                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
      *
           if        w-det-snx-mgs-csc    =    zero
                     move  spaces         to   v-alf
           else      move  "X"            to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Schede anno in corso con valori             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      52                   to   v-pos                  .
      *
           if        w-det-snx-mgs-cvc    =    zero
                     move  spaces         to   v-alf
           else      move  "X"            to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-del-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
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
           move      19                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-del-999.
           exit.

      *    *===========================================================*
      *    * Find su tabella [zma]                                     *
      *    *-----------------------------------------------------------*
       fnd-arc-zma-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2011"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-zma-999.
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
           move      "pgm/cge/prg/obj/pcge2011"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-pdc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-zma-sel
                     move  s-num          to   w-fnd-arc-zma-pdc
           else      move  "#"            to   w-fnd-arc-zma-sel      .
       fnd-arc-zma-999.
           exit.

      *    *===========================================================*
      *    * Find su tabella [zcn]                                     *
      *    *-----------------------------------------------------------*
       fnd-arc-zcn-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2012"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-zcn-999.
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
      *              * Preparazione variabile di i.p.c. per codice ma- *
      *              * stro da interrogare                             *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-pdc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      w-fnd-arc-zcn-pdc    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2012"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-pdc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-zcn-sel
                     move  s-num          to   w-fnd-arc-zcn-pdc
           else      move  "#"            to   w-fnd-arc-zcn-sel      .
       fnd-arc-zcn-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [pdc]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-pdc-999.
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
      *              * Preparazione variabile di i.p.c. per codice ma- *
      *              * stro da interrogare                             *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-mas"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      w-fnd-arc-pdc-mas    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice     *
      *              * conto da interrogare                            *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-con"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      w-fnd-arc-pdc-con    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select pdc"         to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-pdc-sel
                     move  s-num          to   w-fnd-arc-pdc-cod
           else      move  "#"            to   w-fnd-arc-pdc-sel      .
       fnd-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Find su tabella [zsz]                                     *
      *    *-----------------------------------------------------------*
       fnd-arc-zsz-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-zsz-sel
                     go to  fnd-arc-zsz-999.
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
           move      "pgm/cge/prg/obj/pcge2110"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-sez"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-zsz-sel
                     move  s-num          to   w-fnd-arc-zsz-cod
           else      move  "#"            to   w-fnd-arc-zsz-sel      .
       fnd-arc-zsz-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [cdb]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-cdb-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-cdb-sel
                     go to  fnd-arc-cdb-999.
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
           move      "pgm/cge/prg/obj/pcge2210"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "ccb-ext"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-cdb-sel
                     move  s-alf          to   w-fnd-arc-cdb-cod
           else      move  "#"            to   w-fnd-arc-cdb-sel      .
       fnd-arc-cdb-999.
           exit.

      *    *===========================================================*
      *    * Insr su tabella [zsz]                                     *
      *    *-----------------------------------------------------------*
       ins-arc-zsz-000.
      *              *-------------------------------------------------*
      *              * Test se programma di gestione file gia' attivo  *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2100"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  ins-arc-zsz-999.
      *                  *---------------------------------------------*
      *                  * Richiamo programma di gestione archivio     *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2100"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       ins-arc-zsz-999.
           exit.

      *    *===========================================================*
      *    * Insr su archivio [cdb]                                    *
      *    *-----------------------------------------------------------*
       ins-arc-cdb-000.
      *              *-------------------------------------------------*
      *              * Test se programma di gestione file gia' attivo  *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2200"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  ins-arc-cdb-999.
      *                  *---------------------------------------------*
      *                  * Richiamo programma di gestione archivio     *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2200"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       ins-arc-cdb-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura tabella [zma]                          *
      *    *-----------------------------------------------------------*
       let-arc-zma-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zma-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sezione a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zma-cod    =    zero
                     go to let-arc-zma-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODMAS    "         to   f-key                  .
           move      w-let-arc-zma-cod    to   rf-zma-cod-mas         .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zma-400.
       let-arc-zma-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zma-des-mas       to   w-let-arc-zma-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zma-999.
       let-arc-zma-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zma-flg      .
           move      spaces               to   w-let-arc-zma-des      .
           go to     let-arc-zma-999.
       let-arc-zma-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zma-des      .
       let-arc-zma-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura tabella [zcn]                          *
      *    *-----------------------------------------------------------*
       let-arc-zcn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcn-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sezione a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zcn-cod    =    zero
                     go to let-arc-zcn-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCON    "         to   f-key                  .
           move      w-let-arc-zcn-mas    to   rf-zcn-cod-mas         .
           move      w-let-arc-zcn-cod    to   rf-zcn-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcn-400.
       let-arc-zcn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcn-des-con       to   w-let-arc-zcn-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcn-999.
       let-arc-zcn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcn-flg      .
           move      spaces               to   w-let-arc-zcn-des      .
           go to     let-arc-zcn-999.
       let-arc-zcn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcn-des      .
       let-arc-zcn-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura tabella [zsz]                          *
      *    *-----------------------------------------------------------*
       let-arc-zsz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zsz-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sezione a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zsz-cod    =    zero
                     go to let-arc-zsz-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSEZ    "         to   f-key                  .
           move      w-let-arc-zsz-cod    to   rf-zsz-cod-sez         .
           move      "pgm/cge/fls/ioc/obj/iofzsz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsz                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zsz-400.
       let-arc-zsz-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zsz-des-sez       to   w-let-arc-zsz-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zsz-999.
       let-arc-zsz-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zsz-flg      .
           move      all   "."            to   w-let-arc-zsz-des      .
           go to     let-arc-zsz-999.
       let-arc-zsz-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zsz-des      .
       let-arc-zsz-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cdb]                         *
      *    *-----------------------------------------------------------*
       let-arc-cdb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cdb-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sezione a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cdb-tip    =    spaces or
                     w-let-arc-cdb-tip    =    zero
                     go to let-arc-cdb-500.
           if        w-let-arc-cdb-cod    =    zero
                     go to let-arc-cdb-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCDB    "         to   f-key                  .
           move      w-let-arc-cdb-tip    to   rf-cdb-tip-cdb         .
           move      w-let-arc-cdb-cod    to   rf-cdb-cod-cdb         .
           move      "pgm/cge/fls/ioc/obj/iofcdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdb                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cdb-400.
       let-arc-cdb-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cdb-des-cdb       to   w-let-arc-cdb-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cdb-999.
       let-arc-cdb-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cdb-flg      .
           move      all   "."            to   w-let-arc-cdb-des      .
           go to     let-arc-cdb-999.
       let-arc-cdb-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cdb-des      .
       let-arc-cdb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se presenti schede saldo per il sottoconto *
      *    *-----------------------------------------------------------*
       det-snx-mgs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snx-mgs-csc      .
           move      zero                 to   w-det-snx-mgs-cvc      .
           move      zero                 to   w-det-snx-mgs-csp      .
           move      zero                 to   w-det-snx-mgs-cvp      .
      *              *-------------------------------------------------*
      *              * Preparazione anni di esercizio                  *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-det-snx-mgs-sac      .
           move      w-det-snx-mgs-sac    to   w-det-snx-mgs-sap      .
           subtract  1                    from w-det-snx-mgs-sap      .
       det-snx-mgs-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione file [mgs]                      *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Lettura file [mgs] per anno in corso            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-snx-mgs-sac    to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-det-snx-mgs-cod    to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snx-mgs-300.
       det-snx-mgs-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore schede anno in corso       *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snx-mgs-csc      .
      *              *-------------------------------------------------*
      *              * Test su contenuto scheda                        *
      *              *-------------------------------------------------*
           if        rf-mgs-sdo-ini       =    zero and
                     rf-mgs-dar-mes (01)  =    zero and
                     rf-mgs-ave-mes (01)  =    zero and
                     rf-mgs-imp-ret (01)  =    zero and
                     rf-mgs-dar-mes (02)  =    zero and
                     rf-mgs-ave-mes (02)  =    zero and
                     rf-mgs-imp-ret (02)  =    zero and
                     rf-mgs-dar-mes (03)  =    zero and
                     rf-mgs-ave-mes (03)  =    zero and
                     rf-mgs-imp-ret (03)  =    zero and
                     rf-mgs-dar-mes (04)  =    zero and
                     rf-mgs-ave-mes (04)  =    zero and
                     rf-mgs-imp-ret (04)  =    zero and
                     rf-mgs-dar-mes (05)  =    zero and
                     rf-mgs-ave-mes (05)  =    zero and
                     rf-mgs-imp-ret (05)  =    zero and
                     rf-mgs-dar-mes (06)  =    zero and
                     rf-mgs-ave-mes (06)  =    zero and
                     rf-mgs-imp-ret (06)  =    zero and
                     rf-mgs-dar-mes (07)  =    zero and
                     rf-mgs-ave-mes (07)  =    zero and
                     rf-mgs-imp-ret (07)  =    zero and
                     rf-mgs-dar-mes (08)  =    zero and
                     rf-mgs-ave-mes (08)  =    zero and
                     rf-mgs-imp-ret (08)  =    zero and
                     rf-mgs-dar-mes (09)  =    zero and
                     rf-mgs-ave-mes (09)  =    zero and
                     rf-mgs-imp-ret (09)  =    zero and
                     rf-mgs-dar-mes (10)  =    zero and
                     rf-mgs-ave-mes (10)  =    zero and
                     rf-mgs-imp-ret (10)  =    zero and
                     rf-mgs-dar-mes (11)  =    zero and
                     rf-mgs-ave-mes (11)  =    zero and
                     rf-mgs-imp-ret (11)  =    zero and
                     rf-mgs-dar-mes (12)  =    zero and
                     rf-mgs-ave-mes (12)  =    zero and
                     rf-mgs-imp-ret (12)  =    zero and
                     rf-mgs-dar-bil       =    zero and
                     rf-mgs-ave-bil       =    zero
                     go to det-snx-mgs-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore schede anno in corso con   *
      *              * valori                                          *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snx-mgs-cvc      .
       det-snx-mgs-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione file [mgs]                      *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Lettura file [mgs] per anno precedente          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-snx-mgs-sap    to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-det-snx-mgs-cod    to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snx-mgs-800.
       det-snx-mgs-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore schede anno precedente     *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snx-mgs-csp      .
      *              *-------------------------------------------------*
      *              * Test su contenuto scheda                        *
      *              *-------------------------------------------------*
           if        rf-mgs-sdo-ini       =    zero and
                     rf-mgs-dar-mes (01)  =    zero and
                     rf-mgs-ave-mes (01)  =    zero and
                     rf-mgs-imp-ret (01)  =    zero and
                     rf-mgs-dar-mes (02)  =    zero and
                     rf-mgs-ave-mes (02)  =    zero and
                     rf-mgs-imp-ret (02)  =    zero and
                     rf-mgs-dar-mes (03)  =    zero and
                     rf-mgs-ave-mes (03)  =    zero and
                     rf-mgs-imp-ret (03)  =    zero and
                     rf-mgs-dar-mes (04)  =    zero and
                     rf-mgs-ave-mes (04)  =    zero and
                     rf-mgs-imp-ret (04)  =    zero and
                     rf-mgs-dar-mes (05)  =    zero and
                     rf-mgs-ave-mes (05)  =    zero and
                     rf-mgs-imp-ret (05)  =    zero and
                     rf-mgs-dar-mes (06)  =    zero and
                     rf-mgs-ave-mes (06)  =    zero and
                     rf-mgs-imp-ret (06)  =    zero and
                     rf-mgs-dar-mes (07)  =    zero and
                     rf-mgs-ave-mes (07)  =    zero and
                     rf-mgs-imp-ret (07)  =    zero and
                     rf-mgs-dar-mes (08)  =    zero and
                     rf-mgs-ave-mes (08)  =    zero and
                     rf-mgs-imp-ret (08)  =    zero and
                     rf-mgs-dar-mes (09)  =    zero and
                     rf-mgs-ave-mes (09)  =    zero and
                     rf-mgs-imp-ret (09)  =    zero and
                     rf-mgs-dar-mes (10)  =    zero and
                     rf-mgs-ave-mes (10)  =    zero and
                     rf-mgs-imp-ret (10)  =    zero and
                     rf-mgs-dar-mes (11)  =    zero and
                     rf-mgs-ave-mes (11)  =    zero and
                     rf-mgs-imp-ret (11)  =    zero and
                     rf-mgs-dar-mes (12)  =    zero and
                     rf-mgs-ave-mes (12)  =    zero and
                     rf-mgs-imp-ret (12)  =    zero and
                     rf-mgs-dar-bil       =    zero and
                     rf-mgs-ave-bil       =    zero
                     go to det-snx-mgs-800.
      *              *-------------------------------------------------*
      *              * Incremento contatore schede anno precedente con *
      *              * valori                                          *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snx-mgs-cvp      .
       det-snx-mgs-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
       det-snx-mgs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-snx-mgs-999.
       det-snx-mgs-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

