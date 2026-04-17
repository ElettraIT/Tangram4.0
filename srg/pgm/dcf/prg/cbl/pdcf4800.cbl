       Identification Division.
       Program-Id.                                 pdcf4800           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    arc                 *
      *                                   Fase:    dcf480              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/01/01    *
      *                       Ultima revisione:    NdK del 15/12/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione annotazioni fornitori - documenti  *
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
                     "dcf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcf480"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf4800"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "          ANNOTAZIONI FORNITORI         "       .

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
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [dfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdfx"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-fnt          pic  9(07)                  .
               10  w-tes-cod-fnt-rag      pic  x(40)                  .
               10  w-tes-dpz-fnt          pic  x(04)                  .
               10  w-tes-dpz-fnt-rag      pic  x(40)                  .
               10  w-tes-cod-ann          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ann-fnt.
                   15  w-tes-rig-fnt occurs 10
                                          pic  x(40)                  .
               10  w-tes-cod-prg.
                   15  w-tes-cod-prg-dcf  pic  x(01)                  .
                   15  w-tes-cod-prg-orf  pic  x(01)                  .
                   15  w-tes-cod-prg-bfo  pic  x(01)                  .
                   15  w-tes-cod-prg-bol  pic  x(01)                  .
                   15  w-tes-cod-prg-ffo  pic  x(01)                  .
                   15  w-tes-cod-prg-scf  pic  x(01)                  .
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
      *        * Work per salvataggio codice fornitore                 *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fnt              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-dfx.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-dfx-val-max          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-dfx-val-pre          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-dfx-val-pos          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  w-enc-dfx-val-rec          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti dipendenze per il fornitore  *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcf.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il fornitore commerciale ha dipendenze  *
      *            * - N : No, il fornitore commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice fornitore commerciale                      *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-fnt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il fornitore *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-dpz      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det records                                  *
      *        *-------------------------------------------------------*
           05  w-det-rec-dfx.
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-rec-dfx-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice programma di comodo                        *
      *            *---------------------------------------------------*
               10  w-det-rec-dfx-prg      pic  x(08)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Programma                                  *
      *        *-------------------------------------------------------*
           05  w-exp-cod-prg.
               10  w-exp-cod-prg-num      pic  9(02)       value 7    .
               10  w-exp-cod-prg-lun      pic  9(02)       value 30   .
               10  w-exp-cod-prg-tbl.
                   15  filler             pic  x(30) value
                            "[ ] Anagrafica commerciale    "          .
                   15  filler             pic  x(30) value
                            "[ ] Ordini                    "          .
                   15  filler             pic  x(30) value
                            "[ ] Bolle di entrata          "          .
                   15  filler             pic  x(30) value
                            "[ ] Bolle di uscita           "          .
                   15  filler             pic  x(30) value
                            "[ ] Fatture             (ffo) "          .
                   15  filler             pic  x(30) value
                            "[ ] Scadenziario        (scf) "          .
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
      *        * Work per Find su archivio [dfx]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-dfx.
               10  w-fnd-arc-dfx-sel      pic  x(01)                  .
               10  w-fnd-arc-dfx-cod      pic  9(03)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del fornitore*
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acl"                   .

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
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-opn-000  thru cod-cod-dcf-opn-999    .
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
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-cls-000  thru cod-cod-dcf-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [dfx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [dfx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
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
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza fornitore                 *
      *                  *---------------------------------------------*
           perform   acc-dpz-fnt-000      thru acc-dpz-fnt-999        .
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza fornitore                     *
      *              *-------------------------------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-fnt-000      thru pmt-cod-fnt-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza fornitore                     *
      *              *-------------------------------------------------*
           perform   pmt-dpz-fnt-000      thru pmt-dpz-fnt-999        .
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
      *    * Visualizzazione prompts per Codice fornitore              *
      *    *-----------------------------------------------------------*
       pmt-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice     fornitore       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice dipendenza fornitore   *
      *    *-----------------------------------------------------------*
       pmt-dpz-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza fornitore       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dpz-fnt-999.
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
      *    * Accettazione campo chiave : Codice fornitore              *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-sav-cod-fnt          .
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      w-tes-cod-fnt        to   w-cod-mne-dcf-cod      .
           move      04                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      04                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-fnt          .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record da file [dcf]                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      rf-dcf-rag-soc       to   w-tes-cod-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-fnt        =    zero
                     go to acc-cod-fnt-410
           else      go to acc-cod-fnt-450.
       acc-cod-fnt-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore del   *
      *                      * codice dipendenza                       *
      *                      *-----------------------------------------*
           if        w-tes-dpz-fnt        =    spaces
                     go to acc-cod-fnt-415
           else      go to acc-cod-fnt-420.
       acc-cod-fnt-415.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a spaces           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura tasto 'Do' e uscita       *
      *                          *-------------------------------------*
           move      "DO  "               to   v-key                  .
           go to     acc-cod-fnt-800.
       acc-cod-fnt-420.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a non-spaces       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura codice dipendenza a Spa-  *
      *                          * ces con visualizzazione             *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-dpz-fnt          .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice dipendenza   *
      *                          *-------------------------------------*
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-450.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero   *
      *                  *---------------------------------------------*
       acc-cod-fnt-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso da quello   *
      *                      * precedente si forza il codice dipenden- *
      *                      * za a Spaces e lo si visualizza          *
      *                      *-----------------------------------------*
           if        w-tes-cod-fnt        =    w-sav-cod-fnt
                     go to acc-cod-fnt-460.
           if        w-tes-dpz-fnt        =    spaces
                     go to acc-cod-fnt-460.
           move      spaces               to   w-tes-dpz-fnt          .
           perform   vis-dpz-fnt-000      thru vis-dpz-fnt-999        .
       acc-cod-fnt-460.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * lettura anagrafica contabile clienti    *
      *                      *-----------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-fnt-465
           else      go to acc-cod-fnt-470.
       acc-cod-fnt-465.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-470.
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il fornitore commerciale                    *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-det-snd-dcf-fnt      .
           perform   det-snd-dcf-000      thru det-snd-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale segnale di fornitore e dipendenze *
      *                  *---------------------------------------------*
           perform   vis-snd-dcf-000      thru vis-snd-dcf-999        .
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-fnt-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-fnt-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-fnt-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-fnt-999.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice fornitore           *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-fnt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale cliente a  *
      *    *                                fianco del codice fornitore*
      *    *-----------------------------------------------------------*
       vis-cod-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-fnt-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Segnale di cliente con di- *
      *    *                                dipendenze                 *
      *    *-----------------------------------------------------------*
       vis-snd-dcf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      80                   to   v-pos                  .
           if        w-det-snd-dcf-snx    =    "S"
                     move  "*"            to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snd-dcf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice dipendenza             *
      *    *-----------------------------------------------------------*
       acc-dpz-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-det-snd-dcf-snx    not  = "S"
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "A*"                 to   w-cod-cod-dcf-ope      .
           move      w-tes-cod-fnt        to   w-cod-cod-dcf-fnt      .
           move      w-tes-dpz-fnt        to   w-cod-cod-dcf-cod      .
           move      05                   to   w-cod-cod-dcf-lin      .
           move      30                   to   w-cod-cod-dcf-pos      .
           move      09                   to   w-cod-cod-dcf-rln      .
           move      30                   to   w-cod-cod-dcf-rps      .
           move      zero                 to   w-cod-cod-dcf-vln      .
           move      zero                 to   w-cod-cod-dcf-vps      .
           move      zero                 to   w-cod-cod-dcf-lln      .
           move      zero                 to   w-cod-cod-dcf-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
       acc-dpz-fnt-110.
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           if        w-cod-cod-dcf-ope    =    "F+"
                     go to acc-dpz-fnt-115.
           if        w-cod-cod-dcf-ope    =    "AC"
                     go to acc-dpz-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-fnt-115.
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
           go to     acc-dpz-fnt-110.
       acc-dpz-fnt-120.
           move      w-cod-cod-dcf-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-dpz-fnt          .
       acc-dpz-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se '*'                                      *
      *                  *---------------------------------------------*
           if        w-tes-dpz-fnt        not  = "*   "
                     go to acc-dpz-fnt-500.
           move      "Tutte le dipendenze"
                                          to   w-tes-dpz-fnt-rag      .
           go to     acc-dpz-fnt-560.
       acc-dpz-fnt-500.
      *                  *---------------------------------------------*
      *                  * Lettura record da file [dcf]                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-let-arc-dcf-fnt      .
           move      w-tes-dpz-fnt        to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      rf-dcf-rag-soc       to   w-tes-dpz-fnt-rag      .
       acc-dpz-fnt-560.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-dpz-fnt-rag-000  thru vis-dpz-fnt-rag-999    .
       acc-dpz-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dpz-fnt-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-fnt-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dpz-fnt-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice dipendenza          *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpz-fnt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale dipendenza *
      *    *-----------------------------------------------------------*
       vis-dpz-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-fnt-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-fnt-rag-999.
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
      *                  * Find su archivio [dfx]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-dfx-000      thru fnd-arc-dfx-999        .
           if        w-fnd-arc-dfx-sel    not  = spaces
                     go to acc-cod-ann-100.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione e visualizzazione valore    *
      *                  *---------------------------------------------*
           move      w-fnd-arc-dfx-cod    to   w-tes-cod-ann          .
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
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-ann-100.
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
      *                  * Annotazioni                                 *
      *                  *---------------------------------------------*
           perform   acc-ann-fnt-000      thru acc-ann-fnt-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           perform   acc-cod-prg-000      thru acc-cod-prg-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Si/no in stampa                             *
      *                  *---------------------------------------------*
           perform   acc-snx-stp-000      thru acc-snx-stp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
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
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   vis-ann-fnt-000      thru vis-ann-fnt-999        .
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
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   pmt-ann-fnt-000      thru pmt-ann-fnt-999        .
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
      *    * Visualizzazione prompt : Annotazioni                      *
      *    *-----------------------------------------------------------*
       pmt-ann-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Annotazione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ann-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Programma                     *
      *    *-----------------------------------------------------------*
       pmt-cod-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
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
      *    * Accettazione campo testata : Annotazioni                  *
      *    *-----------------------------------------------------------*
       acc-ann-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ann-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      "X"                  to   v-edm                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ann-fnt (1)    to   v-txt                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ann-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ann-fnt-999.
       acc-ann-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-tes-ann-fnt (1)      .
       acc-ann-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-ann-fnt (1)    =    spaces
                     go to acc-ann-fnt-100.
       acc-ann-fnt-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-ann-fnt (1)
                    (01 : 01)             =    spaces
                     go to acc-ann-fnt-100.
       acc-ann-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ann-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ann-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ann-fnt-100.
       acc-ann-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Annotazioni               *
      *    *-----------------------------------------------------------*
       vis-ann-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 1) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 2) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 3) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 4) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 5) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 6) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 7) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 8) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 9) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rig-fnt (1, 10)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Programma                     *
      *    *-----------------------------------------------------------*
       acc-cod-prg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           move      18                   to   v-lin                  .
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
           move      "DS"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-cod-prg-lun    to   v-car                  .
           move      w-exp-cod-prg-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
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
           if        w-tes-cod-fnt        =    zero
                     go to acc-snx-stp-999.
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-scf (1)
                                          not  = spaces or
                     w-tes-cod-prg-scf (1)
                                          not  = spaces or
                     w-tes-cod-prg-scf (1)
                                          not  = spaces
                     move     "N"         to   w-tes-snx-stp (1)
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
           if        w-tes-cod-fnt        =    zero
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
      *              * Controllo codice fornitore                      *
      *              *-------------------------------------------------*
           if        w-tes-cod-fnt        not  = zero
                     go to cnt-tdo-key-200.
           move      "Manca il codice cliente !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
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
      *              *-------------------------------------------------*
      *              * Test su Codice fornitore                        *
      *              *-------------------------------------------------*
           if        w-tes-cod-fnt        =    zero
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
      *              * Controllo su annotazioni                        *
      *              *-------------------------------------------------*
           if        w-tes-ann-fnt (1)    not  = spaces
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
           move      zero                 to   w-tes-cod-fnt          .
           move      spaces               to   w-tes-cod-fnt-rag      .
           move      spaces               to   w-tes-dpz-fnt          .
           move      spaces               to   w-tes-dpz-fnt-rag      .
           move      zero                 to   w-tes-cod-ann          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-ann-fnt (1)      .
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
           move      zero                 to   w-det-rec-dfx-ctr      .
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Start su [dfx]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "FNTANN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-fnt        to   rf-dfx-cod-fnt         .
           move      w-tes-dpz-fnt        to   rf-dfx-dpz-fnt         .
           move      w-tes-cod-ann        to   rf-dfx-cod-ann         .
           move      spaces               to   rf-dfx-cod-prg         .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-600.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dfx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-600.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Test max su [dfx]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        rf-dfx-cod-fnt       not  = w-tes-cod-fnt
                     go to  rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza fornitore         *
      *                  *---------------------------------------------*
           if        rf-dfx-dpz-fnt       not  = w-tes-dpz-fnt
                     go to  rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Test su codice annotazione                  *
      *                  *---------------------------------------------*
           if        rf-dfx-cod-ann       not  = w-tes-cod-ann
                     go to  rou-let-reg-600.
       rou-let-reg-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dfx]                              *
      *              *-------------------------------------------------*
       rou-let-reg-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-rec-dfx-ctr      .
       rou-let-reg-520.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           if        rf-dfx-cod-prg       =    "pdcf4000"
                     move  "X"            to   w-tes-cod-prg-dcf (1)
           else if   rf-dfx-cod-prg       =    "porf3000"
                     move  "X"            to   w-tes-cod-prg-orf (1)
           else if   rf-dfx-cod-prg       =    "pbfo3000"
                     move  "X"            to   w-tes-cod-prg-bfo (1)
           else if   rf-dfx-cod-prg       =    "pbol3000"
                     move  "X"            to   w-tes-cod-prg-bol (1)
           else if   rf-dfx-cod-prg       =    "pffo3000"
                     move  "X"            to   w-tes-cod-prg-ffo (1)
           else if   rf-dfx-cod-prg       =    "pscf3000"
                     move  "X"            to   w-tes-cod-prg-scf (1)
           else if   rf-dfx-cod-prg       =    "pxpg0012"
                     move  "X"            to   w-tes-cod-prg-tel (1)  .
      *                  *---------------------------------------------*
      *                  * Altri dati                                  *
      *                  *---------------------------------------------*
           move      rf-dfx-ann-fnt       to   w-tes-ann-fnt (1)      .
           move      rf-dfx-dat-ini       to   w-tes-dat-ini (1)      .
           move      rf-dfx-dat-fin       to   w-tes-dat-fin (1)      .
           move      rf-dfx-cod-ang       to   w-tes-cod-ang (1)      .
      *
           if        rf-dfx-cod-ang       not  numeric
                     move  zero           to   w-tes-cod-ang (1)      .
      *
           move      rf-dfx-snx-stp       to   w-tes-snx-stp (1)      .
           move      rf-dfx-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-580.
      *              *-------------------------------------------------*
      *              * Riciclo su [dfx]                                *
      *              *-------------------------------------------------*
           go to     rou-let-reg-200.
       rou-let-reg-600.
      *              *-------------------------------------------------*
      *              * Test su contatore record letti                  *
      *              *-------------------------------------------------*
           if        w-det-rec-dfx-ctr    >    zero
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
      *              * Trattamento file [dfx]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [dfx]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-dfx-000      thru wrt-rec-dfx-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [dfx]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-dfx-000      thru rew-rec-dfx-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [dfx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dfx-000      thru del-rec-dfx-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record [dfx]                              *
      *    *-----------------------------------------------------------*
       nor-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
       nor-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dfx]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   rf-dfx-cod-fnt         .
           move      w-tes-dpz-fnt        to   rf-dfx-dpz-fnt         .
           move      w-tes-cod-ann        to   rf-dfx-cod-ann         .
      *                  *---------------------------------------------*
      *                  * Data, utente, fase, di ultimo inserimento o *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dfx-ide-dat         .
           move      s-ute                to   rf-dfx-ide-ute         .
           move      s-fas                to   rf-dfx-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-ann-fnt (1)    to   rf-dfx-ann-fnt         .
           move      w-tes-dat-ini (1)    to   rf-dfx-dat-ini         .
           move      w-tes-dat-fin (1)    to   rf-dfx-dat-fin         .
           move      w-tes-cod-ang (1)    to   rf-dfx-cod-ang         .
           move      w-tes-snx-stp (1)    to   rf-dfx-snx-stp         .
           move      w-tes-alx-exp (1)    to   rf-dfx-alx-exp         .
       cmp-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dfx]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Delete record [dfx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dfx-000      thru del-rec-dfx-999        .
      *              *-------------------------------------------------*
      *              * Scrittura record [dfx]                          *
      *              *-------------------------------------------------*
           perform   scr-rec-dfx-000      thru scr-rec-dfx-999        .
       wrt-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dfx]                                    *
      *    *-----------------------------------------------------------*
       scr-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Composizione record - pdcf4000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-dcf (1)
                                          =    spaces
                     go to scr-rec-dfx-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pdcf4000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-100.
      *              *-------------------------------------------------*
      *              * Composizione record - porf3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-orf (1)
                                          =    spaces
                     go to scr-rec-dfx-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "porf3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-200.
      *              *-------------------------------------------------*
      *              * Composizione record - pbol3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-bol (1)
                                          =    spaces
                     go to scr-rec-dfx-300.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbol3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-300.
      *              *-------------------------------------------------*
      *              * Composizione record - pbfo3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-bfo (1)
                                          =    spaces
                     go to scr-rec-dfx-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbfo3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-400.
      *              *-------------------------------------------------*
      *              * Composizione record - pffo3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-ffo (1)
                                          =    spaces
                     go to scr-rec-dfx-500.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pffo3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-500.
      *              *-------------------------------------------------*
      *              * Composizione record - pscf3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-scf (1)
                                          =    spaces
                     go to scr-rec-dfx-600.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pscf3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-600.
      *              *-------------------------------------------------*
      *              * Composizione record - pxpg0012                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-tes-cod-prg-tel (1)
                                          =    spaces
                     go to scr-rec-dfx-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pxpg0012"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           perform   put-rec-dfx-000      thru put-rec-dfx-999        .
       scr-rec-dfx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-rec-dfx-999.
       scr-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Put record [dfx]                                          *
      *    *-----------------------------------------------------------*
       put-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
       put-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [dfx]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Delete record [dfx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dfx-000      thru del-rec-dfx-999        .
      *              *-------------------------------------------------*
      *              * Write record [dfx]                              *
      *              *-------------------------------------------------*
           perform   wrt-rec-dfx-000      thru wrt-rec-dfx-999        .
       rew-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [dfx]                                *
      *    *-----------------------------------------------------------*
       del-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Composizione record - pdcf4000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pdcf4000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-100.
      *              *-------------------------------------------------*
      *              * Composizione record - porf3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "porf3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-200.
      *              *-------------------------------------------------*
      *              * Composizione record - pbol3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbol3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-300.
      *              *-------------------------------------------------*
      *              * Composizione record - pbfo3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pbfo3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-400.
      *              *-------------------------------------------------*
      *              * Composizione record - pffo3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pffo3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-500.
      *              *-------------------------------------------------*
      *              * Composizione record - pscf3000                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pscf3000"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-600.
      *              *-------------------------------------------------*
      *              * Composizione record - pxpg0012                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           perform   nor-rec-dfx-000      thru nor-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Codice programma                            *
      *                  *---------------------------------------------*
           move      "pxpg0012"           to   rf-dfx-cod-prg         .
      *                  *---------------------------------------------*
      *                  * Completamento composizione                  *
      *                  *---------------------------------------------*
           perform   cmp-rec-dfx-000      thru cmp-rec-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           perform   dlt-rec-dfx-000      thru dlt-rec-dfx-999        .
       del-rec-dfx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-rec-dfx-999.
       del-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [dfx]                                *
      *    *-----------------------------------------------------------*
       dlt-rec-dfx-000.
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
       dlt-rec-dfx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se presenti dipendenze per il fornitore    *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcf-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcf-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcf-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcf]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-det-snd-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-999.
       det-snd-dcf-100.
      *              *-------------------------------------------------*
      *              * Next su [dcf]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Max su [dcf], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcf-cod-fnt       not  = w-det-snd-dcf-fnt
                     go to det-snd-dcf-800.
       det-snd-dcf-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcf-dpz-fnt       =    spaces
                     go to det-snd-dcf-100.
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcf-ctr      .
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcf-ctr    >    1
                     go to det-snd-dcf-500.
           move      rf-dcf-dpz-fnt       to   w-det-snd-dcf-dpz      .
       det-snd-dcf-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcf] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcf-100.
       det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcf-ctr    >    zero
                     go to det-snd-dcf-900
           else      go to det-snd-dcf-999.
       det-snd-dcf-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcf-snx      .
       det-snd-dcf-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [dfx]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-dfx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-dfx-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdfx4810"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-dfx-sel
                     go to  fnd-arc-dfx-999.
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
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-tes-cod-fnt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza del fornitore                          *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dpz-fnt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      w-tes-dpz-fnt        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/prg/obj/pdcf4810"
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
                     go to fnd-arc-dfx-900.
      *                  *---------------------------------------------*
      *                  * Valore letto                                *
      *                  *---------------------------------------------*
           move      s-num                to   w-fnd-arc-dfx-cod      .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-dfx-999.
       fnd-arc-dfx-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-dfx-sel      .
       fnd-arc-dfx-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcf]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-fnt    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      w-let-arc-dcf-dpz    to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcf-400.
       let-arc-dcf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcf-rag-soc       to   w-let-arc-dcf-rag      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcf-999.
       let-arc-dcf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcf-flg      .
           move      all"."               to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-999.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-999.
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
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza fornitore*
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acs"                   .

