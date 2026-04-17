       Identification Division.
       Program-Id.                                 pdcf1500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    tab                 *
      *                                   Fase:    dcf150              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 08/06/92    *
      *                       Ultima revisione:    NdK del 29/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella forme di pagamento         *
      *                    fornitori                                   *
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
                     "dcf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcf150"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf1500"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  GESTIONE FORME DI PAGAMENTO FORNITORI "       .

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
      *            * Numero pagina testata in corso di trattamento     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-npt      pic  9(02)                  .
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

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-fop          pic  9(07)                  .
               10  w-tes-cod-fop-aut      pic  x(01)                  .
               10  w-tes-inx-pag          pic  9(02)                  .
               10  w-tes-lin-pag          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-fop          pic  x(40)                  .
               10  w-tes-sco-fat          pic  9(02)v9(01)            .
               10  w-tes-cpg-ass occurs 03.
                   15  w-tes-cod-pag      pic  9(07)                  .
                   15  w-tes-cod-pag-des  pic  x(40)                  .
                   15  w-tes-cod-pag-nsc  pic  9(02)                  .
                   15  w-tes-cod-pag-dps  pic  9(02)                  .
                   15  w-tes-tip-amm      pic  9(02)                  .
                   15  w-tes-per-toi      pic  9(02)v9(01)            .
                   15  w-tes-dim-act      pic  9(02)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative al record forma di paga-   *
      *        * mento                                                 *
      *        *-------------------------------------------------------*
           05  w-prs-rec-yfp.
      *            *---------------------------------------------------*
      *            * Valore massimo accettabile per il codice          *
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-yfp-mco      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzionamento codice, in creazione           *
      *            * - M : Manuale                                     *
      *            * - A : Automatico                                  *
      *            *---------------------------------------------------*
               10  w-prs-rec-yfp-fco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del mnemonico                     *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-yfp-omn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Unicita' del mnemonico                            *
      *            * - N : Non necessariamente unico, si' duplicati    *
      *            * - U : Unico, duplicati non ammessi                *
      *            *---------------------------------------------------*
               10  w-prs-rec-yfp-umn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del mnemonico                     *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-yfp-mmn      pic  x(01)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zpg]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zpg.
               10  w-let-arc-zpg-flg      pic  x(01)                  .
               10  w-let-arc-zpg-cod      pic  9(07)                  .
               10  w-let-arc-zpg-des      pic  x(40)                  .
               10  w-let-arc-zpg-nsc      pic  9(02)                  .
               10  w-let-arc-zpg-dps      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det su numero Diminuzioni acconto            *
      *        *-------------------------------------------------------*
           05  w-det-dim-act.
               10  w-det-dim-act-num      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det su numero codici di pagamento            *
      *        *-------------------------------------------------------*
           05  w-det-num-cdp.
               10  w-det-num-cdp-num      pic  9(02)                  .
               10  w-det-num-cdp-ira.
                   15  w-det-num-cdp-i01  pic  9(02)                  .
                   15  w-det-num-cdp-i02  pic  9(02)                  .
                   15  w-det-num-cdp-i03  pic  9(02)                  .
               10  w-det-num-cdp-irb redefines
                   w-det-num-cdp-ira.
                   15  w-det-num-cdp-ixx occurs 03
                                          pic  9(02)                  .
               10  w-det-num-cdp-uta occurs 11
                                          pic  9(02)                  .
               10  w-det-num-cdp-ctr      pic  9(02)                  .
               10  w-det-num-cdp-inx      pic  9(02)                  .
               10  w-det-num-cdp-jnx      pic  9(02)                  .
               10  w-det-num-cdp-add      pic  9(02)                  .
               10  w-det-num-cdp-tpe      pic  9(03)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Det su numero scadenze totali                *
      *        *-------------------------------------------------------*
           05  w-det-num-sca.
               10  w-det-num-sca-max      pic  9(02) value 96         .
               10  w-det-num-sca-tot      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det su numero codici di pagamento con decor- *
      *        * renza prima scadenza manuale                          *
      *        *-------------------------------------------------------*
           05  w-det-num-dsm.
               10  w-det-num-dsm-tot      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di ammontare                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-amm.
               10  w-exp-tip-amm-num      pic  9(02)       value 11   .
               10  w-exp-tip-amm-lun      pic  9(02)       value 30   .
               10  w-exp-tip-amm-tbl.
                   15  filler             pic  x(30) value
                            "Il totale documento           "          .
                   15  filler             pic  x(30) value
                            "Il totale imponibile          "          .
                   15  filler             pic  x(30) value
                            "Una % del totale documento    "          .
                   15  filler             pic  x(30) value
                            "Una % del totale imponibile   "          .
                   15  filler             pic  x(30) value
                            "Il totale Iva                 "          .
                   15  filler             pic  x(30) value
                            "Solo una quota a forfait      "          .
                   15  filler             pic  x(30) value
                            "Tot. Iva piu' quota a forfait "          .
                   15  filler             pic  x(30) value
                            "Totale documento meno forfait "          .
                   15  filler             pic  x(30) value
                            "Totale imponibile meno forfait"          .
                   15  filler             pic  x(30) value
                            "Il residuo per il tot. docum. "          .
                   15  filler             pic  x(30) value
                            "Il residuo per il tot. impon. "          .
      *        *-------------------------------------------------------*
      *        * Work per : Diminuzione acconto                        *
      *        *-------------------------------------------------------*
           05  w-exp-dim-act.
               10  w-exp-dim-act-num      pic  9(02)       value 02   .
               10  w-exp-dim-act-lun      pic  9(02)       value 02   .
               10  w-exp-dim-act-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice di pagamento            *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzpg0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(56)                  .
               10  w-err-box-err-e00.
                   15  w-err-box-err-e01  pic  9(01)                  .
                   15  w-err-box-err-e02  pic  x(14) value
                       ". pagamento - "                               .
                   15  w-err-box-err-e03  pic  x(41)                  .

      *    *===========================================================*
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Sconto in fatturazione           *
      *        *-------------------------------------------------------*
           05  w-sav-sco-fat              pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Codice di pagamento              *
      *        *-------------------------------------------------------*
           05  w-sav-cod-pag              pic  9(07)                  .
           05  w-sav-cod-pag-des          pic  x(40)                  .
           05  w-sav-cod-pag-nsc          pic  9(02)                  .
           05  w-sav-cod-pag-dps          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Tipo di ammontare                *
      *        *-------------------------------------------------------*
           05  w-sav-tip-amm              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Percentuale sul totale documento *
      *        * o sul totale imponibile                               *
      *        *-------------------------------------------------------*
           05  w-sav-per-toi              pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Diminuzione acconto              *
      *        *-------------------------------------------------------*
           05  w-sav-dim-act              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-yfp.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-yfp-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-yfp-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-yfp-val-pos          pic  9(07)                  .

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
      *              * Lettura personalizzazioni relative al record    *
      *              * codice di pagamento                             *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf[rec-yfp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-yfp
           else      move  spaces         to   w-prs-rec-yfp          .
           if        w-prs-rec-yfp-mco    not  numeric
                     move  zero           to   w-prs-rec-yfp-mco      .
           if        w-prs-rec-yfp-mco    =    zero
                     move  9999999        to   w-prs-rec-yfp-mco      .
           if        w-prs-rec-yfp-fco    not  = "A"
                     move  "M"            to   w-prs-rec-yfp-fco      .
           if        w-prs-rec-yfp-omn    not  = "O"
                     move  "N"            to   w-prs-rec-yfp-omn      .
           if        w-prs-rec-yfp-umn    not  = "U"
                     move  "N"            to   w-prs-rec-yfp-umn      .
           if        w-prs-rec-yfp-mmn    not  = "N"
                     move  "M"            to   w-prs-rec-yfp-mmn      .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-opn-000  thru cod-mne-yfp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-zpg-opn-000  thru cod-mne-zpg-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-cls-000  thru cod-mne-yfp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice di pagamento   *
      *              *-------------------------------------------------*
           perform   cod-mne-zpg-cls-000  thru cod-mne-zpg-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
       rou-opn-fls-050.
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
       rou-cls-fls-050.
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
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
      *                  * Codice forma di pagamento                   *
      *                  *---------------------------------------------*
           perform   acc-cod-fop-000      thru acc-cod-fop-999        .
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
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice forma di pagamento     *
      *    *-----------------------------------------------------------*
       pmt-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice forma di pagamento     *
      *    *-----------------------------------------------------------*
       acc-cod-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-yfp-ope      .
           move      w-tes-cod-fop        to   w-cod-mne-yfp-cod      .
           move      04                   to   w-cod-mne-yfp-lin      .
           move      30                   to   w-cod-mne-yfp-pos      .
           move      06                   to   w-cod-mne-yfp-dln      .
           move      30                   to   w-cod-mne-yfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
       acc-cod-fop-110.
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           if        w-cod-mne-yfp-ope    =    "F+"
                     go to acc-cod-fop-115.
           if        w-cod-mne-yfp-ope    =    "AC"
                     go to acc-cod-fop-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fop-115.
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
           go to     acc-cod-fop-110.
       acc-cod-fop-120.
           move      w-cod-mne-yfp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-fop-999.
       acc-cod-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-fop          .
       acc-cod-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-fop        =    zero
                     go to acc-cod-fop-410
           else      go to acc-cod-fop-450.
       acc-cod-fop-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-fop-412.
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
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-fop-100.
       acc-cod-fop-412.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il tipo funzio- *
      *                      * namento per il codice e' automatico op- *
      *                      * pure manuale                            *
      *                      *-----------------------------------------*
           if        w-prs-rec-yfp-fco    not  = "A"
                     go to acc-cod-fop-415
           else      go to acc-cod-fop-420.
       acc-cod-fop-415.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' manuale                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna azione, ne' controllo       *
      *                          *-------------------------------------*
           go to     acc-cod-fop-600.
       acc-cod-fop-420.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      w-prs-rec-yfp-mco    to   w-enc-yfp-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-yfp-val-pos    to   w-tes-cod-fop          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-fop-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-fop-000      thru vis-cod-fop-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-fop-600.
       acc-cod-fop-450.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero,  *
      *                  * si controlla che il codice impostato non    *
      *                  * superi il valore massimo impostabile per    *
      *                  * il codice                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore massimo impostabile e' pa- *
      *                      * ri a zero o al valore massimo possibile *
      *                      * il controllo si considera superato      *
      *                      *-----------------------------------------*
           if        w-prs-rec-yfp-mco    =    zero or
                     w-prs-rec-yfp-mco    =    9999999
                     go to acc-cod-fop-600.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato non e' superiore *
      *                      * al valore massimo impostabile, il con-  *
      *                      * trollo e' superato                      *
      *                      *-----------------------------------------*
           if        w-tes-cod-fop        not  > w-prs-rec-yfp-mco
                     go to acc-cod-fop-600.
      *                      *-----------------------------------------*
      *                      * Se controllo formale non superato       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio immagine video          *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Box                                 *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Literals nel box                    *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      57                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Il codice forma di pagamento non puo' essere super
      -              "iore a "            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      13                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "<"                  to   v-edm                  .
           move      w-prs-rec-yfp-mco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Accettazione presa visione          *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Ripristino immagine video           *
      *                          *-------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-fop-100.
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
      *    * Visualizzazione campo chiave : Codice forma di pagamento  *
      *    *-----------------------------------------------------------*
       vis-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-fop        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fop-999.
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
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
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
                     go to acc-nok-reg-800.
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
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
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
      *              * La testata e' composta di nr. 1 pagina          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
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
      *                  * Descrizione forma di pagamento              *
      *                  *---------------------------------------------*
           perform   acc-des-fop-000      thru acc-des-fop-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Sconto in fatturazione                      *
      *                  *---------------------------------------------*
           perform   acc-sco-fat-000      thru acc-sco-fat-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * 1. codice di pagamento                      *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   acc-cod-pag-000      thru acc-cod-pag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-122.
      *                  *---------------------------------------------*
      *                  * 1. tipo di ammontare                        *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   acc-tip-amm-000      thru acc-tip-amm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-124.
      *                  *---------------------------------------------*
      *                  * 1. percentuale                              *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   acc-per-toi-000      thru acc-per-toi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-122.
       acc-tes-reg-126.
      *                  *---------------------------------------------*
      *                  * 1. diminuzione acconto                      *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   acc-dim-act-000      thru acc-dim-act-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-124.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * 2. codice di pagamento                      *
      *                  *---------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   acc-cod-pag-000      thru acc-cod-pag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-126.
       acc-tes-reg-132.
      *                  *---------------------------------------------*
      *                  * 2. tipo di ammontare                        *
      *                  *---------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   acc-tip-amm-000      thru acc-tip-amm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-134.
      *                  *---------------------------------------------*
      *                  * 2. percentuale                              *
      *                  *---------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   acc-per-toi-000      thru acc-per-toi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-132.
       acc-tes-reg-136.
      *                  *---------------------------------------------*
      *                  * 2. diminuzione acconto                      *
      *                  *---------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   acc-dim-act-000      thru acc-dim-act-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-134.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * 3. codice di pagamento                      *
      *                  *---------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   acc-cod-pag-000      thru acc-cod-pag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-136.
       acc-tes-reg-142.
      *                  *---------------------------------------------*
      *                  * 3. tipo di ammontare                        *
      *                  *---------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   acc-tip-amm-000      thru acc-tip-amm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-144.
      *                  *---------------------------------------------*
      *                  * 3. percentuale                              *
      *                  *---------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   acc-per-toi-000      thru acc-per-toi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-142.
       acc-tes-reg-146.
      *                  *---------------------------------------------*
      *                  * 3. diminuzione acconto                      *
      *                  *---------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   acc-dim-act-000      thru acc-dim-act-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-144.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-146.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
      *              * Descrizione forma di pagamento                  *
      *              *-------------------------------------------------*
           perform   vis-des-fop-000      thru vis-des-fop-999        .
      *              *-------------------------------------------------*
      *              * Sconto in fatturazione                          *
      *              *-------------------------------------------------*
           perform   vis-sco-fat-000      thru vis-sco-fat-999        .
      *              *-------------------------------------------------*
      *              * 1. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   vis-cod-pag-000      thru vis-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 1. descrizione codice di pagamento              *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   vis-des-pag-000      thru vis-des-pag-999        .
      *              *-------------------------------------------------*
      *              * 1. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   vis-tip-amm-000      thru vis-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 1. percentuale                                  *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
      *              *-------------------------------------------------*
      *              * 1. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   vis-dim-act-000      thru vis-dim-act-999        .
      *              *-------------------------------------------------*
      *              * 2. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   vis-cod-pag-000      thru vis-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 2. descrizione codice di pagamento              *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   vis-des-pag-000      thru vis-des-pag-999        .
      *              *-------------------------------------------------*
      *              * 2. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   vis-tip-amm-000      thru vis-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 2. percentuale                                  *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
      *              *-------------------------------------------------*
      *              * 2. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   vis-dim-act-000      thru vis-dim-act-999        .
      *              *-------------------------------------------------*
      *              * 3. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   vis-cod-pag-000      thru vis-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 3. descrizione codice di pagamento              *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   vis-des-pag-000      thru vis-des-pag-999        .
      *              *-------------------------------------------------*
      *              * 3. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   vis-tip-amm-000      thru vis-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 3. percentuale                                  *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
      *              *-------------------------------------------------*
      *              * 3. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   vis-dim-act-000      thru vis-dim-act-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
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
           move      06                   to   v-lin                  .
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
      *              * Descrizione forma di pagamento                  *
      *              *-------------------------------------------------*
           perform   pmt-des-fop-000      thru pmt-des-fop-999        .
      *              *-------------------------------------------------*
      *              * Sconto in fatturazione                          *
      *              *-------------------------------------------------*
           perform   pmt-sco-fat-000      thru pmt-sco-fat-999        .
      *              *-------------------------------------------------*
      *              * 1. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   pmt-cod-pag-000      thru pmt-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 1. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   pmt-tip-amm-000      thru pmt-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 1. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-inx-pag          .
           perform   pmt-dim-act-000      thru pmt-dim-act-999        .
      *              *-------------------------------------------------*
      *              * 2. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   pmt-cod-pag-000      thru pmt-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 2. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   pmt-tip-amm-000      thru pmt-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 2. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      02                   to   w-tes-inx-pag          .
           perform   pmt-dim-act-000      thru pmt-dim-act-999        .
      *              *-------------------------------------------------*
      *              * 3. codice di pagamento                          *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   pmt-cod-pag-000      thru pmt-cod-pag-999        .
      *              *-------------------------------------------------*
      *              * 3. tipo di ammontare                            *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   pmt-tip-amm-000      thru pmt-tip-amm-999        .
      *              *-------------------------------------------------*
      *              * 3. diminuzione acconto                          *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-inx-pag          .
           perform   pmt-dim-act-000      thru pmt-dim-act-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione forma di pagamento   *
      *    *-----------------------------------------------------------*
       pmt-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sconto in fatturazione           *
      *    *-----------------------------------------------------------*
       pmt-sco-fat-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sconto in fatturazione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sco-fat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice di pagamento              *
      *    *-----------------------------------------------------------*
       pmt-cod-pag-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-inx-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      02                   to   v-pos                  .
           move      ". Codice di pagamento     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo di ammontare                *
      *    *-----------------------------------------------------------*
       pmt-tip-amm-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      02                   to   v-pos                  .
           move      ". Su tipo di ammontare    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Diminuzione acconto              *
      *    *-----------------------------------------------------------*
       pmt-dim-act-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       02                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      02                   to   v-pos                  .
           move      ". Con detrazione acconto  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-act-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mnemonico           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
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
      *    * Accettazione campo testata : Descrizione forma di paga-   *
      *    *                              mento                        *
      *    *-----------------------------------------------------------*
       acc-des-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-fop (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-fop-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-fop-999.
       acc-des-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-fop (1)      .
       acc-des-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-fop (1)    not  = spaces
                     go to acc-des-fop-450.
           if        v-key                =    "UP  "
                     go to acc-des-fop-600
           else      go to acc-des-fop-100.
       acc-des-fop-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-fop (1)
                    (01 : 01)             =    spaces
                     go to acc-des-fop-100.
       acc-des-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-fop (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-fop-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-fop-100.
       acc-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice di pa- *
      *    *                                 gamento                   *
      *    *-----------------------------------------------------------*
       vis-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-fop (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sconto in fatturazione       *
      *    *-----------------------------------------------------------*
       acc-sco-fat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sco-fat (1)    to   w-sav-sco-fat          .
       acc-sco-fat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-sco-fat (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sco-fat-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sco-fat-999.
       acc-sco-fat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sco-fat (1)      .
       acc-sco-fat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sco-fat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sco-fat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sco-fat-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sco-fat-100.
       acc-sco-fat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sconto in fatturazione    *
      *    *-----------------------------------------------------------*
       vis-sco-fat-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sco-fat (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sco-fat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice di pagamento          *
      *    *-----------------------------------------------------------*
       acc-cod-pag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente e dati asso-  *
      *                  * ciati                                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-pag
                    (1, w-tes-inx-pag)    to   w-sav-cod-pag          .
           move      w-tes-cod-pag-des
                    (1, w-tes-inx-pag)    to   w-sav-cod-pag-des      .
           move      w-tes-cod-pag-nsc
                    (1, w-tes-inx-pag)    to   w-sav-cod-pag-nsc      .
           move      w-tes-cod-pag-dps
                    (1, w-tes-inx-pag)    to   w-sav-cod-pag-dps      .
       acc-cod-pag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           move      "AC"                 to   w-cod-mne-zpg-ope      .
           move      w-tes-cod-pag
                    (1, w-tes-inx-pag)    to   w-cod-mne-zpg-cod      .
           move      w-tes-lin-pag        to   w-cod-mne-zpg-lin      .
           move      30                   to   w-cod-mne-zpg-pos      .
           move      w-tes-lin-pag        to   w-cod-mne-zpg-dln      .
           move      41                   to   w-cod-mne-zpg-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zpg-cll-000  thru cod-mne-zpg-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zpg-foi-000  thru cod-mne-zpg-foi-999    .
       acc-cod-pag-110.
           perform   cod-mne-zpg-cll-000  thru cod-mne-zpg-cll-999    .
           if        w-cod-mne-zpg-ope    =    "F+"
                     go to acc-cod-pag-115.
           if        w-cod-mne-zpg-ope    =    "AC"
                     go to acc-cod-pag-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pag-115.
           perform   cod-mne-zpg-foi-000  thru cod-mne-zpg-foi-999    .
           go to     acc-cod-pag-110.
       acc-cod-pag-120.
           move      w-cod-mne-zpg-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pag-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pag-999.
       acc-cod-pag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-pag
                                              (1, w-tes-inx-pag)      .
       acc-cod-pag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella codici di pagamento         *
      *                  *---------------------------------------------*
           move      w-tes-cod-pag
                    (1, w-tes-inx-pag)    to   w-let-arc-zpg-cod      .
           perform   let-arc-zpg-000      thru let-arc-zpg-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice di  *
      *                  * pagamento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zpg-des    to   w-tes-cod-pag-des
                                              (1, w-tes-inx-pag)      .
           move      w-let-arc-zpg-nsc    to   w-tes-cod-pag-nsc
                                              (1, w-tes-inx-pag)      .
           move      w-let-arc-zpg-dps    to   w-tes-cod-pag-dps
                                              (1, w-tes-inx-pag)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione codice di paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           perform   vis-des-pag-000      thru vis-des-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se codice di pagamento non esistente : re-  *
      *                  * impostazione                                *
      *                  *---------------------------------------------*
           if        w-let-arc-zpg-flg    not  = spaces
                     go to acc-cod-pag-100.
       acc-cod-pag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag
                    (1, w-tes-inx-pag)    =    w-sav-cod-pag
                     go to acc-cod-pag-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-cod-pag        =    zero
                     go to acc-cod-pag-800.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore del codi- *
      *                  * ce di pagamento                             *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-cod-pag-625
           else      go to acc-cod-pag-650.
       acc-cod-pag-625.
      *                  *---------------------------------------------*
      *                  * Se valore codice pagamento : zero           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dei   *
      *                      * valori associati al codice di pagamento *
      *                      *-----------------------------------------*
           move      spaces               to   w-tes-cod-pag-des
                                              (1, w-tes-inx-pag)      .
           move      zero                 to   w-tes-cod-pag-nsc
                                              (1, w-tes-inx-pag)      .
           move      zero                 to   w-tes-cod-pag-dps
                                              (1, w-tes-inx-pag)      .
           move      zero                 to   w-tes-tip-amm
                                              (1, w-tes-inx-pag)      .
           move      zero                 to   w-tes-per-toi
                                              (1, w-tes-inx-pag)      .
           move      zero                 to   w-tes-dim-act
                                              (1, w-tes-inx-pag)      .
           perform   vis-tip-amm-000      thru vis-tip-amm-999        .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
           perform   vis-dim-act-000      thru vis-dim-act-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-pag-800.
       acc-cod-pag-650.
      *                  *---------------------------------------------*
      *                  * Se valore codice pagamento diverso da zero  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-pag-800.
       acc-cod-pag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-pag-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-pag-100.
       acc-cod-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice di pagamento               *
      *    *-----------------------------------------------------------*
       vis-cod-pag-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-pag
                    (1, w-tes-inx-pag)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione codice di pagamento   *
      *    *-----------------------------------------------------------*
       vis-des-pag-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-pag-des
                    (1, w-tes-inx-pag)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-pag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo di ammontare            *
      *    *-----------------------------------------------------------*
       acc-tip-amm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-amm-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il codice pagamento e' zero : nessu- *
      *                      * na accettazione, bensi' normalizzazione *
      *                      *-----------------------------------------*
           if        w-tes-cod-pag
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-tip-amm-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-tip-amm-050.
       acc-tip-amm-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-tip-amm-035.
           move      zero                 to   w-tes-tip-amm
                                              (1, w-tes-inx-pag)      .
           perform   vis-tip-amm-000      thru vis-tip-amm-999        .
       acc-tip-amm-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tip-amm-999.
       acc-tip-amm-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-amm
                    (1, w-tes-inx-pag)    to   w-sav-tip-amm          .
       acc-tip-amm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-amm-lun    to   v-car                  .
           move      w-exp-tip-amm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-amm-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-tip-amm
                    (1, w-tes-inx-pag)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-amm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-amm-999.
       acc-tip-amm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-amm
                                              (1, w-tes-inx-pag)      .
       acc-tip-amm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    not  = zero
                     go to acc-tip-amm-425.
           if        v-key                =    "UP  "
                     go to acc-tip-amm-600
           else      go to acc-tip-amm-100.
       acc-tip-amm-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    >    w-exp-tip-amm-num
                     go to acc-tip-amm-100.
       acc-tip-amm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    =    w-sav-tip-amm
                     go to acc-tip-amm-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-tip-amm        =    zero
                     go to acc-tip-amm-800.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore del tipo  *
      *                  * di ammontare                                *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    not  = 03 and
                     w-tes-tip-amm
                    (1, w-tes-inx-pag)    not  = 04
                     go to acc-tip-amm-625
           else      go to acc-tip-amm-650.
       acc-tip-amm-625.
      *                  *---------------------------------------------*
      *                  * Se il valore del tipo di ammontare non im-  *
      *                  * plica una percentuale                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione della *
      *                      * percentuale                             *
      *                      *-----------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-tip-amm-627.
           move      zero                 to   w-tes-per-toi
                                              (1, w-tes-inx-pag)      .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
       acc-tip-amm-627.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-tip-amm-800.
       acc-tip-amm-650.
      *                  *---------------------------------------------*
      *                  * Se il valore del tipo di ammontare implica  *
      *                  * una percentuale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-tip-amm-800.
       acc-tip-amm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-amm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-amm-100.
       acc-tip-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo di ammontare                 *
      *    *-----------------------------------------------------------*
       vis-tip-amm-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-amm-lun    to   v-car                  .
           move      w-exp-tip-amm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-amm-tbl    to   v-txt                  .
           move      w-tes-tip-amm
                    (1, w-tes-inx-pag)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-amm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Percentuale                  *
      *    *-----------------------------------------------------------*
       acc-per-toi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-per-toi-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il codice pagamento e' zero : nessu- *
      *                      * na accettazione, bensi' normalizzazione *
      *                      *-----------------------------------------*
           if        w-tes-cod-pag
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-per-toi-030.
      *                      *-----------------------------------------*
      *                      * Se il tipo di ammontare non implica una *
      *                      * percentuale : nessuna accettazione,     *
      *                      * bensi' normalizzazione                  *
      *                      *-----------------------------------------*
           if        w-tes-tip-amm
                    (1, w-tes-inx-pag)    not  = 03 and
                     w-tes-tip-amm
                    (1, w-tes-inx-pag)    not  = 04
                     go to acc-per-toi-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-per-toi-050.
       acc-per-toi-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-per-toi-035.
           move      zero                 to   w-tes-per-toi
                                              (1, w-tes-inx-pag)      .
           perform   vis-per-toi-000      thru vis-per-toi-999        .
       acc-per-toi-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-per-toi-999.
       acc-per-toi-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-per-toi
                    (1, w-tes-inx-pag)    to   w-sav-per-toi          .
       acc-per-toi-075.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero si visualizza  *
      *                  * il simbolo di percentuale                   *
      *                  *---------------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    not  = zero
                     go to acc-per-toi-100.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "% : "               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-per-toi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-per-toi
                    (1, w-tes-inx-pag)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-per-toi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-per-toi-999.
       acc-per-toi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-per-toi
                                              (1, w-tes-inx-pag)      .
       acc-per-toi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    not  = zero
                     go to acc-per-toi-600.
           if        v-key                =    "UP  "
                     go to acc-per-toi-600
           else      go to acc-per-toi-100.
       acc-per-toi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore della     *
      *                  * percentuale                                 *
      *                  *---------------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-per-toi-625
           else      go to acc-per-toi-650.
       acc-per-toi-625.
      *                  *---------------------------------------------*
      *                  * Se il valore della percentuale e' a zero    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si elimina la visualizzazione del sim-  *
      *                      * bolo di percentuale                     *
      *                      *-----------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      spaces               to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-per-toi-800.
       acc-per-toi-650.
      *                  *---------------------------------------------*
      *                  * Se il valore della percentuale e' diverso   *
      *                  * da zero                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-per-toi-800.
       acc-per-toi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-per-toi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-per-toi-100.
       acc-per-toi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Percentuale                       *
      *    *-----------------------------------------------------------*
       vis-per-toi-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea per video           *
      *              *-------------------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       01                   to   w-tes-lin-pag          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore della percen- *
      *              * tuale                                           *
      *              *-------------------------------------------------*
           if        w-tes-per-toi
                    (1, w-tes-inx-pag)    =    zero
                     go to vis-per-toi-300
           else      go to vis-per-toi-600.
       vis-per-toi-300.
      *              *-------------------------------------------------*
      *              * Se il valore della percentuale e' a zero        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del simbolo di percentuale  *
      *                  * e del valore a spaces                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-per-toi-999.
       vis-per-toi-600.
      *              *-------------------------------------------------*
      *              * Se il valore della percentuale e' diverso da    *
      *              * zero                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del simbolo di percentuale  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "% : "               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      67                   to   v-pos                  .
           move      w-tes-per-toi
                    (1, w-tes-inx-pag)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-per-toi-999.
       vis-per-toi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Diminuzione acconto          *
      *    *-----------------------------------------------------------*
       acc-dim-act-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-act-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il codice pagamento e' zero : nessu- *
      *                      * na accettazione, bensi' normalizzazione *
      *                      *-----------------------------------------*
           if        w-tes-cod-pag
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-dim-act-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-dim-act-050.
       acc-dim-act-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-dim-act
                    (1, w-tes-inx-pag)    =    zero
                     go to acc-dim-act-035.
           move      zero                 to   w-tes-dim-act
                                              (1, w-tes-inx-pag)      .
           perform   vis-dim-act-000      thru vis-dim-act-999        .
       acc-dim-act-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-dim-act-999.
       acc-dim-act-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dim-act
                    (1, w-tes-inx-pag)    to   w-sav-dim-act          .
       acc-dim-act-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       02                   to   w-tes-lin-pag          .
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dim-act-lun    to   v-car                  .
           move      w-exp-dim-act-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-dim-act-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dim-act
                    (1, w-tes-inx-pag)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-act-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-act-999.
       acc-dim-act-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-act
                                              (1, w-tes-inx-pag)      .
       acc-dim-act-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-dim-act
                    (1, w-tes-inx-pag)    not  = zero
                     go to acc-dim-act-425.
           if        v-key                =    "UP  "
                     go to acc-dim-act-600
           else      go to acc-dim-act-100.
       acc-dim-act-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-dim-act
                    (1, w-tes-inx-pag)    >    w-exp-dim-act-num
                     go to acc-dim-act-100.
       acc-dim-act-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-act-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-act-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-act-100.
       acc-dim-act-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Diminuzione acconto               *
      *    *-----------------------------------------------------------*
       vis-dim-act-000.
           perform   det-lin-pag-000      thru det-lin-pag-999        .
           add       02                   to   w-tes-lin-pag          .
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dim-act-lun    to   v-car                  .
           move      w-exp-dim-act-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-tes-lin-pag        to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-dim-act-tbl    to   v-txt                  .
           move      w-tes-dim-act
                    (1, w-tes-inx-pag)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-act-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice mnemonico             *
      *    *-----------------------------------------------------------*
       acc-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non si e' in Inserimento ed il mnemonico *
      *                  * non e' modificabile si omette l'impostazio- *
      *                  * ne, a meno che il valore attuale non sia a  *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-mne (1)    =    spaces
                     go to acc-cod-mne-100.
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to acc-cod-mne-100.
           if        w-prs-rec-yfp-mmn    =    "N"
                     go to acc-cod-mne-999.
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      21                   to   v-lin                  .
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
      *                  *---------------------------------------------*
      *                  * Test che, se il mnemonico e' obbligatorio,  *
      *                  * il valore non manchi                        *
      *                  *---------------------------------------------*
           if        w-prs-rec-yfp-omn    not  = "O"
                     go to acc-cod-mne-600.
           if        w-tes-cod-mne (1)    not  = spaces
                     go to acc-cod-mne-600.
           if        v-key                not  = "UP  "
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
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero linea video per codice di pagamento *
      *    *                                                           *
      *    * Input  : w-tes-inx-pag = indice per codice di pagamento   *
      *    *                                                           *
      *    * Output : w-tes-lin-pag = linea  per codice di pagamento   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-lin-pag-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           if        w-tes-inx-pag        =    03
                     move   17            to   w-tes-lin-pag
           else if   w-tes-inx-pag        =    02
                     move   13            to   w-tes-lin-pag
           else      move   09            to   w-tes-lin-pag          .
       det-lin-pag-999.
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
           if        w-tes-cod-fop        =    zero
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
       cnt-tdo-nok-025.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di codici di pagamen- *
      *              * to specificati e degli indici per gli stessi    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-cdp-num      .
           move      zero                 to   w-det-num-cdp-i01      .
           move      zero                 to   w-det-num-cdp-i02      .
           move      zero                 to   w-det-num-cdp-i03      .
           if        w-tes-cod-pag (1, 1) not  = zero
                     add   1              to   w-det-num-cdp-num
                     move  1              to   w-det-num-cdp-ixx
                                              (w-det-num-cdp-num)     .
           if        w-tes-cod-pag (1, 2) not  = zero
                     add   1              to   w-det-num-cdp-num
                     move  2              to   w-det-num-cdp-ixx
                                              (w-det-num-cdp-num)     .
           if        w-tes-cod-pag (1, 3) not  = zero
                     add   1              to   w-det-num-cdp-num
                     move  3              to   w-det-num-cdp-ixx
                                              (w-det-num-cdp-num)     .
       cnt-tdo-nok-030.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di utilizzi per ogni  *
      *              * tipo di ammontare specificato                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-cdp-uta (01) .
           move      zero                 to   w-det-num-cdp-uta (02) .
           move      zero                 to   w-det-num-cdp-uta (03) .
           move      zero                 to   w-det-num-cdp-uta (04) .
           move      zero                 to   w-det-num-cdp-uta (05) .
           move      zero                 to   w-det-num-cdp-uta (06) .
           move      zero                 to   w-det-num-cdp-uta (07) .
           move      zero                 to   w-det-num-cdp-uta (08) .
           move      zero                 to   w-det-num-cdp-uta (09) .
           move      zero                 to   w-det-num-cdp-uta (10) .
           move      zero                 to   w-det-num-cdp-uta (11) .
       cnt-tdo-nok-031.
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-032.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-040.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    zero or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          >    11
                     go to cnt-tdo-nok-032.
           move      w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          to   w-det-num-cdp-jnx      .
           add       1                    to   w-det-num-cdp-uta
                                              (w-det-num-cdp-jnx)     .
           go to     cnt-tdo-nok-032.
       cnt-tdo-nok-040.
      *              *-------------------------------------------------*
      *              * Controllo su Descrizione                        *
      *              *-------------------------------------------------*
           if        w-tes-des-fop (1)    not  = spaces
                     go to cnt-tdo-nok-050.
           move      "Manca la descrizione per la forma di pagamento    
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero di codici di  *
      *              * pagamento specificati                           *
      *              *-------------------------------------------------*
           if        w-det-num-cdp-num    =    zero
                     go to cnt-tdo-nok-075
           else if   w-det-num-cdp-num    =    1
                     go to cnt-tdo-nok-100
           else      go to cnt-tdo-nok-150.
       cnt-tdo-nok-075.
      *              *-------------------------------------------------*
      *              * Se il numero di codici di pagamento specificati *
      *              * e' zero                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessun ulteriore controllo sui codici di    *
      *                  * pagamento : a controlli finali              *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-550.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Se il numero di codici di pagamento specificati *
      *              * e' 1                                            *
      *              *-------------------------------------------------*
       cnt-tdo-nok-110.
      *                  *---------------------------------------------*
      *                  * Controllo sul tipo di ammontare             *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  = zero
                     go to cnt-tdo-nok-112.
           move      "Manca il tipo ammontare per il pagamento          
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-112.
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  > w-exp-tip-amm-num
                     go to cnt-tdo-nok-114.
           move      "Tipo di ammontare per il pagamento errato         
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-114.
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  = 10 and
                     w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  = 11
                     go to cnt-tdo-nok-116.
           move      "Tipo ammontare 'Sul residuo' non ammesso          
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-116.
           go to     cnt-tdo-nok-120.
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Controllo sulla percentuale                 *
      *                  *---------------------------------------------*
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  = 03 and
                     w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          not  = 04
                     go to cnt-tdo-nok-122.
           if        w-tes-per-toi
                    (1, w-det-num-cdp-i01)
                                          not  = zero
                     go to cnt-tdo-nok-122.
           move      "Manca la percentuale per il pagamento             
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-122.
           go to     cnt-tdo-nok-130.
       cnt-tdo-nok-130.
      *                  *---------------------------------------------*
      *                  * Fine controlli per il caso in cui i tipi di *
      *                  * ammontare specificati siano solo uno        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A controllo finali                      *
      *                      *-----------------------------------------*
           go to     cnt-tdo-nok-550.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Se il numero di codici di pagamento specificati *
      *              * e' maggiore di 1                                *
      *              *-------------------------------------------------*
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Controlli intrinseci sui codici di pagamen- *
      *                  * to                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore su numero    *
      *                      * codici di pagamento specificati         *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-170.
      *                      *-----------------------------------------*
      *                      * Incremento contatore su numero codici   *
      *                      * di pagamento specificati                *
      *                      *-----------------------------------------*
           add       1                    to   w-det-num-cdp-ctr      .
      *                      *-----------------------------------------*
      *                      * Se oltre numero codici di pagamento     *
      *                      * specificati : fine controlli intrinseci *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-250.
      *                      *-----------------------------------------*
      *                      * Preparazione indice su codice di paga-  *
      *                      * mento                                   *
      *                      *-----------------------------------------*
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
      *                      *-----------------------------------------*
      *                      * Preparazione componente fissa per il    *
      *                      * literal di messaggio di errore          *
      *                      *-----------------------------------------*
           move      w-det-num-cdp-inx    to   w-err-box-err-e01      .
       cnt-tdo-nok-180.
      *                      *-----------------------------------------*
      *                      * Controlli su tipo di ammontare          *
      *                      *-----------------------------------------*
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = zero
                     go to cnt-tdo-nok-182.
           move      "Manca il tipo ammontare                  "
                                          to   w-err-box-err-e03      .
           move      w-err-box-err-e00    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-182.
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  > w-exp-tip-amm-num
                     go to cnt-tdo-nok-184.
           move      "Tipo di ammontare errato                 "
                                          to   w-err-box-err-e03      .
           move      w-err-box-err-e00    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-184.
           go to     cnt-tdo-nok-190.
       cnt-tdo-nok-190.
      *                      *-----------------------------------------*
      *                      * Controlli sulla percentuale             *
      *                      *-----------------------------------------*
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = 03 and
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = 04
                     go to cnt-tdo-nok-192.
           if        w-tes-per-toi
                    (1, w-det-num-cdp-inx)
                                          not  = zero
                     go to cnt-tdo-nok-192.
           move      "Manca la percentuale                     "
                                          to   w-err-box-err-e03      .
           move      w-err-box-err-e00    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-192.
           go to     cnt-tdo-nok-200.
       cnt-tdo-nok-200.
      *                      *-----------------------------------------*
      *                      * Riciclo a codice pagamento successivo   *
      *                      *-----------------------------------------*
           go to     cnt-tdo-nok-170.
       cnt-tdo-nok-250.
      *                  *---------------------------------------------*
      *                  * Controlli combinativi sui codici di paga-   *
      *                  * mento                                       *
      *                  *---------------------------------------------*
       cnt-tdo-nok-260.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul  *
      *                      * totale documento non sia stato specifi- *
      *                      * cato nemmeno una volta                  *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (01)
                                          not  > zero
                     go to cnt-tdo-nok-270.
           move      "Tipo di ammontare sul 'Totale documento' irregolar
      -              "e     "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-270.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul  *
      *                      * totale imponibile non sia stato speci-  *
      *                      * ficato piu' di una volta                *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (02)
                                          not  > 1
                     go to cnt-tdo-nok-280.
           move      "Tipo di ammontare sul 'Totale imponibile' irregola
      -              "re    "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-280.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul  *
      *                      * totale Iva non sia stato specificato    *
      *                      * piu' di una volta                       *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (05)
                                          not  > 1
                     go to cnt-tdo-nok-290.
           move      "Tipo di ammontare sul 'Totale Iva' irregolare     
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-290.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul- *
      *                      * la quota a forfait non sia stato speci- *
      *                      * ficato piu' di una volta                *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (06)
                                          not  > 1
                     go to cnt-tdo-nok-300.
           move      "Tipo di ammontare su 'Quota a forfait' irregolare 
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul- *
      *                      * l'Iva piu' quota a forfait non sia sta- *
      *                      * to specificato piu' di una volta        *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (07)
                                          not  > 1
                     go to cnt-tdo-nok-310.
           move      "Tipo di ammontare su 'Iva piu' quota forfait' irre
      -              "golare"             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-310.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul  *
      *                      * totale documento meno quota a forfait   *
      *                      * non sia stato specificato piu' di una   *
      *                      * volta                                   *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (08)
                                          not  > 1
                     go to cnt-tdo-nok-320.
           move      "Tipo di ammontare su 'Tot. doc. meno forfait' irre
      -              "golare"             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-320.
      *                      *-----------------------------------------*
      *                      * Controllo che il tipo di ammontare sul  *
      *                      * totale imponibile meno quota a forfait  *
      *                      * non sia stato specificato piu' di una   *
      *                      * volta                                   *
      *                      *-----------------------------------------*
           if        w-det-num-cdp-uta (09)
                                          not  > 1
                     go to cnt-tdo-nok-330.
           move      "Tipo di ammontare su 'Imponib. meno forfait' irreg
      -              "olare "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-330.
      *                      *-----------------------------------------*
      *                      * Controllo che i tipi di ammontare sul   *
      *                      * residuo non siano specificati piu' di   *
      *                      * una volta, e se specificati, che occu-  *
      *                      * pino comunque l'ultima posizione        *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-num-cdp-add      .
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-332.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-334.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    10 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     add   1              to   w-det-num-cdp-add      .
           go to     cnt-tdo-nok-332.
       cnt-tdo-nok-334.
           if        w-det-num-cdp-add    =    zero
                     go to cnt-tdo-nok-338.
           if        w-det-num-cdp-add    >    1
                     go to cnt-tdo-nok-336.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-num)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    10 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     go to  cnt-tdo-nok-338.
       cnt-tdo-nok-336.
           move      "Tipo di ammontare sul 'Residuo' irregolare        
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-338.
           go to     cnt-tdo-nok-340.
       cnt-tdo-nok-340.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore del   *
      *                      * primo tipo di ammontare specificato     *
      *                      *                                         *
      *                      * N.B. Il primo tipo di ammontare non     *
      *                      *      puo' essere tra i seguenti :       *
      *                      *        01 : Totale documento            *
      *                      *        10 : Residuo per totale docum.   *
      *                      *        11 : Residuo per totale impon.   *
      *                      *      in quanto gia' escluso da control- *
      *                      *      li eseguiti precedentemente        *
      *                      *-----------------------------------------*
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    02
                     go to cnt-tdo-nok-350
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    03
                     go to cnt-tdo-nok-365
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    04
                     go to cnt-tdo-nok-380
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    05
                     go to cnt-tdo-nok-395
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    06
                     go to cnt-tdo-nok-410
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    07
                     go to cnt-tdo-nok-425
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    08
                     go to cnt-tdo-nok-440
           else if   w-tes-tip-amm
                    (1, w-det-num-cdp-i01)
                                          =    09
                     go to cnt-tdo-nok-455
           else      go to cnt-tdo-nok-500.
       cnt-tdo-nok-350.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 02 : Il totale imponibile               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-351.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-352.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    06 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    09 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-351.
       cnt-tdo-nok-352.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamen-  *
      *                          * te, che possono essere solo :       *
      *                          * - 05 : Il totale Iva                *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * ne sia stato specificato uno solo   *
      *                          *-------------------------------------*
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-500.
       cnt-tdo-nok-365.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 03 : Una % del totale documento         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 05 : Il totale Iva                *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-366.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-367.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    05 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    06 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    09 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-366.
       cnt-tdo-nok-367.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati, che possono    *
      *                          * essere solo :                       *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * le specifiche date siano tali per   *
      *                          * cui la percentuale globale non su-  *
      *                          * peri il 100%                        *
      *                          *-------------------------------------*
           move      zero                 to   w-det-num-cdp-tpe      .
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-368.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-369.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = 03
                     go to cnt-tdo-nok-368.
           add       w-tes-per-toi
                    (1, w-det-num-cdp-inx)
                                          to   w-det-num-cdp-tpe      .
           go to     cnt-tdo-nok-368.
       cnt-tdo-nok-369.
           if        w-det-num-cdp-tpe    >    100,0
                     go to cnt-tdo-nok-530.
       cnt-tdo-nok-370.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * la percentuale sia esattamente pa-  *
      *                          * ri al 100%, il tipo di ammontare :  *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * non sia presente                    *
      *                          *-------------------------------------*
           if        w-det-num-cdp-tpe    <    100,0
                     go to cnt-tdo-nok-371.
           if        w-det-num-cdp-uta (10)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-371.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-380.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 04 : Una % del totale imponibile        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-381.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-382.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    06 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    09
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-381.
       cnt-tdo-nok-382.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati, che possono    *
      *                          * essere solo :                       *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 05 : Il totale Iva                *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * le specifiche date siano tali per   *
      *                          * cui la percentuale globale non su-  *
      *                          * peri il 100%                        *
      *                          *-------------------------------------*
           move      zero                 to   w-det-num-cdp-tpe      .
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-383.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-384.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = 04
                     go to cnt-tdo-nok-383.
           add       w-tes-per-toi
                    (1, w-det-num-cdp-inx)
                                          to   w-det-num-cdp-tpe      .
           go to     cnt-tdo-nok-383.
       cnt-tdo-nok-384.
           if        w-det-num-cdp-tpe    >    100,0
                     go to cnt-tdo-nok-530.
       cnt-tdo-nok-385.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * la percentuale sia esattamente pa-  *
      *                          * ri al 100%, il tipo di ammontare :  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * non sia presente                    *
      *                          *-------------------------------------*
           if        w-det-num-cdp-tpe    <    100,0
                     go to cnt-tdo-nok-386.
           if        w-det-num-cdp-uta (11)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-386.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-395.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 05 : Il totale iva                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 08 : Totale documento meno forf.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-396.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-397.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-396.
       cnt-tdo-nok-397.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati, che possono    *
      *                          * essere solo :                       *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * le specifiche date siano tali per   *
      *                          * cui la percentuale globale non su-  *
      *                          * peri il 100%                        *
      *                          *-------------------------------------*
           move      zero                 to   w-det-num-cdp-tpe      .
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-398.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-399.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          not  = 04
                     go to cnt-tdo-nok-398.
           add       w-tes-per-toi
                    (1, w-det-num-cdp-inx)
                                          to   w-det-num-cdp-tpe      .
           go to     cnt-tdo-nok-398.
       cnt-tdo-nok-399.
           if        w-det-num-cdp-tpe    >    100,0
                     go to cnt-tdo-nok-530.
       cnt-tdo-nok-400.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * la percentuale sia esattamente pa-  *
      *                          * ri al 100%, il tipo di ammontare :  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * non sia presente                    *
      *                          *-------------------------------------*
           if        w-det-num-cdp-tpe    <    100,0
                     go to cnt-tdo-nok-401.
           if        w-det-num-cdp-uta (11)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-401.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * tra i tipi di ammontare specifica-  *
      *                          * ti successivamente compaia il tipo  *
      *                          * - 02 : Il totale imponibile         *
      *                          * non ne compaia nessun altro         *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (02)
                                          =    zero
                     go to cnt-tdo-nok-402.
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-402.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * tra i tipi di ammontare specifica-  *
      *                          * ti successivamente compaia il tipo  *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * non ne compaia nessun altro tra :   *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (04)
                                          =    zero
                     go to cnt-tdo-nok-403.
           if        w-det-num-cdp-uta (06)
                                          >    zero or
                     w-det-num-cdp-uta (09)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-403.
      *                          *-------------------------------------*
      *                          * Si controlla che, nel caso in cui   *
      *                          * tra i tipi di ammontare specifica-  *
      *                          * ti successivamente compaia il tipo  *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * non ne compaia nessun altro tra :   *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (06)
                                          =    zero
                     go to cnt-tdo-nok-404.
           if        w-det-num-cdp-uta (11)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-404.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-410.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 06 : Solo una quota a forfait           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 05 : Il totale Iva                *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-411.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-412.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    05 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-411.
       cnt-tdo-nok-412.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamen-  *
      *                          * te, che possono essere solo :       *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * sia rispettato quanto segue         *
      *                          *-------------------------------------*
       cnt-tdo-nok-413.
      *                          *-------------------------------------*
      *                          * Se e' specificato il tipo di ammon- *
      *                          * tare                                *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * non deve essere specificato nessun  *
      *                          * altro tipo di ammontare             *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (08)
                                          =    zero
                     go to cnt-tdo-nok-414.
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-414.
      *                          *-------------------------------------*
      *                          * Se e' specificato il tipo di ammon- *
      *                          * tare                                *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * non deve essere specificato nessun  *
      *                          * altro tipo di ammontare, tranne     *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (09)
                                          =    zero
                     go to cnt-tdo-nok-415.
           if        w-det-num-cdp-num    =    2
                     go to cnt-tdo-nok-415.
           if        w-det-num-cdp-uta (10)
                                          =    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-415.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-425.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 07 : Tot. Iva piu' quota a forfait      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 05 : Il totale Iva                *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 08 : Totale documento meno forf.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-426.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-427.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    05 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    06 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-426.
       cnt-tdo-nok-427.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamen-  *
      *                          * te, che possono essere solo :       *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * sia rispettato quanto segue         *
      *                          *-------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo uno dei due tipi di ammontare  *
      *                          * puo' essere specificato, e cio' e-  *
      *                          * sclude automaticamente la presenza  *
      *                          * dell'altro tipo di ammontare        *
      *                          *-------------------------------------*
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-440.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 08 : Totale documento meno forfait      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 05 : Il totale Iva                *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 09 : Totale imponib.  meno forf.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-441.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-442.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    05 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    07 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    09 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    11
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-441.
       cnt-tdo-nok-442.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamen-  *
      *                          * te, che possono essere solo :       *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * sia rispettato quanto segue         *
      *                          *-------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo uno dei due tipi di ammontare  *
      *                          * puo' essere specificato, e cio' e-  *
      *                          * sclude automaticamente la presenza  *
      *                          * dell'altro tipo di ammontare        *
      *                          *-------------------------------------*
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-455.
      *                      *-----------------------------------------*
      *                      * Se il primo tipo di ammontare e' :      *
      *                      * 09 : Totale imponibile meno forfait     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamente *
      *                          * non ne compaia nessuno tra :        *
      *                          * - 02 : Il totale imponibile         *
      *                          * - 03 : Una % del totale documento   *
      *                          * - 04 : Una % del totale imponibile  *
      *                          * - 08 : Totale documento meno forf.  *
      *                          *-------------------------------------*
           move      1                    to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-456.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-457.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    02 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    03 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    04 or
                     w-tes-tip-amm
                    (1, w-det-num-cdp-inx)
                                          =    08
                     go to cnt-tdo-nok-520
           else      go to cnt-tdo-nok-456.
       cnt-tdo-nok-457.
      *                          *-------------------------------------*
      *                          * Si controlla che tra i tipi di am-  *
      *                          * montare specificati successivamen-  *
      *                          * te, che possono essere solo :       *
      *                          * - 05 : Il totale Iva                *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * - 10 : Il residuo per il tot. doc.  *
      *                          * - 11 : Il residuo per il tot. imp.  *
      *                          * sia rispettato quanto segue         *
      *                          *-------------------------------------*
       cnt-tdo-nok-458.
      *                          *-------------------------------------*
      *                          * Se e' specificato il tipo di ammon- *
      *                          * tare                                *
      *                          * - 06 : Solo una quota a forfait     *
      *                          * non deve essere specificato anche   *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (06)
                                          =    zero
                     go to cnt-tdo-nok-459.
           if        w-det-num-cdp-uta (07)
                                          >    zero
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-459.
      *                          *-------------------------------------*
      *                          * Se e' specificato il tipo di ammon- *
      *                          * tare                                *
      *                          * - 07 : Totale Iva piu' forfait      *
      *                          * non deve essere specificato nessun  *
      *                          * altro tipo di ammontare             *
      *                          *-------------------------------------*
           if        w-det-num-cdp-uta (07)
                                          =    zero
                     go to cnt-tdo-nok-460.
           if        w-det-num-cdp-num    >    2
                     go to cnt-tdo-nok-520.
       cnt-tdo-nok-460.
           go to     cnt-tdo-nok-500.
       cnt-tdo-nok-500.
      *                  *---------------------------------------------*
      *                  * Controllo che la diminuzione acconto non    *
      *                  * sia specificata piu' di una volta           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-num-cdp-add      .
           move      zero                 to   w-det-num-cdp-ctr      .
       cnt-tdo-nok-502.
           add       1                    to   w-det-num-cdp-ctr      .
           if        w-det-num-cdp-ctr    >    w-det-num-cdp-num
                     go to cnt-tdo-nok-504.
           move      w-det-num-cdp-ixx
                    (w-det-num-cdp-ctr)   to   w-det-num-cdp-inx      .
           if        w-tes-dim-act
                    (1, w-det-num-cdp-inx)
                                          =    02
                     add   1              to   w-det-num-cdp-add      .
           go to     cnt-tdo-nok-502.
       cnt-tdo-nok-504.
           if        w-det-num-cdp-add    <    2
                     go to cnt-tdo-nok-506.
           move      "Detrazione acconto specificata piu' di una volta  
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-506.
           go to     cnt-tdo-nok-510.
       cnt-tdo-nok-510.
      *                  *---------------------------------------------*
      *                  * Fine controlli per il caso in cui i tipi di *
      *                  * ammontare specificati siano piu' di 1       *
      *                  *---------------------------------------------*
      *                          *-------------------------------------*
      *                          * A controlli finali                  *
      *                          *-------------------------------------*
           go to     cnt-tdo-nok-550.
       cnt-tdo-nok-520.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore per in-   *
      *                      * compatibilita' su tipi di ammontare     *
      *                      *-----------------------------------------*
           move      "Incompatibilita' tra i tipi di ammontare specifica
      -              "ti    "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-530.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore per per-  *
      *                      * centuale globale superiore al 100%      *
      *                      *-----------------------------------------*
           move      "La percentuale globale sui pagamenti supera il 100
      -              "%     "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-550.
      *              *-------------------------------------------------*
      *              * Controlli finali                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che il numero scadenze totali co- *
      *                  * munque non superi le 96 scadenze            *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-num-sca-tot      .
           if        w-tes-cod-pag (1, 1) not  = zero
                     add   w-tes-cod-pag-nsc
                          (1, 1)          to   w-det-num-sca-tot      .
           if        w-tes-cod-pag (1, 2) not  = zero
                     add   w-tes-cod-pag-nsc
                          (1, 2)          to   w-det-num-sca-tot      .
           if        w-tes-cod-pag (1, 3) not  = zero
                     add   w-tes-cod-pag-nsc
                          (1, 3)          to   w-det-num-sca-tot      .
           if        w-det-num-sca-tot    not  > w-det-num-sca-max
                     go to cnt-tdo-nok-555.
           move      "Il numero totale di scadenze non puo' essere > di 
      -              "96    "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-555.
      *                  *---------------------------------------------*
      *                  * Controllo che non sia citato piu' di un co- *
      *                  * dice di pagamento con decorrenza della pri- *
      *                  * ma scadenza manuale                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-num-dsm-tot      .
           if        w-tes-cod-pag-dps
                    (1, 1)                =    01
                     add   1              to   w-det-num-dsm-tot      .
           if        w-tes-cod-pag-dps
                    (1, 2)                =    01
                     add   1              to   w-det-num-dsm-tot      .
           if        w-tes-cod-pag-dps
                    (1, 3)                =    01
                     add   1              to   w-det-num-dsm-tot      .
           if        w-det-num-dsm-tot    not  > 1
                     go to cnt-tdo-nok-575.
           move      "Non e' ammesso piu' di un pagamento con scadenza m
      -              "anuale"             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-575.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se obbligatorio        *
      *              *-------------------------------------------------*
           if        w-prs-rec-yfp-omn    not  = "O"
                     go to cnt-tdo-nok-577.
           if        w-tes-cod-mne (1)    not  = spaces
                     go to cnt-tdo-nok-577.
           move      "Manca il codice mnemonico                         
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-577.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se unico               *
      *              *-------------------------------------------------*
           if        w-prs-rec-yfp-umn    not  = "U"
                     go to cnt-tdo-nok-583.
           if        w-tes-cod-mne (1)    =    spaces
                     go to cnt-tdo-nok-583.
       cnt-tdo-nok-579.
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-mne (1)    to   rf-yfp-cod-mne         .
           move      zero                 to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-583.
       cnt-tdo-nok-581.
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-583.
           if        rf-yfp-cod-mne       not  = w-tes-cod-mne (1)
                     go to cnt-tdo-nok-583.
           if        rf-yfp-cod-fop       =    w-tes-cod-fop
                     go to cnt-tdo-nok-581.
           move      "Codice mnemonico gia' esistente                   
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-583.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se non modificabile    *
      *              *-------------------------------------------------*
           if        w-prs-rec-yfp-mmn    not  = "N"
                     go to cnt-tdo-nok-600.
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to cnt-tdo-nok-600.
           if        w-tes-cod-mne (2)    =    spaces
                     go to cnt-tdo-nok-600.
           if        w-tes-cod-mne (1)    =    w-tes-cod-mne (2)
                     go to cnt-tdo-nok-600.
           move      "Il codice mnemonico non puo' essere modificato    
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-625.
      *                  *---------------------------------------------*
      *                  * 1. codice di pagamento                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag (1, 1) =    zero
                     move  zero           to   w-tes-tip-amm (1, 1)
                     move  zero           to   w-tes-per-toi (1, 1)
                     move  zero           to   w-tes-dim-act (1, 1)
                     go to cnt-tdo-nok-650.
           if        w-tes-tip-amm (1, 1) not  = 03 and
                     w-tes-tip-amm (1, 1) not  = 04
                     move  zero           to   w-tes-per-toi (1, 1)   .
           if        w-tes-dim-act (1, 1) not  = 02
                     move  01             to   w-tes-dim-act (1, 1)   .
       cnt-tdo-nok-650.
      *                  *---------------------------------------------*
      *                  * 2. codice di pagamento                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag (1, 2) =    zero
                     move  zero           to   w-tes-tip-amm (1, 2)
                     move  zero           to   w-tes-per-toi (1, 2)
                     move  zero           to   w-tes-dim-act (1, 2)
                     go to cnt-tdo-nok-675.
           if        w-tes-tip-amm (1, 2) not  = 03 and
                     w-tes-tip-amm (1, 2) not  = 04
                     move  zero           to   w-tes-per-toi (1, 2)   .
           if        w-tes-dim-act (1, 2) not  = 02
                     move  01             to   w-tes-dim-act (1, 2)   .
       cnt-tdo-nok-675.
      *                  *---------------------------------------------*
      *                  * 3. codice di pagamento                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag (1, 3) =    zero
                     move  zero           to   w-tes-tip-amm (1, 3)
                     move  zero           to   w-tes-per-toi (1, 3)
                     move  zero           to   w-tes-dim-act (1, 3)
                     go to cnt-tdo-nok-700.
           if        w-tes-tip-amm (1, 3) not  = 03 and
                     w-tes-tip-amm (1, 3) not  = 04
                     move  zero           to   w-tes-per-toi (1, 3)   .
           if        w-tes-dim-act (1, 3) not  = 02
                     move  01             to   w-tes-dim-act (1, 3)   .
       cnt-tdo-nok-700.
      *                  *---------------------------------------------*
      *                  * Compattamento dei codici di pagamento nel-  *
      *                  * la tabella                                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag (1, 1) =    zero and
                     w-tes-cod-pag (1, 2) =    zero and
                     w-tes-cod-pag (1, 3) =    zero
                     go to cnt-tdo-nok-725.
           if        w-tes-cod-pag (1, 1) not  = zero and
                     w-tes-cod-pag (1, 2) =    zero   and
                     w-tes-cod-pag (1, 3) =    zero
                     go to cnt-tdo-nok-725.
           if        w-tes-cod-pag (1, 1) not  = zero and
                     w-tes-cod-pag (1, 2) not  = zero and
                     w-tes-cod-pag (1, 3) =    zero
                     go to cnt-tdo-nok-725.
           if        w-tes-cod-pag (1, 1) not  = zero and
                     w-tes-cod-pag (1, 2) not  = zero and
                     w-tes-cod-pag (1, 3) not  = zero
                     go to cnt-tdo-nok-725.
           if        w-tes-cod-pag (1, 1) =    zero
                     move  w-tes-cod-pag (1, 2)
                                          to   w-tes-cod-pag (1, 1)
                     move  w-tes-tip-amm (1, 2)
                                          to   w-tes-tip-amm (1, 1)
                     move  w-tes-per-toi (1, 2)
                                          to   w-tes-per-toi (1, 1)
                     move  w-tes-dim-act (1, 2)
                                          to   w-tes-dim-act (1, 1)
                     move  w-tes-cod-pag (1, 3)
                                          to   w-tes-cod-pag (1, 2)
                     move  w-tes-tip-amm (1, 3)
                                          to   w-tes-tip-amm (1, 2)
                     move  w-tes-per-toi (1, 3)
                                          to   w-tes-per-toi (1, 2)
                     move  w-tes-dim-act (1, 3)
                                          to   w-tes-dim-act (1, 2)
                     move  zero           to   w-tes-cod-pag (1, 3)
                     move  zero           to   w-tes-tip-amm (1, 3)
                     move  zero           to   w-tes-per-toi (1, 3)
                     move  zero           to   w-tes-dim-act (1, 3)   .
           if        w-tes-cod-pag (1, 2) =    zero
                     move  w-tes-cod-pag (1, 3)
                                          to   w-tes-cod-pag (1, 2)
                     move  w-tes-tip-amm (1, 3)
                                          to   w-tes-tip-amm (1, 2)
                     move  w-tes-per-toi (1, 3)
                                          to   w-tes-per-toi (1, 2)
                     move  w-tes-dim-act (1, 3)
                                          to   w-tes-dim-act (1, 2)
                     move  zero           to   w-tes-cod-pag (1, 3)
                     move  zero           to   w-tes-tip-amm (1, 3)
                     move  zero           to   w-tes-per-toi (1, 3)
                     move  zero           to   w-tes-dim-act (1, 3)   .
       cnt-tdo-nok-725.
      *                  *---------------------------------------------*
      *                  * Fine normalizzazioni                        *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-800.
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
           move      zero                 to   w-tes-cod-fop          .
           move      spaces               to   w-tes-cod-fop-aut      .
           move      zero                 to   w-tes-inx-pag          .
           move      zero                 to   w-tes-lin-pag          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-fop (1)      .
           move      zero                 to   w-tes-sco-fat (1)      .
           move      zero                 to   w-tes-cod-pag (1, 1)   .
           move      spaces               to   w-tes-cod-pag-des
                                              (1, 1)                  .
           move      zero                 to   w-tes-cod-pag-nsc
                                              (1, 1)                  .
           move      zero                 to   w-tes-cod-pag-dps
                                              (1, 1)                  .
           move      zero                 to   w-tes-tip-amm (1, 1)   .
           move      zero                 to   w-tes-per-toi (1, 1)   .
           move      zero                 to   w-tes-dim-act (1, 1)   .
           move      zero                 to   w-tes-cod-pag (1, 2)   .
           move      spaces               to   w-tes-cod-pag-des
                                              (1, 2)                  .
           move      zero                 to   w-tes-cod-pag-nsc
                                              (1, 2)                  .
           move      zero                 to   w-tes-cod-pag-dps
                                              (1, 2)                  .
           move      zero                 to   w-tes-tip-amm (1, 2)   .
           move      zero                 to   w-tes-per-toi (1, 2)   .
           move      zero                 to   w-tes-dim-act (1, 2)   .
           move      zero                 to   w-tes-cod-pag (1, 3)   .
           move      spaces               to   w-tes-cod-pag-des
                                              (1, 3)                  .
           move      zero                 to   w-tes-cod-pag-nsc
                                              (1, 3)                  .
           move      zero                 to   w-tes-cod-pag-dps
                                              (1, 3)                  .
           move      zero                 to   w-tes-tip-amm (1, 3)   .
           move      zero                 to   w-tes-per-toi (1, 3)   .
           move      zero                 to   w-tes-dim-act (1, 3)   .
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
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      w-tes-cod-fop        to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
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
           move      rf-yfp-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-yfp-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-yfp-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-yfp-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-yfp-des-key       to   w-tes-des-key (1)      .
           move      rf-yfp-des-fop       to   w-tes-des-fop (1)      .
           move      rf-yfp-sco-fat       to   w-tes-sco-fat (1)      .
           move      rf-yfp-cod-pag (1)   to   w-tes-cod-pag (1, 1)   .
           move      rf-yfp-tip-amm (1)   to   w-tes-tip-amm (1, 1)   .
           move      rf-yfp-per-toi (1)   to   w-tes-per-toi (1, 1)   .
           move      rf-yfp-dim-act (1)   to   w-tes-dim-act (1, 1)   .
           move      rf-yfp-cod-pag (2)   to   w-tes-cod-pag (1, 2)   .
           move      rf-yfp-tip-amm (2)   to   w-tes-tip-amm (1, 2)   .
           move      rf-yfp-per-toi (2)   to   w-tes-per-toi (1, 2)   .
           move      rf-yfp-dim-act (2)   to   w-tes-dim-act (1, 2)   .
           move      rf-yfp-cod-pag (3)   to   w-tes-cod-pag (1, 3)   .
           move      rf-yfp-tip-amm (3)   to   w-tes-tip-amm (1, 3)   .
           move      rf-yfp-per-toi (3)   to   w-tes-per-toi (1, 3)   .
           move      rf-yfp-dim-act (3)   to   w-tes-dim-act (1, 3)   .
           move      rf-yfp-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-200.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [yfp]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * 1. codice di pagamento : de-    *
      *                              *    scrizione, numero scadenze,  *
      *                              *    decorrenza prima scadenza    *
      *                              *---------------------------------*
           move      w-tes-cod-pag (1, 1) to   w-let-arc-zpg-cod      .
           perform   let-arc-zpg-000      thru let-arc-zpg-999        .
           move      w-let-arc-zpg-des    to   w-tes-cod-pag-des
                                              (1, 1)                  .
           move      w-let-arc-zpg-nsc    to   w-tes-cod-pag-nsc
                                              (1, 1)                  .
           move      w-let-arc-zpg-dps    to   w-tes-cod-pag-dps
                                              (1, 1)                  .
      *                              *---------------------------------*
      *                              * 2. codice di pagamento : de-    *
      *                              *    scrizione, numero scadenze,  *
      *                              *    decorrenza prima scadenza    *
      *                              *---------------------------------*
           move      w-tes-cod-pag (1, 2) to   w-let-arc-zpg-cod      .
           perform   let-arc-zpg-000      thru let-arc-zpg-999        .
           move      w-let-arc-zpg-des    to   w-tes-cod-pag-des
                                              (1, 2)                  .
           move      w-let-arc-zpg-nsc    to   w-tes-cod-pag-nsc
                                              (1, 2)                  .
           move      w-let-arc-zpg-dps    to   w-tes-cod-pag-dps
                                              (1, 2)                  .
      *                              *---------------------------------*
      *                              * 3. codice di pagamento : de-    *
      *                              *    scrizione, numero scadenze,  *
      *                              *    decorrenza prima scadenza    *
      *                              *---------------------------------*
           move      w-tes-cod-pag (1, 3) to   w-let-arc-zpg-cod      .
           perform   let-arc-zpg-000      thru let-arc-zpg-999        .
           move      w-let-arc-zpg-des    to   w-tes-cod-pag-des
                                              (1, 3)                  .
           move      w-let-arc-zpg-nsc    to   w-tes-cod-pag-nsc
                                              (1, 3)                  .
           move      w-let-arc-zpg-dps    to   w-tes-cod-pag-dps
                                              (1, 3)                  .
       rou-let-reg-300.
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
      *              *-------------------------------------------------*
      *              * Se e' stata eseguita l'attribuzione del codice  *
      *              * in automatico, si ripristina, se possibile, il  *
      *              * codice al valore precedente l'incremento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non attribuzione automatica : uscita     *
      *                  *---------------------------------------------*
           if        w-tes-cod-fop-aut    =    spaces
                     go to pos-exi-ins-999.
      *                  *---------------------------------------------*
      *                  * Ripristino codice automatico progressivo    *
      *                  *---------------------------------------------*
           perform   rip-cod-aut-000      thru rip-cod-aut-999        .
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
      *              * Trattamento file [yfp]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [yfp]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-yfp-000      thru wrt-rec-yfp-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [yfp]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-yfp-000      thru rew-rec-yfp-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [yfp]                             *
      *              *-------------------------------------------------*
           perform   del-rec-yfp-000      thru del-rec-yfp-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [yfp]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-yfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop        to   rf-yfp-cod-fop         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-yfp-ide-dat         .
           move      s-ute                to   rf-yfp-ide-ute         .
           move      s-fas                to   rf-yfp-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-yfp-cod-mne         .
           move      w-tes-des-key (1)    to   rf-yfp-des-key         .
           move      w-tes-des-fop (1)    to   rf-yfp-des-fop         .
           move      w-tes-sco-fat (1)    to   rf-yfp-sco-fat         .
           move      w-tes-cod-pag (1, 1) to   rf-yfp-cod-pag (1)     .
           move      w-tes-tip-amm (1, 1) to   rf-yfp-tip-amm (1)     .
           move      w-tes-per-toi (1, 1) to   rf-yfp-per-toi (1)     .
           move      w-tes-dim-act (1, 1) to   rf-yfp-dim-act (1)     .
           move      w-tes-cod-pag (1, 2) to   rf-yfp-cod-pag (2)     .
           move      w-tes-tip-amm (1, 2) to   rf-yfp-tip-amm (2)     .
           move      w-tes-per-toi (1, 2) to   rf-yfp-per-toi (2)     .
           move      w-tes-dim-act (1, 2) to   rf-yfp-dim-act (2)     .
           move      w-tes-cod-pag (1, 3) to   rf-yfp-cod-pag (3)     .
           move      w-tes-tip-amm (1, 3) to   rf-yfp-tip-amm (3)     .
           move      w-tes-per-toi (1, 3) to   rf-yfp-per-toi (3)     .
           move      w-tes-dim-act (1, 3) to   rf-yfp-dim-act (3)     .
           move      w-tes-alx-exp (1)    to   rf-yfp-alx-exp         .
       cmp-rec-yfp-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [yfp]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-yfp-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yfp-000      thru cmp-rec-yfp-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
       wrt-rec-yfp-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [yfp]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-yfp-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yfp-000      thru cmp-rec-yfp-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
       rew-rec-yfp-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [yfp]                                *
      *    *-----------------------------------------------------------*
       del-rec-yfp-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yfp-000      thru cmp-rec-yfp-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
       del-rec-yfp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zpg]                             *
      *    *-----------------------------------------------------------*
       let-arc-zpg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpg-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zpg-cod    =    zero
                     go to let-arc-zpg-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPAG    "         to   f-key                  .
           move      w-let-arc-zpg-cod    to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zpg-400.
       let-arc-zpg-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zpg-des-pag       to   w-let-arc-zpg-des      .
           move      rf-zpg-num-sca       to   w-let-arc-zpg-nsc      .
           move      rf-zpg-dec-prs       to   w-let-arc-zpg-dps      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zpg-999.
       let-arc-zpg-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zpg-flg      .
           move      all   "."            to   w-let-arc-zpg-des      .
           go to     let-arc-zpg-600.
       let-arc-zpg-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpg-des      .
       let-arc-zpg-600.
           move      zero                 to   w-let-arc-zpg-nsc      .
           move      zero                 to   w-let-arc-zpg-dps      .
       let-arc-zpg-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [yfp]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to att-cod-aut-400.
       att-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "Ep"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      s-num                to   w-enc-yfp-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-yfp-val-pre    to   w-enc-yfp-val-pos      .
           add       1                    to   w-enc-yfp-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-yfp-val-pos    =    zero
                     move  1              to   w-enc-yfp-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-yfp-val-pos    >    w-enc-yfp-val-max
                     move  1              to   w-enc-yfp-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      w-enc-yfp-val-pos    to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
           if        f-sts                =    e-not-err
                     go to att-cod-aut-600
           else      go to att-cod-aut-700.
       att-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-yfp-val-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-cod-aut-500.
       att-cod-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           move      w-enc-yfp-val-pos    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to att-cod-aut-000.
       att-cod-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice automatico progressivo       *
      *    *-----------------------------------------------------------*
       rip-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [yfp]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to rip-cod-aut-400.
       rip-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        s-num                =    w-enc-yfp-val-pos
                     go to rip-cod-aut-600.
       rip-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "Er"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento del codice automatico con *
      *                      * il valore pre incremento                *
      *                      *-----------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "yfp "               to   s-nam                  .
           move      w-enc-yfp-val-pre    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to rip-cod-aut-000.
       rip-cod-aut-999.
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
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice di pagamento    *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzpg0.acs"                   .
