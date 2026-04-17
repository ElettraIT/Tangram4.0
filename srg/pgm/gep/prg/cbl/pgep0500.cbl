       Identification Division.
       Program-Id.                                 pgep0500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    tab                 *
      *                                   Fase:    gep050              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/05/91    *
      *                       Ultima revisione:    NdK del 29/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella codici di pagamento        *
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
                     "gep"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "gep050"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pgep0500"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      GESTIONE CODICI DI PAGAMENTO      "       .

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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
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
               10  w-tes-cod-pag          pic  9(07)                  .
               10  w-tes-cod-pag-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-pag          pic  x(40)                  .
               10  w-tes-tip-pag          pic  9(02)                  .
               10  w-tes-num-sca          pic  9(02)                  .
               10  w-tes-dec-prs          pic  9(02)                  .
               10  w-tes-dap-mes          pic  9(02)                  .
               10  w-tes-dap-gio          pic  9(02)                  .
               10  w-tes-ggg-int          pic  9(02)                  .
               10  w-tes-tip-scm          pic  9(02)                  .
               10  w-tes-gio-scm          pic  9(02)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 98  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative al record codice pagamento *
      *        *-------------------------------------------------------*
           05  w-prs-rec-zpg.
      *            *---------------------------------------------------*
      *            * Valore massimo accettabile per il codice          *
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-zpg-mco      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzionamento codice, in creazione           *
      *            * - M : Manuale                                     *
      *            * - A : Automatico                                  *
      *            *---------------------------------------------------*
               10  w-prs-rec-zpg-fco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del mnemonico                     *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-zpg-omn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Unicita' del mnemonico                            *
      *            * - N : Non necessariamente unico, si' duplicati    *
      *            * - U : Unico, duplicati non ammessi                *
      *            *---------------------------------------------------*
               10  w-prs-rec-zpg-umn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del mnemonico                     *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-zpg-mmn      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det su giorno prefissato massimo nel mese    *
      *        *-------------------------------------------------------*
           05  w-det-dap-gio.
               10  w-det-dap-gio-tbl.
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 28         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
               10  w-det-dap-gio-tbr redefines
                   w-det-dap-gio-tbl.
                   15  w-det-dap-gio-tbx occurs 12
                                          pic  9(02)                  .
               10  w-det-dap-gio-inx      pic  9(02)                  .
               
      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di pagamento                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-pag.
               10  w-exp-tip-pag-num      pic  9(02)       value 11   .
               10  w-exp-tip-pag-lun      pic  9(02)       value 30   .
               10  w-exp-tip-pag-tbl.
                   15  filler             pic  x(30) value
                            "Rimessa diretta               "          .
                   15  filler             pic  x(30) value
                            "Incasso elettronico           "          .
                   15  filler             pic  x(30) value
                            "Ri.Ba.                        "          .
                   15  filler             pic  x(30) value
                            "C.d.O.                        "          .
                   15  filler             pic  x(30) value
                            "M.Av.                         "          .
                   15  filler             pic  x(30) value
                            "R.I.D.                        "          .
                   15  filler             pic  x(30) value
                            "Bonifico bancario             "          .
                   15  filler             pic  x(30) value
                            "C/C postale                   "          .
                   15  filler             pic  x(30) value
                            "Ricevuta bancaria             "          .
                   15  filler             pic  x(30) value
                            "Tratta                        "          .
                   15  filler             pic  x(30) value
                            "Paghero' cambiario            "          .
      *        *-------------------------------------------------------*
      *        * Work per : Decorrenza prima scadenza                  *
      *        *-------------------------------------------------------*
           05  w-exp-dec-prs.
               10  w-exp-dec-prs-num      pic  9(02)       value 12   .
               10  w-exp-dec-prs-lun      pic  9(02)       value 30   .
               10  w-exp-dec-prs-tbl.
                   15  filler             pic  x(30) value
                            "Manuale                       "          .
                   15  filler             pic  x(30) value
                            "Ad una data prefissata (gg/mm)"          .
                   15  filler             pic  x(30) value
                            "A vista                       "          .
                   15  filler             pic  x(30) value
                            "A 15 giorni                   "          .
                   15  filler             pic  x(30) value
                            "A 30 giorni                   "          .
                   15  filler             pic  x(30) value
                            "A 45 giorni                   "          .
                   15  filler             pic  x(30) value
                            "A 60 giorni                   "          .
                   15  filler             pic  x(30) value
                            "A 90 giorni                   "          .
                   15  filler             pic  x(30) value
                            "A 120 giorni                  "          .
                   15  filler             pic  x(30) value
                            "A 150 giorni                  "          .
                   15  filler             pic  x(30) value
                            "A 180 giorni                  "          .
                   15  filler             pic  x(30) value
                            "A 'n' giorni                  "          .
      *        *-------------------------------------------------------*
      *        * Work per : Giorni di intervallo tra due scadenze      *
      *        *-------------------------------------------------------*
           05  w-exp-ggg-int.
               10  w-exp-ggg-int-num      pic  9(02)       value 2    .
               10  w-exp-ggg-int-lun      pic  9(02)       value 40   .
               10  w-exp-ggg-int-tbl.
                   15  filler             pic  x(40) value
                            "Di 30 giorni in 30 giorni               ".
                   15  filler             pic  x(40) value
                            "Di 15 giorni in 15 giorni               ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di scadenza nel mese                  *
      *        *-------------------------------------------------------*
           05  w-exp-tip-scm.
               10  w-exp-tip-scm-num      pic  9(02)       value 4    .
               10  w-exp-tip-scm-lun      pic  9(02)       value 40   .
               10  w-exp-tip-scm-tbl.
                   15  filler             pic  x(40) value
                            "Giorno di decorrenza                    ".
                   15  filler             pic  x(40) value
                            "Fine mese                               ".
                   15  filler             pic  x(40) value
                            "Giorno fisso nel mese                   ".
                   15  filler             pic  x(40) value
                            "Giorno fisso, successivo al fine mese   ".

      *    *===========================================================*
      *    * Work-area per accettazioni                                *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione giorno e mese della data     *
      *        * prefissata per accettazione di 'n' giorni             *
      *        *                                                       *
      *        * N.B.: Il mese contiene le centinaia, il giorno le de- *
      *        *       cine                                            *
      *        *-------------------------------------------------------*
           05  w-acc-nnn-gio              pic  9(04)                  .
           05  w-acc-nnn-gio-r redefines
               w-acc-nnn-gio.
               10  w-acc-nnn-gio-mes      pic  9(02)                  .
               10  w-acc-nnn-gio-gio      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice di pagamento            *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzpg0.acl"                   .

      *    *===========================================================*
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Tipo di pagamento                *
      *        *-------------------------------------------------------*
           05  w-sav-tip-pag              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Numero di scadenze               *
      *        *-------------------------------------------------------*
           05  w-sav-num-sca              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Decorrenza prima scadenza        *
      *        *-------------------------------------------------------*
           05  w-sav-dec-prs              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Data prefissata, mese            *
      *        *-------------------------------------------------------*
           05  w-sav-dap-mes              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Data prefissata, giorno          *
      *        *-------------------------------------------------------*
           05  w-sav-dap-gio              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio 'n' giorni                       *
      *        *-------------------------------------------------------*
           05  w-sav-nnn-gio              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Giorni di intervallo tra due     *
      *        * denze                                                 *
      *        *-------------------------------------------------------*
           05  w-sav-ggg-int              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Tipo di scadenza nel mese        *
      *        *-------------------------------------------------------*
           05  w-sav-tip-scm              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Giorno fisso di scadenza nel me- *
      *        * se                                                    *
      *        *-------------------------------------------------------*
           05  w-sav-gio-scm              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-zpg.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-zpg-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-zpg-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-zpg-val-pos          pic  9(07)                  .

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
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (18)             .
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
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni relative al record    *
      *              * codice di pagamento                             *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep[rec-zpg]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-zpg
           else      move  spaces         to   w-prs-rec-zpg          .
           if        w-prs-rec-zpg-mco    not  numeric
                     move  zero           to   w-prs-rec-zpg-mco      .
           if        w-prs-rec-zpg-mco    =    zero
                     move  9999999        to   w-prs-rec-zpg-mco      .
           if        w-prs-rec-zpg-fco    not  = "A"
                     move  "M"            to   w-prs-rec-zpg-fco      .
           if        w-prs-rec-zpg-omn    not  = "O"
                     move  "N"            to   w-prs-rec-zpg-omn      .
           if        w-prs-rec-zpg-umn    not  = "U"
                     move  "N"            to   w-prs-rec-zpg-umn      .
           if        w-prs-rec-zpg-mmn    not  = "N"
                     move  "M"            to   w-prs-rec-zpg-mmn      .
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice pagamento       *
      *              *-------------------------------------------------*
           perform   cod-mne-zpg-opn-000  thru cod-mne-zpg-opn-999    .
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
      *              * Close modulo accettazione codice pagamento      *
      *              *-------------------------------------------------*
           perform   cod-mne-zpg-cls-000  thru cod-mne-zpg-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *                  * Codice di pagamento                         *
      *                  *---------------------------------------------*
           perform   acc-cod-pag-000      thru acc-cod-pag-999        .
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
      *              * Codice di pagamento                             *
      *              *-------------------------------------------------*
           perform   vis-cod-pag-000      thru vis-cod-pag-999        .
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
      *              * Codice di pagamento                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-pag-000      thru pmt-cod-pag-999        .
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
      *    * Visualizzazione prompts per Codice di pagamento           *
      *    *-----------------------------------------------------------*
       pmt-cod-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice di pagamento        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-pag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice di pagamento           *
      *    *-----------------------------------------------------------*
       acc-cod-pag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zpg-ope      .
           move      w-tes-cod-pag        to   w-cod-mne-zpg-cod      .
           move      04                   to   w-cod-mne-zpg-lin      .
           move      30                   to   w-cod-mne-zpg-pos      .
           move      06                   to   w-cod-mne-zpg-dln      .
           move      30                   to   w-cod-mne-zpg-dps      .
           move      "<B"                 to   v-edm                  .
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
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pag-999.
       acc-cod-pag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-pag          .
       acc-cod-pag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-pag        =    zero
                     go to acc-cod-pag-410
           else      go to acc-cod-pag-450.
       acc-cod-pag-410.
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
                     go to acc-cod-pag-412.
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
           go to     acc-cod-pag-100.
       acc-cod-pag-412.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il tipo funzio- *
      *                      * namento per il codice e' automatico op- *
      *                      * pure manuale                            *
      *                      *-----------------------------------------*
           if        w-prs-rec-zpg-fco    not  = "A"
                     go to acc-cod-pag-415
           else      go to acc-cod-pag-420.
       acc-cod-pag-415.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' manuale                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna azione, ne' controllo       *
      *                          *-------------------------------------*
           go to     acc-cod-pag-600.
       acc-cod-pag-420.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      w-prs-rec-zpg-mco    to   w-enc-zpg-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-zpg-val-pos    to   w-tes-cod-pag          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-pag-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-pag-000      thru vis-cod-pag-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-pag-600.
       acc-cod-pag-450.
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
           if        w-prs-rec-zpg-mco    =    zero or
                     w-prs-rec-zpg-mco    =    9999999
                     go to acc-cod-pag-600.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato non e' superiore *
      *                      * al valore massimo impostabile, il con-  *
      *                      * trollo e' superato                      *
      *                      *-----------------------------------------*
           if        w-tes-cod-pag        not  > w-prs-rec-zpg-mco
                     go to acc-cod-pag-600.
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
           move      09                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Literals nel box                    *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Il codice pagamento non puo' essere superiore a "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      13                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      "<"                  to   v-edm                  .
           move      w-prs-rec-zpg-mco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      68                   to   v-pos                  .
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
           move      69                   to   v-pos                  .
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
           go to     acc-cod-pag-100.
       acc-cod-pag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-pag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-pag-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pag-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-pag-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-pag-999.
       acc-cod-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice di pagamento        *
      *    *-----------------------------------------------------------*
       vis-cod-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pag-999.
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
      *                  * Descrizione codice di pagamento             *
      *                  *---------------------------------------------*
           perform   acc-des-pag-000      thru acc-des-pag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Tipo di pagamento                           *
      *                  *---------------------------------------------*
           perform   acc-tip-pag-000      thru acc-tip-pag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Numero di scadenze                          *
      *                  *---------------------------------------------*
           perform   acc-num-sca-000      thru acc-num-sca-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Decorrenza prima scadenza                   *
      *                  *---------------------------------------------*
           perform   acc-dec-prs-000      thru acc-dec-prs-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-132.
      *                  *---------------------------------------------*
      *                  * Data prefissata, mese                       *
      *                  *---------------------------------------------*
           perform   acc-dap-mes-000      thru acc-dap-mes-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-134.
      *                  *---------------------------------------------*
      *                  * Data prefissata, giorno                     *
      *                  *---------------------------------------------*
           perform   acc-dap-gio-000      thru acc-dap-gio-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-132.
       acc-tes-reg-136.
      *                  *---------------------------------------------*
      *                  * 'n' giorni                                  *
      *                  *---------------------------------------------*
           perform   acc-nnn-gio-000      thru acc-nnn-gio-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-134.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Giorni di intervallo tra due scadenze       *
      *                  *---------------------------------------------*
           perform   acc-ggg-int-000      thru acc-ggg-int-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-134.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Tipo di scadenza nel mese                   *
      *                  *---------------------------------------------*
           perform   acc-tip-scm-000      thru acc-tip-scm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Giorno di scadenza fisso nel mese           *
      *                  *---------------------------------------------*
           perform   acc-gio-scm-000      thru acc-gio-scm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
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
      *              * Descrizione codice di pagamento                 *
      *              *-------------------------------------------------*
           perform   vis-des-pag-000      thru vis-des-pag-999        .
      *              *-------------------------------------------------*
      *              * Tipo di pagamento                               *
      *              *-------------------------------------------------*
           perform   vis-tip-pag-000      thru vis-tip-pag-999        .
      *              *-------------------------------------------------*
      *              * Numero di scadenze                              *
      *              *-------------------------------------------------*
           perform   vis-num-sca-000      thru vis-num-sca-999        .
      *              *-------------------------------------------------*
      *              * Decorrenza prima scadenza                       *
      *              *-------------------------------------------------*
           perform   vis-dec-prs-000      thru vis-dec-prs-999        .
      *              *-------------------------------------------------*
      *              * Data prefissata, mese                           *
      *              *-------------------------------------------------*
           perform   vis-dap-mes-000      thru vis-dap-mes-999        .
      *              *-------------------------------------------------*
      *              * Data prefissata, giorno                         *
      *              *-------------------------------------------------*
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
      *              *-------------------------------------------------*
      *              * 'n' giorni                                      *
      *              *-------------------------------------------------*
           perform   vis-nnn-gio-000      thru vis-nnn-gio-999        .
      *              *-------------------------------------------------*
      *              * Giorni di intervallo tra due scadenze           *
      *              *-------------------------------------------------*
           perform   vis-ggg-int-000      thru vis-ggg-int-999        .
      *              *-------------------------------------------------*
      *              * Tipo di scadenza nel mese                       *
      *              *-------------------------------------------------*
           perform   vis-tip-scm-000      thru vis-tip-scm-999        .
      *              *-------------------------------------------------*
      *              * Giorno di scadenza fisso nel mese               *
      *              *-------------------------------------------------*
           perform   vis-gio-scm-000      thru vis-gio-scm-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
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
      *              * Descrizione codice di pagamento                 *
      *              *-------------------------------------------------*
           perform   pmt-des-pag-000      thru pmt-des-pag-999        .
      *              *-------------------------------------------------*
      *              * Tipo di pagamento                               *
      *              *-------------------------------------------------*
           perform   pmt-tip-pag-000      thru pmt-tip-pag-999        .
      *              *-------------------------------------------------*
      *              * Numero di scadenze                              *
      *              *-------------------------------------------------*
           perform   pmt-num-sca-000      thru pmt-num-sca-999        .
      *              *-------------------------------------------------*
      *              * Decorrenza prima scadenza                       *
      *              *-------------------------------------------------*
           perform   pmt-dec-prs-000      thru pmt-dec-prs-999        .
      *              *-------------------------------------------------*
      *              * Giorni di intervallo tra due scadenze           *
      *              *-------------------------------------------------*
           perform   pmt-ggg-int-000      thru pmt-ggg-int-999        .
      *              *-------------------------------------------------*
      *              * Tipo di scadenza nel mese                       *
      *              *-------------------------------------------------*
           perform   pmt-tip-scm-000      thru pmt-tip-scm-999        .
      *              *-------------------------------------------------*
      *              * Giorno di scadenza fisso nel mese               *
      *              *-------------------------------------------------*
           perform   pmt-gio-scm-000      thru pmt-gio-scm-999        .
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
      *    * Visualizzazione prompt : Descrizione codice di pagamento  *
      *    *-----------------------------------------------------------*
       pmt-des-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione pagamento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo di pagamento                *
      *    *-----------------------------------------------------------*
       pmt-tip-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di pagamento          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero di scadenze               *
      *    *-----------------------------------------------------------*
       pmt-num-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di scadenze         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Decorrenza prima scadenza        *
      *    *-----------------------------------------------------------*
       pmt-dec-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Decorrenza prima scadenza  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dec-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Giorni di intervallo tra due     *
      *    *                          scadenze                         *
      *    *-----------------------------------------------------------*
       pmt-ggg-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Intervallo tra le scadenze :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ggg-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo di scadenza nel mese        *
      *    *-----------------------------------------------------------*
       pmt-tip-scm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di scadenza nel mese  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-scm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Giorno di scadenza fisso nel me- *
      *    *                          se                               *
      *    *-----------------------------------------------------------*
       pmt-gio-scm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Giorno di scadenza fisso   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-gio-scm-999.
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
      *    * Accettazione campo testata : Descrizione codice di paga-  *
      *    *                              mento                        *
      *    *-----------------------------------------------------------*
       acc-des-pag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-pag-100.
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
           move      w-tes-des-pag (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-pag-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-pag-999.
       acc-des-pag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-pag (1)      .
       acc-des-pag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-pag (1)    not  = spaces
                     go to acc-des-pag-450.
           if        v-key                =    "UP  "
                     go to acc-des-pag-600
           else      go to acc-des-pag-100.
       acc-des-pag-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-pag (1)
                    (01 : 01)             =    spaces
                     go to acc-des-pag-100.
       acc-des-pag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-pag (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-pag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-pag-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-pag-100.
       acc-des-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice di pa- *
      *    *                                 gamento                   *
      *    *-----------------------------------------------------------*
       vis-des-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-pag (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-pag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo di pagamento            *
      *    *-----------------------------------------------------------*
       acc-tip-pag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-pag (1)    to   w-sav-tip-pag          .
       acc-tip-pag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pag-lun    to   v-car                  .
           move      w-exp-tip-pag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pag-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-tip-pag (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pag-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pag-999.
       acc-tip-pag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-pag (1)      .
       acc-tip-pag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-tip-pag (1)    not  = zero
                     go to acc-tip-pag-450.
           if        v-key                =    "UP  "
                     go to acc-tip-pag-500
           else      go to acc-tip-pag-100.
       acc-tip-pag-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-pag (1)    >    w-exp-tip-pag-num
                     go to acc-tip-pag-100.
       acc-tip-pag-500.
      *                  *---------------------------------------------*
      *                  * Test che il tipo pagamento non sia pari a   *
      *                  * 04, cioe' C.d.O., in quanto questo tipo di  *
      *                  * pagamento non e' accettabile in un codice   *
      *                  * di pagamento, bensi' lo sara' solamente do- *
      *                  * po l'accettazione della presentazione di u- *
      *                  * na distinta da parte della banca.           *
      *                  *---------------------------------------------*
           if        w-tes-tip-pag (1)    not  = 04
                     go to acc-tip-pag-600.
           move      "Tipo pagamento non accettabile. Usare Incasso elet
      -              "tronico.       "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-tip-pag-100.
       acc-tip-pag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-tip-pag (1)    =    w-sav-tip-pag
                     go to acc-tip-pag-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-tip-pag        =    zero
                     go to acc-tip-pag-800.
       acc-tip-pag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-pag-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-pag-100.
       acc-tip-pag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo di pagamento         *
      *    *-----------------------------------------------------------*
       vis-tip-pag-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pag-lun    to   v-car                  .
           move      w-exp-tip-pag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pag-tbl    to   v-txt                  .
           move      w-tes-tip-pag (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-pag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero di scadenze           *
      *    *-----------------------------------------------------------*
       acc-num-sca-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-num-sca (1)    to   w-sav-num-sca          .
       acc-num-sca-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-sca (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-sca-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-sca-999.
       acc-num-sca-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-sca (1)      .
       acc-num-sca-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-num-sca (1)    not  = zero
                     go to acc-num-sca-450.
           if        v-key                =    "UP  "
                     go to acc-num-sca-600
           else      go to acc-num-sca-100.
       acc-num-sca-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito                                  *
      *                  *---------------------------------------------*
           if        w-tes-num-sca (1)    >    96
                     go to acc-num-sca-100.
       acc-num-sca-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-num-sca (1)    =    w-sav-num-sca
                     go to acc-num-sca-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-num-sca        =    zero
                     go to acc-num-sca-800.
       acc-num-sca-625.
      *                  *---------------------------------------------*
      *                  * Dipendenze effettive                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero di    *
      *                      * scadenze                                *
      *                      *-----------------------------------------*
           if        w-tes-num-sca (1)    >    1
                     go to acc-num-sca-650
           else      go to acc-num-sca-675.
       acc-num-sca-650.
      *                      *-----------------------------------------*
      *                      * Se numero di scadenze maggiore di 1     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-num-sca-800.
       acc-num-sca-675.
      *                      *-----------------------------------------*
      *                      * Se numero di scadenze non maggiore di 1 *
      *                      *-----------------------------------------*
       acc-num-sca-700.
      *                          *-------------------------------------*
      *                          * Controllo decorrenza prima scaden-  *
      *                          * za                                  *
      *                          *-------------------------------------*
           if        w-tes-dec-prs (1)    =    00 or
                     w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to acc-num-sca-750.
       acc-num-sca-705.
      *                          *-------------------------------------*
      *                          * Se la decorrenza prima scadenza non *
      *                          * e' compatibile per piu' scadenze,   *
      *                          * si normalizza a si visualizza la    *
      *                          * voce e i dati ad essa associati     *
      *                          *-------------------------------------*
           if        w-tes-dec-prs (1)    =    00
                     go to acc-num-sca-710.
           move      zero                 to   w-tes-dec-prs (1)      .
           perform   vis-dec-prs-000      thru vis-dec-prs-999        .
       acc-num-sca-710.
           if        w-tes-dap-mes (1)    =    00
                     go to acc-num-sca-715.
           move      zero                 to   w-tes-dap-mes (1)      .
           perform   vis-dap-mes-000      thru vis-dap-mes-999        .
       acc-num-sca-715.
           if        w-tes-dap-gio (1)    =    00
                     go to acc-num-sca-750.
           move      zero                 to   w-tes-dap-gio (1)      .
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
       acc-num-sca-750.
      *                          *-------------------------------------*
      *                          * Si forza il numero giorni di inter- *
      *                          * vallo tra due scadenze a zero       *
      *                          *-------------------------------------*
           if        w-tes-ggg-int (1)    =    zero
                     go to acc-num-sca-800.
           move      zero                 to   w-tes-ggg-int (1)      .
           perform   vis-ggg-int-000      thru vis-ggg-int-999        .
       acc-num-sca-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-sca-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-sca-100.
       acc-num-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero di scadenze        *
      *    *-----------------------------------------------------------*
       vis-num-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-sca (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-sca-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decorrenza prima scadenza    *
      *    *-----------------------------------------------------------*
       acc-dec-prs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dec-prs (1)    to   w-sav-dec-prs          .
       acc-dec-prs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dec-prs-lun    to   v-car                  .
           move      w-exp-dec-prs-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-dec-prs-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dec-prs (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dec-prs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dec-prs-999.
       acc-dec-prs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dec-prs (1)      .
       acc-dec-prs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    not  = zero
                     go to acc-dec-prs-425.
           if        v-key                =    "UP  "
                     go to acc-dec-prs-600
           else      go to acc-dec-prs-100.
       acc-dec-prs-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    >    w-exp-dec-prs-num
                     go to acc-dec-prs-100.
       acc-dec-prs-500.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di sca-   *
      *                  * denze                                       *
      *                  *---------------------------------------------*
           if        w-tes-num-sca (1)    >    1
                     go to acc-dec-prs-525
           else      go to acc-dec-prs-550.
       acc-dec-prs-525.
      *                  *---------------------------------------------*
      *                  * Se numero di scadenze maggiore di 1         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si controlla che il tipo di decorrenza  *
      *                      * prima scadenza non possa essere uno dei *
      *                      * seguenti :                              *
      *                      * - 01 : Manuale                          *
      *                      * - 02 : Ad una data prefissata           *
      *                      * - 03 : A vista                          *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to acc-dec-prs-100
           else      go to acc-dec-prs-600.
       acc-dec-prs-550.
      *                  *---------------------------------------------*
      *                  * Se numero di scadenze non maggiore di 1     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si controlla che se il tipo pagamento   *
      *                      * e' tra i seguenti :                     *
      *                      * - 02 : Incasso elettronico              *
      *                      * - 03 : Ri.Ba.                           *
      *                      * - 04 : C.d.O.                           *
      *                      * - 05 : M.av.                            *
      *                      * - 06 : R.I.D.                           *
      *                      * allora la decorrenza non puo' essere    *
      *                      * - 03 : A vista                          *
      *                      *-----------------------------------------*
           if        w-tes-tip-pag (1)    not  = 02 and
                     w-tes-tip-pag (1)    not  = 03 and
                     w-tes-tip-pag (1)    not  = 04 and
                     w-tes-tip-pag (1)    not  = 05 and
                     w-tes-tip-pag (1)    not  = 06
                     go to acc-dec-prs-600.
           if        w-tes-dec-prs (1)    not  = 03
                     go to acc-dec-prs-600.
           go to     acc-dec-prs-100.
       acc-dec-prs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    =    w-sav-dec-prs
                     go to acc-dec-prs-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-dec-prs        =    zero
                     go to acc-dec-prs-800.
       acc-dec-prs-625.
      *                  *---------------------------------------------*
      *                  * Dipendenze effettive                        *
      *                  *---------------------------------------------*
       acc-dec-prs-650.
      *                      *-----------------------------------------*
      *                      * Mese prefissato                         *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01
                     go to acc-dec-prs-675.
           if        w-tes-dap-mes (1)    =    zero
                     go to acc-dec-prs-675.
           move      zero                 to   w-tes-dap-mes (1)      .
           perform   vis-dap-mes-000      thru vis-dap-mes-999        .
       acc-dec-prs-675.
      *                      *-----------------------------------------*
      *                      * Giorno prefissato                       *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01
                     go to acc-dec-prs-700.
           if        w-tes-dap-gio (1)    =    zero
                     go to acc-dec-prs-700.
           move      zero                 to   w-tes-dap-gio (1)      .
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
       acc-dec-prs-700.
      *                      *-----------------------------------------*
      *                      * Giorni di intervallo tra due scadenze   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la decorrenza della prima sca-   *
      *                          * denza e' :                          *
      *                          * - 01 : Manuale                      *
      *                          * - 02 : Ad una data prefissata       *
      *                          * - 03 : A vista                      *
      *                          * si forza il numero giorni di inter- *
      *                          * vallo tra due scadenze a zero       *
      *                          *-------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01 and
                     w-tes-dec-prs (1)    not  = 02 and
                     w-tes-dec-prs (1)    not  = 03
                     go to acc-dec-prs-725.
           if        w-tes-ggg-int (1)    =    zero
                     go to acc-dec-prs-725.
           move      zero                 to   w-tes-ggg-int (1)      .
           perform   vis-ggg-int-000      thru vis-ggg-int-999        .
       acc-dec-prs-725.
      *                      *-----------------------------------------*
      *                      * Tipo di scadenza nel mese               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la decorrenza della prima sca-   *
      *                          * denza e' :                          *
      *                          * - 01 : Manuale                      *
      *                          * - 02 : Ad una data prefissata       *
      *                          * - 03 : A vista                      *
      *                          * si forza il tipo di scadenza nel    *
      *                          * mese a zero                         *
      *                          *-------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01 and
                     w-tes-dec-prs (1)    not  = 02 and
                     w-tes-dec-prs (1)    not  = 03
                     go to acc-dec-prs-750.
           if        w-tes-tip-scm (1)    =    zero
                     go to acc-dec-prs-750.
           move      zero                 to   w-tes-tip-scm (1)      .
           perform   vis-tip-scm-000      thru vis-tip-scm-999        .
       acc-dec-prs-750.
      *                      *-----------------------------------------*
      *                      * Giorno di scadenza fisso nel mese       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la decorrenza della prima sca-   *
      *                          * denza e' :                          *
      *                          * - 01 : Manuale                      *
      *                          * - 02 : Ad una data prefissata       *
      *                          * - 03 : A vista                      *
      *                          * si forza il giorno fisso di scaden- *
      *                          * za nel mese a zero                  *
      *                          *-------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01 and
                     w-tes-dec-prs (1)    not  = 02 and
                     w-tes-dec-prs (1)    not  = 03
                     go to acc-dec-prs-800.
           if        w-tes-gio-scm (1)    =    zero
                     go to acc-dec-prs-800.
           move      zero                 to   w-tes-gio-scm (1)      .
           perform   vis-gio-scm-000      thru vis-gio-scm-999        .
       acc-dec-prs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dec-prs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dec-prs-100.
       acc-dec-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decorrenza prima scadenza *
      *    *-----------------------------------------------------------*
       vis-dec-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dec-prs-lun    to   v-car                  .
           move      w-exp-dec-prs-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-dec-prs-tbl    to   v-txt                  .
           move      w-tes-dec-prs (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dec-prs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Mese per data prefissata     *
      *    *-----------------------------------------------------------*
       acc-dap-mes-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dap-mes-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la decorrenza prima scadenza e' di-  *
      *                      * versa da :                              *
      *                      * - 02 : Ad una data prefissata           *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    not  = 02
                     go to acc-dap-mes-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-dap-mes-050.
       acc-dap-mes-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-dap-mes (1)    =    zero
                     go to acc-dap-mes-035.
           move      zero                 to   w-tes-dap-mes (1)      .
           perform   vis-dap-mes-000      thru vis-dap-mes-999        .
       acc-dap-mes-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-dap-mes-999.
       acc-dap-mes-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dap-mes (1)    to   w-sav-dap-mes          .
       acc-dap-mes-075.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero si visualizza  *
      *                  * il prompt                                   *
      *                  *---------------------------------------------*
           if        w-tes-dap-mes (1)    not  = zero
                     go to acc-dap-mes-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "- MM:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-dap-mes-100.
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
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dap-mes (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dap-mes-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dap-mes-999.
       acc-dap-mes-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dap-mes (1)      .
       acc-dap-mes-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-dap-mes (1)    not  = zero
                     go to acc-dap-mes-450.
           if        v-key                =    "UP  "
                     go to acc-dap-mes-600
           else      go to acc-dap-mes-100.
       acc-dap-mes-450.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore sia 01..12          *
      *                  *---------------------------------------------*
           if        w-tes-dap-mes (1)    <    01 or
                     w-tes-dap-mes (1)    >    12
                     go to acc-dap-mes-100.
       acc-dap-mes-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore           *
      *                  *---------------------------------------------*
           if        w-tes-dap-mes (1)    =    zero
                     go to acc-dap-mes-625
           else      go to acc-dap-mes-650.
       acc-dap-mes-625.
      *                  *---------------------------------------------*
      *                  * Se il valore e' a zero                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione a spaces              *
      *                      *-----------------------------------------*
           perform   vis-dap-mes-000      thru vis-dap-mes-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione del   *
      *                      * giorno per data prefissata              *
      *                      *-----------------------------------------*
           if        w-tes-dap-gio (1)    =    zero
                     go to acc-dap-mes-800.
           move      zero                 to   w-tes-dap-gio (1)      .
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
           go to     acc-dap-mes-800.
       acc-dap-mes-650.
      *                  *---------------------------------------------*
      *                  * Se il valore e' diverso da zero             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-dap-mes-800.
       acc-dap-mes-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dap-mes-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dap-mes-100.
       acc-dap-mes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Mese per data prefissata          *
      *    *-----------------------------------------------------------*
       vis-dap-mes-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore               *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    =    12
                     go to vis-dap-mes-999.
           if        w-tes-dap-mes (1)    =    zero
                     go to vis-dap-mes-300
           else      go to vis-dap-mes-600.
       vis-dap-mes-300.
      *              *-------------------------------------------------*
      *              * Se il valore e' a zero                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione di prompt e valore a spaces *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dap-mes-999.
       vis-dap-mes-600.
      *              *-------------------------------------------------*
      *              * Se il valore e' diverso da zero                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del prompt                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "- MM:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-tes-dap-mes (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dap-mes-999.
       vis-dap-mes-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Giorno per data prefissata   *
      *    *-----------------------------------------------------------*
       acc-dap-gio-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dap-gio-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la decorrenza prima scadenza e' di-  *
      *                      * versa da :                              *
      *                      * - 02 : Ad una data prefissata           *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    not  = 02
                     go to acc-dap-gio-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-dap-gio-050.
       acc-dap-gio-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-dap-gio (1)    =    zero
                     go to acc-dap-gio-035.
           move      zero                 to   w-tes-dap-gio (1)      .
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
       acc-dap-gio-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-dap-gio-999.
       acc-dap-gio-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dap-gio (1)    to   w-sav-dap-gio          .
       acc-dap-gio-075.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero si visualizza  *
      *                  * il prompt                                   *
      *                  *---------------------------------------------*
           if        w-tes-dap-gio (1)    not  = zero
                     go to acc-dap-gio-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "GG:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-dap-gio-100.
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
           move      77                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dap-gio (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dap-gio-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dap-gio-999.
       acc-dap-gio-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dap-gio (1)      .
       acc-dap-gio-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-dap-gio (1)    not  = zero
                     go to acc-dap-gio-450.
           if        v-key                =    "UP  "
                     go to acc-dap-gio-600
           else      go to acc-dap-gio-100.
       acc-dap-gio-450.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore sia congruente con  *
      *                  * il mese per la data prefissata              *
      *                  *---------------------------------------------*
           if        w-tes-dap-mes (1)    <    01 or
                     w-tes-dap-mes (1)    >    12
                     go to acc-dap-gio-100.
           move      w-tes-dap-mes (1)    to   w-det-dap-gio-inx      .
           if        w-tes-dap-gio (1)    >    w-det-dap-gio-tbx
                                              (w-det-dap-gio-inx)
                     go to acc-dap-gio-100.
       acc-dap-gio-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore           *
      *                  *---------------------------------------------*
           if        w-tes-dap-gio (1)    =    zero
                     go to acc-dap-gio-625
           else      go to acc-dap-gio-650.
       acc-dap-gio-625.
      *                  *---------------------------------------------*
      *                  * Se il valore e' a zero                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione a spaces              *
      *                      *-----------------------------------------*
           perform   vis-dap-gio-000      thru vis-dap-gio-999        .
           go to     acc-dap-gio-800.
       acc-dap-gio-650.
      *                  *---------------------------------------------*
      *                  * Se il valore e' diverso da zero             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-dap-gio-800.
       acc-dap-gio-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dap-gio-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dap-gio-100.
       acc-dap-gio-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Giorno per data prefissata        *
      *    *-----------------------------------------------------------*
       vis-dap-gio-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore               *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    =    12
                     go to vis-dap-gio-999.
           if        w-tes-dap-gio (1)    =    zero
                     go to vis-dap-gio-300
           else      go to vis-dap-gio-600.
       vis-dap-gio-300.
      *              *-------------------------------------------------*
      *              * Se il valore e' a zero                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione di prompt e valore a spaces *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dap-gio-999.
       vis-dap-gio-600.
      *              *-------------------------------------------------*
      *              * Se il valore e' diverso da zero                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del prompt                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "GG:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      w-tes-dap-gio (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dap-gio-999.
       vis-dap-gio-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : 'n' giorni                   *
      *    *-----------------------------------------------------------*
       acc-nnn-gio-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nnn-gio-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la decorrenza prima scadenza e' di-  *
      *                      * versa da :                              *
      *                      * - 12 : A 'n' giorni                     *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    not  = 12
                     go to acc-nnn-gio-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-nnn-gio-050.
       acc-nnn-gio-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-acc-nnn-gio        =    zero
                     go to acc-nnn-gio-035.
           move      zero                 to   w-acc-nnn-gio          .
           perform   vis-nnn-gio-000      thru vis-nnn-gio-999        .
       acc-nnn-gio-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-nnn-gio-999.
       acc-nnn-gio-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-acc-nnn-gio        to   w-sav-nnn-gio          .
       acc-nnn-gio-075.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero si visualizza  *
      *                  * il prompt                                   *
      *                  *---------------------------------------------*
           if        w-acc-nnn-gio        not  = zero
                     go to acc-nnn-gio-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "GG:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nnn-gio-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<9B"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-acc-nnn-gio        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nnn-gio-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nnn-gio-999.
       acc-nnn-gio-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-nnn-gio          .
       acc-nnn-gio-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-acc-nnn-gio        not  = zero
                     go to acc-nnn-gio-600.
           if        v-key                =    "UP  "
                     go to acc-nnn-gio-600
           else      go to acc-nnn-gio-100.
       acc-nnn-gio-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nnn-gio-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nnn-gio-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nnn-gio-100.
       acc-nnn-gio-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : 'n' giorni                        *
      *    *-----------------------------------------------------------*
       vis-nnn-gio-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore               *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    not  = 12
                     go to vis-nnn-gio-999.
           if        w-acc-nnn-gio        =    zero
                     go to vis-nnn-gio-300
           else      go to vis-nnn-gio-600.
       vis-nnn-gio-300.
      *              *-------------------------------------------------*
      *              * Se il valore e' a zero                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione di prompt e valore a spaces *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-nnn-gio-999.
       vis-nnn-gio-600.
      *              *-------------------------------------------------*
      *              * Se il valore e' diverso da zero                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del prompt                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "GG:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-acc-nnn-gio        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-nnn-gio-999.
       vis-nnn-gio-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Giorni di intervallo tra due *
      *    *                              scadenze                     *
      *    *-----------------------------------------------------------*
       acc-ggg-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ggg-int-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il numero scadenze non e' superiore  *
      *                      * a uno : nessuna accettazione, bensi'    *
      *                      * normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-tes-num-sca (1)    not  > 01
                     go to acc-ggg-int-030.
      *                      *-----------------------------------------*
      *                      * Se la decorrenza della prima scadenza   *
      *                      * e' tra le seguenti :                    *
      *                      * - 01 : Manuale                          *
      *                      * - 02 : Ad una data prefissata           *
      *                      * - 03 : A vista                          *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to acc-ggg-int-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-ggg-int-050.
       acc-ggg-int-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-ggg-int (1)    =    zero
                     go to acc-ggg-int-035.
           move      zero                 to   w-tes-ggg-int (1)      .
           perform   vis-ggg-int-000      thru vis-ggg-int-999        .
       acc-ggg-int-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-ggg-int-999.
       acc-ggg-int-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-ggg-int (1)    to   w-sav-ggg-int          .
       acc-ggg-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ggg-int-lun    to   v-car                  .
           move      w-exp-ggg-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-ggg-int-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ggg-int (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ggg-int-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ggg-int-999.
       acc-ggg-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ggg-int (1)      .
       acc-ggg-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-ggg-int (1)    not  = zero
                     go to acc-ggg-int-450.
           if        v-key                =    "UP  "
                     go to acc-ggg-int-600
           else      go to acc-ggg-int-100.
       acc-ggg-int-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-ggg-int (1)    >    w-exp-ggg-int-num
                     go to acc-ggg-int-100.
       acc-ggg-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ggg-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ggg-int-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ggg-int-100.
       acc-ggg-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Giorni di intervallo tra due sca- *
      *    *                         denze                             *
      *    *-----------------------------------------------------------*
       vis-ggg-int-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ggg-int-lun    to   v-car                  .
           move      w-exp-ggg-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-ggg-int-tbl    to   v-txt                  .
           move      w-tes-ggg-int (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ggg-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo di scadenza nel mese    *
      *    *-----------------------------------------------------------*
       acc-tip-scm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-scm-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la decorrenza della prima scadenza   *
      *                      * e' tra le seguenti :                    *
      *                      * - 01 : Manuale                          *
      *                      * - 02 : Ad una data prefissata           *
      *                      * - 03 : A vista                          *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to acc-tip-scm-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-tip-scm-050.
       acc-tip-scm-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-tip-scm (1)    =    zero
                     go to acc-tip-scm-035.
           move      zero                 to   w-tes-tip-scm (1)      .
           perform   vis-tip-scm-000      thru vis-tip-scm-999        .
       acc-tip-scm-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tip-scm-999.
       acc-tip-scm-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-scm (1)    to   w-sav-tip-scm          .
       acc-tip-scm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scm-lun    to   v-car                  .
           move      w-exp-tip-scm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-scm-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-tip-scm (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-scm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-scm-999.
       acc-tip-scm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-scm (1)      .
       acc-tip-scm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-tip-scm (1)    not  = zero
                     go to acc-tip-scm-450.
           if        v-key                =    "UP  "
                     go to acc-tip-scm-600
           else      go to acc-tip-scm-100.
       acc-tip-scm-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito in tabella                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-scm (1)    >    w-exp-tip-scm-num
                     go to acc-tip-scm-100.
       acc-tip-scm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore non e' cambiato rispetto al    *
      *                  * valore precedente : nessuna dipendenza      *
      *                  *---------------------------------------------*
           if        w-tes-tip-scm (1)    =    w-sav-tip-scm
                     go to acc-tip-scm-800.
      *                  *---------------------------------------------*
      *                  * Se il valore precedente era a zero : nessu- *
      *                  * na dipendenza                               *
      *                  *---------------------------------------------*
           if        w-sav-tip-scm        =    zero
                     go to acc-tip-scm-800.
       acc-tip-scm-650.
      *                  *---------------------------------------------*
      *                  * Dipendenze effettive                        *
      *                  *---------------------------------------------*
       acc-tip-scm-660.
      *                      *-----------------------------------------*
      *                      * Giorno di scadenza fisso nel mese       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il tipo di scadenza nel mese non *
      *                          * e' Giorno fisso nel mese, si forza  *
      *                          * il giorno di scadenza fisso nel me- *
      *                          * se a zero                           *
      *                          *-------------------------------------*
           if        w-tes-tip-scm (1)    =    01 or
                     w-tes-tip-scm (1)    =    02
                     go to acc-tip-scm-662
           else      go to acc-tip-scm-670.
       acc-tip-scm-662.
           if        w-tes-gio-scm (1)    =    zero
                     go to acc-tip-scm-670.
           move      zero                 to   w-tes-gio-scm (1)      .
           perform   vis-gio-scm-000      thru vis-gio-scm-999        .
       acc-tip-scm-670.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-tip-scm-800.
       acc-tip-scm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-scm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-scm-100.
       acc-tip-scm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo di scadenza nel mese         *
      *    *-----------------------------------------------------------*
       vis-tip-scm-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scm-lun    to   v-car                  .
           move      w-exp-tip-scm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-scm-tbl    to   v-txt                  .
           move      w-tes-tip-scm (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-scm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Giorno di scadenza fisso nel *
      *    *                              mese                         *
      *    *-----------------------------------------------------------*
       acc-gio-scm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-gio-scm-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la decorrenza della prima scadenza   *
      *                      * e' tra le seguenti :                    *
      *                      * - 01 : Manuale                          *
      *                      * - 02 : Ad una data prefissata           *
      *                      * - 03 : A vista                          *
      *                      * nessuna accettazione, bensi' normaliz-  *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to acc-gio-scm-030.
      *                      *-----------------------------------------*
      *                      * Se il tipo di scadenza nel mese non e'  *
      *                      * Giorno fisso nel mese : nessuna accet-  *
      *                      * tazione, bensi' normalizzazione         *
      *                      *-----------------------------------------*
           if        w-tes-tip-scm (1)    =    01   or
                     w-tes-tip-scm (1)    =    02
                     go to acc-gio-scm-030.
      *                      *-----------------------------------------*
      *                      * Altrimenti si esegue l'accettazione     *
      *                      *-----------------------------------------*
           go to     acc-gio-scm-050.
       acc-gio-scm-030.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-gio-scm (1)    =    zero
                     go to acc-gio-scm-035.
           move      zero                 to   w-tes-gio-scm (1)      .
           perform   vis-gio-scm-000      thru vis-gio-scm-999        .
       acc-gio-scm-035.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-gio-scm-999.
       acc-gio-scm-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-gio-scm (1)    to   w-sav-gio-scm          .
       acc-gio-scm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-gio-scm (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-gio-scm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-gio-scm-999.
       acc-gio-scm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-gio-scm (1)      .
       acc-gio-scm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-gio-scm (1)    not  = zero
                     go to acc-gio-scm-450.
           if        v-key                =    "UP  "
                     go to acc-gio-scm-600
           else      go to acc-gio-scm-100.
       acc-gio-scm-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il massimo    *
      *                  * consentito                                  *
      *                  *---------------------------------------------*
           if        w-tes-gio-scm (1)    >    28
                     go to acc-gio-scm-100.
       acc-gio-scm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-gio-scm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-gio-scm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-gio-scm-100.
       acc-gio-scm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Giorno di scadenza fisso nel mese *
      *    *-----------------------------------------------------------*
       vis-gio-scm-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-gio-scm (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gio-scm-999.
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
           if        w-prs-rec-zpg-mmn    =    "N"
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
           if        w-prs-rec-zpg-omn    not  = "O"
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
           if        w-tes-cod-pag        =    zero
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
      *              * Controllo su Descrizione                        *
      *              *-------------------------------------------------*
           if        w-tes-des-pag (1)    not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca la descrizione per il codice di pagamento   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo di pagamento                  *
      *              *-------------------------------------------------*
           if        w-tes-tip-pag (1)    not  = zero
                     go to cnt-tdo-nok-105.
           move      "Manca il tipo di pagamento                        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-105.
           if        w-tes-tip-pag (1)    not  > w-exp-tip-pag-num
                     go to cnt-tdo-nok-110.
           move      "Tipo di pagamento errato                          
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-110.
           if        w-tes-tip-pag (1)    not  = 04
                     go to cnt-tdo-nok-150.
           move      "Tipo pagamento non accettabile. Usare Incasso elet
      -              "tronico.       "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo su Numero di scadenze                 *
      *              *-------------------------------------------------*
           if        w-tes-num-sca (1)    not  = zero
                     go to cnt-tdo-nok-155.
           move      "Manca il numero di scadenze                       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-155.
           if        w-tes-num-sca (1)    not  > 96
                     go to cnt-tdo-nok-200.
           move      "Numero di scadenze errato                         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su Decorrenza prima scadenza, e su    *
      *              * mese e giorno per data prefissata               *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    not  = zero
                     go to cnt-tdo-nok-205.
           move      "Manca la decorrenza per la prima scadenza         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-205.
           if        w-tes-dec-prs (1)    not  > w-exp-dec-prs-num
                     go to cnt-tdo-nok-207.
           move      "Decorrenza per la prima scadenza errata           
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-207.
           if        w-tes-dec-prs (1)    =    12
                     go to cnt-tdo-nok-330.
           if        w-tes-dec-prs (1)    not  = 03
                     go to cnt-tdo-nok-210.
           if        w-tes-tip-pag (1)    not  = 02 and
                     w-tes-tip-pag (1)    not  = 03 and
                     w-tes-tip-pag (1)    not  = 04 and
                     w-tes-tip-pag (1)    not  = 05 and
                     w-tes-tip-pag (1)    not  = 06
                     go to cnt-tdo-nok-210.
           move      "Decorrenza per la prima scadenza errata           
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-210.
           if        w-tes-num-sca (1)    >    01
                     go to cnt-tdo-nok-215
           else      go to cnt-tdo-nok-220.
       cnt-tdo-nok-215.
           if        w-tes-dec-prs (1)    =    04 or
                     w-tes-dec-prs (1)    =    05 or
                     w-tes-dec-prs (1)    =    06 or
                     w-tes-dec-prs (1)    =    07 or
                     w-tes-dec-prs (1)    =    08 or
                     w-tes-dec-prs (1)    =    09 or
                     w-tes-dec-prs (1)    =    10 or
                     w-tes-dec-prs (1)    =    11
                     go to cnt-tdo-nok-230.
           move      "Decorrenza prima scadenza errata                  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-220.
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03 or
                     w-tes-dec-prs (1)    =    04 or
                     w-tes-dec-prs (1)    =    05 or
                     w-tes-dec-prs (1)    =    06 or
                     w-tes-dec-prs (1)    =    07 or
                     w-tes-dec-prs (1)    =    08 or
                     w-tes-dec-prs (1)    =    09 or
                     w-tes-dec-prs (1)    =    10 or
                     w-tes-dec-prs (1)    =    11
                     go to cnt-tdo-nok-230.
           move      "Decorrenza prima scadenza errata                  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-230.
           if        w-tes-dec-prs (1)    not  = 02
                     go to cnt-tdo-nok-250.
       cnt-tdo-nok-235.
           if        w-tes-dap-mes (1)    not  = zero
                     go to cnt-tdo-nok-237.
           move      "Manca il mese per la data scadenza prefissata     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-237.
           if        w-tes-dap-mes (1)    not  < 01 and
                     w-tes-dap-mes (1)    not  > 12
                     go to cnt-tdo-nok-240.
           move      "Mese per la data scadenza prefissata errato       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-240.
           if        w-tes-dap-gio (1)    not  = zero
                     go to cnt-tdo-nok-242.
           move      "Manca il giorno per la data scadenza prefissata   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-242.
           move      w-tes-dap-mes (1)    to   w-det-dap-gio-inx      .
           if        w-tes-dap-gio (1)    not  > w-det-dap-gio-tbx
                                                (w-det-dap-gio-inx)
                     go to cnt-tdo-nok-250.
           move      "Giorno per la data scadenza prefissata errato     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo su Giorni di intervallo tra due sca-  *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-tes-num-sca (1)    not  > 01
                     go to cnt-tdo-nok-300.
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to cnt-tdo-nok-300.
           if        w-tes-ggg-int (1)    not  = zero
                     go to cnt-tdo-nok-255.
           move      "Mancano i giorni di intervallo tra due scadenze   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-255.
           if        w-tes-ggg-int (1)    not  > w-exp-ggg-int-num
                     go to cnt-tdo-nok-300.
           move      "Giorni di intervallo tra due scadenze errati      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo di scadenza nel mese          *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    =    12
                     go to cnt-tdo-nok-330.
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to cnt-tdo-nok-350.
           if        w-tes-tip-scm (1)    not  = zero
                     go to cnt-tdo-nok-305.
           move      "Manca il tipo di scadenza nel mese                
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-305.
           if        w-tes-tip-scm (1)    not  > w-exp-tip-scm-num
                     go to cnt-tdo-nok-350.
           move      "Tipo di scadenza nel mese errato                  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-330.
      *              *-------------------------------------------------*
      *              * Controllo su 'n' giorni                         *
      *              *-------------------------------------------------*
           if        w-acc-nnn-gio        >    zero
                     go to cnt-tdo-nok-350.
           move      "Manca il numero di giorni di decorrenza della prim
      -              "a scadenza !   "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-350.
      *              *-------------------------------------------------*
      *              * Controllo su Giorno di scadenza fisso nel mese  *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    =    01 or
                     w-tes-dec-prs (1)    =    02 or
                     w-tes-dec-prs (1)    =    03
                     go to cnt-tdo-nok-400.
           if        w-tes-tip-scm (1)    =    01 or
                     w-tes-tip-scm (1)    =    02
                     go to cnt-tdo-nok-400.
           if        w-tes-gio-scm (1)    not  = zero
                     go to cnt-tdo-nok-355.
           move      "Manca il giorno di scadenza fisso nel mese        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-355.
           if        w-tes-gio-scm (1)    not  > 28
                     go to cnt-tdo-nok-400.
           move      "Giorno di scadenza fisso nel mese errato          
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se obbligatorio        *
      *              *-------------------------------------------------*
           if        w-prs-rec-zpg-omn    not  = "O"
                     go to cnt-tdo-nok-500.
           if        w-tes-cod-mne (1)    not  = spaces
                     go to cnt-tdo-nok-500.
           move      "Manca il codice mnemonico                         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se unico               *
      *              *-------------------------------------------------*
           if        w-prs-rec-zpg-umn    not  = "U"
                     go to cnt-tdo-nok-550.
           if        w-tes-cod-mne (1)    =    spaces
                     go to cnt-tdo-nok-550.
       cnt-tdo-nok-505.
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-mne (1)    to   rf-zpg-cod-mne         .
           move      zero                 to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-550.
       cnt-tdo-nok-510.
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-550.
           if        rf-zpg-cod-mne       not  = w-tes-cod-mne (1)
                     go to cnt-tdo-nok-550.
           if        rf-zpg-cod-pag       =    w-tes-cod-pag
                     go to cnt-tdo-nok-510.
           move      "Codice mnemonico gia' esistente                   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-550.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se non modificabile    *
      *              *-------------------------------------------------*
           if        w-prs-rec-zpg-mmn    not  = "N"
                     go to cnt-tdo-nok-600.
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to cnt-tdo-nok-600.
           if        w-tes-cod-mne (2)    =    spaces
                     go to cnt-tdo-nok-600.
           if        w-tes-cod-mne (1)    =    w-tes-cod-mne (2)
                     go to cnt-tdo-nok-600.
           move      "Il codice mnemonico non puo' essere modificato    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mese e giorno per data scadenza prefissata  *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    =    02
                     go to cnt-tdo-nok-625.
           move      zero                 to   w-tes-dap-mes (1)      .
           move      zero                 to   w-tes-dap-gio (1)      .
       cnt-tdo-nok-625.
      *                  *---------------------------------------------*
      *                  * Giorni di intervallo tra due scadenze       *
      *                  *---------------------------------------------*
           if        w-tes-num-sca (1)    >    1
                     go to cnt-tdo-nok-650.
           if        w-tes-dec-prs (1)    not  = 01 and
                     w-tes-dec-prs (1)    not  = 02 and
                     w-tes-dec-prs (1)    not  = 03
                     go to cnt-tdo-nok-650.
           go to     cnt-tdo-nok-650.
       cnt-tdo-nok-650.
      *                  *---------------------------------------------*
      *                  * Tipo di scadenza nel mese                   *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    not  = 01 and
                     w-tes-dec-prs (1)    not  = 02 and
                     w-tes-dec-prs (1)    not  = 03
                     go to cnt-tdo-nok-675.
           move      zero                 to   w-tes-tip-scm (1)      .
       cnt-tdo-nok-675.
      *                  *---------------------------------------------*
      *                  * Giorno di scadenza fisso nel mese           *
      *                  *---------------------------------------------*
           if        w-tes-tip-scm (1)    =    03 or
                     w-tes-tip-scm (1)    =    04
                     go to cnt-tdo-nok-800.
           move      zero                 to   w-tes-gio-scm (1)      .
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
           move      zero                 to   w-tes-cod-pag          .
           move      spaces               to   w-tes-cod-pag-aut      .
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
           move      spaces               to   w-tes-des-pag (1)      .
           move      zero                 to   w-tes-tip-pag (1)      .
           move      zero                 to   w-tes-num-sca (1)      .
           move      zero                 to   w-tes-dec-prs (1)      .
           move      zero                 to   w-tes-dap-mes (1)      .
           move      zero                 to   w-tes-dap-gio (1)      .
           move      zero                 to   w-tes-ggg-int (1)      .
           move      zero                 to   w-tes-tip-scm (1)      .
           move      zero                 to   w-tes-gio-scm (1)      .
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
           move      "CODPAG    "         to   f-key                  .
           move      w-tes-cod-pag        to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
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
           move      rf-zpg-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-zpg-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-zpg-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-zpg-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-zpg-des-key       to   w-tes-des-key (1)      .
           move      rf-zpg-des-pag       to   w-tes-des-pag (1)      .
           move      rf-zpg-tip-pag       to   w-tes-tip-pag (1)      .
           move      rf-zpg-num-sca       to   w-tes-num-sca (1)      .
           move      rf-zpg-dec-prs       to   w-tes-dec-prs (1)      .
           move      rf-zpg-dap-mes       to   w-tes-dap-mes (1)      .
           move      rf-zpg-dap-gio       to   w-tes-dap-gio (1)      .
           move      rf-zpg-ggg-int       to   w-tes-ggg-int (1)      .
           move      rf-zpg-tip-scm       to   w-tes-tip-scm (1)      .
           move      rf-zpg-gio-scm       to   w-tes-gio-scm (1)      .
           move      rf-zpg-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-200.
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Preparazione comodo di accettazione 'n' *
      *                      * giorni                                  *
      *                      *-----------------------------------------*
           move      w-tes-dap-mes (1)    to   w-acc-nnn-gio-mes      .
           move      w-tes-dap-gio (1)    to   w-acc-nnn-gio-gio      .
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
       pre-acc-ins-100.
      *              *-------------------------------------------------*
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Routine di duplicazione record              *
      *                  *---------------------------------------------*
           perform   rou-dup-rec-000      thru rou-dup-rec-999        .
       pre-acc-ins-200.
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine duplicazione record                               *
      *    *-----------------------------------------------------------*
       rou-dup-rec-000.
      *              *-------------------------------------------------*
      *              * Copia dei valori di testata precedenti          *
      *              *-------------------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
       rou-dup-rec-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione flags di controllo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di impostazione pagine di testata      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *                  *---------------------------------------------*
      *                  * Flags di impostazione pagine di testata     *
      *                  * eseguita                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-imp-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di ingresso in pagine di testata      *
      *                  * eseguito                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-ing-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di visualizzazione per le pagine      *
      *                  *---------------------------------------------*
           move      "#                   "
                                          to   w-cnt-sts-vis-pte      .
       rou-dup-rec-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo di accettazione 'n'      *
      *              * giorni                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-nnn-gio          .
       rou-dup-rec-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagine di testata               *
      *              *-------------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
       rou-dup-rec-999.
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
           if        w-tes-cod-pag-aut    =    spaces
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
      *              * Trattamento file [zpg]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zpg]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zpg-000      thru wrt-rec-zpg-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zpg]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zpg-000      thru rew-rec-zpg-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zpg]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zpg-000      thru del-rec-zpg-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zpg]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zpg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-pag        to   rf-zpg-cod-pag         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-zpg-ide-dat         .
           move      s-ute                to   rf-zpg-ide-ute         .
           move      s-fas                to   rf-zpg-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-zpg-cod-mne         .
           move      w-tes-des-key (1)    to   rf-zpg-des-key         .
           move      w-tes-des-pag (1)    to   rf-zpg-des-pag         .
           move      w-tes-tip-pag (1)    to   rf-zpg-tip-pag         .
           move      w-tes-num-sca (1)    to   rf-zpg-num-sca         .
           move      w-tes-dec-prs (1)    to   rf-zpg-dec-prs         .
           move      w-tes-dap-mes (1)    to   rf-zpg-dap-mes         .
           move      w-tes-dap-gio (1)    to   rf-zpg-dap-gio         .
           move      w-tes-ggg-int (1)    to   rf-zpg-ggg-int         .
           move      w-tes-tip-scm (1)    to   rf-zpg-tip-scm         .
           move      w-tes-gio-scm (1)    to   rf-zpg-gio-scm         .
           move      w-tes-alx-exp (1)    to   rf-zpg-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Eventuale riassestamento giorno e mese data *
      *                  * prefissata                                  *
      *                  *---------------------------------------------*
           if        w-tes-dec-prs (1)    not  = 12
                     go to cmp-rec-zpg-999.
           move      w-acc-nnn-gio-mes    to   rf-zpg-dap-mes         .
           move      w-acc-nnn-gio-gio    to   rf-zpg-dap-gio         .
       cmp-rec-zpg-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zpg]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zpg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpg-000      thru cmp-rec-zpg-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
       wrt-rec-zpg-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zpg]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zpg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpg-000      thru cmp-rec-zpg-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
       rew-rec-zpg-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zpg]                                *
      *    *-----------------------------------------------------------*
       del-rec-zpg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zpg-000      thru cmp-rec-zpg-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
       del-rec-zpg-999.
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
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [zpg]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "zpg "               to   s-nam                  .
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
           move      "zpg "               to   s-nam                  .
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
           move      s-num                to   w-enc-zpg-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-zpg-val-pre    to   w-enc-zpg-val-pos      .
           add       1                    to   w-enc-zpg-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-zpg-val-pos    =    zero
                     move  1              to   w-enc-zpg-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-zpg-val-pos    >    w-enc-zpg-val-max
                     move  1              to   w-enc-zpg-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPAG    "         to   f-key                  .
           move      w-enc-zpg-val-pos    to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
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
           add       1                    to   w-enc-zpg-val-pos      .
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
           move      "zpg "               to   s-nam                  .
           move      w-enc-zpg-val-pos    to   s-num                  .
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
      *              * Lettura codice automatico per [zpg]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "zpg "               to   s-nam                  .
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
           if        s-num                =    w-enc-zpg-val-pos
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
           move      "zpg "               to   s-nam                  .
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
           move      "zpg "               to   s-nam                  .
           move      w-enc-zpg-val-pre    to   s-num                  .
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice di pagamento    *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzpg0.acs"                   .
