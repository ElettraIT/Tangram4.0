       Identification Division.
       Program-Id.                                 pdcf1000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    tab                 *
      *                                   Fase:    dcf100              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/06/92    *
      *                       Ultima revisione:    NdK del 19/05/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio case produttrici          *
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
                     "dcf100"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf1000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   GESTIONE ARCHIVIO CASE PRODUTTRICI   "       .

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
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-pdt          pic  9(07)                  .
               10  w-tes-cod-pdt-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-rag-key          pic  x(40)                  .
               10  w-tes-rag-soc          pic  x(40)                  .
               10  w-tes-via-pdt          pic  x(40)                  .
               10  w-tes-loc-pdt          pic  x(40)                  .
               10  w-tes-cod-naz          pic  x(03)                  .
               10  w-tes-cod-naz-des      pic  x(20)                  .
               10  w-tes-cod-cmn          pic  9(05)                  .
               10  w-tes-cod-cmn-des      pic  x(30)                  .
               10  w-tes-cod-cmn-prv      pic  x(02)                  .
               10  w-tes-cod-fzn          pic  9(03)                  .
               10  w-tes-cod-fzn-des      pic  x(30)                  .
               10  w-tes-cod-lct          pic  9(03)                  .
               10  w-tes-cod-lct-des      pic  x(30)                  .
               10  w-tes-num-tel          pic  x(20)                  .
               10  w-tes-num-fax          pic  x(20)                  .
               10  w-tes-num-tlx          pic  x(20)                  .
               10  w-tes-nom-int          pic  x(30)                  .
               10  w-tes-prt-iva          pic  9(11)                  .
               10  w-tes-cod-fis          pic  x(16)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative al record casa produttrice *
      *        *-------------------------------------------------------*
           05  w-prs-rec-pdt.
      *            *---------------------------------------------------*
      *            * Valore massimo accettabile per il codice          *
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-mco      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzionamento codice, in creazione           *
      *            * - M : Manuale                                     *
      *            * - A : Automatico                                  *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-fco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del mnemonico                     *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-omn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Unicita' del mnemonico                            *
      *            * - N : Non necessariamente unico, si' duplicati    *
      *            * - U : Unico, duplicati non ammessi                *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-umn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del mnemonico                     *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-mmn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del sottoconto contabile          *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-osc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del sottoconto contabile          *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-pdt-msc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxn]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxn.
               10  w-let-arc-gxn-flg      pic  x(01)                  .
               10  w-let-arc-gxn-cod      pic  x(03)                  .
               10  w-let-arc-gxn-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : errore formale partita iva                 *
      *        *-------------------------------------------------------*
           05  w-exp-err-piv.
               10  w-exp-err-piv-num      pic  9(02)       value 2    .
               10  w-exp-err-piv-lun      pic  9(02)       value 40   .
               10  w-exp-err-piv-tbl.
                   15  filler             pic  x(40) value
                            "Reimpostazione della partita iva        ".
                   15  filler             pic  x(40) value
                            "Forzatura del valore impostato          ".
               10  w-exp-err-piv-sce      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : errore formale codice fiscale              *
      *        *-------------------------------------------------------*
           05  w-exp-err-cfi.
               10  w-exp-err-cfi-num      pic  9(02)       value 2    .
               10  w-exp-err-cfi-lun      pic  9(02)       value 40   .
               10  w-exp-err-cfi-tbl.
                   15  filler             pic  x(40) value
                            "Reimpostazione del codice fiscale       ".
                   15  filler             pic  x(40) value
                            "Forzatura del valore impostato          ".
               10  w-exp-err-cfi-sce      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di controllo formale Partita Iva e   *
      *    * Codici Fiscale                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wkl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice casa produttrice        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nazione                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione Indirizzo, C.a.p. e citta'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/aiplgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione C.a.p. e citta'                *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acecgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione comune                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione frazione                       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione localita'                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acl"                   .

      *    *===========================================================*
      *    * Work per ridefinizioni partita iva / codice fiscale       *
      *    *-----------------------------------------------------------*
       01  w-piv-cfi.
      *        *-------------------------------------------------------*
      *        * Codice fiscale in rappresentazione alfanumerica       *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-cfi-alf          pic  x(16)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione del codice fiscale come due porzioni,   *
      *        * la prima di 11 caratteri, la seconda di 5 caratteri   *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-cfi-016 redefines
               w-piv-cfi-cfi-alf.
               10  w-piv-cfi-cfi-11a      pic  x(11)                  .
               10  w-piv-cfi-cfi-11n redefines
                   w-piv-cfi-cfi-11a      pic  9(11)                  .
               10  w-piv-cfi-cfi-05a      pic  x(05)                  .
               10  w-piv-cfi-cfi-05n redefines
                   w-piv-cfi-cfi-05a      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice comune                    *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cmn              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice frazione                  *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fzn              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio partita iva                      *
      *        *-------------------------------------------------------*
           05  w-sav-prt-iva              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice fiscale                   *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fis              pic  x(16)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-pdt.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-pdt-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-pdt-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-pdt-val-pos          pic  9(07)                  .

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
      *              * casa produttrice                                *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf[rec-pdt]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-pdt
           else      move  spaces         to   w-prs-rec-pdt          .
           if        w-prs-rec-pdt-mco    not  numeric
                     move  zero           to   w-prs-rec-pdt-mco      .
           if        w-prs-rec-pdt-mco    =    zero
                     move  9999999        to   w-prs-rec-pdt-mco      .
           if        w-prs-rec-pdt-fco    not  = "A"
                     move  "M"            to   w-prs-rec-pdt-fco      .
           if        w-prs-rec-pdt-omn    not  = "O"
                     move  "N"            to   w-prs-rec-pdt-omn      .
           if        w-prs-rec-pdt-umn    not  = "U"
                     move  "N"            to   w-prs-rec-pdt-umn      .
           if        w-prs-rec-pdt-mmn    not  = "N"
                     move  "M"            to   w-prs-rec-pdt-mmn      .
           if        w-prs-rec-pdt-osc    not  = "N"
                     move  "N"            to   w-prs-rec-pdt-osc      .
           if        w-prs-rec-pdt-msc    not  = "N"
                     move  "M"            to   w-prs-rec-pdt-msc      .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice casa produttr.  *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-opn-000  thru cod-mne-pdt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nazione         *
      *              *-------------------------------------------------*
           perform   cod-des-naz-opn-000  thru cod-des-naz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione indirizzo + citta'     *
      *              *-------------------------------------------------*
           perform   ind-cec-geo-opn-000  thru ind-cec-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione c.a.p. e citta'        *
      *              *-------------------------------------------------*
           perform   cap-cit-geo-opn-000  thru cap-cit-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione comune                 *
      *              *-------------------------------------------------*
           perform   cod-com-geo-opn-000  thru cod-com-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione frazione               *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-opn-000  thru cod-fra-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione localita'              *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-opn-000  thru cod-loc-geo-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice casa produttr. *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-cls-000  thru cod-mne-pdt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nazione        *
      *              *-------------------------------------------------*
           perform   cod-des-naz-cls-000  thru cod-des-naz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione indirizzo + citta'    *
      *              *-------------------------------------------------*
           perform   ind-cec-geo-cls-000  thru ind-cec-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione c.a.p. e citta'       *
      *              *-------------------------------------------------*
           perform   cap-cit-geo-cls-000  thru cap-cit-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione comune                *
      *              *-------------------------------------------------*
           perform   cod-com-geo-cls-000  thru cod-com-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione frazione              *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-cls-000  thru cod-fra-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione localita'             *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-cls-000  thru cod-loc-geo-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [pdt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [pdt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *                  * Codice casa produttrice                     *
      *                  *---------------------------------------------*
           perform   acc-cod-pdt-000      thru acc-cod-pdt-999        .
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
      *              * Codice casa produttrice                         *
      *              *-------------------------------------------------*
           perform   vis-cod-pdt-000      thru vis-cod-pdt-999        .
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
      *              * Codice casa produttrice                         *
      *              *-------------------------------------------------*
           perform   pmt-cod-pdt-000      thru pmt-cod-pdt-999        .
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
      *    * Visualizzazione prompts per Codice casa produttrice       *
      *    *-----------------------------------------------------------*
       pmt-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice casa produttrice    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice casa produttrice       *
      *    *-----------------------------------------------------------*
       acc-cod-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdt-ope      .
           move      w-tes-cod-pdt        to   w-cod-mne-pdt-cod      .
           move      04                   to   w-cod-mne-pdt-lin      .
           move      30                   to   w-cod-mne-pdt-pos      .
           move      08                   to   w-cod-mne-pdt-rln      .
           move      30                   to   w-cod-mne-pdt-rps      .
           move      zero                 to   w-cod-mne-pdt-vln      .
           move      zero                 to   w-cod-mne-pdt-vps      .
           move      zero                 to   w-cod-mne-pdt-lln      .
           move      zero                 to   w-cod-mne-pdt-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdt-cll-000  thru cod-mne-pdt-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdt-foi-000  thru cod-mne-pdt-foi-999    .
       acc-cod-pdt-110.
           perform   cod-mne-pdt-cll-000  thru cod-mne-pdt-cll-999    .
           if        w-cod-mne-pdt-ope    =    "F+"
                     go to acc-cod-pdt-115.
           if        w-cod-mne-pdt-ope    =    "AC"
                     go to acc-cod-pdt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pdt-115.
           perform   cod-mne-pdt-foi-000  thru cod-mne-pdt-foi-999    .
           go to     acc-cod-pdt-110.
       acc-cod-pdt-120.
           move      w-cod-mne-pdt-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pdt-999.
       acc-cod-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-pdt          .
       acc-cod-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-pdt        =    zero
                     go to acc-cod-pdt-410
           else      go to acc-cod-pdt-450.
       acc-cod-pdt-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il tipo funzio- *
      *                      * namento per il codice e' automatico op- *
      *                      * pure manuale                            *
      *                      *-----------------------------------------*
           if        w-prs-rec-pdt-fco    not  = "A"
                     go to acc-cod-pdt-415
           else      go to acc-cod-pdt-420.
       acc-cod-pdt-415.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' manuale                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna azione, ne' controllo       *
      *                          *-------------------------------------*
           go to     acc-cod-pdt-600.
       acc-cod-pdt-420.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      w-prs-rec-pdt-mco    to   w-enc-pdt-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-pdt-val-pos    to   w-tes-cod-pdt          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-pdt-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-pdt-000      thru vis-cod-pdt-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-pdt-600.
       acc-cod-pdt-450.
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
           if        w-prs-rec-pdt-mco    =    zero or
                     w-prs-rec-pdt-mco    =    9999999
                     go to acc-cod-pdt-600.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato non e' superiore *
      *                      * al valore massimo impostabile, il con-  *
      *                      * trollo e' superato                      *
      *                      *-----------------------------------------*
           if        w-tes-cod-pdt        not  > w-prs-rec-pdt-mco
                     go to acc-cod-pdt-600.
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
           move      10                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      71                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Literals nel box                    *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Il codice casa produttrice non puo' superare "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      13                   to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "<"                  to   v-edm                  .
           move      w-prs-rec-pdt-mco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      67                   to   v-pos                  .
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
           move      68                   to   v-pos                  .
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
           go to     acc-cod-pdt-100.
       acc-cod-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-pdt-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pdt-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-pdt-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-pdt-999.
       acc-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice casa produttrice    *
      *    *-----------------------------------------------------------*
       vis-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-pdt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pdt-999.
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
      *              * La testata e' composta di nr. 2 pagine          *
      *              *-------------------------------------------------*
           move      2                    to   w-cnt-sts-imp-mpt      .
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
                     acc-tes-reg-200
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           perform   acc-cod-naz-000      thru acc-cod-naz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           perform   acc-rag-soc-000      thru acc-rag-soc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           perform   acc-via-pdt-000      thru acc-via-pdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           perform   acc-loc-pdt-000      thru acc-loc-pdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           perform   acc-cod-cmn-000      thru acc-cod-cmn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           perform   acc-cod-fzn-000      thru acc-cod-fzn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           perform   acc-cod-lct-000      thru acc-cod-lct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Partita iva                                 *
      *                  *---------------------------------------------*
           perform   acc-prt-iva-000      thru acc-prt-iva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Codice fiscale                              *
      *                  *---------------------------------------------*
           perform   acc-cod-fis-000      thru acc-cod-fis-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 1                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Telefono                                    *
      *                  *---------------------------------------------*
           perform   acc-num-tel-000      thru acc-num-tel-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Telefax                                     *
      *                  *---------------------------------------------*
           perform   acc-num-fax-000      thru acc-num-fax-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Telex                                       *
      *                  *---------------------------------------------*
           perform   acc-num-tlx-000      thru acc-num-tlx-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-210.
       acc-tes-reg-230.
      *                  *---------------------------------------------*
      *                  * Interlocutore                               *
      *                  *---------------------------------------------*
           perform   acc-nom-int-000      thru acc-nom-int-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
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
                     vis-tes-reg-200
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-naz-000      thru vis-cod-naz-999        .
      *              *-------------------------------------------------*
      *              * Descrizione nazione                             *
      *              *-------------------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   vis-rag-pdt-000      thru vis-rag-pdt-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   vis-via-pdt-000      thru vis-via-pdt-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   vis-loc-pdt-000      thru vis-loc-pdt-999        .
      *              *-------------------------------------------------*
      *              * Codice comune                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione comune                              *
      *              *-------------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice provincia                                *
      *              *-------------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione frazione                            *
      *              *-------------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita'                                *
      *              *-------------------------------------------------*
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * Descrizione localita'                           *
      *              *-------------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   vis-prt-iva-000      thru vis-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-fis-000      thru vis-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           perform   vis-num-tel-000      thru vis-num-tel-999        .
      *              *-------------------------------------------------*
      *              * Telefax                                         *
      *              *-------------------------------------------------*
           perform   vis-num-fax-000      thru vis-num-fax-999        .
      *              *-------------------------------------------------*
      *              * Telex                                           *
      *              *-------------------------------------------------*
           perform   vis-num-tlx-000      thru vis-num-tlx-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore                                   *
      *              *-------------------------------------------------*
           perform   vis-nom-int-000      thru vis-nom-int-999        .
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
                     pmt-tes-reg-200
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-naz-000      thru pmt-cod-naz-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   pmt-rag-pdt-000      thru pmt-rag-pdt-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   pmt-via-pdt-000      thru pmt-via-pdt-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   pmt-loc-pdt-000      thru pmt-loc-pdt-999        .
      *              *-------------------------------------------------*
      *              * Codice comune                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-cmn-000      thru pmt-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-fzn-000      thru pmt-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita'                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-lct-000      thru pmt-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           perform   pmt-num-tel-000      thru pmt-num-tel-999        .
      *              *-------------------------------------------------*
      *              * Telefax                                         *
      *              *-------------------------------------------------*
           perform   pmt-num-fax-000      thru pmt-num-fax-999        .
      *              *-------------------------------------------------*
      *              * Telex                                           *
      *              *-------------------------------------------------*
           perform   pmt-num-tlx-000      thru pmt-num-tlx-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore                                   *
      *              *-------------------------------------------------*
           perform   pmt-nom-int-000      thru pmt-nom-int-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice nazione                   *
      *    *-----------------------------------------------------------*
       pmt-cod-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice nazione             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ragione sociale                  *
      *    *-----------------------------------------------------------*
       pmt-rag-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rag-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indirizzo                        *
      *    *-----------------------------------------------------------*
       pmt-via-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indirizzo                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-via-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : C.a.p. e citta'                  *
      *    *-----------------------------------------------------------*
       pmt-loc-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "C.a.p. e citta'            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-loc-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice comune                    *
      *    *-----------------------------------------------------------*
       pmt-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice comune              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice frazione                  *
      *    *-----------------------------------------------------------*
       pmt-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice frazione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice localita'                 *
      *    *-----------------------------------------------------------*
       pmt-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice localita'           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Partita iva                      *
      *    *-----------------------------------------------------------*
       pmt-prt-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT "
                     move  "Partita iva                :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fiscale                   *
      *    *-----------------------------------------------------------*
       pmt-cod-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT "
                     move  "Codice fiscale             :"
                                          to   v-alf
           else      move  "Codice iva                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mnemonico           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Telefono                         *
      *    *-----------------------------------------------------------*
       pmt-num-tel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di telefono         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-tel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Telefax                          *
      *    *-----------------------------------------------------------*
       pmt-num-fax-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di telefax          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-fax-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Telex                            *
      *    *-----------------------------------------------------------*
       pmt-num-tlx-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di telex            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-tlx-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Interlocutore                    *
      *    *-----------------------------------------------------------*
       pmt-nom-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome interlocutore         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nom-int-999.
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
      *    * Accettazione campo testata : Codice nazione               *
      *    *-----------------------------------------------------------*
       acc-cod-naz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore gia' diverso da spaces non si *
      *                      * prepara alcun default                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-naz (1)    not  = spaces
                     go to acc-cod-naz-100.
      *                      *-----------------------------------------*
      *                      * Preparazione del valore di default      *
      *                      *-----------------------------------------*
           move      "IT "                to   w-tes-cod-naz (1)      .
      *                      *-----------------------------------------*
      *                      * Preparazione della descrizione relativa *
      *                      * al valore di default                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore di default       *
      *                      *-----------------------------------------*
           perform   vis-cod-naz-000      thru vis-cod-naz-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione relativa al *
      *                      * valore di default                       *
      *                      *-----------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
       acc-cod-naz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-naz-ope      .
           move      w-tes-cod-naz (1)    to   w-cod-des-naz-cod      .
           move      06                   to   w-cod-des-naz-lin      .
           move      30                   to   w-cod-des-naz-pos      .
           move      06                   to   w-cod-des-naz-dln      .
           move      36                   to   w-cod-des-naz-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
       acc-cod-naz-110.
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           if        w-cod-des-naz-ope    =    "F+"
                     go to acc-cod-naz-115.
           if        w-cod-des-naz-ope    =    "AC"
                     go to acc-cod-naz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-naz-115.
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
           go to     acc-cod-naz-110.
       acc-cod-naz-120.
           move      w-cod-des-naz-cod    to   v-alf                  .
       acc-cod-naz-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-naz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-naz-999.
       acc-cod-naz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-naz (1)      .
       acc-cod-naz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxn-flg    not  = spaces
                     go to acc-cod-naz-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    =    spaces
                     go to acc-cod-naz-100.
       acc-cod-naz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se Italia o Estero     *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    =    "IT "
                     go to acc-cod-naz-620
           else      go to acc-cod-naz-640.
       acc-cod-naz-620.
      *                  *---------------------------------------------*
      *                  * Se Italia                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Codice fiscale                        *
      *                      *-----------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-naz-800.
       acc-cod-naz-640.
      *                  *---------------------------------------------*
      *                  * Se Estero                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Comune                                *
      *                      * - Frazione                              *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-naz-642.
      *                          *-------------------------------------*
      *                          * Comune                              *
      *                          *-------------------------------------*
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-naz-644.
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
       acc-cod-naz-644.
      *                          *-------------------------------------*
      *                          * Frazione                            *
      *                          *-------------------------------------*
           if        w-tes-cod-fzn (1)    =    zero
                     go to acc-cod-naz-646.
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
       acc-cod-naz-646.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-naz-648.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-naz-648.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           if        w-tes-prt-iva (1)    =    zero
                     go to acc-cod-naz-650.
           move      zero                 to   w-tes-prt-iva (1)      .
           perform   vis-prt-iva-000      thru vis-prt-iva-999        .
       acc-cod-naz-650.
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Codice fiscale                        *
      *                      *-----------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-naz-800.
       acc-cod-naz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-naz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-naz-100.
       acc-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice nazione            *
      *    *-----------------------------------------------------------*
       vis-cod-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-naz (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione nazione       *
      *    *-----------------------------------------------------------*
       vis-des-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-naz-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-naz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Ragione sociale              *
      *    *-----------------------------------------------------------*
       acc-rag-soc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rag-soc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-rag-soc (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rag-soc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rag-soc-999.
       acc-rag-soc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-rag-soc (1)      .
       acc-rag-soc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-rag-soc (1)    not  = spaces
                     go to acc-rag-soc-450.
           if        v-key                =    "UP  "
                     go to acc-rag-soc-600
           else      go to acc-rag-soc-100.
       acc-rag-soc-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-rag-soc (1)
                    (01 : 01)             =    spaces
                     go to acc-rag-soc-100.
       acc-rag-soc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      w-tes-rag-soc (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-rag-key (1)      .
       acc-rag-soc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rag-soc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rag-soc-100.
       acc-rag-soc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ragione sociale           *
      *    *-----------------------------------------------------------*
       vis-rag-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rag-soc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Indirizzo                    *
      *    *-----------------------------------------------------------*
       acc-via-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-via-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-ind-cec-geo-naz      .
           move      w-tes-via-pdt (1)    to   w-ind-cec-geo-ind      .
           move      09                   to   w-ind-cec-geo-lin      .
           move      30                   to   w-ind-cec-geo-pos      .
           move      w-tes-loc-pdt (1)    to   w-ind-cec-geo-cec      .
           move      w-tes-cod-cmn (1)    to   w-ind-cec-geo-cmn      .
           move      w-tes-cod-cmn-des (1)
                                          to   w-ind-cec-geo-dco      .
           move      w-tes-cod-cmn-prv (1)
                                          to   w-ind-cec-geo-prv      .
           move      w-tes-cod-fzn (1)    to   w-ind-cec-geo-fzn      .
           move      w-tes-cod-fzn-des (1)
                                          to   w-ind-cec-geo-dfr      .
           move      w-tes-cod-lct (1)    to   w-ind-cec-geo-lct      .
           move      w-tes-cod-lct-des (1)
                                          to   w-ind-cec-geo-dlo      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   ind-cec-geo-foi-000  thru ind-cec-geo-foi-999    .
       acc-via-pdt-110.
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           if        w-ind-cec-geo-ope    =    "F+"
                     go to acc-via-pdt-115.
           if        w-ind-cec-geo-ope    =    "AC"
                     go to acc-via-pdt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-via-pdt-115.
           perform   ind-cec-geo-foi-000  thru ind-cec-geo-foi-999    .
           go to     acc-via-pdt-110.
       acc-via-pdt-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-via-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-via-pdt-999.
       acc-via-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-ind-cec-geo-ind    to   w-tes-via-pdt (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-aut    =    spaces
                     go to acc-via-pdt-400.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cec    to   w-tes-loc-pdt (1)      .
           perform   vis-loc-pdt-000      thru vis-loc-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Comune                                      *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cmn    to   w-tes-cod-cmn (1)      .
           move      w-ind-cec-geo-dco    to   w-tes-cod-cmn-des (1)  .
           move      w-ind-cec-geo-prv    to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Frazione                                    *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-fzn    to   w-tes-cod-fzn (1)      .
           move      w-ind-cec-geo-dfr    to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-lct    to   w-tes-cod-lct (1)      .
           move      w-ind-cec-geo-dlo    to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-via-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-via-pdt (1)    =    spaces
                     go to acc-via-pdt-600.
      *
           if        w-tes-via-pdt (1)
                    (01 : 01)             =    spaces
                     go to acc-via-pdt-100.
       acc-via-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-via-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-via-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-via-pdt-100.
       acc-via-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Indirizzo                 *
      *    *-----------------------------------------------------------*
       vis-via-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-via-pdt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-via-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : C.a.p. e citta'              *
      *    *-----------------------------------------------------------*
       acc-loc-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-loc-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-cap-cit-geo-naz      .
           move      w-tes-loc-pdt (1)    to   w-cap-cit-geo-cec      .
           move      10                   to   w-cap-cit-geo-lin      .
           move      30                   to   w-cap-cit-geo-pos      .
           move      w-tes-cod-cmn (1)    to   w-cap-cit-geo-cmn      .
           move      w-tes-cod-cmn-des (1)
                                          to   w-cap-cit-geo-dco      .
           move      w-tes-cod-cmn-prv (1)
                                          to   w-cap-cit-geo-prv      .
           move      w-tes-cod-fzn (1)    to   w-cap-cit-geo-fzn      .
           move      w-tes-cod-fzn-des (1)
                                          to   w-cap-cit-geo-dfr      .
           move      w-tes-cod-lct (1)    to   w-cap-cit-geo-lct      .
           move      w-tes-cod-lct-des (1)
                                          to   w-cap-cit-geo-dlo      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cap-cit-geo-cll-000  thru cap-cit-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cap-cit-geo-foi-000  thru cap-cit-geo-foi-999    .
       acc-loc-pdt-110.
           perform   cap-cit-geo-cll-000  thru cap-cit-geo-cll-999    .
           if        w-cap-cit-geo-ope    =    "F+"
                     go to acc-loc-pdt-115.
           if        w-cap-cit-geo-ope    =    "AC"
                     go to acc-loc-pdt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-loc-pdt-115.
           perform   cap-cit-geo-foi-000  thru cap-cit-geo-foi-999    .
           go to     acc-loc-pdt-110.
       acc-loc-pdt-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-loc-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-loc-pdt-999.
       acc-loc-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cap-cit-geo-cec    to   w-tes-loc-pdt (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-cap-cit-geo-aut    =    spaces
                     go to acc-loc-pdt-400.
       acc-loc-pdt-300.
      *                  *---------------------------------------------*
      *                  * Indirizzo, solo se l'elemento selezionato   *
      *                  * e' una localita'                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non si tratta di una localita' non   *
      *                      * si esegue alcun automatismo sull'indi-  *
      *                      * rizzo                                   *
      *                      *-----------------------------------------*
           if        w-cap-cit-geo-lct    =    zero
                     go to acc-loc-pdt-350.
      *                      *-----------------------------------------*
      *                      * Concatenamento della localita' selezio- *
      *                      * nata con l'indirizzo precedente         *
      *                      *-----------------------------------------*
           move      "IL"                 to   w-ind-cec-geo-ope      .
           move      w-tes-via-pdt (1)    to   w-ind-cec-geo-ind      .
           move      w-cap-cit-geo-dlo    to   w-ind-cec-geo-dlo      .
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           move      w-ind-cec-geo-ind    to   w-tes-via-pdt (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione indirizzo concatenato   *
      *                      *-----------------------------------------*
           perform   vis-via-pdt-000      thru vis-via-pdt-999        .
       acc-loc-pdt-350.
      *                  *---------------------------------------------*
      *                  * Comune                                      *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-cmn    to   w-tes-cod-cmn (1)      .
           move      w-cap-cit-geo-dco    to   w-tes-cod-cmn-des (1)  .
           move      w-cap-cit-geo-prv    to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Frazione                                    *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-fzn    to   w-tes-cod-fzn (1)      .
           move      w-cap-cit-geo-dfr    to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-lct    to   w-tes-cod-lct (1)      .
           move      w-cap-cit-geo-dlo    to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-loc-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-loc-pdt (1)    =    spaces
                     go to acc-loc-pdt-600.
      *
           if        w-tes-loc-pdt (1)
                    (01 : 01)             =    spaces
                     go to acc-loc-pdt-100.
       acc-loc-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-loc-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-loc-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-loc-pdt-100.
       acc-loc-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : C.a.p. e citta'           *
      *    *-----------------------------------------------------------*
       vis-loc-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-loc-pdt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-loc-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice comune                *
      *    *-----------------------------------------------------------*
       acc-cod-cmn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cmn-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-cmn-999.
       acc-cod-cmn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cmn (1)    to   w-sav-cod-cmn          .
       acc-cod-cmn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-com-geo-cmn      .
           move      12                   to   w-cod-com-geo-lin      .
           move      30                   to   w-cod-com-geo-pos      .
           move      12                   to   w-cod-com-geo-dln      .
           move      36                   to   w-cod-com-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
       acc-cod-cmn-110.
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           if        w-cod-com-geo-ope    =    "F+"
                     go to acc-cod-cmn-115.
           if        w-cod-com-geo-ope    =    "AC"
                     go to acc-cod-cmn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cmn-115.
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
           go to     acc-cod-cmn-110.
       acc-cod-cmn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
       acc-cod-cmn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-com-geo-cmn    to   w-tes-cod-cmn (1)      .
       acc-cod-cmn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione provincia                   *
      *                  *---------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-cmn-100.
       acc-cod-cmn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    =    w-sav-cod-cmn
                     go to acc-cod-cmn-625
           else      go to acc-cod-cmn-650.
       acc-cod-cmn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Frazione                              *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-cmn-652.
      *                          *-------------------------------------*
      *                          * Frazione                            *
      *                          *-------------------------------------*
           if        w-tes-cod-fzn (1)    =    zero
                     go to acc-cod-cmn-654.
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
       acc-cod-cmn-654.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-cmn-656.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-cmn-656.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cmn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cmn-100.
       acc-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice Comune             *
      *    *-----------------------------------------------------------*
       vis-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cmn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione Comune        *
      *    *-----------------------------------------------------------*
       vis-des-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-cmn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice provincia          *
      *    *-----------------------------------------------------------*
       vis-cod-prv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-tes-cod-cmn-prv (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-prv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice frazione              *
      *    *-----------------------------------------------------------*
       acc-cod-fzn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fzn-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-fzn-999.
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-fzn-999.
       acc-cod-fzn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fzn (1)    to   w-sav-cod-fzn          .
       acc-cod-fzn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-fra-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-fra-geo-fzn      .
           move      13                   to   w-cod-fra-geo-lin      .
           move      30                   to   w-cod-fra-geo-pos      .
           move      13                   to   w-cod-fra-geo-dln      .
           move      36                   to   w-cod-fra-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
       acc-cod-fzn-110.
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           if        w-cod-fra-geo-ope    =    "F+"
                     go to acc-cod-fzn-115.
           if        w-cod-fra-geo-ope    =    "AC"
                     go to acc-cod-fzn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fzn-115.
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
           go to     acc-cod-fzn-110.
       acc-cod-fzn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
       acc-cod-fzn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-fra-geo-fzn    to   w-tes-cod-fzn (1)      .
       acc-cod-fzn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-fzn-100.
       acc-cod-fzn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fzn (1)    =    w-sav-cod-fzn
                     go to acc-cod-fzn-625
           else      go to acc-cod-fzn-650.
       acc-cod-fzn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-fzn-652.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-fzn-654.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-fzn-654.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fzn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fzn-100.
       acc-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice frazione           *
      *    *-----------------------------------------------------------*
       vis-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fzn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione frazione      *
      *    *-----------------------------------------------------------*
       vis-des-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-fzn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fzn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice localita'             *
      *    *-----------------------------------------------------------*
       acc-cod-lct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-lct-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-lct-999.
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-lct-999.
       acc-cod-lct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-loc-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-loc-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-loc-geo-fzn      .
           move      w-tes-cod-lct (1)    to   w-cod-loc-geo-lct      .
           move      14                   to   w-cod-loc-geo-lin      .
           move      30                   to   w-cod-loc-geo-pos      .
           move      14                   to   w-cod-loc-geo-dln      .
           move      36                   to   w-cod-loc-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
       acc-cod-lct-110.
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           if        w-cod-loc-geo-ope    =    "F+"
                     go to acc-cod-lct-115.
           if        w-cod-loc-geo-ope    =    "AC"
                     go to acc-cod-lct-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lct-115.
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
           go to     acc-cod-lct-110.
       acc-cod-lct-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
       acc-cod-lct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-loc-geo-lct    to   w-tes-cod-lct (1)      .
       acc-cod-lct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-lct-100.
       acc-cod-lct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-lct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-lct-100.
       acc-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice localita'          *
      *    *-----------------------------------------------------------*
       vis-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-lct (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione localita'     *
      *    *-----------------------------------------------------------*
       vis-des-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-lct-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-lct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Partita iva                  *
      *    *-----------------------------------------------------------*
       acc-prt-iva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-prt-iva-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio partita iva e codice fiscale    *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-sav-prt-iva          .
           move      w-tes-cod-fis (1)    to   w-sav-cod-fis          .
      *                  *---------------------------------------------*
      *                  * Partita iva in rappresentazione alfanumeri- *
      *                  * ca                                          *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-ctl-prt-iva-num      .
           if        w-ctl-prt-iva-num    =    zero
                     move  spaces         to   w-ctl-prt-iva-alf      .
       acc-prt-iva-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-ctl-prt-iva-alf    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prt-iva-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prt-iva-999.
       acc-prt-iva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione con   *
      *              * rappresentazione alfanumerica                   *
      *              *-------------------------------------------------*
           move      v-alf                to   w-ctl-prt-iva-alf      .
       acc-prt-iva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : forzatura di zero      *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-alf    =    spaces
                     move  zero           to   w-ctl-prt-iva-num      .
      *                  *---------------------------------------------*
      *                  * Se valore non-numerico : reimpostazione     *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-num    not  numeric
                     go to acc-prt-iva-100.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo definitivo        *
      *                  *---------------------------------------------*
           move      w-ctl-prt-iva-num    to   w-tes-prt-iva (1)      .
      *                  *---------------------------------------------*
      *                  * Se la partita iva e' a zero non si esegue   *
      *                  * il controllo                                *
      *                  *---------------------------------------------*
           if        w-tes-prt-iva (1)    =    "000000000000"
                     go to acc-prt-iva-600.
      *                  *---------------------------------------------*
      *                  * Se la partita iva e' pari al valore prece-  *
      *                  * dente non si esegue il controllo            *
      *                  *---------------------------------------------*
           if        w-tes-prt-iva (1)    =    w-sav-prt-iva
                     go to acc-prt-iva-600.
      *                  *---------------------------------------------*
      *                  * Controllo formale sulla partita iva         *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-ctl-prt-iva-piv      .
           perform   ctl-prt-iva-000      thru ctl-prt-iva-999        .
      *                  *---------------------------------------------*
      *                  * Se controllo formale superato si prosegue   *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-flg    =    spaces
                     go to  acc-prt-iva-600.
       acc-prt-iva-420.
      *                  *---------------------------------------------*
      *                  * Se controllo formale non superato           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Literals nel box                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione :  La partita iva risulta formalmente s
      -              "corretta.     "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scelta     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prt-iva-425.
      *                      *-----------------------------------------*
      *                      * Accettazione risposta                   *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-err-piv-lun    to   v-car                  .
           move      w-exp-err-piv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-err-piv-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-err-piv-sce      .
      *                      *-----------------------------------------*
      *                      * Controllo risposta                      *
      *                      *-----------------------------------------*
           if        w-exp-err-piv-sce    not  = 01 and
                     w-exp-err-piv-sce    not  = 02
                     go to acc-prt-iva-425.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della risposta     *
      *                      *-----------------------------------------*
           if        w-exp-err-piv-sce    =    01
                     go to acc-prt-iva-100.
       acc-prt-iva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore variato rispetto al valore prece- *
      *                  * dente si ricalcola automaticamente, se pos- *
      *                  * sibile, il codice fiscale in funzione della *
      *                  * partita iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se avvenuta variazione             *
      *                      *-----------------------------------------*
           if        w-tes-prt-iva (1)    =    w-sav-prt-iva
                     go to acc-prt-iva-612.
      *                      *-----------------------------------------*
      *                      * Se il codice fiscale attuale e' a spa-  *
      *                      * ces si esegue comunque l'automatismo    *
      *                      *-----------------------------------------*
           if        w-tes-cod-fis (1)    =    spaces
                     go to acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Se precedentemente alla variazione il   *
      *                      * codice fiscale era pari alla partita    *
      *                      * iva, si esegue l'automatismo            *
      *                      *-----------------------------------------*
           move      spaces               to   w-piv-cfi-cfi-alf      .
           move      w-sav-prt-iva        to   w-piv-cfi-cfi-11n      .
           if        w-piv-cfi-cfi-alf    =    w-sav-cod-fis
                     go to acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Altrimenti l'automatismo non viene ese- *
      *                      * guito                                   *
      *                      *-----------------------------------------*
           go to     acc-prt-iva-612.
       acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Esecuzione automatismo                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice fiscale attuale pari alla    *
      *                          * partita iva attuale                 *
      *                          *-------------------------------------*
           move      spaces               to   w-piv-cfi-cfi-alf      .
           if        w-tes-prt-iva (1)    =    zero
                     go to acc-prt-iva-606.
           move      w-tes-prt-iva (1)    to   w-piv-cfi-cfi-11n      .
       acc-prt-iva-606.
           move      w-piv-cfi-cfi-alf    to   w-tes-cod-fis (1)      .
       acc-prt-iva-609.
      *                          *-------------------------------------*
      *                          * Visualizzazione codice fiscale      *
      *                          *-------------------------------------*
           perform   vis-cod-fis-000      thru vis-cod-fis-999        .
       acc-prt-iva-612.
           go to     acc-prt-iva-800.
       acc-prt-iva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prt-iva-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prt-iva-100.
       acc-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Partita iva               *
      *    *-----------------------------------------------------------*
       vis-prt-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prt-iva (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice fiscale                       *
      *    *-----------------------------------------------------------*
       acc-cod-fis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio codice fiscale                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-fis (1)    to   w-sav-cod-fis          .
       acc-cod-fis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-fis (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fis-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fis-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-fis (1)      .
       acc-cod-fis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la casa produttrice e' estera non si e-  *
      *                  * segue il controllo                          *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' completamente a     *
      *                  * spaces non si esegue il controllo           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    spaces
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari al valore pre- *
      *                  * cedente non si esegue il controllo          *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    w-sav-cod-fis
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari alla partita   *
      *                  * iva non si esegue il controllo              *
      *                  *---------------------------------------------*
           move      w-tes-cod-fis (1)    to   w-piv-cfi-cfi-alf      .
           if        w-piv-cfi-cfi-11n    =    w-tes-prt-iva (1) and
                     w-piv-cfi-cfi-05a    =    spaces
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della lunghezza ef-  *
      *                  * fettiva del valore                          *
      *                  *---------------------------------------------*
           if        w-piv-cfi-cfi-05a    not  = spaces
                     go to acc-cod-fis-406.
       acc-cod-fis-403.
      *                  *---------------------------------------------*
      *                  * Se la lunghezza effettiva del valore non    *
      *                  * supera gli 11 caratteri                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore non e' numerico : errore   *
      *                      *-----------------------------------------*
           if        w-piv-cfi-cfi-11n    not  numeric
                     go to acc-cod-fis-409.
      *                      *-----------------------------------------*
      *                      * Controllo formale come partita iva      *
      *                      *-----------------------------------------*
           move      w-piv-cfi-cfi-11n    to   w-ctl-prt-iva-piv      .
           perform   ctl-prt-iva-000      thru ctl-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Se controllo formale superato si prose- *
      *                      * gue, altrimenti si emette la segnala-   *
      *                      * zione di errore                         *
      *                      *-----------------------------------------*
           if        w-ctl-prt-iva-flg    =    spaces
                     go to  acc-cod-fis-415
           else      go to  acc-cod-fis-409.
       acc-cod-fis-406.
      *                  *---------------------------------------------*
      *                  * Se la lunghezza effettiva del valore supera *
      *                  * gli 11 caratteri                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo formale come codice fiscale   *
      *                      *-----------------------------------------*
           move      w-tes-cod-fis (1)    to   w-ctl-cod-fis-cof      .
           perform   ctl-cod-fis-000      thru ctl-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Se controllo formale superato si prose- *
      *                      * gue, altrimenti si emette la segnala-   *
      *                      * zione di errore                         *
      *                      *-----------------------------------------*
           if        w-ctl-cod-fis-flg    =    spaces
                     go to  acc-cod-fis-415
           else      go to  acc-cod-fis-409.
       acc-cod-fis-409.
      *                  *---------------------------------------------*
      *                  * Se controllo formale non superato           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Literals nel box                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione :  Il codice fiscale risulta formalment
      -              "e scorretto.  "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scelta     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-fis-412.
      *                      *-----------------------------------------*
      *                      * Accettazione risposta                   *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-err-cfi-lun    to   v-car                  .
           move      w-exp-err-cfi-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-err-cfi-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-err-cfi-sce      .
      *                      *-----------------------------------------*
      *                      * Controllo risposta                      *
      *                      *-----------------------------------------*
           if        w-exp-err-cfi-sce    not  = 01 and
                     w-exp-err-cfi-sce    not  = 02
                     go to acc-cod-fis-412.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della risposta     *
      *                      *-----------------------------------------*
           if        w-exp-err-cfi-sce    =    01
                     go to acc-cod-fis-100
           else      go to acc-cod-fis-415.
       acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo controlli                 *
      *                  *---------------------------------------------*
           go to     acc-cod-fis-600.
       acc-cod-fis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fis-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fis-100.
       acc-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice fiscale                    *
      *    *-----------------------------------------------------------*
       vis-cod-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fis (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fis-999.
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
           if        w-prs-rec-pdt-mmn    =    "N"
                     go to acc-cod-mne-999.
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      19                   to   v-lin                  .
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
           if        w-prs-rec-pdt-omn    not  = "O"
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
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Telefono                     *
      *    *-----------------------------------------------------------*
       acc-num-tel-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-tel-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-tel (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-tel-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-tel-999.
       acc-num-tel-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-num-tel (1)      .
       acc-num-tel-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-tel-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-tel-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-tel-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-tel-100.
       acc-num-tel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Telefono                  *
      *    *-----------------------------------------------------------*
       vis-num-tel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-tel (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-tel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Telefax                      *
      *    *-----------------------------------------------------------*
       acc-num-fax-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-fax-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-fax (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-fax-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-fax-999.
       acc-num-fax-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-num-fax (1)      .
       acc-num-fax-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-fax-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-fax-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-fax-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-fax-100.
       acc-num-fax-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Telefax                   *
      *    *-----------------------------------------------------------*
       vis-num-fax-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-fax (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-fax-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Telex                        *
      *    *-----------------------------------------------------------*
       acc-num-tlx-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-tlx-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-tlx (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-tlx-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-tlx-999.
       acc-num-tlx-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-num-tlx (1)      .
       acc-num-tlx-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-tlx-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-tlx-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-tlx-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-tlx-100.
       acc-num-tlx-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Telex                     *
      *    *-----------------------------------------------------------*
       vis-num-tlx-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-tlx (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-tlx-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Interlocutore                *
      *    *-----------------------------------------------------------*
       acc-nom-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nom-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-nom-int (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nom-int-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nom-int-999.
       acc-nom-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-nom-int (1)      .
       acc-nom-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nom-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nom-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nom-int-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nom-int-100.
       acc-nom-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Interlocutore             *
      *    *-----------------------------------------------------------*
       vis-nom-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-nom-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-int-999.
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
           if        w-tes-cod-pdt        =    zero
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
      *              * Controllo su Codice nazione                     *
      *              *-------------------------------------------------*
           if        w-tes-cod-naz (1)    not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca il codice nazione                         "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su Ragione sociale                    *
      *              *-------------------------------------------------*
           if        w-tes-rag-soc (1)    not  = spaces
                     go to cnt-tdo-nok-150.
           move      "Manca la ragione sociale                        "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se obbligatorio        *
      *              *-------------------------------------------------*
           if        w-prs-rec-pdt-omn    not  = "O"
                     go to cnt-tdo-nok-200.
           if        w-tes-cod-mne (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Manca il codice mnemonico                       "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se unico               *
      *              *-------------------------------------------------*
           if        w-prs-rec-pdt-umn    not  = "U"
                     go to cnt-tdo-nok-250.
           if        w-tes-cod-mne (1)    =    spaces
                     go to cnt-tdo-nok-250.
       cnt-tdo-nok-205.
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-mne (1)    to   rf-pdt-cod-mne         .
           move      zero                 to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-250.
       cnt-tdo-nok-210.
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-250.
           if        rf-pdt-cod-mne       not  = w-tes-cod-mne (1)
                     go to cnt-tdo-nok-250.
           if        rf-pdt-cod-pdt       =    w-tes-cod-pdt
                     go to cnt-tdo-nok-210.
           move      "Codice mnemonico gia' esistente                 "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se non modificabile    *
      *              *-------------------------------------------------*
           if        w-prs-rec-pdt-mmn    not  = "N"
                     go to cnt-tdo-nok-300.
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to cnt-tdo-nok-300.
           if        w-tes-cod-mne (2)    =    spaces
                     go to cnt-tdo-nok-300.
           if        w-tes-cod-mne (1)    =    w-tes-cod-mne (2)
                     go to cnt-tdo-nok-300.
           move      "Il codice mnemonico non puo' essere modificato  "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
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
           move      zero                 to   w-tes-cod-pdt          .
           move      spaces               to   w-tes-cod-pdt-aut      .
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
           move      spaces               to   w-tes-rag-key (1)      .
           move      spaces               to   w-tes-rag-soc (1)      .
           move      spaces               to   w-tes-via-pdt (1)      .
           move      spaces               to   w-tes-loc-pdt (1)      .
           move      spaces               to   w-tes-cod-naz (1)      .
           move      spaces               to   w-tes-cod-naz-des (1)  .
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-fax (1)      .
           move      spaces               to   w-tes-num-tlx (1)      .
           move      spaces               to   w-tes-nom-int (1)      .
           move      zero                 to   w-tes-prt-iva (1)      .
           move      spaces               to   w-tes-cod-fis (1)      .
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
           move      "CODPDT    "         to   f-key                  .
           move      w-tes-cod-pdt        to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
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
           move      rf-pdt-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-pdt-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-pdt-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-pdt-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-pdt-rag-key       to   w-tes-rag-key (1)      .
           move      rf-pdt-rag-soc       to   w-tes-rag-soc (1)      .
           move      rf-pdt-via-pdt       to   w-tes-via-pdt (1)      .
           move      rf-pdt-loc-pdt       to   w-tes-loc-pdt (1)      .
           move      rf-pdt-cod-naz       to   w-tes-cod-naz (1)      .
           move      rf-pdt-cod-cmn       to   w-tes-cod-cmn (1)      .
           move      rf-pdt-cod-fzn       to   w-tes-cod-fzn (1)      .
           move      rf-pdt-cod-lct       to   w-tes-cod-lct (1)      .
           move      rf-pdt-num-tel       to   w-tes-num-tel (1)      .
           move      rf-pdt-num-fax       to   w-tes-num-fax (1)      .
           move      rf-pdt-num-tlx       to   w-tes-num-tlx (1)      .
           move      rf-pdt-nom-int       to   w-tes-nom-int (1)      .
           move      rf-pdt-prt-iva       to   w-tes-prt-iva (1)      .
           move      rf-pdt-cod-fis       to   w-tes-cod-fis (1)      .
           move      rf-pdt-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-200.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [pdt]                        *
      *                          *-------------------------------------*
       rou-let-reg-205.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * nazione                         *
      *                              *---------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
       rou-let-reg-210.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Comune                          *
      *                              *---------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
       rou-let-reg-215.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Frazione                        *
      *                              *---------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
       rou-let-reg-220.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
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
           if        w-tes-cod-pdt-aut    =    spaces
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
      *              * Trattamento file [pdt]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [pdt]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-pdt-000      thru wrt-rec-pdt-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [pdt]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-pdt-000      thru rew-rec-pdt-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [pdt]                             *
      *              *-------------------------------------------------*
           perform   del-rec-pdt-000      thru del-rec-pdt-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdt]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-pdt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-pdt        to   rf-pdt-cod-pdt         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-pdt-ide-dat         .
           move      s-ute                to   rf-pdt-ide-ute         .
           move      s-fas                to   rf-pdt-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-pdt-cod-mne         .
           move      w-tes-rag-key (1)    to   rf-pdt-rag-key         .
           move      w-tes-rag-soc (1)    to   rf-pdt-rag-soc         .
           move      w-tes-via-pdt (1)    to   rf-pdt-via-pdt         .
           move      w-tes-loc-pdt (1)    to   rf-pdt-loc-pdt         .
           move      w-tes-cod-naz (1)    to   rf-pdt-cod-naz         .
           move      w-tes-cod-cmn (1)    to   rf-pdt-cod-cmn         .
           move      w-tes-cod-fzn (1)    to   rf-pdt-cod-fzn         .
           move      w-tes-cod-lct (1)    to   rf-pdt-cod-lct         .
           move      w-tes-num-tel (1)    to   rf-pdt-num-tel         .
           move      w-tes-num-fax (1)    to   rf-pdt-num-fax         .
           move      w-tes-num-tlx (1)    to   rf-pdt-num-tlx         .
           move      w-tes-nom-int (1)    to   rf-pdt-nom-int         .
           move      w-tes-prt-iva (1)    to   rf-pdt-prt-iva         .
           move      w-tes-cod-fis (1)    to   rf-pdt-cod-fis         .
           move      w-tes-alx-exp (1)    to   rf-pdt-alx-exp         .
       cmp-rec-pdt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdt]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-pdt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdt-000      thru cmp-rec-pdt-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
       wrt-rec-pdt-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [pdt]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-pdt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdt-000      thru cmp-rec-pdt-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
       rew-rec-pdt-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdt]                                *
      *    *-----------------------------------------------------------*
       del-rec-pdt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdt-000      thru cmp-rec-pdt-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
       del-rec-pdt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [gxn]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    spaces
                     go to let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      w-let-arc-gxn-cod    to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxn-400.
       let-arc-gxn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxn-des-naz       to   w-let-arc-gxn-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxn-999.
       let-arc-gxn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxn-flg      .
           move      all   "."            to   w-let-arc-gxn-des      .
           go to     let-arc-gxn-999.
       let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-des      .
       let-arc-gxn-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines di controllo formale Partita Iva e Codice     *
      *    * Fiscale                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wks"                   .

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [pdt]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "pdt "               to   s-nam                  .
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
           move      "pdt "               to   s-nam                  .
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
           move      s-num                to   w-enc-pdt-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-pdt-val-pre    to   w-enc-pdt-val-pos      .
           add       1                    to   w-enc-pdt-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-pdt-val-pos    =    zero
                     move  1              to   w-enc-pdt-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-pdt-val-pos    >    w-enc-pdt-val-max
                     move  1              to   w-enc-pdt-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDT    "         to   f-key                  .
           move      w-enc-pdt-val-pos    to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
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
           add       1                    to   w-enc-pdt-val-pos      .
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
           move      "pdt "               to   s-nam                  .
           move      w-enc-pdt-val-pos    to   s-num                  .
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
      *              * Lettura codice automatico per [pdt]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "pdt "               to   s-nam                  .
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
           if        s-num                =    w-enc-pdt-val-pos
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
           move      "pdt "               to   s-nam                  .
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
           move      "pdt "               to   s-nam                  .
           move      w-enc-pdt-val-pre    to   s-num                  .
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
      *    * Subroutines per l'accettazione del codice casa produttr.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione Indirizzo + Citta'         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/aiplgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione C.a.p. e citta'            *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acecgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Comune 'geo'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Frazione 'geo'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Localita' 'geo'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acs"                   .

