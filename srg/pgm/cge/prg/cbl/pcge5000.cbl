       Identification Division.
       Program-Id.                                 pcge5000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    for                 *
      *                                   Fase:    cge500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/05/91    *
      *                       Ultima revisione:    NdK del 05/11/20    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio fornitori contabilita'    *
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
                     "for"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge500"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge5000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "       GESTIONE ARCHIVIO FORNITORI      "       .

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
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-fnt          pic  9(07)                  .
               10  w-tes-cod-fnt-aut      pic  x(01)                  .
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
               10  w-tes-via-fnt          pic  x(40)                  .
               10  w-tes-loc-fnt          pic  x(40)                  .
               10  w-tes-cod-naz          pic  x(03)                  .
               10  w-tes-cod-naz-des      pic  x(20)                  .
               10  w-tes-cod-cmn          pic  9(05)                  .
               10  w-tes-cod-cmn-des      pic  x(30)                  .
               10  w-tes-cod-cmn-prv      pic  x(02)                  .
               10  w-tes-cod-fzn          pic  9(03)                  .
               10  w-tes-cod-fzn-des      pic  x(30)                  .
               10  w-tes-cod-lct          pic  9(03)                  .
               10  w-tes-cod-lct-des      pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Numero fax riutilizzato come codice fiscale dei   *
      *            * fornitori estero                                  *
      *            *---------------------------------------------------*
               10  w-tes-num-fax          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Numeri utenza obsoleti                            *
      *            *---------------------------------------------------*
               10  w-tes-num-tel          pic  x(20)                  .
               10  w-tes-num-tlx          pic  x(20)                  .
               10  w-tes-nom-int          pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Contatti                                          *
      *            *---------------------------------------------------*
               10  w-tes-con-arc occurs 50.
                   15  w-tes-tip-con      pic  x(03)                  .
                   15  w-tes-num-con      pic  x(80)                  .
                   15  w-tes-int-con      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice esenzione Iva                              *
      *            *---------------------------------------------------*
               10  w-tes-cod-iva          pic  9(05)                  .
               10  w-tes-cod-iva-des      pic  x(15)                  .
               10  w-tes-prt-iva          pic  9(11)                  .
               10  w-tes-cod-fis          pic  x(20)                  .
               10  w-tes-snx-a13          pic  x(01)                  .
               10  w-tes-cod-cge          pic  9(07)                  .
               10  w-tes-cod-cge-des      pic  x(40)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative al record fornitore        *
      *        *-------------------------------------------------------*
           05  w-prs-rec-fnt.
      *            *---------------------------------------------------*
      *            * Valore massimo accettabile per il codice          *
      *            *                                                   *
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-mxc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzionamento codice, in creazione           *
      *            *                                                   *
      *            * - M : Manuale                                     *
      *            * - A : Automatico                                  *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-fco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del mnemonico                     *
      *            *                                                   *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-omn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Unicita' del mnemonico                            *
      *            *                                                   *
      *            * - N : Non necessariamente unico, si' duplicati    *
      *            * - U : Unico, duplicati non ammessi                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-umn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del mnemonico                     *
      *            *                                                   *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-mmn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del sottoconto contabile          *
      *            *                                                   *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-osc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del sottoconto contabile          *
      *            *                                                   *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-msc      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo accettazione ragione sociale fornitore       *
      *            *                                                   *
      *            * - N : Normale                                     *
      *            * - U : In uppercase (maiuscole)                    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-ars      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo accettazione indirizzo fornitore             *
      *            *                                                   *
      *            * - N : Normale                                     *
      *            * - U : In uppercase (maiuscole)                    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-ain      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo accettazione localita' fornitore             *
      *            *                                                   *
      *            * - N : Normale                                     *
      *            * - U : In uppercase (maiuscole)                    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-alo      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' della Partita Iva                 *
      *            *                                                   *
      *            * - N : Non obbligatoria                            *
      *            * - M : Non obbligatoria, ma con messaggio          *
      *            * - O : Obbligatoria                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-opi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del Codice fiscale                *
      *            *                                                   *
      *            * - N : Non obbligatorio                            *
      *            * - M : Non obbligatorio, ma con messaggio          *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-ocf      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore minimo accettabile per il codice           *
      *            *                                                   *
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-mic      pic  9(07)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Range di codici in base alla dipendenza           *
      *            *                                                   *
      *            * - N : Nessun range                                *
      *            * - R : Da apposita referenza                       *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-rcd      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
               
      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Sottoconto contabile da proporre come default         *
      *        *-------------------------------------------------------*
           05  w-ref-stc-fnt              pic  9(07)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
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
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

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
      *        * Work per : Modifica partita iva                       *
      *        *-------------------------------------------------------*
           05  w-exp-prt-iva.
               10  w-exp-prt-iva-num      pic  9(02)       value 2    .
               10  w-exp-prt-iva-lun      pic  9(02)       value 40   .
               10  w-exp-prt-iva-tbl.
                   15  filler             pic  x(40) value
                            "Aggiornamento del codice fiscale        ".
                   15  filler             pic  x(40) value
                            "Nessuna azione                          ".
               10  w-exp-prt-iva-sce      pic  9(02)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per : Si/no in elenco Iva                        *
      *        *-------------------------------------------------------*
           05  w-exp-snx-ecf.
               10  w-exp-snx-ecf-num      pic  9(02)       value 2    .
               10  w-exp-snx-ecf-lun      pic  9(02)       value 02   .
               10  w-exp-snx-ecf-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl se partita iva gia' assegnata            *
      *        *-------------------------------------------------------*
           05  w-ctl-dup-piv.
               10  w-ctl-dup-piv-flg      pic  x(01)                  .
               10  w-ctl-dup-piv-fnt      pic  9(07)                  .
               10  w-ctl-dup-piv-ftt      pic  9(07)                  .
               10  w-ctl-dup-piv-ctr      pic  9(05)                  .
               10  w-ctl-dup-piv-piv      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Ctl se codice fiscale gia' assegnato         *
      *        *-------------------------------------------------------*
           05  w-ctl-dup-cfi.
               10  w-ctl-dup-cfi-flg      pic  x(01)                  .
               10  w-ctl-dup-cfi-fnt      pic  9(07)                  .
               10  w-ctl-dup-cfi-ftt      pic  9(07)                  .
               10  w-ctl-dup-cfi-ctr      pic  9(05)                  .
               10  w-ctl-dup-cfi-cfi      pic  x(20)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore contabile     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

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
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di controllo formale Partita Iva e   *
      *    * Codici Fiscale                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wkl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

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
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

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
           05  w-sav-cod-fis              pic  x(20)                  .

      *    *===========================================================*
      *    * Work per accettazione contatti e utenze                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/aconarc0.acl"                   .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-fnt.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-fnt-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-fnt-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-fnt-val-pos          pic  9(07)                  .

      *    *===========================================================*
      *    * Work per buffer clienti inseriti o modificati             *
      *    *-----------------------------------------------------------*
       01  w-buf-pro.
      *        *-------------------------------------------------------*
      *        * Work per buffer                                       *
      *        *-------------------------------------------------------*
           05  w-buf-fnt-iom.
      *            *---------------------------------------------------*
      *            * Data di richiesta                                 *
      *            *---------------------------------------------------*
               10  w-buf-fnt-iom-drc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-buf-fnt-iom-ute      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Flag di selezione                                 *
      *            *---------------------------------------------------*
               10  w-buf-fnt-iom-fds      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valori selezionati                                *
      *            *---------------------------------------------------*
               10  w-buf-fnt-iom-cod      pic  9(07)                  .
               10  w-buf-fnt-iom-dpz      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-buf-fnt-iom-c01      pic  9(02)                  .
               10  w-buf-fnt-iom-c02      pic  9(02)                  .
               10  w-buf-fnt-iom-c03      pic  9(02)                  .
               10  w-buf-fnt-iom-c04      pic  9(02)                  .
               10  w-buf-fnt-iom-c05      pic  9(02)                  .
               10  w-buf-fnt-iom-nli      pic  9(02)                  .
               10  w-buf-fnt-iom-crb      pic  9(02)                  .
               10  w-buf-fnt-iom-cpb      pic  9(02)                  .
               10  w-buf-fnt-iom-cpa      pic  9(02)                  .
               10  w-buf-fnt-iom-max      pic  9(02) value 54         .
               10  w-buf-fnt-iom-buf occurs 54.
                   15  w-buf-fnt-iom-bco  pic  9(07)                  .
                   15  w-buf-fnt-iom-bdp  pic  x(04)                  .
                   15  w-buf-fnt-iom-brs  pic  x(40)                  .
               10  w-buf-fnt-iom-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-buf-fnt-iom-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-buf-fnt-iom-lt2  pic  9(01)                  .

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
      *              * Se in impostazione altri campi riga corpo : si  *
      *              * ignora                                          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "R"
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
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Relative al record fornitore                *
      *                  *---------------------------------------------*
           perform   prs-rec-fnt-000      thru prs-rec-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Numero livelli del piano dei conti          *
      *                  *---------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sottoconto di default                       *
      *                  *---------------------------------------------*
           perform   ref-def-stc-000      thru ref-def-stc-999        .
       pre-exe-pgm-600.
      *              *-------------------------------------------------*
      *              * Open dei moduli di accettazione                 *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al record dei    *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       prs-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[rec-fnt]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-fnt
           else      move  spaces         to   w-prs-rec-fnt          .
      *
           if        w-prs-rec-fnt-mxc    not  numeric
                     move  zero           to   w-prs-rec-fnt-mxc      .
           if        w-prs-rec-fnt-mxc    =    zero
                     move  9999999        to   w-prs-rec-fnt-mxc      .
           if        w-prs-rec-fnt-fco    not  = "A"
                     move  "M"            to   w-prs-rec-fnt-fco      .
           if        w-prs-rec-fnt-omn    not  = "O"
                     move  "N"            to   w-prs-rec-fnt-omn      .
           if        w-prs-rec-fnt-umn    not  = "U"
                     move  "N"            to   w-prs-rec-fnt-umn      .
           if        w-prs-rec-fnt-mmn    not  = "N"
                     move  "M"            to   w-prs-rec-fnt-mmn      .
           if        w-prs-rec-fnt-osc    not  = "N"
                     move  "N"            to   w-prs-rec-fnt-osc      .
           if        w-prs-rec-fnt-msc    not  = "N"
                     move  "M"            to   w-prs-rec-fnt-msc      .
           if        w-prs-rec-fnt-ars    not  = "U"
                     move  "N"            to   w-prs-rec-fnt-ars      .
           if        w-prs-rec-fnt-ain    not  = "U"
                     move  "N"            to   w-prs-rec-fnt-ain      .
           if        w-prs-rec-fnt-alo    not  = "U"
                     move  "N"            to   w-prs-rec-fnt-alo      .
           if        w-prs-rec-fnt-opi    not  = "M" and
                     w-prs-rec-fnt-opi    not  = "O"
                     move  "N"            to   w-prs-rec-fnt-opi      .
           if        w-prs-rec-fnt-ocf    not  = "M" and
                     w-prs-rec-fnt-ocf    not  = "O"
                     move  "N"            to   w-prs-rec-fnt-ocf      .
      *
           if        w-prs-rec-fnt-mic    not  numeric
                     move  zero           to   w-prs-rec-fnt-mic      .
      *
           if        w-prs-rec-fnt-rcd    not  = "R"
                     move  "N"            to   w-prs-rec-fnt-rcd      .
       prs-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al numero di li- *
      *    * velli del piano dei conti                                 *
      *    *-----------------------------------------------------------*
       prs-liv-pdc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 2
                     move  3              to   w-prs-liv-pdc          .
       prs-liv-pdc-999.
           exit.

      *    *===========================================================*
      *    * Lettura della referenza sottoconto contabile da proporre  *
      *    * come default                                              *
      *    *-----------------------------------------------------------*
       ref-def-stc-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza sottoconto contabile da pro-  *
      *              * porre come default                              *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/fnt[def-stc]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-stc-fnt
           else      move  zero           to   w-ref-stc-fnt          .
       ref-def-stc-999.
           exit.

      *    *===========================================================*
      *    * Open dei moduli di accettazione                           *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore       *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-opn-000  thru cod-mne-fnt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
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
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close dei moduli di accettazione                *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-000      thru cls-mdl-acc-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Close dei moduli di accettazione                          *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore      *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-cls-000  thru cod-mne-fnt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
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
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione contatti          *
      *              *-------------------------------------------------*
           perform   con-arc-tip-opn-000  thru con-arc-tip-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione contatti         *
      *              *-------------------------------------------------*
           perform   con-arc-tip-cls-000  thru con-arc-tip-cls-999    .
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
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale fornitore                       *
      *              *-------------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-fnt-000      thru pmt-cod-fnt-999        .
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
      *    * Visualizzazione prompts per Codice fornitore              *
      *    *-----------------------------------------------------------*
       pmt-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice fornitore              *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare valori da       *
      *                  * eventuale routine di ultimo inserimento o   *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-fnt-iom-cod      .
           move      spaces               to   w-buf-fnt-iom-dpz      .
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-fnt-ope      .
           move      w-tes-cod-fnt        to   w-cod-mne-fnt-cod      .
           move      04                   to   w-cod-mne-fnt-lin      .
           move      30                   to   w-cod-mne-fnt-pos      .
           move      08                   to   w-cod-mne-fnt-rln      .
           move      30                   to   w-cod-mne-fnt-rps      .
           move      zero                 to   w-cod-mne-fnt-vln      .
           move      zero                 to   w-cod-mne-fnt-vps      .
           move      zero                 to   w-cod-mne-fnt-lln      .
           move      zero                 to   w-cod-mne-fnt-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           if        w-cod-mne-fnt-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-fnt-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-fnt-cod    to   v-num                  .
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
       acc-cod-fnt-300.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cod-fnt-400.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-buf-fnt-iom-drc      .
           move      s-ute                to   w-buf-fnt-iom-ute      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   buf-fnt-iom-000      thru buf-fnt-iom-999        .
      *                      *-----------------------------------------*
      *                      * Test sul flag di uscita                 *
      *                      *-----------------------------------------*
           if        w-buf-fnt-iom-fds    not  = spaces
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Valori selezionati                          *
      *                  *---------------------------------------------*
           move      w-buf-fnt-iom-cod    to   w-tes-cod-fnt          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-fnt-412.
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
           go to     acc-cod-fnt-100.
       acc-cod-fnt-412.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il tipo funzio- *
      *                      * namento per il codice e' automatico op- *
      *                      * pure manuale                            *
      *                      *-----------------------------------------*
           if        w-prs-rec-fnt-fco    not  = "A"
                     go to acc-cod-fnt-415
           else      go to acc-cod-fnt-420.
       acc-cod-fnt-415.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' manuale                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna azione, ne' controllo       *
      *                          *-------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-420.
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      w-prs-rec-fnt-mxc    to   w-enc-fnt-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-fnt-val-pos    to   w-tes-cod-fnt          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-fnt-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-450.
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
           if        w-prs-rec-fnt-mxc    =    zero or
                     w-prs-rec-fnt-mxc    =    9999999
                     go to acc-cod-fnt-600.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato non e' superiore *
      *                      * al valore massimo impostabile, il con-  *
      *                      * trollo e' superato                      *
      *                      *-----------------------------------------*
           if        w-tes-cod-fnt        not  > w-prs-rec-fnt-mxc
                     go to acc-cod-fnt-600.
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
           move      "Il cod. fornitore non puo' essere superiore a"
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
           move      w-prs-rec-fnt-mxc    to   v-num                  .
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
           go to     acc-cod-fnt-100.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *    * Visualizzazione campo chiave : Ragione sociale fornitore  *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      40                   to   v-pos                  .
           if        w-cnt-sts-imp-npt    =    1
                     move  spaces         to   v-alf
           else      move  w-tes-rag-soc (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-rag-999.
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
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
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
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           perform   acc-via-fnt-000      thru acc-via-fnt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           perform   acc-loc-fnt-000      thru acc-loc-fnt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           perform   acc-cod-cmn-000      thru acc-cod-cmn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           perform   acc-cod-fzn-000      thru acc-cod-fzn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           perform   acc-cod-lct-000      thru acc-cod-lct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Partita iva                                 *
      *                  *---------------------------------------------*
           perform   acc-prt-iva-000      thru acc-prt-iva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Codice fiscale                              *
      *                  *---------------------------------------------*
           perform   acc-cod-fis-000      thru acc-cod-fis-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Sottoconto contabile                        *
      *                  *---------------------------------------------*
           perform   acc-cod-cge-000      thru acc-cod-cge-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Codice esenzione iva                        *
      *                  *---------------------------------------------*
           perform   acc-cod-iva-000      thru acc-cod-iva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Si/no in elenco iva                         *
      *                  *---------------------------------------------*
           perform   acc-snx-ecf-000      thru acc-snx-ecf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-190.
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
                     go to acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Contatti e utenze                           *
      *                  *---------------------------------------------*
           perform   acc-con-arc-000      thru acc-con-arc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
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
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Descrizione nazione                             *
      *              *-------------------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   vis-rag-fnt-000      thru vis-rag-fnt-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   vis-via-fnt-000      thru vis-via-fnt-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   vis-loc-fnt-000      thru vis-loc-fnt-999        .
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
      *              * Sottoconto contabile                            *
      *              *-------------------------------------------------*
           perform   vis-cod-cge-000      thru vis-cod-cge-999        .
      *              *-------------------------------------------------*
      *              * Descrizione sottoconto                          *
      *              *-------------------------------------------------*
           perform   vis-des-cge-000      thru vis-des-cge-999        .
      *              *-------------------------------------------------*
      *              * Codice esenzione iva                            *
      *              *-------------------------------------------------*
           perform   vis-cod-iva-000      thru vis-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Descrizione iva                                 *
      *              *-------------------------------------------------*
           perform   vis-des-iva-000      thru vis-des-iva-999        .
      *              *-------------------------------------------------*
      *              * Si/no in elenco iva                             *
      *              *-------------------------------------------------*
           perform   vis-snx-ecf-000      thru vis-snx-ecf-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Contatti e utenze                               *
      *              *-------------------------------------------------*
           perform   vis-con-arc-000      thru vis-con-arc-999        .
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
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   pmt-rag-fnt-000      thru pmt-rag-fnt-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   pmt-via-fnt-000      thru pmt-via-fnt-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   pmt-loc-fnt-000      thru pmt-loc-fnt-999        .
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
      *              * Sottoconto contabile                            *
      *              *-------------------------------------------------*
           perform   pmt-cod-cge-000      thru pmt-cod-cge-999        .
      *              *-------------------------------------------------*
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
           perform   pmt-cod-iva-000      thru pmt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Si/no in elenco iva                             *
      *              *-------------------------------------------------*
           perform   pmt-snx-ecf-000      thru pmt-snx-ecf-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Contatti e utenze                               *
      *              *-------------------------------------------------*
           perform   pmt-con-arc-000      thru pmt-con-arc-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      "Mnemonico :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ragione sociale                  *
      *    *-----------------------------------------------------------*
       pmt-rag-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rag-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indirizzo                        *
      *    *-----------------------------------------------------------*
       pmt-via-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indirizzo                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-via-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : C.a.p. e citta'                  *
      *    *-----------------------------------------------------------*
       pmt-loc-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "C.a.p. e citta'            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-loc-fnt-999.
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
      *
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT"
                     move  "Partita iva                :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
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
      *
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT "
                     move  "Codice fiscale             :"
                                          to   v-alf
           else      move  "Codice iva                 :"
                                          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottoconto contabile             *
      *    *-----------------------------------------------------------*
       pmt-cod-cge-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottoconto contabile       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice iva                       *
      *    *-----------------------------------------------------------*
       pmt-cod-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice di esenzione Iva    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no in elenco iva              *
      *    *-----------------------------------------------------------*
       pmt-snx-ecf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "Elenco Iva :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ecf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contatti e utenze                *
      *    *-----------------------------------------------------------*
       pmt-con-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo     Interlocutore / Reparto                  
      -              "      Utenza                  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----  ------------------------------   -----------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-con-arc-999.
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
           if        w-prs-rec-fnt-mmn    =    "N"
                     go to acc-cod-mne-999.
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      71                   to   v-pos                  .
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
      *                  * Test che non contenga caratteri numerici    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se a spazi                         *
      *                      *-----------------------------------------*
           if        w-tes-cod-mne (1)    =    spaces
                     go to acc-cod-mne-420.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-mne (1)    to   w-all-str-alf          .
           perform   all-str-num-000      thru all-str-num-999        .
      *                      *-----------------------------------------*
      *                      * Esito                                   *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    "#"
                     go to acc-cod-mne-420.
      *                      *-----------------------------------------*
      *                      * Suggerimento                            *
      *                      *-----------------------------------------*
           move      "Si sconsiglia l'uso di caratteri numerici nel codi
      -              "ce Mnemonico..."    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-mne-420.
      *                  *---------------------------------------------*
      *                  * Test che, se il mnemonico e' obbligatorio,  *
      *                  * il valore non manchi                        *
      *                  *---------------------------------------------*
           if        w-prs-rec-fnt-omn    not  = "O"
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
           move      06                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
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
      *
           if        w-prs-rec-fnt-ars    =    "U"
                     move  "U"            to   v-tip
           else      move  "A"            to   v-tip                  .
      *
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
       vis-rag-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rag-soc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Indirizzo                    *
      *    *-----------------------------------------------------------*
       acc-via-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-via-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-ind-cec-geo-naz      .
           move      w-tes-via-fnt (1)    to   w-ind-cec-geo-ind      .
           move      09                   to   w-ind-cec-geo-lin      .
           move      30                   to   w-ind-cec-geo-pos      .
           move      w-tes-loc-fnt (1)    to   w-ind-cec-geo-cec      .
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
       acc-via-fnt-110.
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           if        w-ind-cec-geo-ope    =    "F+"
                     go to acc-via-fnt-115.
           if        w-ind-cec-geo-ope    =    "AC"
                     go to acc-via-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-via-fnt-115.
           perform   ind-cec-geo-foi-000  thru ind-cec-geo-foi-999    .
           go to     acc-via-fnt-110.
       acc-via-fnt-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-via-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-via-fnt-999.
       acc-via-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-ind-cec-geo-ind    to   w-tes-via-fnt (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-aut    =    spaces
                     go to acc-via-fnt-400.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cec    to   w-tes-loc-fnt (1)      .
           perform   vis-loc-fnt-000      thru vis-loc-fnt-999        .
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
       acc-via-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-via-fnt (1)    =    spaces
                     go to acc-via-fnt-600.
           if        w-tes-via-fnt (1)
                    (01 : 01)             =    spaces
                     go to acc-via-fnt-100.
       acc-via-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale indirizzo in uppercase            *
      *                  *---------------------------------------------*
           if        w-prs-rec-fnt-ain    =    "N"
                     go to acc-via-fnt-800.
      *                  *---------------------------------------------*
      *                  * Trasformazione                              *
      *                  *---------------------------------------------*
           move      w-tes-via-fnt (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-via-fnt (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           perform   vis-via-fnt-000      thru vis-via-fnt-999        .
       acc-via-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-via-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-via-fnt-100.
       acc-via-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Indirizzo                 *
      *    *-----------------------------------------------------------*
       vis-via-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-via-fnt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-via-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : C.a.p. e citta'              *
      *    *-----------------------------------------------------------*
       acc-loc-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-loc-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-cap-cit-geo-naz      .
           move      w-tes-loc-fnt (1)    to   w-cap-cit-geo-cec      .
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
       acc-loc-fnt-110.
           perform   cap-cit-geo-cll-000  thru cap-cit-geo-cll-999    .
           if        w-cap-cit-geo-ope    =    "F+"
                     go to acc-loc-fnt-115.
           if        w-cap-cit-geo-ope    =    "AC"
                     go to acc-loc-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-loc-fnt-115.
           perform   cap-cit-geo-foi-000  thru cap-cit-geo-foi-999    .
           go to     acc-loc-fnt-110.
       acc-loc-fnt-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-loc-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-loc-fnt-999.
       acc-loc-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cap-cit-geo-cec    to   w-tes-loc-fnt (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-cap-cit-geo-aut    =    spaces
                     go to acc-loc-fnt-400.
       acc-loc-fnt-300.
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
                     go to acc-loc-fnt-350.
      *                      *-----------------------------------------*
      *                      * Concatenamento della localita' selezio- *
      *                      * nata con l'indirizzo precedente         *
      *                      *-----------------------------------------*
           move      "IL"                 to   w-ind-cec-geo-ope      .
           move      w-tes-via-fnt (1)    to   w-ind-cec-geo-ind      .
           move      w-cap-cit-geo-dlo    to   w-ind-cec-geo-dlo      .
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           move      w-ind-cec-geo-ind    to   w-tes-via-fnt (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione indirizzo concatenato   *
      *                      *-----------------------------------------*
           perform   vis-via-fnt-000      thru vis-via-fnt-999        .
       acc-loc-fnt-350.
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
       acc-loc-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-loc-fnt (1)    =    spaces
                     go to acc-loc-fnt-600.
           if        w-tes-loc-fnt (1)
                    (01 : 01)             =    spaces
                     go to acc-loc-fnt-100.
       acc-loc-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale localita' in uppercase            *
      *                  *---------------------------------------------*
           if        w-prs-rec-fnt-alo    =    "N"
                     go to acc-loc-fnt-800.
      *                  *---------------------------------------------*
      *                  * Trasformazione                              *
      *                  *---------------------------------------------*
           move      w-tes-loc-fnt (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-loc-fnt (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           perform   vis-loc-fnt-000      thru vis-loc-fnt-999        .
       acc-loc-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-loc-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-loc-fnt-100.
       acc-loc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : C.a.p. e citta'           *
      *    *-----------------------------------------------------------*
       vis-loc-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-loc-fnt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-loc-fnt-999.
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
                     go to  acc-prt-iva-500.
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
           move      "RF#"                to   v-msk                  .
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
       acc-prt-iva-500.
      *                  *---------------------------------------------*
      *                  * Controllo che la partita Iva non sia gia'   *
      *                  * stata assegnata ad un altro fornitore       *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-ctl-dup-piv-fnt      .
           move      w-tes-prt-iva (1)    to   w-ctl-dup-piv-piv      .
           perform   ctl-dup-piv-000      thru ctl-dup-piv-999        .
      *                  *---------------------------------------------*
      *                  * Se controllo superato si prosegue           *
      *                  *---------------------------------------------*
           if        w-ctl-dup-piv-flg    =    spaces
                     go to  acc-prt-iva-600.
      *                  *---------------------------------------------*
      *                  * Box di avvertimento                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing del codice fornitore trovato    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-ctl-dup-piv-ftt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio di errore        *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Partita iva gia' assegnata al fornitore "
                                delimited by size
                     v-edt      delimited by spaces
                                          into w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
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
      *                      * Box di richiesta per avvenuta variazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           perform   acc-prt-iva-bxm-000  thru acc-prt-iva-bxm-999    .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della risposta *
      *                          *-------------------------------------*
           if        w-exp-prt-iva-sce    =    01
                     go to acc-prt-iva-603
           else      go to acc-prt-iva-612.
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
      *    * Accettazione campo testata : Partita iva                  *
      *    *                                                           *
      *    * Box per eventuali modifiche                               *
      *    *-----------------------------------------------------------*
       acc-prt-iva-bxm-000.
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
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literals nel box                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione : e' stata variata la Partita Iva."
                                          to   v-alf                  .
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
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prt-iva-bxm-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-prt-iva-lun    to   v-car                  .
           move      w-exp-prt-iva-num    to   v-ldt                  .
           move      "AN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-prt-iva-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-prt-iva-sce      .
      *              *-------------------------------------------------*
      *              * Controllo risposta                              *
      *              *-------------------------------------------------*
           if        w-exp-prt-iva-sce    not  = 01 and
                     w-exp-prt-iva-sce    not  = 02
                     go to acc-prt-iva-bxm-200.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prt-iva-bxm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-prt-iva-bxm-999.
       acc-prt-iva-bxm-999.
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
      *
           if        w-tes-cod-naz (1)    not  = "IT "
                     move  20             to   v-car
           else      move  16             to   v-car                  .
      *
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
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-fis (1)    to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        =    spaces
                     go to acc-cod-fis-402.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Spazi vuoti non accettabili!"
                                          to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione                         *
      *                      *-----------------------------------------*
           go to     acc-cod-fis-100.
       acc-cod-fis-402.
      *                  *---------------------------------------------*
      *                  * Se il fornitore e' estero non si esegue il  *
      *                  * controllo formale                           *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-fis-500.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' completamente a     *
      *                  * spaces non si esegue il controllo           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    spaces
                     go to acc-cod-fis-600.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari al valore pre- *
      *                  * cedente non si esegue il controllo          *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    w-sav-cod-fis
                     go to acc-cod-fis-600.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari alla partita   *
      *                  * iva non si esegue il controllo              *
      *                  *---------------------------------------------*
           move      w-tes-cod-fis (1)    to   w-piv-cfi-cfi-alf      .
           if        w-piv-cfi-cfi-11n    =    w-tes-prt-iva (1) and
                     w-piv-cfi-cfi-05a    =    spaces
                     go to acc-cod-fis-600.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della lunghezza ef-  *
      *                  * fettiva del valore                          *
      *                  *---------------------------------------------*
           if        w-piv-cfi-cfi-05a    not  = spaces
                     go to acc-cod-fis-406.
       acc-cod-fis-403.
      *                  *---------------------------------------------*
      *                  * Altri controlli                             *
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
                     go to  acc-cod-fis-600
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
                     go to  acc-cod-fis-500
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
           move      "RF#"                to   v-msk                  .
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
           else      go to acc-cod-fis-500.
       acc-cod-fis-500.
      *                  *---------------------------------------------*
      *                  * Controllo che il codice fiscale non sia gia'*
      *                  * stato assegnato ad un altro fornitore       *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   w-ctl-dup-cfi-fnt      .
           move      w-tes-cod-fis (1)    to   w-ctl-dup-cfi-cfi      .
           perform   ctl-dup-cfi-000      thru ctl-dup-cfi-999        .
      *                  *---------------------------------------------*
      *                  * Se controllo superato si prosegue           *
      *                  *---------------------------------------------*
           if        w-ctl-dup-cfi-flg    =    spaces
                     go to  acc-cod-fis-600.
      *                  *---------------------------------------------*
      *                  * Box di avvertimento                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing del codice fornitore trovato    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-ctl-dup-cfi-ftt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio di errore        *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Codice fiscale gia' assegnato al fornitore "
                                delimited by size
                     v-edt      delimited by spaces
                                          into w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
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
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fis (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottoconto contabile         *
      *    *-----------------------------------------------------------*
       acc-cod-cge-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore e' gia' stato preparato    *
      *                      * si visualizza comunque la descrizione   *
      *                      *-----------------------------------------*
           if        w-tes-cod-cge (1)    not  = zero   and
                     w-tes-cod-cge-des (1)
                                          not  = spaces
                     go to acc-cod-cge-080.
      *                      *-----------------------------------------*
      *                      * Se valore gia' diverso da zero non si   *
      *                      * prepara alcun default                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-cge (1)    not  = zero
                     go to acc-cod-cge-100.
      *                      *-----------------------------------------*
      *                      * Preparazione del valore di default      *
      *                      *-----------------------------------------*
           move      w-ref-stc-fnt        to   w-tes-cod-cge (1)      .
      *                      *-----------------------------------------*
      *                      * Preparazione della descrizione relativa *
      *                      * al valore di default                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-cod-cge-des (1)  .
       acc-cod-cge-080.
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore di default       *
      *                      *-----------------------------------------*
           perform   vis-cod-cge-000      thru vis-cod-cge-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione relativa al *
      *                      * valore di default                       *
      *                      *-----------------------------------------*
           perform   vis-des-cge-000      thru vis-des-cge-999        .
       acc-cod-cge-090.
      *                  *---------------------------------------------*
      *                  * Se non si e' in Inserimento ed il sottocon- *
      *                  * to non e' modificabile si omette l'imposta- *
      *                  * zione, a meno che il valore attuale non sia *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-cge (1)    =    zero
                     go to acc-cod-cge-100.
           if        w-prs-rec-fnt-msc    =    "N"
                     go to acc-cod-cge-999.
       acc-cod-cge-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-cod-cge (1)    to   w-cod-mne-pdc-cod      .
           move      19                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      19                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-cod-cge-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-cod-cge-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-cod-cge-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cge-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-cod-cge-110.
       acc-cod-cge-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cge-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cge-999.
       acc-cod-cge-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cge (1)      .
       acc-cod-cge-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-cod-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-cod-cge-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-cge-000      thru vis-des-cge-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-cod-cge-100.
      *                  *---------------------------------------------*
      *                  * Se sottoconto obbligatorio e valore mancan- *
      *                  * te : reimpostazione                         *
      *                  *---------------------------------------------*
           if        w-prs-rec-fnt-osc    not  = "O"
                     go to acc-cod-cge-600.
           if        w-tes-cod-cge (1)    not  = zero
                     go to acc-cod-cge-600.
           if        v-key                not  = "UP  "
                     go to acc-cod-cge-100.
       acc-cod-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cge-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cge-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cge-100.
       acc-cod-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottoconto contabile      *
      *    *-----------------------------------------------------------*
       vis-cod-cge-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-cod-cge (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    *-----------------------------------------------------------*
       vis-des-cge-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cge-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cge-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Accettazione campo testata : Codice di esenzione IVA      *
      *    *-----------------------------------------------------------*
       acc-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-iva-100.
      *              *-------------------------------------------------*
      *              * Editing preliminare per l'accettazione          *
      *              *-------------------------------------------------*
           move      w-tes-cod-iva (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zci-ope      .
           move      w-edt-iva-cie        to   w-cod-mne-zci-cod      .
           move      20                   to   w-cod-mne-zci-lin      .
           move      30                   to   w-cod-mne-zci-pos      .
           move      20                   to   w-cod-mne-zci-dln      .
           move      41                   to   w-cod-mne-zci-dps      .
           move      "<BD"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
       acc-cod-iva-110.
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           if        w-cod-mne-zci-ope    =    "F+"
                     go to acc-cod-iva-115.
           if        w-cod-mne-zci-ope    =    "AC"
                     go to acc-cod-iva-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-iva-115.
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
           go to     acc-cod-iva-110.
       acc-cod-iva-120.
      *              *-------------------------------------------------*
      *              * Editing valore impostato                        *
      *              *-------------------------------------------------*
           move      w-cod-mne-zci-cod    to   w-edt-iva-cie          .
           perform   edt-iva-cod-000      thru edt-iva-cod-999        .
           move      w-edt-iva-cod        to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-iva-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-iva-999.
       acc-cod-iva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-iva (1)      .
       acc-cod-iva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zci-des    to   w-tes-cod-iva-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-iva-000      thru vis-des-iva-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zci-flg    not  = spaces
                     go to acc-cod-iva-100.
      *                  *---------------------------------------------*
      *                  * Se codice imponibile, messaggio             *
      *                  *---------------------------------------------*
           if        w-tes-cod-iva (1)    =    zero
                     go to acc-cod-iva-600.
           move      w-tes-cod-iva (1)    to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    not  zero
                     go to acc-cod-iva-600.
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Indicare un codice di esenzione! "
                                          to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-iva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-iva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-iva-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-iva-100.
       acc-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice di esenzione IVA   *
      *    *-----------------------------------------------------------*
       vis-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Editing preliminare                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-iva (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice IVA    *
      *    *-----------------------------------------------------------*
       vis-des-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-iva-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no in elenco Iva                  *
      *    *-----------------------------------------------------------*
       acc-snx-ecf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-a13 (1)    not  = spaces
                     go to acc-snx-ecf-100.
           move      "S"                  to   w-tes-snx-a13 (1)      .
       acc-snx-ecf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ecf-lun    to   v-car                  .
           move      w-exp-snx-ecf-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-ecf-tbl    to   v-txt                  .
      *
           if        w-tes-snx-a13 (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-a13 (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ecf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ecf-999.
       acc-snx-ecf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-a13 (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-a13 (1)
           else      move  spaces         to   w-tes-snx-a13 (1)      .
       acc-snx-ecf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-snx-a13 (1)    not  = spaces
                     go to acc-snx-ecf-600.
           if        v-key                =    "UP  "
                     go to acc-snx-ecf-600
           else      go to acc-snx-ecf-100.
       acc-snx-ecf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ecf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ecf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ecf-100.
       acc-snx-ecf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no in elenco Iva               *
      *    *-----------------------------------------------------------*
       vis-snx-ecf-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-ecf-lun    to   v-car                  .
           move      w-exp-snx-ecf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-ecf-tbl    to   v-txt                  .
      *
           if        w-tes-snx-a13 (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-a13 (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ecf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione e visualizzazione : Contatti e utenze        *
      *    *                                                           *
      *    * Subroutine in copy di acc-con-arc-000 e vis-con-arc-000   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/aconarc0.acs"                   .

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
           if        w-prs-rec-fnt-omn    not  = "O"
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
           if        w-prs-rec-fnt-umn    not  = "U"
                     go to cnt-tdo-nok-250.
           if        w-tes-cod-mne (1)    =    spaces
                     go to cnt-tdo-nok-250.
       cnt-tdo-nok-205.
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-mne (1)    to   rf-fnt-cod-mne         .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-250.
       cnt-tdo-nok-210.
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-250.
           if        rf-fnt-cod-mne       not  = w-tes-cod-mne (1)
                     go to cnt-tdo-nok-250.
           if        rf-fnt-cod-fnt       =    w-tes-cod-fnt
                     go to cnt-tdo-nok-210.
           move      "Codice mnemonico gia' esistente                 "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo sul mnemonico, se non modificabile    *
      *              *-------------------------------------------------*
           if        w-prs-rec-fnt-mmn    not  = "N"
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
      *              * Controllo su Partita Iva, se obbligatoria       *
      *              *-------------------------------------------------*
           if        w-prs-rec-fnt-opi    =    "N"
                     go to cnt-tdo-nok-320.
           if        w-tes-prt-iva (1)    not  = "00000000000"
                     go to cnt-tdo-nok-320.
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to cnt-tdo-nok-320.
           move      "Manca la Partita Iva !                            
      -              "               "    to   w-err-box-err-msg      .
           if        w-prs-rec-fnt-opi    =    "M"
                     perform   box-msg-err-000
                                          thru box-msg-err-999
                     go to cnt-tdo-nok-320.
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-320.
      *              *-------------------------------------------------*
      *              * Controllo su Codice fiscale, se obbligatorio    *
      *              *-------------------------------------------------*
           if        w-prs-rec-fnt-ocf    =    "N"
                     go to cnt-tdo-nok-330.
           if        w-tes-cod-fis (1)    not  = spaces
                     go to cnt-tdo-nok-330.
       cnt-tdo-nok-322.
           move      "Manca il Codice fiscale !                            
      -              "               "    to   w-err-box-err-msg      .
           if        w-prs-rec-fnt-ocf    =    "M"
                     perform   box-msg-err-000
                                          thru box-msg-err-999
                     go to cnt-tdo-nok-330.
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-330.
      *              *-------------------------------------------------*
      *              * Controllo sul sottoconto, se obbligatorio       *
      *              *-------------------------------------------------*
           if        w-prs-rec-fnt-osc    not  = "O"
                     go to cnt-tdo-nok-350.
           if        w-tes-cod-cge (1)    not  = zero
                     go to cnt-tdo-nok-350.
           move      "Manca il codice del sottoconto di contabilita'    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-350.
      *              *-------------------------------------------------*
      *              * Controllo sul sottoconto, se non modificabile   *
      *              *-------------------------------------------------*
           if        w-prs-rec-fnt-msc    not  = "N"
                     go to cnt-tdo-nok-400.
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to cnt-tdo-nok-400.
           if        w-tes-cod-cge (2)    =    zero
                     go to cnt-tdo-nok-400.
           if        w-tes-cod-cge (1)    =    w-tes-cod-cge (2)
                     go to cnt-tdo-nok-400.
           move      "Il codice sottoconto non puo' essere modificato   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
       cnt-tdo-nok-450.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box di errore                               *
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
           move      15                   to   v-lto                  .
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
           move      spaces               to   w-tes-cod-fnt-aut      .
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
           move      spaces               to   w-tes-via-fnt (1)      .
           move      spaces               to   w-tes-loc-fnt (1)      .
           move      spaces               to   w-tes-cod-naz (1)      .
           move      spaces               to   w-tes-cod-naz-des (1)  .
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           move      zero                 to   w-tes-cod-iva (1)      .
           move      spaces               to   w-tes-cod-iva-des (1)  .
           move      zero                 to   w-tes-prt-iva (1)      .
           move      spaces               to   w-tes-cod-fis (1)      .
           move      spaces               to   w-tes-snx-a13 (1)      .
           move      zero                 to   w-tes-cod-cge (1)      .
           move      spaces               to   w-tes-cod-cge-des (1)  .
           move      spaces               to   w-tes-alx-exp (1)      .
      *              *-------------------------------------------------*
      *              * Valori riutilizzati                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-tes-num-fax (1)      .
      *              *-------------------------------------------------*
      *              * Valori in obsolescenza                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-tlx (1)      .
           move      spaces               to   w-tes-nom-int (1)      .
      *              *-------------------------------------------------*
      *              * Contatti                                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       nor-nok-tes-600.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to nor-nok-tes-620.
           move      spaces               to   w-tes-tip-con
                                              (1, w-acc-con-arc-c0w)  .
           move      spaces               to   w-tes-num-con
                                              (1, w-acc-con-arc-c0w)  .
           move      spaces               to   w-tes-int-con
                                              (1, w-acc-con-arc-c0w)  .
           go to     nor-nok-tes-600.
       nor-nok-tes-620.
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
           move      "CODFNT    "         to   f-key                  .
           move      w-tes-cod-fnt        to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
                     go to rou-let-reg-053.
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
       rou-let-reg-053.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-055.
      *                      *-----------------------------------------*
      *                      * Preparazione defaults per inserimento   *
      *                      *-----------------------------------------*
       rou-let-reg-060.
      *                          *-------------------------------------*
      *                          * Sottoconto contabile                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se default a zero : no prepara- *
      *                              * zione                           *
      *                              *---------------------------------*
           if        w-ref-stc-fnt        =    zero
                     go to rou-let-reg-065.
      *                              *---------------------------------*
      *                              * Valore di default               *
      *                              *---------------------------------*
           move      w-ref-stc-fnt        to    w-tes-cod-cge (1)     .
      *                              *---------------------------------*
      *                              * Descrizione per il valore di    *
      *                              * default                         *
      *                              *---------------------------------*
           move      w-tes-cod-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-cod-cge-des (1)  .
      *                              *---------------------------------*
      *                              * Se anagrafica sottoconto non e- *
      *                              * sistente : no defaults          *
      *                              *---------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     move  zero           to   w-tes-cod-cge (1)
                     move  spaces         to   w-tes-cod-cge-des (1)  .
       rou-let-reg-065.
      *                          *-------------------------------------*
      *                          * Si/No in elenco iva                 *
      *                          *-------------------------------------*
           move      "S"                  to   w-tes-snx-a13 (1)      .
       rou-let-reg-070.
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
           move      rf-fnt-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-fnt-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-fnt-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-fnt-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-fnt-rag-key       to   w-tes-rag-key (1)      .
           move      rf-fnt-rag-soc       to   w-tes-rag-soc (1)      .
           move      rf-fnt-via-fnt       to   w-tes-via-fnt (1)      .
           move      rf-fnt-loc-fnt       to   w-tes-loc-fnt (1)      .
           move      rf-fnt-cod-naz       to   w-tes-cod-naz (1)      .
           move      rf-fnt-cod-cmn       to   w-tes-cod-cmn (1)      .
           move      rf-fnt-cod-fzn       to   w-tes-cod-fzn (1)      .
           move      rf-fnt-cod-lct       to   w-tes-cod-lct (1)      .
           move      rf-fnt-num-tel       to   w-tes-num-tel (1)      .
           move      rf-fnt-num-fax       to   w-tes-num-fax (1)      .
           move      rf-fnt-num-tlx       to   w-tes-num-tlx (1)      .
           move      rf-fnt-nom-int       to   w-tes-nom-int (1)      .
           move      rf-fnt-cod-iva       to   w-tes-cod-iva (1)      .
           move      rf-fnt-prt-iva       to   w-tes-prt-iva (1)      .
           move      rf-fnt-cod-fis       to   w-tes-cod-fis (1)      .
           move      rf-fnt-snx-a13       to   w-tes-snx-a13 (1)      .
           move      rf-fnt-cod-cge       to   w-tes-cod-cge (1)      .
           move      rf-fnt-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-200.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [fnt]                        *
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
       rou-let-reg-240.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * esenzione iva                   *
      *                              *---------------------------------*
           move      w-tes-cod-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-cod-iva-des (1)  .
       rou-let-reg-250.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * sottoconto contabile            *
      *                              *---------------------------------*
           move      w-tes-cod-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-cod-cge-des (1)  .
      *                              *---------------------------------*
      *                              * Se fornitore estero, il codice  *
      *                              * fiscale viene letto dal campo   *
      *                              * riutilizzato                    *
      *                              *---------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     move  rf-fnt-num-fax to   w-tes-cod-fis (1)      .
      *                              *---------------------------------*
      *                              * Lettura contatti [adc]          *
      *                              *---------------------------------*
           perform   rou-let-reg-adc-000  thru rou-let-reg-adc-999    .
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
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Contatti                                                  *
      *    *-----------------------------------------------------------*
       rou-let-reg-adc-000.
      *              *-------------------------------------------------*
      *              * Determinazione contatti cliente commerciale     *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      11                   to   d-con-arc-tip-arc      .
           move      w-tes-cod-fnt        to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *              *-------------------------------------------------*
      *              * Ciclo di bufferizzazione                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       rou-let-reg-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    d-con-arc-num-ele
                     go to rou-let-reg-adc-900.
           move      d-con-arc-tip-con
                    (w-acc-con-arc-c0w)   to   w-tes-tip-con
                                              (1, w-acc-con-arc-c0w)  .
           move      d-con-arc-num-con
                    (w-acc-con-arc-c0w)   to   w-tes-num-con
                                              (1, w-acc-con-arc-c0w)  .
           move      d-con-arc-int-con
                    (w-acc-con-arc-c0w)   to   w-tes-int-con
                                              (1, w-acc-con-arc-c0w)  .
           go to     rou-let-reg-adc-200.
       rou-let-reg-adc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-adc-999.
       rou-let-reg-adc-999.
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
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
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
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
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
           if        w-tes-cod-fnt-aut    =    spaces
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
       pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Put della variabile di i.p.c. per il richiamo   *
      *              * eventuale del programma da un livello preceden- *
      *              * te                                              *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-tes-cod-fnt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
       pos-cnf-mod-200.
      *              *-------------------------------------------------*
      *              * Put della variabile di i.p.c. per il richiamo   *
      *              * eventuale del programma da un livello preceden- *
      *              * te                                              *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-tes-cod-fnt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
      *              * Trattamento file [fnt]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [fnt]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-fnt-000      thru wrt-rec-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Write record [adc]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-adc-000      thru wrt-rec-adc-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [fnt]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-fnt-000      thru rew-rec-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [adc]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-adc-000      thru rew-rec-adc-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [fnt]                             *
      *              *-------------------------------------------------*
           perform   del-rec-fnt-000      thru del-rec-fnt-999        .
      *              *-------------------------------------------------*
      *              * Delete record [adc]                             *
      *              *-------------------------------------------------*
           perform   del-rec-adc-000      thru del-rec-adc-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [fnt]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   rf-fnt-cod-fnt         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-fnt-ide-dat         .
           move      s-ute                to   rf-fnt-ide-ute         .
           move      s-fas                to   rf-fnt-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-fnt-cod-mne         .
           move      w-tes-rag-key (1)    to   rf-fnt-rag-key         .
           move      w-tes-rag-soc (1)    to   rf-fnt-rag-soc         .
           move      w-tes-via-fnt (1)    to   rf-fnt-via-fnt         .
           move      w-tes-loc-fnt (1)    to   rf-fnt-loc-fnt         .
           move      w-tes-cod-naz (1)    to   rf-fnt-cod-naz         .
           move      w-tes-cod-cmn (1)    to   rf-fnt-cod-cmn         .
           move      w-tes-cod-fzn (1)    to   rf-fnt-cod-fzn         .
           move      w-tes-cod-lct (1)    to   rf-fnt-cod-lct         .
           move      w-tes-num-tel (1)    to   rf-fnt-num-tel         .
           move      w-tes-num-fax (1)    to   rf-fnt-num-fax         .
           move      w-tes-num-tlx (1)    to   rf-fnt-num-tlx         .
           move      w-tes-nom-int (1)    to   rf-fnt-nom-int         .
           move      w-tes-cod-iva (1)    to   rf-fnt-cod-iva         .
           move      w-tes-prt-iva (1)    to   rf-fnt-prt-iva         .
           move      w-tes-cod-fis (1)    to   rf-fnt-cod-fis         .
           move      w-tes-snx-a13 (1)    to   rf-fnt-snx-a13         .
           move      w-tes-cod-cge (1)    to   rf-fnt-cod-cge         .
      *
           if        w-tes-cod-naz (1)    not  = "IT "
                     move  w-tes-cod-fis (1)
                                          to   rf-fnt-num-fax         .
      *
           move      w-tes-alx-exp (1)    to   rf-fnt-alx-exp         .
       cmp-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [fnt]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fnt-000      thru cmp-rec-fnt-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       wrt-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [fnt]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fnt-000      thru cmp-rec-fnt-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       rew-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [fnt]                                *
      *    *-----------------------------------------------------------*
       del-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fnt-000      thru cmp-rec-fnt-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       del-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [adc]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Cancellazione preventiva delle righe            *
      *              *-------------------------------------------------*
           perform   del-rec-adc-000      thru del-rec-adc-999        .
       rew-rec-adc-400.
      *              *-------------------------------------------------*
      *              * Scrittura delle righe                           *
      *              *-------------------------------------------------*
           perform   wrt-rec-adc-000      thru wrt-rec-adc-999        .
       rew-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [adc]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       cmp-rec-adc-100.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-adc-ide-dat         .
           move      s-ute                to   rf-adc-ide-ute         .
           move      s-fas                to   rf-adc-ide-fas         .
           move      11                   to   rf-adc-tip-arc         .
           move      w-tes-cod-fnt        to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
      *
           move      w-tes-rag-key (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   rf-adc-des-key         .
      *
           move      w-tes-tip-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-tip-con         .
           move      w-acc-con-arc-c0w    to   rf-adc-num-prg         .
           move      w-tes-int-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-int-con         .
           move      spaces               to   rf-adc-rep-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-tes-num-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-num-con         .
           move      rf-adc-ide-dat       to   rf-adc-dat-agg         .
           move      spaces               to   rf-adc-alx-exp         .
       cmp-rec-adc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-adc-999.
       cmp-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [adc]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Scrittura delle righe                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       wrt-rec-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to wrt-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Test se riga da scrivere                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-con
                    (1, w-acc-con-arc-c0w)
                                          =    spaces
                     go to wrt-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
           perform   cmp-rec-adc-000      thru cmp-rec-adc-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga successiva           *
      *                  *---------------------------------------------*
           go to     wrt-rec-adc-200.
       wrt-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [adc]                                *
      *    *-----------------------------------------------------------*
       del-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Cancellazione delle righe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       del-rec-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to del-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
           perform   cmp-rec-adc-000      thru cmp-rec-adc-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga successiva           *
      *                  *---------------------------------------------*
           go to     del-rec-adc-200.
       del-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
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
      *              * Normalizzazione marker di uscita                *
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
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Subroutines di controllo formale Partita Iva e Codice     *
      *    * Fiscale                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wks"                   .

      *    *===========================================================*
      *    * Controllo che la partita Iva non sia gia' stata assegnata *
      *    * ad un altro fornitore                                     *
      *    *-----------------------------------------------------------*
       ctl-dup-piv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-dup-piv-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-dup-piv-ctr      .
      *              *-------------------------------------------------*
      *              * Se la Partita Iva e' a zero : uscita            *
      *              *-------------------------------------------------*
           if        w-ctl-dup-piv-piv    =    zero
                     go to ctl-dup-piv-999.
       ctl-dup-piv-010.
      *              *-------------------------------------------------*
      *              * Start su file [fnt]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRTIVA    "         to   f-key                  .
           move      w-ctl-dup-piv-piv    to   rf-fnt-prt-iva         .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita con flag a spaces  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-dup-piv-999.
       ctl-dup-piv-100.
      *              *-------------------------------------------------*
      *              * Next su [fnt]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a test finale                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-dup-piv-800.
       ctl-dup-piv-200.
      *              *-------------------------------------------------*
      *              * Max su [fnt], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-fnt-prt-iva       not  = w-ctl-dup-piv-piv
                     go to ctl-dup-piv-800.
      *              *-------------------------------------------------*
      *              * Test che non sia il fornitore in corso di trat- *
      *              * tamento                                         *
      *              *-------------------------------------------------*
           if        rf-fnt-cod-fnt       =    w-ctl-dup-piv-fnt
                     go to ctl-dup-piv-100.
       ctl-dup-piv-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-ctl-dup-piv-ctr      .
       ctl-dup-piv-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione dell'ultimo codice fornitore    *
      *              * trovato con la stessa partita Iva               *
      *              *-------------------------------------------------*
           move      rf-fnt-cod-fnt       to   w-ctl-dup-piv-ftt      .
       ctl-dup-piv-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [fnt] successivo               *
      *              *-------------------------------------------------*
           go to     ctl-dup-piv-100.
       ctl-dup-piv-800.
      *              *-------------------------------------------------*
      *              * Test finale                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * fornitore con la stessa partita Iva si pone *
      *                  * il flag a non spaces                        *
      *                  *---------------------------------------------*
           if        w-ctl-dup-piv-ctr    >    zero
                     go to ctl-dup-piv-900
           else      go to ctl-dup-piv-999.
       ctl-dup-piv-900.
      *              *-------------------------------------------------*
      *              * Uscita con errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ctl-dup-piv-flg      .
       ctl-dup-piv-999.
           exit.

      *    *===========================================================*
      *    * Controllo che il codice fiscale non sia gia' stata as-    *
      *    * segnato ad un altro fornitore                             *
      *    *-----------------------------------------------------------*
       ctl-dup-cfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-dup-cfi-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-dup-cfi-ctr      .
      *              *-------------------------------------------------*
      *              * Se la Codice fiscale e' a spaces : uscita       *
      *              *-------------------------------------------------*
           if        w-ctl-dup-cfi-cfi    =    spaces
                     go to ctl-dup-cfi-999.
       ctl-dup-cfi-010.
      *              *-------------------------------------------------*
      *              * Start su file [fnt]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFIS    "         to   f-key                  .
           move      w-ctl-dup-cfi-cfi    to   rf-fnt-cod-fis         .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita con flag a spaces  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-dup-cfi-999.
       ctl-dup-cfi-100.
      *              *-------------------------------------------------*
      *              * Next su [fnt]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a test finale                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-dup-cfi-800.
       ctl-dup-cfi-200.
      *              *-------------------------------------------------*
      *              * Max su [fnt], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-fnt-cod-fis       not  = w-ctl-dup-cfi-cfi
                     go to ctl-dup-cfi-800.
      *              *-------------------------------------------------*
      *              * Test che non sia il fornitore in corso di trat- *
      *              * tamento                                         *
      *              *-------------------------------------------------*
           if        rf-fnt-cod-fnt       =    w-ctl-dup-cfi-fnt
                     go to ctl-dup-cfi-100.
       ctl-dup-cfi-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-ctl-dup-cfi-ctr      .
       ctl-dup-cfi-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione dell'ultimo codice fornitore    *
      *              * trovato con lo stesso codice fiscale            *
      *              *-------------------------------------------------*
           move      rf-fnt-cod-fnt       to   w-ctl-dup-cfi-ftt      .
       ctl-dup-cfi-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [fnt] successivo               *
      *              *-------------------------------------------------*
           go to     ctl-dup-cfi-100.
       ctl-dup-cfi-800.
      *              *-------------------------------------------------*
      *              * Test finale                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * fornitore con lo stesso codice fiscale si   *
      *                  * pone il flag a non spaces                   *
      *                  *---------------------------------------------*
           if        w-ctl-dup-cfi-ctr    >    zero
                     go to ctl-dup-cfi-900
           else      go to ctl-dup-cfi-999.
       ctl-dup-cfi-900.
      *              *-------------------------------------------------*
      *              * Uscita con errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ctl-dup-cfi-flg      .
       ctl-dup-cfi-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [fnt]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "fnt "               to   s-nam                  .
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
           move      "fnt "               to   s-nam                  .
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
           move      s-num                to   w-enc-fnt-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-fnt-val-pre    to   w-enc-fnt-val-pos      .
           add       1                    to   w-enc-fnt-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-fnt-val-pos    =    zero
                     move  1              to   w-enc-fnt-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-fnt-val-pos    >    w-enc-fnt-val-max
                     move  1              to   w-enc-fnt-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-enc-fnt-val-pos    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
           add       1                    to   w-enc-fnt-val-pos      .
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
           move      "fnt "               to   s-nam                  .
           move      w-enc-fnt-val-pos    to   s-num                  .
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
      *              * Lettura codice automatico per [fnt]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "fnt "               to   s-nam                  .
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
           if        s-num                =    w-enc-fnt-val-pos
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
           move      "fnt "               to   s-nam                  .
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
           move      "fnt "               to   s-nam                  .
           move      w-enc-fnt-val-pre    to   s-num                  .
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
      *    * Ricerca su buffer fornitori inseriti o modificati         *
      *    *-----------------------------------------------------------*
       buf-fnt-iom-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-fnt-iom-fds      .
           move      zero                 to   w-buf-fnt-iom-cod      .
           move      spaces               to   w-buf-fnt-iom-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-fnt-iom-crb      .
       buf-fnt-iom-100.
      *              *-------------------------------------------------*
      *              * Start su [fnt]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATSYS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-buf-fnt-iom-drc    to   rf-fnt-ide-dat         .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-buf-fnt-iom-fds
                     go to  buf-fnt-iom-999.
       buf-fnt-iom-200.
      *              *-------------------------------------------------*
      *              * Lettura [fnt]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-fnt-iom-500.
       buf-fnt-iom-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-fnt-ide-dat       not  = w-buf-fnt-iom-drc
                     go to buf-fnt-iom-500.
       buf-fnt-iom-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [fnt]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice utente                       *
      *                  *---------------------------------------------*
           if        rf-fnt-ide-ute       not  = w-buf-fnt-iom-ute
                     go to buf-fnt-iom-200.
       buf-fnt-iom-410.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-fnt-iom-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-buf-fnt-iom-crb    >    w-buf-fnt-iom-max
                     go to buf-fnt-iom-500.
       buf-fnt-iom-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           move      rf-fnt-cod-fnt       to   w-buf-fnt-iom-bco
                                              (w-buf-fnt-iom-crb)     .
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-fnt-rag-soc       to   w-buf-fnt-iom-brs
                                              (w-buf-fnt-iom-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-200.
       buf-fnt-iom-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-buf-fnt-iom-crb    =    zero
                     go to buf-fnt-iom-999.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-fnt-iom-crb    to   w-buf-fnt-iom-cpb      .
           subtract  1                    from w-buf-fnt-iom-cpb      .
           divide    6                    into w-buf-fnt-iom-cpb      .
           add       1                    to   w-buf-fnt-iom-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-fnt-iom-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Editing data                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-fnt-iom-drc    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione fincatura                  *
      *                      *-----------------------------------------*
           move      60                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "FORNITORI INSERITI O MODIFICATI"
                                          to   w-all-str-cat (1)      .
      *
           move      w-buf-fnt-iom-drc    to   s-dat                  .
           if        s-gio                =    01 or
                     s-gio                =    08 or
                     s-gio                =    11
                     move  "L'"           to   w-all-str-cat (2)
           else      move  "IL"           to   w-all-str-cat (2)      .
      *
           move      v-edt                to   w-all-str-cat (3)      .
           move      "DA ["               to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-buf-fnt-iom-ute    to   w-all-str-cat (2)      .
           move      "]"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   buf-fnt-iom-950      thru buf-fnt-iom-959        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-fnt-iom-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-buf-fnt-iom-c01    to   w-buf-fnt-iom-nli      .
       buf-fnt-iom-555.
           if        w-buf-fnt-iom-nli    >    6
                     subtract  6          from w-buf-fnt-iom-nli
                     go to buf-fnt-iom-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       06                   to   w-buf-fnt-iom-nli      .
       buf-fnt-iom-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       buf-fnt-iom-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-fnt-iom-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-fnt-iom-c01    <    w-buf-fnt-iom-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-fnt-iom-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-fnt-iom-cpa    <    w-buf-fnt-iom-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-fnt-iom-nli    to   v-lin                  .
           move      28                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-fnt-iom-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to buf-fnt-iom-582
           else if   v-key                =    "UP  "
                     go to buf-fnt-iom-584
           else if   v-key                =    "DOWN"
                     go to buf-fnt-iom-586
           else if   v-key                =    "EXIT"
                     go to buf-fnt-iom-598
           else if   v-key                =    "NXSC"
                     go to buf-fnt-iom-592
           else if   v-key                =    "PRSC"
                     go to buf-fnt-iom-594
           else      go to buf-fnt-iom-575.
       buf-fnt-iom-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori selezionati           *
      *                  *---------------------------------------------*
           move      w-buf-fnt-iom-bco
                    (w-buf-fnt-iom-c01)   to   w-buf-fnt-iom-cod      .
           move      w-buf-fnt-iom-bdp
                    (w-buf-fnt-iom-c01)   to   w-buf-fnt-iom-dpz      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-999.
       buf-fnt-iom-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-fnt-iom-c01      .
           if        w-buf-fnt-iom-nli    =    07
                     go to buf-fnt-iom-590
           else      go to buf-fnt-iom-550.
       buf-fnt-iom-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento contatore linee                 *
      *                  *---------------------------------------------*
           if        w-buf-fnt-iom-c01    <    w-buf-fnt-iom-crb
                     add   1              to   w-buf-fnt-iom-c01
                     go to buf-fnt-iom-588
           else      go to buf-fnt-iom-575.
       buf-fnt-iom-588.
      *                  *---------------------------------------------*
      *                  * Test se ultima linea                        *
      *                  *---------------------------------------------*
           if        w-buf-fnt-iom-nli    =    12
                     go to buf-fnt-iom-590
           else      go to buf-fnt-iom-550.
       buf-fnt-iom-590.
      *                  *---------------------------------------------*
      *                  * Se dall'ultima linea : pagina successiva    *
      *                  *---------------------------------------------*
           perform   buf-fnt-iom-950      thru buf-fnt-iom-959        .
      *                  *---------------------------------------------*
      *                  * A riaccettazione                            *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-550.
       buf-fnt-iom-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento contatore pagine                *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-fnt-iom-cpa      .
      *                  *---------------------------------------------*
      *                  * A gestione cambiamento pagina               *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-596.
       buf-fnt-iom-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento contatore pagine                *
      *                  *---------------------------------------------*
           subtract  1                    from w-buf-fnt-iom-cpa      .
       buf-fnt-iom-596.
      *                  *---------------------------------------------*
      *                  * Gestione cambiamento pagina                 *
      *                  *---------------------------------------------*
           move      w-buf-fnt-iom-cpa    to   w-buf-fnt-iom-c01      .
           multiply  6                    by   w-buf-fnt-iom-c01      .
           subtract  5                    from w-buf-fnt-iom-c01      .
      *                  *---------------------------------------------*
      *                  * A cambiamento pagina                        *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-590.
       buf-fnt-iom-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-fnt-iom-fds      .
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-fnt-iom-800.
       buf-fnt-iom-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-fnt-iom-999.
       buf-fnt-iom-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-buf-fnt-iom-c01    to   w-buf-fnt-iom-c02      .
           add       5                    to   w-buf-fnt-iom-c02      .
           divide    6                    into w-buf-fnt-iom-c02      .
           move      w-buf-fnt-iom-c02    to   w-buf-fnt-iom-cpa      .
           subtract  1                    from w-buf-fnt-iom-c02      .
           multiply  6                    by   w-buf-fnt-iom-c02      .
           add       1                    to   w-buf-fnt-iom-c02      .
           add       5
                     w-buf-fnt-iom-c02  giving w-buf-fnt-iom-c03      .
           move      w-buf-fnt-iom-c03    to   w-buf-fnt-iom-c04      .
           if        w-buf-fnt-iom-c03    >    w-buf-fnt-iom-crb
                     move  w-buf-fnt-iom-crb
                                          to   w-buf-fnt-iom-c03      .
           move      07                   to   w-buf-fnt-iom-c05      .
       buf-fnt-iom-951.
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-buf-fnt-iom-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-buf-fnt-iom-bco
                    (w-buf-fnt-iom-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza fornitore                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-buf-fnt-iom-c05    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-fnt-iom-bdp
                    (w-buf-fnt-iom-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale fornitore                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-buf-fnt-iom-c05    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-buf-fnt-iom-brs
                    (w-buf-fnt-iom-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-fnt-iom-c02      .
           add       1                    to   w-buf-fnt-iom-c05      .
           if        w-buf-fnt-iom-c02    not  > w-buf-fnt-iom-c03
                     go to buf-fnt-iom-951.
       buf-fnt-iom-952.
           if        w-buf-fnt-iom-c02    >    w-buf-fnt-iom-c04
                     go to buf-fnt-iom-955.
           if        w-buf-fnt-iom-crb    not  > 6
                     go to buf-fnt-iom-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-buf-fnt-iom-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-fnt-iom-c02      .
           add       1                    to   w-buf-fnt-iom-c05      .
           go to     buf-fnt-iom-952.
       buf-fnt-iom-955.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-buf-fnt-iom-cpa    to   w-buf-fnt-iom-lt1      .
           move      w-buf-fnt-iom-cpb    to   w-buf-fnt-iom-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-fnt-iom-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-fnt-iom-959.
           exit.
       buf-fnt-iom-999.
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
      *    * Subroutines per l'accettazione del codice fornitore       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

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

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione contatti                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dts"                   .

