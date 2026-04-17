       Identification Division.
       Program-Id.                                 prda2000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    rda                 *
      *                                Settore:    arc                 *
      *                                   Fase:    rda200              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/06/01    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio fornitori con ritenuta    *
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
                     "rda"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "rda200"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "prda2000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "GESTIONE ARCHIVIO FORNITORI CON RITENUTA"       .

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
      *        * [raa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfraa"                          .
      *        *-------------------------------------------------------*
      *        * [zcr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfzcr"                          .
      *        *-------------------------------------------------------*
      *        * [zco]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfzco"                          .
      *        *-------------------------------------------------------*
      *        * [ztr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfztr"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
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
               10  w-tes-cod-fnt          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
      *            *---------------------------------------------------*
      *            * Valori anagrafica contabile fornitore             *
      *            *---------------------------------------------------*
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
               10  w-tes-num-tel          pic  x(20)                  .
               10  w-tes-num-fax          pic  x(20)                  .
               10  w-tes-num-tlx          pic  x(20)                  .
               10  w-tes-nom-int          pic  x(30)                  .
               10  w-tes-cod-iva          pic  9(05)                  .
               10  w-tes-cod-iva-des      pic  x(15)                  .
               10  w-tes-prt-iva          pic  9(11)                  .
               10  w-tes-cod-fis          pic  x(16)                  .
               10  w-tes-snx-a13          pic  x(01)                  .
               10  w-tes-cod-cge          pic  9(07)                  .
               10  w-tes-cod-cge-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Valori integrativi anagrafica percipiente         *
      *            *---------------------------------------------------*
               10  w-tes-snx-prf          pic  9(02)                  .
               10  w-tes-nom-fnt          pic  x(40)                  .
               10  w-tes-cgn-fnt          pic  x(40)                  .
               10  w-tes-dat-nsc          pic  9(07)                  .
               10  w-tes-loc-nsc          pic  x(40)                  .
               10  w-tes-prv-nsc          pic  x(03)                  .
               10  w-tes-sex-fnt          pic  9(02)                  .
               10  w-tes-snx-rsi          pic  9(02)                  .
               10  w-tes-snx-tpv          pic  9(02)                  .
               10  w-tes-cod-cau          pic  9(05)                  .
               10  w-tes-cod-cau-des      pic  x(40)                  .
               10  w-tes-cod-cau-per      pic  9(03)v9(02)            .
               10  w-tes-cod-tcb          pic  9(05)                  .
               10  w-tes-cod-tcb-des      pic  x(40)                  .
               10  w-tes-cod-tcb-per      pic  9(03)v9(02)            .
               10  w-tes-cod-trb          pic  9(05)                  .
               10  w-tes-cod-trb-des      pic  x(40)                  .
               10  w-tes-cod-trb-per      pic  9(03)v9(02)            .
               10  w-tes-alx-exp.
                   15  filler  occurs 197 pic  x(01)                  .

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
      *            * - 0000001..9999999                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-mco      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzionamento codice, in creazione           *
      *            * - M : Manuale                                     *
      *            * - A : Automatico                                  *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-fco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del mnemonico                     *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-omn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Unicita' del mnemonico                            *
      *            * - N : Non necessariamente unico, si' duplicati    *
      *            * - U : Unico, duplicati non ammessi                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-umn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del mnemonico                     *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-mmn      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Obbligatorieta' del sottoconto contabile          *
      *            * - N : Non obbligatorio                            *
      *            * - O : Obbligatorio                                *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-osc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Modificabilita' del sottoconto contabile          *
      *            * - M : Modificabile                                *
      *            * - N : Non piu' modificabile dopo l'inserimento    *
      *            *---------------------------------------------------*
               10  w-prs-rec-fnt-msc      pic  x(01)                  .
               
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
      *    * Work per Let su archivio [zcr]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larczcr0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [zco]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larczco0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [ztr]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larcztr0.ltw"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/no persona fisica                       *
      *        *-------------------------------------------------------*
           05  w-exp-snx-prf.
               10  w-exp-snx-prf-num      pic  9(02)       value 2    .
               10  w-exp-snx-prf-lun      pic  9(02)       value 02   .
               10  w-exp-snx-prf-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no residenza estero                     *
      *        *-------------------------------------------------------*
           05  w-exp-snx-rsi.
               10  w-exp-snx-rsi-num      pic  9(02)       value 2    .
               10  w-exp-snx-rsi-lun      pic  9(02)       value 02   .
               10  w-exp-snx-rsi-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no trattamento provvigioni              *
      *        *-------------------------------------------------------*
           05  w-exp-snx-tpv.
               10  w-exp-snx-tpv-num      pic  9(02)       value 2    .
               10  w-exp-snx-tpv-lun      pic  9(02)       value 02   .
               10  w-exp-snx-tpv-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Sesso                                      *
      *        *-------------------------------------------------------*
           05  w-exp-sex-fnt.
               10  w-exp-sex-fnt-num      pic  9(02)       value 3    .
               10  w-exp-sex-fnt-lun      pic  9(02)       value 40   .
               10  w-exp-sex-fnt-tbl.
                   15  filler             pic  x(40) value
                            "Maschio                                 ".
                   15  filler             pic  x(40) value
                            "Femmina                                 ".
                   15  filler             pic  x(40) value
                            "Non si tratta di persona fisica         ".

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
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da livello precedente             *
      *        *-------------------------------------------------------*
           05  w-ipc-dlp.
      *            *---------------------------------------------------*
      *            * Codice fornitore                                  *
      *            *---------------------------------------------------*
               10  w-ipc-dlp-cod-fnt      pic  9(07)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acl"                   .

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
      *    * Link-area per accettazione codice causale ritenuta        *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnzcr0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo contributo         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnzco0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tributo                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnztr0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(48)                  .

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
                     v-tip                =    "P" or
                     v-tip                =    "V" or
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
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[rec-fnt]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-fnt
           else      move  spaces         to   w-prs-rec-fnt          .
           if        w-prs-rec-fnt-mco    not  numeric
                     move  zero           to   w-prs-rec-fnt-mco      .
           if        w-prs-rec-fnt-mco    =    zero
                     move  9999999        to   w-prs-rec-fnt-mco      .
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
      *              *-------------------------------------------------*
      *              * Estrazione variabili di i.p.c. da livello pre-  *
      *              * cedente                                         *
      *              *-------------------------------------------------*
           perform   ipc-dlp-000          thru ipc-dlp-999            .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore       *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-opn-000  thru cod-mne-fnt-opn-999    .
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
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-mne-zcr-opn-000  thru cod-mne-zcr-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice tipo contributo *
      *              *-------------------------------------------------*
           perform   cod-mne-zco-opn-000  thru cod-mne-zco-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice tributo         *
      *              *-------------------------------------------------*
           perform   cod-mne-ztr-opn-000  thru cod-mne-ztr-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura i.p.c. da livello precedente                      *
      *    *-----------------------------------------------------------*
       ipc-dlp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ipc-dlp-cod-fnt      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se variabile non esistente : oltre              *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ipc-dlp-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valore                          *
      *              *-------------------------------------------------*
           move      s-num                to   w-ipc-dlp-cod-fnt      .
       ipc-dlp-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore      *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-cls-000  thru cod-mne-fnt-cls-999    .
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
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-mne-zcr-cls-000  thru cod-mne-zcr-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice tipo contrib.  *
      *              *-------------------------------------------------*
           perform   cod-mne-zco-cls-000  thru cod-mne-zco-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice tributo        *
      *              *-------------------------------------------------*
           perform   cod-mne-ztr-cls-000  thru cod-mne-ztr-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [zcr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofzcr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcr                 .
      *              *-------------------------------------------------*
      *              * [zco]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofzco"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zco                 .
      *              *-------------------------------------------------*
      *              * [ztr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofztr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztr                 .
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
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [zcr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofzcr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcr                 .
      *              *-------------------------------------------------*
      *              * [zco]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofzco"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zco                 .
      *              *-------------------------------------------------*
      *              * [ztr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofztr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztr                 .
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
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-cod-fnt    =    zero
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           move      w-ipc-dlp-cod-fnt    to   w-tes-cod-fnt          .
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
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-450.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero,  *
      *                  * si controlla che esista il valore           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-tes-cod-fnt        to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                      *-----------------------------------------*
      *                      * Test di esistenza                       *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-cod-fnt-600.
       acc-cod-fnt-500.
      *                      *-----------------------------------------*
      *                      * Se controllo non superato               *
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
           move      46                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Non esiste alcun fornitore con questo codice !"
                                          to   v-alf                  .
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
      *              * La testata e' composta di nr. 3 pagine          *
      *              *-------------------------------------------------*
           move      3                    to   w-cnt-sts-imp-mpt      .
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
                     acc-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
                     go to acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Si/no persona fisica                        *
      *                  *---------------------------------------------*
           perform   acc-snx-prf-000      thru acc-snx-prf-999        .
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
      *                  * Sesso                                       *
      *                  *---------------------------------------------*
           perform   acc-sex-fnt-000      thru acc-sex-fnt-999        .
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
      *                  * Nome                                        *
      *                  *---------------------------------------------*
           perform   acc-nom-fnt-000      thru acc-nom-fnt-999        .
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
      *                  * Cognome                                     *
      *                  *---------------------------------------------*
           perform   acc-cgn-fnt-000      thru acc-cgn-fnt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
       acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Data di nascita                             *
      *                  *---------------------------------------------*
           perform   acc-dat-nsc-000      thru acc-dat-nsc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-230.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Comune di nascita                           *
      *                  *---------------------------------------------*
           perform   acc-loc-nsc-000      thru acc-loc-nsc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
       acc-tes-reg-255.
      *                  *---------------------------------------------*
      *                  * Provincia di nascita                        *
      *                  *---------------------------------------------*
           perform   acc-prv-nsc-000      thru acc-prv-nsc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-260.
      *                  *---------------------------------------------*
      *                  * Si/no residente estero                      *
      *                  *---------------------------------------------*
           perform   acc-snx-rsi-000      thru acc-snx-rsi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 2                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-260.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Si/no trattamento provvigionale             *
      *                  *---------------------------------------------*
           perform   acc-snx-tpv-000      thru acc-snx-tpv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-310.
      *                  *---------------------------------------------*
      *                  * Causale                                     *
      *                  *---------------------------------------------*
           perform   acc-cod-cau-000      thru acc-cod-cau-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-320.
      *                  *---------------------------------------------*
      *                  * Codice tipo contributo                      *
      *                  *---------------------------------------------*
           perform   acc-cod-tcb-000      thru acc-cod-tcb-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-310.
       acc-tes-reg-330.
      *                  *---------------------------------------------*
      *                  * Codice tributario                           *
      *                  *---------------------------------------------*
           perform   acc-cod-trb-000      thru acc-cod-trb-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
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
                     vis-tes-reg-300
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
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no persona fisica                            *
      *              *-------------------------------------------------*
           perform   vis-snx-prf-000      thru vis-snx-prf-999        .
      *              *-------------------------------------------------*
      *              * Sesso                                           *
      *              *-------------------------------------------------*
           perform   vis-sex-fnt-000      thru vis-sex-fnt-999        .
      *              *-------------------------------------------------*
      *              * Nome                                            *
      *              *-------------------------------------------------*
           perform   vis-nom-fnt-000      thru vis-nom-fnt-999        .
      *              *-------------------------------------------------*
      *              * Cognome                                         *
      *              *-------------------------------------------------*
           perform   vis-cgn-fnt-000      thru vis-cgn-fnt-999        .
      *              *-------------------------------------------------*
      *              * Data di nascita                                 *
      *              *-------------------------------------------------*
           perform   vis-dat-nsc-000      thru vis-dat-nsc-999        .
      *              *-------------------------------------------------*
      *              * Comune di nascita                               *
      *              *-------------------------------------------------*
           perform   vis-loc-nsc-000      thru vis-loc-nsc-999        .
      *              *-------------------------------------------------*
      *              * Provincia di nascita                            *
      *              *-------------------------------------------------*
           perform   vis-prv-nsc-000      thru vis-prv-nsc-999        .
      *              *-------------------------------------------------*
      *              * Si/no residente estero                          *
      *              *-------------------------------------------------*
           perform   vis-snx-rsi-000      thru vis-snx-rsi-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Si/no trattamento provvigionale                 *
      *              *-------------------------------------------------*
           perform   vis-snx-tpv-000      thru vis-snx-tpv-999        .
      *              *-------------------------------------------------*
      *              * Causale                                         *
      *              *-------------------------------------------------*
           perform   vis-cod-cau-000      thru vis-cod-cau-999        .
           perform   vis-cod-cau-des-000  thru vis-cod-cau-des-999    .
           perform   vis-cod-cau-per-000  thru vis-cod-cau-per-999    .
      *              *-------------------------------------------------*
      *              * Codice tipo contributo                          *
      *              *-------------------------------------------------*
           perform   vis-cod-tcb-000      thru vis-cod-tcb-999        .
           perform   vis-cod-tcb-des-000  thru vis-cod-tcb-des-999    .
           perform   vis-cod-tcb-per-000  thru vis-cod-tcb-per-999    .
      *              *-------------------------------------------------*
      *              * Codice tributario                               *
      *              *-------------------------------------------------*
           perform   vis-cod-trb-000      thru vis-cod-trb-999        .
           perform   vis-cod-trb-des-000  thru vis-cod-trb-des-999    .
           perform   vis-cod-trb-per-000  thru vis-cod-trb-per-999    .
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
                     pmt-tes-reg-300
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
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no persona fisica                            *
      *              *-------------------------------------------------*
           perform   pmt-snx-prf-000      thru pmt-snx-prf-999        .
      *              *-------------------------------------------------*
      *              * Sesso                                           *
      *              *-------------------------------------------------*
           perform   pmt-sex-fnt-000      thru pmt-sex-fnt-999        .
      *              *-------------------------------------------------*
      *              * Nome                                            *
      *              *-------------------------------------------------*
           perform   pmt-nom-fnt-000      thru pmt-nom-fnt-999        .
      *              *-------------------------------------------------*
      *              * Cognome                                         *
      *              *-------------------------------------------------*
           perform   pmt-cgn-fnt-000      thru pmt-cgn-fnt-999        .
      *              *-------------------------------------------------*
      *              * Data di nascita                                 *
      *              *-------------------------------------------------*
           perform   pmt-dat-nsc-000      thru pmt-dat-nsc-999        .
      *              *-------------------------------------------------*
      *              * Comune di nascita                               *
      *              *-------------------------------------------------*
           perform   pmt-loc-nsc-000      thru pmt-loc-nsc-999        .
      *              *-------------------------------------------------*
      *              * Si/no residente estero                          *
      *              *-------------------------------------------------*
           perform   pmt-snx-rsi-000      thru pmt-snx-rsi-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Si/no trattamento provvigionale                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-tpv-000      thru pmt-snx-tpv-999        .
      *              *-------------------------------------------------*
      *              * Causale                                         *
      *              *-------------------------------------------------*
           perform   pmt-cod-cau-000      thru pmt-cod-cau-999        .
           perform   pmt-cod-cau-per-000  thru pmt-cod-cau-per-999    .
      *              *-------------------------------------------------*
      *              * Codice tipo contributo                          *
      *              *-------------------------------------------------*
           perform   pmt-cod-tcb-000      thru pmt-cod-tcb-999        .
           perform   pmt-cod-tcb-per-000  thru pmt-cod-tcb-per-999    .
      *              *-------------------------------------------------*
      *              * Codice tributario                               *
      *              *-------------------------------------------------*
           perform   pmt-cod-trb-000      thru pmt-cod-trb-999        .
           perform   pmt-cod-trb-per-000  thru pmt-cod-trb-per-999    .
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
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT"
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
      *    * Visualizzazione prompt : Si/no persona fisica             *
      *    *-----------------------------------------------------------*
       pmt-snx-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Persona fisica             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sesso                            *
      *    *-----------------------------------------------------------*
       pmt-sex-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sesso                      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sex-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Nome                             *
      *    *-----------------------------------------------------------*
       pmt-nom-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome                       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nom-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Cognome                          *
      *    *-----------------------------------------------------------*
       pmt-cgn-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cognome                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cgn-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data di nascita                  *
      *    *-----------------------------------------------------------*
       pmt-dat-nsc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data di nascita            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-nsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Comune di nascita                *
      *    *-----------------------------------------------------------*
       pmt-loc-nsc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Localita' di nascita       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-loc-nsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no residente estero           *
      *    *-----------------------------------------------------------*
       pmt-snx-rsi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Residente all'estero       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-rsi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no trattamento provvigionale  *
      *    *-----------------------------------------------------------*
       pmt-snx-tpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Trattamento provvigionale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-tpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale                          *
      *    *-----------------------------------------------------------*
       pmt-cod-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Causale                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : % ritenuta                       *
      *    *-----------------------------------------------------------*
       pmt-cod-cau-per-000.
       pmt-cod-cau-per-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice tipo contributo           *
      *    *-----------------------------------------------------------*
       pmt-cod-tcb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo contributo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-tcb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : % contributo                     *
      *    *-----------------------------------------------------------*
       pmt-cod-tcb-per-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-tcb-per-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice tributario                *
      *    *-----------------------------------------------------------*
       pmt-cod-trb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tributario          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-trb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : % ritenuta                       *
      *    *-----------------------------------------------------------*
       pmt-cod-trb-per-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-trb-per-999.
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
      *    * Accettazione campo : Si/No persona fisica                 *
      *    *-----------------------------------------------------------*
       acc-snx-prf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-prf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-prf-lun    to   v-car                  .
           move      w-exp-snx-prf-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-prf-tbl    to   v-txt                  .
           move      w-tes-snx-prf (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-prf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-prf-999.
       acc-snx-prf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-prf (1)      .
       acc-snx-prf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-snx-prf-600.
           if        v-key                not  = "UP  "
                     go to acc-snx-prf-100.
           if        w-cnt-sts-imp-tes    =    spaces
                     go to acc-snx-prf-600
           else      go to acc-snx-prf-100.
       acc-snx-prf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-prf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-prf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-prf-100.
       acc-snx-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No persona fisica              *
      *    *-----------------------------------------------------------*
       vis-snx-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-prf-lun    to   v-car                  .
           move      w-exp-snx-prf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-prf-tbl    to   v-txt                  .
           move      w-tes-snx-prf (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-prf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Sesso                                *
      *    *-----------------------------------------------------------*
       acc-sex-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-sex-fnt (1)    not  = zero
                     go to acc-sex-fnt-100.
           if        w-tes-snx-prf (1)    =    02
                     go to acc-sex-fnt-100.
           move      03                   to   w-tes-sex-fnt (1)      .
       acc-sex-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sex-fnt-lun    to   v-car                  .
           move      w-exp-sex-fnt-num    to   v-ldt                  .
           move      "MFN#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sex-fnt-tbl    to   v-txt                  .
           move      w-tes-sex-fnt (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sex-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sex-fnt-999.
       acc-sex-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sex-fnt (1)      .
       acc-sex-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-sex-fnt-600.
           if        v-key                not  = "UP  "
                     go to acc-sex-fnt-100.
           if        w-cnt-sts-imp-tes    =    spaces
                     go to acc-sex-fnt-600
           else      go to acc-sex-fnt-100.
       acc-sex-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sex-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sex-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sex-fnt-100.
       acc-sex-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Sesso                             *
      *    *-----------------------------------------------------------*
       vis-sex-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sex-fnt-lun    to   v-car                  .
           move      w-exp-sex-fnt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sex-fnt-tbl    to   v-txt                  .
           move      w-tes-sex-fnt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sex-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Nome                                 *
      *    *-----------------------------------------------------------*
       acc-nom-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nom-fnt-100.
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
           move      w-tes-nom-fnt (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nom-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nom-fnt-999.
       acc-nom-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-nom-fnt (1)      .
       acc-nom-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nom-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nom-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nom-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nom-fnt-100.
       acc-nom-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nome                              *
      *    *-----------------------------------------------------------*
       vis-nom-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-nom-fnt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Cognome                              *
      *    *-----------------------------------------------------------*
       acc-cgn-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cgn-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cgn-fnt (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cgn-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cgn-fnt-999.
       acc-cgn-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cgn-fnt (1)      .
       acc-cgn-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cgn-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cgn-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cgn-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cgn-fnt-100.
       acc-cgn-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Cognome                           *
      *    *-----------------------------------------------------------*
       vis-cgn-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cgn-fnt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cgn-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data di nascita                      *
      *    *-----------------------------------------------------------*
       acc-dat-nsc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-nsc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dat-nsc (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-nsc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-nsc-999.
       acc-dat-nsc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-nsc (1)      .
       acc-dat-nsc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-nsc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-nsc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-nsc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-nsc-100.
       acc-dat-nsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data di nascita                   *
      *    *-----------------------------------------------------------*
       vis-dat-nsc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dat-nsc (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-nsc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Comune di nascita                    *
      *    *-----------------------------------------------------------*
       acc-loc-nsc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-loc-nsc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-loc-nsc (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-loc-nsc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-loc-nsc-999.
       acc-loc-nsc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-loc-nsc (1)      .
       acc-loc-nsc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-loc-nsc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-loc-nsc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-loc-nsc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-loc-nsc-100.
       acc-loc-nsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Comune di nascita                 *
      *    *-----------------------------------------------------------*
       vis-loc-nsc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-loc-nsc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-loc-nsc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Provincia di nascita                 *
      *    *-----------------------------------------------------------*
       acc-prv-nsc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prv-nsc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-prv-nsc (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prv-nsc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prv-nsc-999.
       acc-prv-nsc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-prv-nsc (1)      .
       acc-prv-nsc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prv-nsc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prv-nsc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prv-nsc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prv-nsc-100.
       acc-prv-nsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Provincia di nascita              *
      *    *-----------------------------------------------------------*
       vis-prv-nsc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-prv-nsc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prv-nsc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no residente estero               *
      *    *-----------------------------------------------------------*
       acc-snx-rsi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-rsi (1)    not  = zero
                     go to acc-snx-rsi-100.
           move      01                   to   w-tes-snx-rsi (1)      .
       acc-snx-rsi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rsi-lun    to   v-car                  .
           move      w-exp-snx-rsi-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rsi-tbl    to   v-txt                  .
           move      w-tes-snx-rsi (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rsi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rsi-999.
       acc-snx-rsi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-rsi (1)      .
       acc-snx-rsi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-snx-rsi-600.
           if        v-key                not  = "UP  "
                     go to acc-snx-rsi-100.
           if        w-cnt-sts-imp-tes    =    spaces
                     go to acc-snx-rsi-600
           else      go to acc-snx-rsi-100.
       acc-snx-rsi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-rsi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-rsi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-rsi-100.
       acc-snx-rsi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no residente estero            *
      *    *-----------------------------------------------------------*
       vis-snx-rsi-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rsi-lun    to   v-car                  .
           move      w-exp-snx-rsi-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rsi-tbl    to   v-txt                  .
           move      w-tes-snx-rsi (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-rsi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no trattamento provvigioni        *
      *    *-----------------------------------------------------------*
       acc-snx-tpv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-tpv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tpv-lun    to   v-car                  .
           move      w-exp-snx-tpv-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tpv-tbl    to   v-txt                  .
           move      w-tes-snx-tpv (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-tpv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-tpv-999.
       acc-snx-tpv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-tpv (1)      .
       acc-snx-tpv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-snx-tpv-600.
           if        v-key                not  = "UP  "
                     go to acc-snx-tpv-100.
           if        w-cnt-sts-imp-tes    =    spaces
                     go to acc-snx-tpv-600
           else      go to acc-snx-tpv-100.
       acc-snx-tpv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-tpv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-tpv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-tpv-100.
       acc-snx-tpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no trattamento provvigioni     *
      *    *-----------------------------------------------------------*
       vis-snx-tpv-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tpv-lun    to   v-car                  .
           move      w-exp-snx-tpv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tpv-tbl    to   v-txt                  .
           move      w-tes-snx-tpv (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-tpv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice causale                       *
      *    *-----------------------------------------------------------*
       acc-cod-cau-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cau-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcr-ope      .
           move      w-tes-cod-cau (1)    to   w-cod-mne-zcr-cod      .
           move      09                   to   w-cod-mne-zcr-lin      .
           move      30                   to   w-cod-mne-zcr-pos      .
           move      09                   to   w-cod-mne-zcr-dln      .
           move      41                   to   w-cod-mne-zcr-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zcr-cll-000  thru cod-mne-zcr-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcr-foi-000  thru cod-mne-zcr-foi-999    .
       acc-cod-cau-110.
           perform   cod-mne-zcr-cll-000  thru cod-mne-zcr-cll-999    .
           if        w-cod-mne-zcr-ope    =    "F+"
                     go to acc-cod-cau-115.
           if        w-cod-mne-zcr-ope    =    "AC"
                     go to acc-cod-cau-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cau-115.
           perform   cod-mne-zcr-foi-000  thru cod-mne-zcr-foi-999    .
           go to     acc-cod-cau-110.
       acc-cod-cau-120.
           move      w-cod-mne-zcr-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cau-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cau-999.
       acc-cod-cau-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cau (1)      .
       acc-cod-cau-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [zcr]                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-cau (1)    to   w-let-arc-zcr-cod      .
           perform   let-arc-zcr-000      thru let-arc-zcr-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      w-let-arc-zcr-des    to   w-tes-cod-cau-des (1)  .
           move      w-let-arc-zcr-per    to   w-tes-cod-cau-per (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori letti                *
      *                  *---------------------------------------------*
           perform   vis-cod-cau-des-000  thru vis-cod-cau-des-999    .
           perform   vis-cod-cau-per-000  thru vis-cod-cau-per-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-let-arc-zcr-flg    not  = spaces
                     go to acc-cod-cau-100.
       acc-cod-cau-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cau-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cau-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cau-100.
       acc-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice causale                    *
      *    *-----------------------------------------------------------*
       vis-cod-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cau(1)     to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione causale               *
      *    *-----------------------------------------------------------*
       vis-cod-cau-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cau-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cau-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : % ritenuta causale                *
      *    *-----------------------------------------------------------*
       vis-cod-cau-per-000.
       vis-cod-cau-per-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice tipo contributo               *
      *    *-----------------------------------------------------------*
       acc-cod-tcb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tcb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zco-ope      .
           move      w-tes-cod-tcb (1)    to   w-cod-mne-zco-cod      .
           move      11                   to   w-cod-mne-zco-lin      .
           move      30                   to   w-cod-mne-zco-pos      .
           move      11                   to   w-cod-mne-zco-dln      .
           move      41                   to   w-cod-mne-zco-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zco-cll-000  thru cod-mne-zco-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zco-foi-000  thru cod-mne-zco-foi-999    .
       acc-cod-tcb-110.
           perform   cod-mne-zco-cll-000  thru cod-mne-zco-cll-999    .
           if        w-cod-mne-zco-ope    =    "F+"
                     go to acc-cod-tcb-115.
           if        w-cod-mne-zco-ope    =    "AC"
                     go to acc-cod-tcb-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tcb-115.
           perform   cod-mne-zco-foi-000  thru cod-mne-zco-foi-999    .
           go to     acc-cod-tcb-110.
       acc-cod-tcb-120.
           move      w-cod-mne-zco-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-tcb-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-tcb-999.
       acc-cod-tcb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-tcb (1)      .
       acc-cod-tcb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [zco]                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-tcb (1)    to   w-let-arc-zco-cod      .
           perform   let-arc-zco-000      thru let-arc-zco-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      w-let-arc-zco-des    to   w-tes-cod-tcb-des (1)  .
           move      w-let-arc-zco-per    to   w-tes-cod-tcb-per (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori letti                *
      *                  *---------------------------------------------*
           perform   vis-cod-tcb-des-000  thru vis-cod-tcb-des-999    .
           perform   vis-cod-tcb-per-000  thru vis-cod-tcb-per-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-let-arc-zco-flg    not  = spaces
                     go to acc-cod-tcb-100.
       acc-cod-tcb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tcb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-tcb-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-tcb-100.
       acc-cod-tcb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice tipo contributo            *
      *    *-----------------------------------------------------------*
       vis-cod-tcb-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-tcb(1)     to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tcb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tipo contributo       *
      *    *-----------------------------------------------------------*
       vis-cod-tcb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-tcb-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tcb-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : % contributo                      *
      *    *-----------------------------------------------------------*
       vis-cod-tcb-per-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-tes-cod-tcb-per (1)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tcb-per-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice tributo                       *
      *    *-----------------------------------------------------------*
       acc-cod-trb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-trb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-ztr-ope      .
           move      w-tes-cod-trb (1)    to   w-cod-mne-ztr-cod      .
           move      13                   to   w-cod-mne-ztr-lin      .
           move      30                   to   w-cod-mne-ztr-pos      .
           move      13                   to   w-cod-mne-ztr-dln      .
           move      41                   to   w-cod-mne-ztr-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-ztr-cll-000  thru cod-mne-ztr-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-ztr-foi-000  thru cod-mne-ztr-foi-999    .
       acc-cod-trb-110.
           perform   cod-mne-ztr-cll-000  thru cod-mne-ztr-cll-999    .
           if        w-cod-mne-ztr-ope    =    "F+"
                     go to acc-cod-trb-115.
           if        w-cod-mne-ztr-ope    =    "AC"
                     go to acc-cod-trb-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-trb-115.
           perform   cod-mne-ztr-foi-000  thru cod-mne-ztr-foi-999    .
           go to     acc-cod-trb-110.
       acc-cod-trb-120.
           move      w-cod-mne-ztr-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-trb-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-trb-999.
       acc-cod-trb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-trb (1)      .
       acc-cod-trb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [ztr]                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-trb (1)    to   w-let-arc-ztr-cod      .
           perform   let-arc-ztr-000      thru let-arc-ztr-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      w-let-arc-ztr-des    to   w-tes-cod-trb-des (1)  .
           move      w-let-arc-ztr-per    to   w-tes-cod-trb-per (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori letti                *
      *                  *---------------------------------------------*
           perform   vis-cod-trb-des-000  thru vis-cod-trb-des-999    .
           perform   vis-cod-trb-per-000  thru vis-cod-trb-per-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-let-arc-ztr-flg    not  = spaces
                     go to acc-cod-trb-100.
       acc-cod-trb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-trb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-trb-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-trb-100.
       acc-cod-trb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice tributo                    *
      *    *-----------------------------------------------------------*
       vis-cod-trb-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-trb(1)     to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-trb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tributo               *
      *    *-----------------------------------------------------------*
       vis-cod-trb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-trb-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-trb-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : % ritenuta                        *
      *    *-----------------------------------------------------------*
       vis-cod-trb-per-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      13                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-tes-cod-trb-per (1)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-trb-per-999.
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
      *              * Nessun controllo                                *
      *              *-------------------------------------------------*
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-520.
      *                  *---------------------------------------------*
      *                  * Si/no residenza estero                      *
      *                  *---------------------------------------------*
           if        w-tes-snx-rsi (1)    not  = zero
                     go to cnt-tdo-nok-800.
           move      02                   to   w-tes-snx-rsi (1)      .
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
      *    * Box per emissione messaggio di errore                     *
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
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-fax (1)      .
           move      spaces               to   w-tes-num-tlx (1)      .
           move      spaces               to   w-tes-nom-int (1)      .
           move      zero                 to   w-tes-cod-iva (1)      .
           move      spaces               to   w-tes-cod-iva-des (1)  .
           move      zero                 to   w-tes-prt-iva (1)      .
           move      spaces               to   w-tes-cod-fis (1)      .
           move      spaces               to   w-tes-snx-a13 (1)      .
           move      zero                 to   w-tes-cod-cge (1)      .
           move      spaces               to   w-tes-cod-cge-des (1)  .
           move      spaces               to   w-tes-alx-exp (1)      .
      *              *-------------------------------------------------*
      *              * Valori integrativi anagrafica percipiente       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-snx-prf (1)      .
           move      spaces               to   w-tes-nom-fnt (1)      .
           move      spaces               to   w-tes-cgn-fnt (1)      .
           move      zero                 to   w-tes-dat-nsc (1)      .
           move      spaces               to   w-tes-loc-nsc (1)      .
           move      spaces               to   w-tes-prv-nsc (1)      .
           move      zero                 to   w-tes-sex-fnt (1)      .
           move      zero                 to   w-tes-snx-rsi (1)      .
           move      zero                 to   w-tes-snx-tpv (1)      .
           move      zero                 to   w-tes-cod-cau (1)      .
           move      spaces               to   w-tes-cod-cau-des (1)  .
           move      zero                 to   w-tes-cod-cau-per (1)  .
           move      zero                 to   w-tes-cod-tcb (1)      .
           move      spaces               to   w-tes-cod-tcb-des (1)  .
           move      zero                 to   w-tes-cod-tcb-per (1)  .
           move      zero                 to   w-tes-cod-trb (1)      .
           move      spaces               to   w-tes-cod-trb-des (1)  .
           move      zero                 to   w-tes-cod-trb-per (1)  .
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
      *              * Precaricamento dati fornitore                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione dati fornitore              *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Lettura dati fornitore                      *
      *                  *---------------------------------------------*
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
      *                  * Preparazione valori di testata              *
      *                  *---------------------------------------------*
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
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [fnt]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * nazione                         *
      *                              *---------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
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
       rou-let-reg-020.
      *              *-------------------------------------------------*
      *              * Lettura scheda integrativa                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-tes-cod-fnt        to   rf-raa-cod-fnt         .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-060.
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
           move      rf-raa-snx-prf       to   w-tes-snx-prf (1)      .
           move      rf-raa-rag-soc       to   w-tes-rag-soc (1)      .
           move      rf-raa-nom-fnt       to   w-tes-nom-fnt (1)      .
           move      rf-raa-cgn-fnt       to   w-tes-cgn-fnt (1)      .
      *                          *-------------------------------------*
      *                          * Attualmente si forzano i dati letti *
      *                          * dall'anagrafica contabile           *
      *                          *-------------------------------------*
______*    move      rf-raa-via-fnt       to   w-tes-via-fnt (1)      .
______*    move      rf-raa-loc-fnt       to   w-tes-loc-fnt (1)      .
      *
           move      rf-raa-dat-nsc       to   w-tes-dat-nsc (1)      .
           move      rf-raa-loc-nsc       to   w-tes-loc-nsc (1)      .
           move      rf-raa-sex-fnt       to   w-tes-sex-fnt (1)      .
           move      rf-raa-snx-rsi       to   w-tes-snx-rsi (1)      .
           move      rf-raa-snx-tpv       to   w-tes-snx-tpv (1)      .
           move      rf-raa-cod-cau       to   w-tes-cod-cau (1)      .
           move      rf-raa-cod-tcb       to   w-tes-cod-tcb (1)      .
           move      rf-raa-cod-trb       to   w-tes-cod-trb (1)      .
           move      rf-raa-prv-nsc       to   w-tes-prv-nsc (1)      .
           move      rf-raa-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [raa]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [zcr]          *
      *                              *---------------------------------*
           move      w-tes-cod-cau (1)    to   w-let-arc-zcr-cod      .
           perform   let-arc-zcr-000      thru let-arc-zcr-999        .
           move      w-let-arc-zcr-des    to   w-tes-cod-cau-des (1)  .
           move      w-let-arc-zcr-per    to   w-tes-cod-cau-per (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zco]          *
      *                              *---------------------------------*
           move      w-tes-cod-tcb (1)    to   w-let-arc-zco-cod      .
           perform   let-arc-zco-000      thru let-arc-zco-999        .
           move      w-let-arc-zco-des    to   w-tes-cod-tcb-des (1)  .
           move      w-let-arc-zco-per    to   w-tes-cod-tcb-per (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [ztr]          *
      *                              *---------------------------------*
           move      w-tes-cod-trb (1)    to   w-let-arc-ztr-cod      .
           perform   let-arc-ztr-000      thru let-arc-ztr-999        .
           move      w-let-arc-ztr-des    to   w-tes-cod-trb-des (1)  .
           move      w-let-arc-ztr-per    to   w-tes-cod-trb-per (1)  .
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
      *              *-------------------------------------------------*
      *              * Visualizzazione valori di testata               *
      *              *-------------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
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
      *              * Trattamento file [raa]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [raa]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-raa-000      thru wrt-rec-raa-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [raa]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-raa-000      thru rew-rec-raa-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [raa]                             *
      *              *-------------------------------------------------*
           perform   del-rec-raa-000      thru del-rec-raa-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [raa]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-raa-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt        to   rf-raa-cod-fnt         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-raa-ide-dat         .
           move      s-ute                to   rf-raa-ide-ute         .
           move      s-fas                to   rf-raa-ide-fas         .
           move      w-tes-snx-prf (1)    to   rf-raa-snx-prf         .
           move      w-tes-rag-soc (1)    to   rf-raa-rag-soc         .
           move      w-tes-nom-fnt (1)    to   rf-raa-nom-fnt         .
           move      w-tes-cgn-fnt (1)    to   rf-raa-cgn-fnt         .
           move      w-tes-via-fnt (1)    to   rf-raa-via-fnt         .
           move      w-tes-loc-fnt (1)    to   rf-raa-loc-fnt         .
           move      w-tes-dat-nsc (1)    to   rf-raa-dat-nsc         .
           move      w-tes-loc-nsc (1)    to   rf-raa-loc-nsc         .
           move      w-tes-prv-nsc (1)    to   rf-raa-prv-nsc         .
           move      w-tes-sex-fnt (1)    to   rf-raa-sex-fnt         .
           move      w-tes-snx-rsi (1)    to   rf-raa-snx-rsi         .
           move      w-tes-snx-tpv (1)    to   rf-raa-snx-tpv         .
           move      w-tes-cod-cau (1)    to   rf-raa-cod-cau         .
           move      w-tes-cod-tcb (1)    to   rf-raa-cod-tcb         .
           move      w-tes-cod-trb (1)    to   rf-raa-cod-trb         .
           move      w-tes-alx-exp (1)    to   rf-raa-alx-exp         .
       cmp-rec-raa-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [raa]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-raa-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-raa-000      thru cmp-rec-raa-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
       wrt-rec-raa-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [raa]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-raa-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-raa-000      thru cmp-rec-raa-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
       rew-rec-raa-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [fnt]                                *
      *    *-----------------------------------------------------------*
       del-rec-raa-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-raa-000      thru cmp-rec-raa-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
       del-rec-raa-999.
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
      *    * Routine di lettura archivio [zcr]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larczcr0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zco]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larczco0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [ztr]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/larcztr0.lts"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acs"                   .

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
      *    * Subroutines per l'accettazione del codice causale         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnzcr0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice tipo contributo *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnzco0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice tributo         *
      *    *-----------------------------------------------------------*
           copy      "pgm/rda/prg/cpy/acmnztr0.acs"                   .

