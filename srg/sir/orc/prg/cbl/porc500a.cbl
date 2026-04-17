       Identification Division.
       Program-Id.                                 porc500a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    sir                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    ric                 *
      *                                   Fase:    orc500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/05/95    *
      *                       Ultima revisione:    NdK del 26/05/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Supporto per la ricezione ordini clienti    *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Scheda anagrafica commerciale cliente       *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    VERSIONE SU MISURA PER AZIENDA MECOM        *
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
                     "sir"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "orc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "ric"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "orc500"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "porc500a"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   SCHEDA SINTETICA ANAGRAFICA CLIENTE  "       .

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
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-cli          pic  9(07)                  .
               10  w-tes-cod-cli-des      pic  x(40)                  .
               10  w-tes-cod-cli-naz      pic  x(03)                  .
               10  w-tes-dpz-cli          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-rag-dcc          pic  x(40)                  .
               10  w-tes-via-dcc          pic  x(40)                  .
               10  w-tes-loc-dcc          pic  x(40)                  .
               10  w-tes-naz-dcc          pic  x(03)                  .
               10  w-tes-prt-iva          pic  9(11)                  .
               10  w-tes-cod-fis          pic  x(16)                  .
               10  w-tes-num-tel          pic  x(20)                  .
               10  w-tes-num-fax          pic  x(20)                  .
               10  w-tes-nom-int          pic  x(30)                  .
               10  w-tes-int-alt          pic  x(30)                  .
               10  w-tes-cod-age          pic  9(07)                  .
               10  w-tes-cod-age-nom      pic  x(20)                  .
               10  w-tes-cod-vet          pic  9(07)                  .
               10  w-tes-cod-vet-rag      pic  x(40)                  .
               10  w-tes-cod-por          pic  x(03)                  .
               10  w-tes-cod-por-des      pic  x(25)                  .
               10  w-tes-cod-fop          pic  9(07)                  .
               10  w-tes-cod-fop-des      pic  x(40)                  .
               10  w-tes-cod-abi          pic  9(05)                  .
               10  w-tes-cod-abi-den      pic  x(40)                  .
               10  w-tes-cod-cab          pic  9(05)                  .
               10  w-tes-cod-cab-den      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
           05  filler                     pic  x(01)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-tle      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-abi      pic  9(05)                  .
               10  w-let-arc-dcc-cab      pic  9(05)                  .
               10  w-let-arc-dcc-ban      pic  x(10)                  .
               10  w-let-arc-dcc-ccp      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [vet]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-vet.
               10  w-let-arc-vet-flg      pic  x(01)                  .
               10  w-let-arc-vet-cod      pic  9(07)                  .
               10  w-let-arc-vet-rag      pic  x(40)                  .
               10  w-let-arc-vet-via      pic  x(40)                  .
               10  w-let-arc-vet-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti dipendenze per il cliente    *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcc.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il cliente   commerciale ha dipendenze  *
      *            * - N : No, il cliente   commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente   commerciale                      *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il cliente   *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-dpz      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per voci descrittive in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-des-zvf.
               10  w-det-des-zvf-num      pic  9(01)                  .
               10  w-det-des-zvf-cod      pic  x(03)                  .
               10  w-det-des-zvf-lng      pic  x(03)                  .
               10  w-det-des-zvf-des      pic  x(25)                  .

      *    *===========================================================*
      *    * Work per ridefinizioni partita iva / codice fiscale       *
      *    *-----------------------------------------------------------*
       01  w-piv-cfi.
      *        *-------------------------------------------------------*
      *        * Partita iva in rappresentazione numerica              *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-piv-num          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Partita iva in rappresentazione alfanumerica          *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-piv-alf redefines
               w-piv-cfi-piv-num.
               10  w-piv-cfi-piv-cha occurs 11
                                          pic  x(01)                  .
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
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-box-msg-err.
               10  w-box-msg-err-msg      pic  x(60)                  .

      *    *===========================================================*
      *    * Work per subroutines di Rea(d)                            *
      *    *-----------------------------------------------------------*
       01  w-rea.
      *        *-------------------------------------------------------*
      *        * Per Read record da [cli]                              *
      *        *-------------------------------------------------------*
           05  w-rea-rec-cli.
               10  w-rea-rec-cli-flg      pic  x(01)                  .
               10  w-rea-rec-cli-cod      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per Read record da [dcc]                              *
      *        *-------------------------------------------------------*
           05  w-rea-rec-dcc.
               10  w-rea-rec-dcc-flg      pic  x(01)                  .
               10  w-rea-rec-dcc-cod      pic  9(07)                  .
               10  w-rea-rec-dcc-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

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
      *              *-------------------------------------------------*
      *              * Open modulo accettazione cliente commerciale    *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione cliente commerciale   *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           perform   acc-dpz-cli-000      thru acc-dpz-cli-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
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
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-key-reg-200.
       vis-key-reg-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *                  *---------------------------------------------*
      *                  * Ragione sociale a fianco del codice cliente *
      *                  *---------------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale segnale di cliente con dipendenze *
      *                  *---------------------------------------------*
           perform   vis-snd-dcc-000      thru vis-snd-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-key-reg-900.
       vis-key-reg-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in presenza del codice dipenden- *
      *                  * za                                          *
      *                  *---------------------------------------------*
           if        w-tes-dpz-cli        not  = spaces
                     go to vis-key-reg-240.
       vis-key-reg-220.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a spaces               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *                      *-----------------------------------------*
      *                      * Ragione sociale cliente                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cli-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Eventuale segnale di cliente con dipen- *
      *                      * za                                      *
      *                      *-----------------------------------------*
           perform   vis-snd-dcc-000      thru vis-snd-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     vis-key-reg-900.
       vis-key-reg-240.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a non spaces           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing codice cliente                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Concatenamento con codice dipendenza    *
      *                      *-----------------------------------------*
           move      12                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      v-edt                to   w-all-str-cat (1)      .
           move      "-"                  to   w-all-str-cat (2)      .
           move      w-tes-dpz-cli        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione stringa ottenuta        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale cliente                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cli-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     vis-key-reg-900.
       vis-key-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-key-reg-999.
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to pmt-key-reg-200.
       pmt-key-reg-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per codice cliente                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice contabile  cliente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per codice dipendenza                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice dipendenza cliente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-900.
       pmt-key-reg-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per codice cliente                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cliente    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-900.
       pmt-key-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-key-reg-999.
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice cliente                *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-cod-cli        to   w-cod-mne-dcc-cod      .
           move      04                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      04                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-cli-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cli          .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record da file [cli]                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-rea-rec-cli-cod      .
           perform   rea-rec-cli-000      thru rea-rec-cli-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice nazione              *
      *                  *---------------------------------------------*
           move      rf-cli-cod-naz       to   w-tes-cod-cli-naz      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione descrizione                 *
      *                  *---------------------------------------------*
           move      rf-cli-rag-soc       to   w-tes-cod-cli-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione Partita Iva                 *
      *                  *---------------------------------------------*
           move      rf-cli-prt-iva       to   w-tes-prt-iva (1)      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione Codice Fiscale              *
      *                  *---------------------------------------------*
           move      rf-cli-cod-fis       to   w-tes-cod-fis (1)      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-cod-cli-410
           else      go to acc-cod-cli-450.
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore del   *
      *                      * codice dipendenza                       *
      *                      *-----------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to acc-cod-cli-415
           else      go to acc-cod-cli-420.
       acc-cod-cli-415.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a spaces           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura tasto 'Do' e uscita       *
      *                          *-------------------------------------*
           move      "DO  "               to   v-key                  .
           go to     acc-cod-cli-800.
       acc-cod-cli-420.
      *                      *-----------------------------------------*
      *                      * Se codice dipendenza a non-spaces       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura codice dipendenza a Spa-  *
      *                          * ces con visualizzazione             *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-dpz-cli          .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice dipendenza   *
      *                          *-------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-450.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' diverso da zero   *
      *                  *---------------------------------------------*
       acc-cod-cli-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso da quello   *
      *                      * precedente si forza il codice dipenden- *
      *                      * za a Spaces e lo si visualizza          *
      *                      *-----------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to acc-cod-cli-460.
           move      spaces               to   w-tes-dpz-cli          .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
       acc-cod-cli-460.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione dell'esito della *
      *                      * lettura anagrafica contabile clienti    *
      *                      *-----------------------------------------*
           if        w-rea-rec-cli-flg    =    spaces
                     go to acc-cod-cli-465
           else      go to acc-cod-cli-470.
       acc-cod-cli-465.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-470.
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il cliente commerciale                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-det-snd-dcc-cli      .
           perform   det-snd-dcc-000      thru det-snd-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale segnale di cliente con dipendenze *
      *                  *---------------------------------------------*
           perform   vis-snd-dcc-000      thru vis-snd-dcc-999        .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-cli-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-cli-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice cliente             *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-cod-cli-200.
       vis-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-900.
       vis-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-900.
       vis-cod-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-cod-cli-999.
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Ragione sociale cliente a  *
      *    *                                fianco del codice cliente  *
      *    *-----------------------------------------------------------*
       vis-des-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cli-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Segnale di cliente con di- *
      *    *                                dipendenze                 *
      *    *-----------------------------------------------------------*
       vis-snd-dcc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-snd-dcc-200.
       vis-snd-dcc-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      80                   to   v-pos                  .
           if        w-det-snd-dcc-snx    =    "S"
                     move  "*"            to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-snd-dcc-900.
       vis-snd-dcc-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      80                   to   v-pos                  .
           if        w-det-snd-dcc-snx    =    "S"
                     move  "*"            to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-snd-dcc-900.
       vis-snd-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-snd-dcc-999.
       vis-snd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice dipendenza             *
      *    *-----------------------------------------------------------*
       acc-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-det-snd-dcc-snx    not  = "S"
                     go to acc-dpz-cli-999.
       acc-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      w-tes-cod-cli        to   w-cod-cod-dcc-cli      .
           move      w-tes-dpz-cli        to   w-cod-cod-dcc-cod      .
           move      05                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      09                   to   w-cod-cod-dcc-rln      .
           move      30                   to   w-cod-cod-dcc-rps      .
           move      zero                 to   w-cod-cod-dcc-vln      .
           move      zero                 to   w-cod-cod-dcc-vps      .
           move      zero                 to   w-cod-cod-dcc-lln      .
           move      zero                 to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-dpz-cli-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-cli-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-cli-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-cli-110.
       acc-dpz-cli-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
       acc-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-dpz-cli          .
       acc-dpz-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-dpz-cli        to   w-all-str-alf          .
           move      04                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-dpz-cli-100.
       acc-dpz-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dpz-cli-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dpz-cli-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dpz-cli-999.
       acc-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice dipendenza          *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-dpz-cli-200.
       vis-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpz-cli        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-dpz-cli-900.
       vis-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-dpz-cli        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-dpz-cli-900.
       vis-dpz-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-dpz-cli-999.
       vis-dpz-cli-999.
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
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
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
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   vis-rag-dcc-000      thru vis-rag-dcc-999        .
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   vis-naz-dcc-000      thru vis-naz-dcc-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   vis-via-dcc-000      thru vis-via-dcc-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   vis-loc-dcc-000      thru vis-loc-dcc-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   vis-prt-iva-000      thru vis-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-fis-000      thru vis-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           perform   vis-num-tel-000      thru vis-num-tel-999        .
      *              *-------------------------------------------------*
      *              * Telefax                                         *
      *              *-------------------------------------------------*
           perform   vis-num-fax-000      thru vis-num-fax-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore                                   *
      *              *-------------------------------------------------*
           perform   vis-nom-int-000      thru vis-nom-int-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore alternativo                       *
      *              *-------------------------------------------------*
           perform   vis-int-alt-000      thru vis-int-alt-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
           perform   vis-nom-age-000      thru vis-nom-age-999        .
      *              *-------------------------------------------------*
      *              * Codice vettore abituale                         *
      *              *-------------------------------------------------*
           perform   vis-cod-vet-000      thru vis-cod-vet-999        .
           perform   vis-cod-vet-rag-000  thru vis-cod-vet-rag-999    .
      *              *-------------------------------------------------*
      *              * Codice 'porto'                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-por-000      thru vis-cod-por-999        .
           perform   vis-cod-por-des-000  thru vis-cod-por-des-999    .
      *              *-------------------------------------------------*
      *              * Codice forma di pagamento                       *
      *              *-------------------------------------------------*
           perform   vis-cod-fop-000      thru vis-cod-fop-999        .
           perform   vis-des-fop-000      thru vis-des-fop-999        .
      *              *-------------------------------------------------*
      *              * Codice A.B.I.                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-abi-000      thru vis-cod-abi-999        .
           perform   vis-den-abi-000      thru vis-den-abi-999        .
      *              *-------------------------------------------------*
      *              * Codice C.A.B.                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-cab-000      thru vis-cod-cab-999        .
           perform   vis-den-cab-000      thru vis-den-cab-999        .
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
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to pmt-tes-reg-020.
       pmt-tes-reg-010.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * A prompts                                   *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-050.
       pmt-tes-reg-020.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * A prompts                                   *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-050.
       pmt-tes-reg-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   pmt-rag-dcc-000      thru pmt-rag-dcc-999        .
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-naz-000      thru pmt-cod-naz-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   pmt-via-dcc-000      thru pmt-via-dcc-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   pmt-loc-dcc-000      thru pmt-loc-dcc-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           perform   pmt-num-tel-000      thru pmt-num-tel-999        .
      *              *-------------------------------------------------*
      *              * Telefax                                         *
      *              *-------------------------------------------------*
           perform   pmt-num-fax-000      thru pmt-num-fax-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore                                   *
      *              *-------------------------------------------------*
           perform   pmt-nom-int-000      thru pmt-nom-int-999        .
      *              *-------------------------------------------------*
      *              * Interlocutore alternativo                       *
      *              *-------------------------------------------------*
           perform   pmt-int-alt-000      thru pmt-int-alt-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Codice vettore abituale                         *
      *              *-------------------------------------------------*
           perform   pmt-cod-vet-000      thru pmt-cod-vet-999        .
      *              *-------------------------------------------------*
      *              * Codice 'porto'                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-por-000      thru pmt-cod-por-999        .
      *              *-------------------------------------------------*
      *              * Codice forma di pagamento                       *
      *              *-------------------------------------------------*
           perform   pmt-cod-fop-000      thru pmt-cod-fop-999        .
      *              *-------------------------------------------------*
      *              * Codice A.B.I.                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-abi-000      thru pmt-cod-abi-999        .
      *              *-------------------------------------------------*
      *              * Codice C.A.B.                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-cab-000      thru pmt-cod-cab-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ragione sociale                  *
      *    *-----------------------------------------------------------*
       pmt-rag-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rag-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice nazione                   *
      *    *-----------------------------------------------------------*
       pmt-cod-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "Naz.:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indirizzo                        *
      *    *-----------------------------------------------------------*
       pmt-via-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indirizzo                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-via-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : C.a.p. e citta'                  *
      *    *-----------------------------------------------------------*
       pmt-loc-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "C.a.p. e citta'            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-loc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Partita iva                      *
      *    *-----------------------------------------------------------*
       pmt-prt-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-naz-dcc (1)    =    spaces or
                     w-tes-naz-dcc (1)    =    "IT"
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
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-naz-dcc (1)    =    spaces or
                     w-tes-naz-dcc (1)    =    "IT"
                     move  spaces         to   v-alf
           else      move  "Codice iva                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Telefono                         *
      *    *-----------------------------------------------------------*
       pmt-num-tel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di telefax          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-fax-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Interlocutore                    *
      *    *-----------------------------------------------------------*
       pmt-nom-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome interlocutore         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nom-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Interlocutore alternativo        *
      *    *-----------------------------------------------------------*
       pmt-int-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome interlocutore 2       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-int-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice agente                    *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si tratta di un cliente *
      *              * principale o di una sua dipendenza              *
      *              *-------------------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to pmt-cod-age-200
           else      go to pmt-cod-age-400.
       pmt-cod-age-200.
      *              *-------------------------------------------------*
      *              * Se cliente principale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-age-999.
       pmt-cod-age-400.
      *              *-------------------------------------------------*
      *              * Se dipendenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-age-999.
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice vettore abituale          *
      *    *-----------------------------------------------------------*
       pmt-cod-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice vettore abituale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice 'porto'                   *
      *    *-----------------------------------------------------------*
       pmt-cod-por-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Porto                      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-por-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice forma di pagamento        *
      *    *-----------------------------------------------------------*
       pmt-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice A.B.I.                    *
      *    *-----------------------------------------------------------*
       pmt-cod-abi-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si tratta di un cliente *
      *              * principale o di una sua dipendenza              *
      *              *-------------------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to pmt-cod-abi-200
           else      go to pmt-cod-abi-400.
       pmt-cod-abi-200.
      *              *-------------------------------------------------*
      *              * Se cliente principale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cod. ABI banca d'appoggio  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-abi-999.
       pmt-cod-abi-400.
      *              *-------------------------------------------------*
      *              * Se dipendenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cod. ABI banca d'appoggio  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-abi-999.
       pmt-cod-abi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice C.A.B.                    *
      *    *-----------------------------------------------------------*
       pmt-cod-cab-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si tratta di un cliente *
      *              * principale o di una sua dipendenza              *
      *              *-------------------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to pmt-cod-cab-200
           else      go to pmt-cod-cab-400.
       pmt-cod-cab-200.
      *              *-------------------------------------------------*
      *              * Se cliente principale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cod. CAB sportello         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-cab-999.
       pmt-cod-cab-400.
      *              *-------------------------------------------------*
      *              * Se dipendenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cod. CAB sportello         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-cab-999.
       pmt-cod-cab-999.
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
      *    * Visualizzazione campo testata : Ragione sociale           *
      *    *-----------------------------------------------------------*
       vis-rag-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rag-dcc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice nazione            *
      *    *-----------------------------------------------------------*
       vis-naz-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tes-naz-dcc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-naz-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Indirizzo                 *
      *    *-----------------------------------------------------------*
       vis-via-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-via-dcc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-via-dcc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : C.a.p. e citta'           *
      *    *-----------------------------------------------------------*
       vis-loc-dcc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-loc-dcc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-loc-dcc-999.
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
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-naz-dcc (1)    =    spaces or
                     w-tes-naz-dcc (1)    =    "IT"
                     move  w-tes-prt-iva (1)
                                          to   v-num
           else      move  zero           to   v-num                  .
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
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-naz-dcc (1)    =    spaces or
                     w-tes-naz-dcc (1)    =    "IT"
                     move  spaces         to   v-alf
           else      move  w-tes-cod-fis (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Telefono                  *
      *    *-----------------------------------------------------------*
       vis-num-tel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-tel (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-tel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Telefax                   *
      *    *-----------------------------------------------------------*
       vis-num-fax-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-fax (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-fax-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Interlocutore             *
      *    *-----------------------------------------------------------*
       vis-nom-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-nom-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Interlocutore alternativo *
      *    *-----------------------------------------------------------*
       vis-int-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-int-alt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-int-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice agente             *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si tratta di cliente    *
      *              * principale o di una dipendenza                  *
      *              *-------------------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to vis-cod-age-200
           else      go to vis-cod-age-400.
       vis-cod-age-200.
      *              *-------------------------------------------------*
      *              * Se cliente principale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-age (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-age-999.
       vis-cod-age-400.
      *              *-------------------------------------------------*
      *              * Se dipendenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-age (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-age-999.
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Nome agente               *
      *    *-----------------------------------------------------------*
       vis-nom-age-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si tratta di cliente    *
      *              * principale o di una dipendenza                  *
      *              *-------------------------------------------------*
           if        w-tes-dpz-cli        =    spaces
                     go to vis-nom-age-200
           else      go to vis-nom-age-400.
       vis-nom-age-200.
      *              *-------------------------------------------------*
      *              * Se cliente principale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-age-nom (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-nom-age-999.
       vis-nom-age-400.
      *              *-------------------------------------------------*
      *              * Se dipendenza                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-age-nom (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-nom-age-999.
       vis-nom-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice vettore abituale   *
      *    *-----------------------------------------------------------*
       vis-cod-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-vet (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ragione sociale vettore   *
      *    *-----------------------------------------------------------*
       vis-cod-vet-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-vet-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-vet-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice 'porto'           *
      *    *-----------------------------------------------------------*
       vis-cod-por-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-por (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-por-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione 'porto'       *
      *    *-----------------------------------------------------------*
       vis-cod-por-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-por-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-por-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Forma di pagamento        *
      *    *-----------------------------------------------------------*
       vis-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fop (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione pagamento     *
      *    *-----------------------------------------------------------*
       vis-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-fop-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice ABI banca          *
      *    *-----------------------------------------------------------*
       vis-cod-abi-000.
      *              *-------------------------------------------------*
      *              * N.B. : la linea a video varia a seconda se      *
      *              *        - cliente principale                     *
      *              *        - dipendenza                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-tes-dpz-cli        =    spaces
                     move  20             to   v-lin
           else      move  20             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-abi (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-abi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Denominazione banca       *
      *    *-----------------------------------------------------------*
       vis-den-abi-000.
      *              *-------------------------------------------------*
      *              * N.B. : la linea a video varia a seconda se      *
      *              *        - cliente principale                     *
      *              *        - dipendenza                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           if        w-tes-dpz-cli        =    spaces
                     move  20             to   v-lin
           else      move  20             to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-abi-den (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-den-abi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice CAB                *
      *    *-----------------------------------------------------------*
       vis-cod-cab-000.
      *              *-------------------------------------------------*
      *              * N.B. : la linea a video varia a seconda se      *
      *              *        - cliente principale                     *
      *              *        - dipendenza                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-tes-dpz-cli        =    spaces
                     move  21             to   v-lin
           else      move  21             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cab (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cab-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Denominazione sportello   *
      *    *-----------------------------------------------------------*
       vis-den-cab-000.
      *              *-------------------------------------------------*
      *              * N.B. : la linea a video varia a seconda se      *
      *              *        - cliente principale                     *
      *              *        - dipendenza                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           if        w-tes-dpz-cli        =    spaces
                     move  21             to   v-lin
           else      move  21             to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cab-den (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-den-cab-999.
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
           if        w-tes-cod-cli        =    zero
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
           move      zero                 to   w-tes-cod-cli          .
           move      spaces               to   w-tes-cod-cli-des      .
           move      spaces               to   w-tes-cod-cli-naz      .
           move      spaces               to   w-tes-dpz-cli          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-rag-dcc (1)      .
           move      spaces               to   w-tes-via-dcc (1)      .
           move      spaces               to   w-tes-loc-dcc (1)      .
           move      spaces               to   w-tes-naz-dcc (1)      .
           move      zero                 to   w-tes-prt-iva (1)      .
           move      spaces               to   w-tes-cod-fis (1)      .
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-fax (1)      .
           move      spaces               to   w-tes-nom-int (1)      .
           move      spaces               to   w-tes-int-alt (1)      .
           move      zero                 to   w-tes-cod-age (1)      .
           move      spaces               to   w-tes-cod-age-nom (1)  .
           move      zero                 to   w-tes-cod-vet (1)      .
           move      spaces               to   w-tes-cod-vet-rag (1)  .
           move      spaces               to   w-tes-cod-por (1)      .
           move      spaces               to   w-tes-cod-por-des (1)  .
           move      zero                 to   w-tes-cod-fop (1)      .
           move      spaces               to   w-tes-cod-fop-des (1)  .
           move      zero                 to   w-tes-cod-abi (1)      .
           move      spaces               to   w-tes-cod-abi-den (1)  .
           move      zero                 to   w-tes-cod-cab (1)      .
           move      spaces               to   w-tes-cod-cab-den (1)  .
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
       rou-let-reg-010.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica commerciale da trattare      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-tes-cod-cli        to   rf-dcc-cod-cli         .
           move      w-tes-dpz-cli        to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-400.
       rou-let-reg-020.
      *              *-------------------------------------------------*
      *              * Se anagrafica commerciale da trattare non esi-  *
      *              * stente                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se anagrafica del cli- *
      *                  * ente principale o di una sua dipendenza     *
      *                  *---------------------------------------------*
           if        w-tes-dpz-cli        not  = spaces
                     go to rou-let-reg-100.
       rou-let-reg-040.
      *                  *---------------------------------------------*
      *                  * Se anagrafica del cliente principale        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se anagrafica di una sua dipendenza         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-400.
      *              *-------------------------------------------------*
      *              * Se anagrafica commerciale da trattare esistente *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se anagrafica del cli- *
      *                  * ente principale o di una sua dipendenza     *
      *                  *---------------------------------------------*
           if        w-tes-dpz-cli        not  = spaces
                     go to rou-let-reg-600.
       rou-let-reg-420.
      *                  *---------------------------------------------*
      *                  * Se anagrafica del cliente principale        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Ripresa valori da record [dcc] letto    *
      *                      * per il cliente principale               *
      *                      *-----------------------------------------*
           perform   rou-let-reg-cli-000  thru rou-let-reg-cli-999    .
       rou-let-reg-460.
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica per il     *
      *                      * cliente principale                      *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Se anagrafica di una dipendenza             *
      *                  *---------------------------------------------*
       rou-let-reg-620.
      *                      *-----------------------------------------*
      *                      * Ripresa valori da record [dcc] letto    *
      *                      * per la dipendenza                       *
      *                      *-----------------------------------------*
           perform   rou-let-reg-dpz-000  thru rou-let-reg-dpz-999    .
       rou-let-reg-660.
      *                      *-----------------------------------------*
      *                      * Lettura dell'anagrafica commerciale del *
      *                      * cliente principale per verificarne      *
      *                      * l'esistenza                             *
      *                      *-----------------------------------------*
           move      w-tes-cod-cli        to   w-rea-rec-dcc-cod      .
           move      spaces               to   w-rea-rec-dcc-dpz      .
           perform   rea-rec-dcc-000      thru rea-rec-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Deviazione secondo l'esito della lettu- *
      *                      * ra                                      *
      *                      *-----------------------------------------*
           if        w-rea-rec-dcc-flg    =    spaces
                     go to rou-let-reg-700.
       rou-let-reg-680.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale non esistente                *
      *                      *-----------------------------------------*
           move      "Manca l'anagrafica commerciale del cliente princip
      -              "ale       "         to   w-box-msg-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Status di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-700.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale esistente                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo funzionamento : Modifica       *
      *                          *-------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-720.
      *                          *-------------------------------------*
      *                          * Preparazione valori relativi alla   *
      *                          * dipendenza, ma contenuti nel re-    *
      *                          * cord del cliente principale         *
      *                          *-------------------------------------*
           perform   rou-let-reg-dmd-000  thru rou-let-reg-dmd-999    .
       rou-let-reg-740.
      *                          *-------------------------------------*
      *                          * Valori precedenti anagrafica per la *
      *                          * dipendenza                          *
      *                          *-------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se lo status di uscita e' ad errore : si e- *
      *                  * sce immediatamente                          *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to rou-let-reg-999.
      *                  *---------------------------------------------*
      *                  * Se si e' in modifica : si forza la sola vi- *
      *                  * sualizzazione e si esce immediatamente      *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"
                     move  "V"            to   w-cnt-mfu-tip-fun
                     go to rou-let-reg-999.
      *                  *---------------------------------------------*
      *                  * Se si e' in inserimento : lo si rifiuta e-  *
      *                  * mettendo un messaggio di errore             *
      *                  *---------------------------------------------*
           move      "Non e' consentita la funzione di inserimento !    
      -              "          "         to   w-box-msg-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-rou-let-reg      .
           go to     rou-let-reg-999.
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Cliente principale                                        *
      *    *-----------------------------------------------------------*
       rou-let-reg-cli-000.
      *              *-------------------------------------------------*
      *              * Ripresa valori da record [dcc] letto            *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-tes-rag-dcc (1)      .
           move      rf-dcc-via-dcc       to   w-tes-via-dcc (1)      .
           move      rf-dcc-loc-dcc       to   w-tes-loc-dcc (1)      .
           move      rf-dcc-cod-naz       to   w-tes-naz-dcc (1)      .
           move      rf-dcc-cod-age       to   w-tes-cod-age (1)      .
           move      rf-dcc-cod-vet       to   w-tes-cod-vet (1)      .
           move      rf-dcc-vde-cod (2)   to   w-tes-cod-por (1)      .
           move      rf-dcc-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-dcc-cod-abi       to   w-tes-cod-abi (1)      .
           move      rf-dcc-cod-cab       to   w-tes-cod-cab (1)      .
       rou-let-reg-cli-200.
      *              *-------------------------------------------------*
      *              * Valori contenuti indirettamente in record [dcc] *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome per codice agente                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-nom    to   w-tes-cod-age-nom (1)  .
      *                  *---------------------------------------------*
      *                  * Ragione sociale vettore abituale            *
      *                  *---------------------------------------------*
           move      w-tes-cod-vet (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-tes-cod-vet-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione 'porto'                         *
      *                  *---------------------------------------------*
           move      02                   to   w-det-des-zvf-num      .
           move      w-tes-cod-por (1)    to   w-det-des-zvf-cod      .
           move      rf-dcc-cod-lng       to   w-det-des-zvf-lng      .
           perform   det-des-zvf-000      thru det-des-zvf-999        .
           move      w-det-des-zvf-des    to   w-tes-cod-por-des (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione per codice forma di pagamento   *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop (1)    to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
           move      w-let-arc-zfp-des    to   w-tes-cod-fop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione per codice A.B.I.               *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-cod-abi-den (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione per codice C.A.B.               *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cod-cab (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cod-cab-den (1)  .
      *                  *---------------------------------------------*
      *                  * Lettura contatti [adc]                      *
      *                  *---------------------------------------------*
           perform   rou-let-reg-adc-000  thru rou-let-reg-adc-999    .
       rou-let-reg-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-cli-999.
       rou-let-reg-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Dipendenza                                                *
      *    *-----------------------------------------------------------*
       rou-let-reg-dpz-000.
      *              *-------------------------------------------------*
      *              * Ripresa valori da record [dcc] letto            *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-tes-rag-dcc (1)      .
           move      rf-dcc-via-dcc       to   w-tes-via-dcc (1)      .
           move      rf-dcc-loc-dcc       to   w-tes-loc-dcc (1)      .
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-fax (1)      .
           move      spaces               to   w-tes-nom-int (1)      .
           move      spaces               to   w-tes-int-alt (1)      .
           move      rf-dcc-cod-age       to   w-tes-cod-age (1)      .
           move      rf-dcc-cod-vet       to   w-tes-cod-vet (1)      .
           move      rf-dcc-cod-abi       to   w-tes-cod-abi (1)      .
           move      rf-dcc-cod-cab       to   w-tes-cod-cab (1)      .
       rou-let-reg-dpz-200.
      *              *-------------------------------------------------*
      *              * Valori contenuti indirettamente in reord [dcc]  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome per codice agente                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-nom    to   w-tes-cod-age-nom (1)  .
      *                  *---------------------------------------------*
      *                  * Ragione sociale vettore abituale            *
      *                  *---------------------------------------------*
           move      w-tes-cod-vet (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-tes-cod-vet-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione per codice A.B.I.               *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-cod-abi-den (1)  .
      *                  *---------------------------------------------*
      *                  * Descrizione per codice C.A.B.               *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cod-cab (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cod-cab-den (1)  .
      *                  *---------------------------------------------*
      *                  * Lettura contatti [adc]                      *
      *                  *---------------------------------------------*
           perform   rou-let-reg-adc-000  thru rou-let-reg-adc-999    .
       rou-let-reg-dpz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-dpz-999.
       rou-let-reg-dpz-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Preparazione dei defaults per la modifica della dipen-    *
      *    * denza                                                     *
      *    *-----------------------------------------------------------*
       rou-let-reg-dmd-000.
      *              *-------------------------------------------------*
      *              * Valori letti da anagrafica commerciale cliente  *
      *              * principale da esporre per la modifica della     *
      *              * dipendenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-naz       to   w-tes-naz-dcc (1)      .
           if        w-tes-naz-dcc (1)    =    spaces
                     move  "IT "          to   w-tes-naz-dcc (1)      .
       rou-let-reg-dmd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-dmd-999.
       rou-let-reg-dmd-999.
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
           move      02                   to   d-con-arc-tip-arc      .
           move      w-tes-cod-cli        to   d-con-arc-cod-arc      .
           move      w-tes-dpz-cli        to   d-con-arc-dpz-arc      .
           move      "TEL"                to   d-con-arc-tip-sel      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *              *-------------------------------------------------*
      *              * Bufferizzazione telefono                        *
      *              *-------------------------------------------------*
           move      d-con-arc-num-con (1)
                                          to   w-tes-num-tel (1)      .
           move      d-con-arc-int-con (1)
                                          to   w-tes-nom-int (1)      .
           move      d-con-arc-int-con (2)
                                          to   w-tes-int-alt (1)      .
      *              *-------------------------------------------------*
      *              * Determinazione contatti cliente commerciale     *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      02                   to   d-con-arc-tip-arc      .
           move      w-tes-cod-cli        to   d-con-arc-cod-arc      .
           move      w-tes-dpz-cli        to   d-con-arc-dpz-arc      .
           move      "FAX"                to   d-con-arc-tip-sel      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *              *-------------------------------------------------*
      *              * Bufferizzazione telefono                        *
      *              *-------------------------------------------------*
           move      d-con-arc-num-con (1)
                                          to   w-tes-num-fax (1)      .
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
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
       pos-cnf-ann-999.
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
           move      07                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-box-msg-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      70                   to   v-pos                  .
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
           move      71                   to   v-pos                  .
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
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-nom      .
           go to     let-arc-age-999.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [vet]                         *
      *    *-----------------------------------------------------------*
       let-arc-vet-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-vet-cod    =    zero
                     go to let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET"             to   f-key                  .
           move      w-let-arc-vet-cod    to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-vet-400.
       let-arc-vet-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-vet-rag-soc       to   w-let-arc-vet-rag      .
           move      rf-vet-via-vet       to   w-let-arc-vet-via      .
           move      rf-vet-loc-vet       to   w-let-arc-vet-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-vet-999.
       let-arc-vet-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-vet-flg      .
           move      all   "."            to   w-let-arc-vet-rag      .
           go to     let-arc-vet-600.
       let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-rag      .
       let-arc-vet-600.
           move      spaces               to   w-let-arc-vet-via      .
           move      spaces               to   w-let-arc-vet-loc      .
       let-arc-vet-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zfp]                            *
      *    *-----------------------------------------------------------*
       let-arc-zfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zfp-cod    =    zero
                     go to let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      w-let-arc-zfp-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfp-400.
       let-arc-zfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfp-des-fop       to   w-let-arc-zfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfp-999.
       let-arc-zfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfp-flg      .
           move      all   "."            to   w-let-arc-zfp-des      .
           go to     let-arc-zfp-999.
       let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-des      .
       let-arc-zfp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-999.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice banca a zero                     *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB    "         to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-999.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-999.
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
      *    * Lettura record da archivio [cli], senza area di transito  *
      *    *-----------------------------------------------------------*
       rea-rec-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-rea-rec-cli-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione area record [cli]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      w-rea-rec-cli-cod    to   rf-cli-cod-cli         .
      *              *-------------------------------------------------*
      *              * Se chiave vuota : uscita                        *
      *              *-------------------------------------------------*
           if        rf-cli-cod-cli       =    zero
                     go to rea-rec-cli-999.
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rea-rec-cli-999.
      *              *-------------------------------------------------*
      *              * Flag di uscita : ad errore                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rea-rec-cli-flg      .
      *              *-------------------------------------------------*
      *              * Puntini di segnalazione                         *
      *              *-------------------------------------------------*
           move      all   "."            to   rf-cli-rag-soc         .
       rea-rec-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura record da archivio [dcc], senza area di transito  *
      *    *-----------------------------------------------------------*
       rea-rec-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-rea-rec-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione area record [dcc]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Preaprazione chiave                             *
      *              *-------------------------------------------------*
           move      w-rea-rec-dcc-cod    to   rf-dcc-cod-cli         .
           move      w-rea-rec-dcc-dpz    to   rf-dcc-dpz-cli         .
      *              *-------------------------------------------------*
      *              * Se chiave vuota : uscita                        *
      *              *-------------------------------------------------*
           if        rf-dcc-cod-cli       =    zero   and
                     rf-dcc-dpz-cli       =    spaces
                     go to rea-rec-dcc-999.
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rea-rec-dcc-999.
      *              *-------------------------------------------------*
      *              * Flag di uscita : ad errore                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rea-rec-dcc-flg      .
       rea-rec-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se presenti dipendenze per il cliente      *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcc-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcc-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcc-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-det-snd-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-999.
       det-snd-dcc-100.
      *              *-------------------------------------------------*
      *              * Next su [dcc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Max su [dcc], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcc-cod-cli       not  = w-det-snd-dcc-cli
                     go to det-snd-dcc-800.
       det-snd-dcc-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcc-dpz-cli       =    spaces
                     go to det-snd-dcc-100.
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcc-ctr      .
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcc-ctr    >    1
                     go to det-snd-dcc-500.
           move      rf-dcc-dpz-cli       to   w-det-snd-dcc-dpz      .
       det-snd-dcc-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcc] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcc-100.
       det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcc-ctr    >    zero
                     go to det-snd-dcc-900
           else      go to det-snd-dcc-999.
       det-snd-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcc-snx      .
       det-snd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Descrizione in lingua per la stampa di : voci descrittive *
      *    * in fattura per i clienti                                  *
      *    *-----------------------------------------------------------*
       det-des-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-des-zvf-des      .
       det-des-zvf-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [zvf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua del cliente                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      w-det-des-zvf-lng    to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-200.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua per l'Italia                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-400.
      *              *-------------------------------------------------*
      *              * Uscita con valore a spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione cliente commerciale        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

